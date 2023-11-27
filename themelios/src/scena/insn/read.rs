use gospel::read::{Le as _, Reader};
use snafu::prelude::*;

use super::{Arg, Insn};
use crate::scena::code::visit::visit_args;
use crate::scena::code::Code;
use crate::scena::insn::{Expr, Unit};
use crate::scena::{insn_set as iset, CharId, EventId, FuncId, LookPointId};
use crate::types::*;
use crate::util::{ReaderExt, ValueError};

#[derive(Debug, Snafu)]
pub enum ReadError {
	#[snafu(context(false))]
	Gospel { source: gospel::read::Error },
	#[snafu(context(false))]
	Decode { source: crate::util::DecodeError },
	#[snafu(context(false))]
	Value { source: crate::util::ValueError },
	#[snafu(whatever, display("{message}"))]
	Whatever { message: String },
	#[snafu(display("error reading instruction at {pos}, after {context:?}"))]
	Insn {
		context: Vec<Insn>,
		pos: usize,
		source: Box<ReadError>,
	},
}

type Result<T, E = ReadError> = std::result::Result<T, E>;

pub struct InsnReader<'iset, 'read, 'buf> {
	f: &'read mut Reader<'buf>,
	iset: &'iset iset::InsnSet<'iset>,
}

impl<'iset, 'read, 'buf> InsnReader<'iset, 'read, 'buf> {
	pub fn new(f: &'read mut Reader<'buf>, iset: &'iset iset::InsnSet) -> Self {
		Self { f, iset }
	}

	pub fn pos(&self) -> usize {
		self.f.pos()
	}

	pub fn code(&mut self, end: usize) -> Result<Code> {
		let mut insns = Vec::new();
		while self.pos() < end {
			self.read_insn(&mut insns)?;
		}
		ensure_whatever!(self.pos() == end, "overshot end: {} > {}", self.pos(), end);

		Ok(Code(insns))
	}

	pub fn code_approx(&mut self, end: usize, valid_end: impl Fn(&Reader) -> bool) -> Result<Code> {
		let mut insns = Vec::new();
		let mut extent = self.pos();
		while self.pos() < end {
			let i = self.read_insn(&mut insns)?;
			if i.name == "Return"
				&& self.pos() > extent
				&& (self.f.clone().check_u8(0).is_ok() || valid_end(self.f))
			// TODO This null check is needed for a9000, and does not roundtrip. Eugh.
			{
				break;
			}
			visit_args(i, |arg| {
				if let Arg::Label(l) = arg {
					extent = extent.max(*l);
				}
			});
		}
		ensure_whatever!(self.pos() <= end, "overshot end: {} > {}", self.pos(), end);

		Ok(Code(insns))
	}

	fn read_insn<'a>(&mut self, insns: &'a mut Vec<Insn>) -> Result<&'a mut Insn, ReadError> {
		let pos = self.pos();
		insns.push(Insn::new("_label", vec![Arg::Label(pos)]));
		let insn = self.insn().map_err(Box::new).with_context(|_| InsnSnafu {
			pos,
			context: insns
				.iter()
				.rev()
				.take(10)
				.rev()
				.cloned()
				.collect::<Vec<_>>(),
		})?;
		insns.push(insn);
		Ok(insns.last_mut().unwrap())
	}

	pub fn insn(&mut self) -> Result<Insn> {
		let mut args = Vec::new();
		let byte = self.f.u8()? as usize;
		let name = self.insn_inner(&mut args, &self.iset.insns, byte)?;
		Ok(Insn::new(name, args))
	}

	fn insn_inner(
		&mut self,
		out: &mut Vec<Arg>,
		insns: &'iset [iset::Insn],
		n: usize,
	) -> Result<&'iset str> {
		match insns.get(n) {
			None | Some(iset::Insn::Blank) => whatever!("unknown instruction {n:02X}"),
			Some(iset::Insn::Regular { name, args }) => {
				for arg in args {
					self.arg(out, arg)?;
				}
				Ok(name)
			}
			Some(iset::Insn::Match { head, on, cases }) => {
				for arg in head {
					self.arg(out, arg)?;
				}
				let n = self.int(*on)? as usize;
				self.insn_inner(out, cases, n)
			}
		}
	}

	fn int(&mut self, int: iset::IntType) -> Result<i64> {
		let f = &mut self.f;
		Ok(match int {
			iset::IntType::u8 => f.u8()? as i64,
			iset::IntType::u16 => f.u16()? as i64,
			iset::IntType::u24 => (f.u16()? as i64) | (f.u8()? as i64) << 16,
			iset::IntType::u32 => f.u32()? as i64,
			iset::IntType::i8 => f.i8()? as i64,
			iset::IntType::i16 => f.i16()? as i64,
			iset::IntType::i32 => f.i32()? as i64,
			iset::IntType::Const(v) => v,
			iset::IntType::ED7Battle => f.check_u32(0xFFFFFFFF).is_ok().into(),
		})
	}

	fn arg(&mut self, out: &mut Vec<Arg>, arg: &iset::Arg) -> Result<(), ReadError> {
		match arg {
			iset::Arg::Int(int, ty) => {
				let v = self.int(*int)?;
				out.extend(int_arg(self.iset, v, *ty)?)
			}
			iset::Arg::Misc(ty) => self.misc(out, ty)?,
			iset::Arg::Tuple(args) => {
				let mut values = Vec::new();
				for arg in args {
					self.arg(&mut values, arg)?;
				}
				out.push(Arg::Tuple(values))
			}
		}
		Ok(())
	}

	fn misc(&mut self, out: &mut Vec<Arg>, ty: &iset::MiscArg) -> Result<()> {
		let f = &mut self.f;
		use iset::MiscArg as T;
		match ty {
			T::String => out.push(Arg::String(f.string()?)),
			T::TString => out.push(Arg::TString(TString(f.string()?))),
			T::Text => text(f, out)?,

			T::Pos2 => out.push(Arg::Pos2(Pos2 {
				x: f.i32()?,
				z: f.i32()?,
			})),
			T::Pos3 => out.push(Arg::Pos3(Pos3 {
				x: f.i32()?,
				y: f.i32()?,
				z: f.i32()?,
			})),
			T::RPos3 => out.push(Arg::RPos3(Pos3 {
				x: f.i32()?,
				y: f.i32()?,
				z: f.i32()?,
			})),

			T::Expr => out.push(Arg::Expr(self.expr()?)),

			T::Fork => {
				let len = f.u8()? as usize;
				let pos = f.pos();
				let code = self.code(pos + len)?;
				if len > 0 {
					self.f.check_u8(0)?;
				}
				out.push(Arg::Code(code))
			}

			T::ForkLoop(next) => {
				let len = f.u8()? as usize;
				let pos = f.pos();
				let code = self.code(pos + len)?;

				let insns = [self.insn()?, self.insn()?];
				#[rustfmt::skip]
				ensure_whatever!(
					insns == [
						Insn::new(next, vec![]),
						Insn::new("_goto", vec![Arg::Label(pos)]),
					],
					"invalid ForkLoop: ended with {insns:?}"
				);
				out.push(Arg::Code(code))
			}

			T::SwitchTable(count, case) => {
				let count = self.int(*count)? as usize;
				let mut cs = Vec::with_capacity(count);
				for _ in 0..count {
					self.arg(&mut cs, case)?;
				}
				out.push(Arg::Tuple(cs));
			}

			T::QuestList => loop {
				match f.u16()? {
					0xFFFF => break,
					q => out.push(Arg::Quest(QuestId(q))),
				}
			},

			T::Menu => {
				for line in f.string()?.split_terminator('\x01') {
					out.push(Arg::TString(TString(line.to_owned())))
				}
			}

			T::PartySelectMandatory => {
				let mut v = Vec::with_capacity(4);
				for _ in 0..4 {
					match f.u16()? {
						0xFF => v.push(Arg::Char(CharId::Null)),
						n => v.push(Arg::Name(NameId(n))),
					}
				}
				out.push(Arg::Tuple(v));
			}

			T::PartySelectOptional => loop {
				match f.u16()? {
					0xFFFF => break,
					q => out.push(Arg::Name(NameId(q))),
				}
			},

			T::TcMembers => {
				// TODO Not 100% sure this bit→nameid mapping is correct, but let's assume it is for now
				let mut v = Vec::with_capacity(4);
				let n = f.u32()?;
				for i in 0..32 {
					if n & (1 << i) != 0 {
						v.push(Arg::Name(NameId(i)));
					}
				}
				out.push(Arg::Tuple(v));
			}

			T::ED7CharAnimation => {
				let n = f.u8()?;
				let mut v = Vec::with_capacity(n as usize);
				if n == 0 {
					f.check_u8(0)?;
				}
				for _ in 0..n {
					v.push(Arg::Int(f.u8()? as i64));
				}
				out.push(Arg::Tuple(v));
			}

			T::EvoSave => {
				if self.iset.variant == iset::Variant::Evo {
					self.arg(out, &iset::Arg::Int(iset::IntType::u8, iset::IntArg::Int))?;
				}
			}

			T::KaiSoundId => {
				let int = if self.iset.variant == iset::Variant::Kai {
					iset::IntType::u32
				} else {
					iset::IntType::u16
				};
				self.arg(out, &iset::Arg::Int(int, iset::IntArg::SoundId))?;
			}

			T::FcPartyEquip => {
				let int = if matches!(out[1], Arg::Item(ItemId(600..=799))) {
					iset::IntType::u8
				} else {
					iset::IntType::Const(0)
				};
				self.arg(out, &iset::Arg::Int(int, iset::IntArg::Int))?;
			}

			T::ScPartySetSlot => {
				let int = if matches!(out[1], Arg::Int(0x7F..=0xFE)) {
					iset::IntType::u8
				} else {
					iset::IntType::Const(0)
				};
				self.arg(out, &iset::Arg::Int(int, iset::IntArg::Int))?;
			}

			T::EffPlayPos => {
				let pos = Pos3 {
					x: f.i32()?,
					y: f.i32()?,
					z: f.i32()?,
				};
				if matches!(out[0], Arg::Char(CharId::Null)) {
					out.push(Arg::Pos3(pos))
				} else {
					out.push(Arg::RPos3(pos))
				}
			}
		}
		Ok(())
	}

	fn expr(&mut self) -> Result<Expr> {
		use crate::scena::insn::{Op, Term};
		let mut terms = Vec::new();
		loop {
			let f = &mut self.f;
			let op = f.u8()?;
			let term = if let Ok(op) = Op::try_from(op) {
				Term::Op(op)
			} else {
				match op {
					0x00 => Term::Arg(Arg::Int(f.u32()? as i64)),
					0x01 => break,
					0x1C => Term::Insn(Box::new(self.insn()?)),
					0x1E => Term::Arg(Arg::Flag(Flag(f.u16()?))),
					0x1F => Term::Arg(Arg::Var(f.u16()?)),
					0x20 => Term::Arg(Arg::Attr(f.u8()?)),
					0x21 => Term::Arg(Arg::CharAttr(
						CharId::from_u16(self.iset.game, f.u16()?)?,
						f.u8()?,
					)),
					0x22 => Term::Rand,
					0x23 => Term::Arg(Arg::Global(f.u8()?)),
					op => Err(ValueError::new("Expr", format!("0x{op:02X}")))?,
				}
			};
			terms.push(term);
		}
		Ok(Expr(terms))
	}
}

fn int_arg(iset: &iset::InsnSet, v: i64, ty: iset::IntArg) -> Result<Option<Arg>> {
	use crate::util::cast;
	use iset::IntArg as T;

	Ok(Some(match ty {
		T::Int => Arg::Int(v),
		T::Const(w) => {
			ensure_whatever!(v == w, "{v} != {w}");
			return Ok(None);
		}

		T::Address => Arg::Label(cast(v)?),

		T::Time => Arg::Scalar(cast(v)?, Unit::Time),
		T::Length => Arg::Scalar(cast(v)?, Unit::Length),
		T::Speed => Arg::Scalar(cast(v)?, Unit::Speed),
		T::Angle => Arg::Scalar(cast(v)?, Unit::Angle),
		T::AngularSpeed => Arg::Scalar(cast(v)?, Unit::AngularSpeed),
		T::Angle32 => Arg::Angle32(cast(v)?),
		T::Color => Arg::Color(cast(v)?),

		T::FileId => Arg::File(FileId(cast(v)?)),

		T::BattleId => Arg::Battle(BattleId(cast(v)?)),
		T::BgmId => Arg::Bgm(BgmId(cast(v)?)),
		T::ItemId => Arg::Item(ItemId(cast(v)?)),
		T::MagicId => Arg::Magic(MagicId(cast(v)?)),
		T::NameId => Arg::Name(NameId(cast(v)?)),
		T::QuestId => Arg::Quest(QuestId(cast(v)?)),
		T::RecipeId => Arg::Recipe(RecipeId(cast(v)?)),
		T::ShopId => Arg::Shop(ShopId(cast(v)?)),
		T::SoundId => Arg::Sound(SoundId(cast(v)?)),
		T::TownId => Arg::Town(TownId(cast(v)?)),

		T::FuncId => Arg::Func(FuncId(cast(v & 0xFF)?, cast(v >> 8)?)),
		T::LookPointId => Arg::LookPoint(LookPointId(cast(v)?)),
		T::EventId => Arg::Event(EventId(cast(v)?)),
		T::EntranceId => Arg::Entrance(cast(v)?),
		T::ObjectId => Arg::Object(cast(v)?),

		T::ForkId => Arg::ForkId(cast(v)?),
		T::MenuId => Arg::MenuId(cast(v)?),
		T::EffId => Arg::EffId(cast(v)?),
		T::EffInstanceId => Arg::EffInstanceId(cast(v)?),
		T::ChipId => Arg::ChipId(cast(v)?),
		T::VisId => Arg::VisId(cast(v)?),

		T::CharId => Arg::Char(CharId::from_u16(iset.game, cast(v)?)?),

		T::Flag => Arg::Flag(Flag(cast(v)?)),
		T::Var => Arg::Var(cast(v)?),
		T::Global => Arg::Global(cast(v)?),
		T::Attr => Arg::Attr(cast(v)?),
		T::CharAttr => {
			let char = CharId::from_u16(iset.game, cast(v & 0xFFFF)?)?;
			let attr: u8 = cast(v >> 16)?;
			Arg::CharAttr(char, attr)
		}

		T::QuestTask => Arg::QuestTask(cast(v)?),
		T::QuestFlags => Arg::QuestFlags(cast(v)?),
		T::SystemFlags => Arg::SystemFlags(cast(v)?),
		T::LookPointFlags => Arg::LookPointFlags(cast(v)?),
		T::ObjectFlags => Arg::ObjectFlags(cast(v)?),
		T::EventFlags => Arg::EventFlags(cast(v)?),
		T::CharFlags => Arg::CharFlags(cast(v)?),
		T::CharFlags2 => Arg::CharFlags2(cast(v)?),
	}))
}

fn text(f: &mut Reader, out: &mut Vec<Arg>) -> Result<()> {
	loop {
		let (page, more) = text_page(f)?;
		out.push(Arg::Text(page));
		if !more {
			break;
		}
	}
	Ok(())
}

fn text_page(f: &mut Reader) -> Result<(TString, bool)> {
	let mut buf = String::new();
	let more = loop {
		match f.u8()? {
			0x00 => break false,
			0x01 => buf.push_str("\\n"), // newline → line feed
			0x02 => buf.push_str("\\f"), // pause → page break
			0x03 => break true,          // page break
			0x07 => buf.push_str(&format!("\\{}C", f.u8()?)),
			0x0D => buf.push_str("\\r"),
			0x1F => buf.push_str(&format!("\\{}i", f.u16()?)),
			ch @ (0x00..=0x1F) => buf.push_str(&format!("\\{}x", ch)),
			b'\\' => buf.push_str("\\\\"),
			ch @ 0x20.. => {
				let result = falcom_sjis::decode_char_from(ch, || f.u8().ok());
				match result {
					Ok(ch) => buf.push(ch),
					Err(enc) => {
						for ch in enc {
							buf.push_str(&format!("\\{}x", ch))
						}
					}
				}
			}
		}
	};
	Ok((TString(buf), more))
}
