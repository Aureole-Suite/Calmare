use gospel::read::{Le as _, Reader};
use snafu::prelude::*;
use strict_result::Strict as _;

use super::{Arg, Atom, Code, Expr, Insn};
use crate::gamedata as iset;
use crate::scena::code::visit::visit_labels;
use crate::scena::*;
use crate::types::*;
use crate::util::cast;
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
	#[snafu(display("failed to read argument {pos} ({arg:?}) of {name}"))]
	Arg {
		name: String,
		pos: usize,
		arg: iset::Arg,
		source: Box<ReadError>,
	},
}

type Result<T, E = ReadError> = std::result::Result<T, E>;

pub struct InsnReader<'iset, 'buf> {
	f: Reader<'buf>,
	iset: &'iset iset::InsnSet<'iset>,
}

impl<'iset, 'buf> InsnReader<'iset, 'buf> {
	pub fn new(f: Reader<'buf>, iset: &'iset iset::InsnSet) -> Self {
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
				&& (self.f.clone().check_u8(0).is_ok() || valid_end(&self.f))
			// TODO This null check is needed for a9000, and does not roundtrip. Eugh.
			{
				break;
			}

			visit_labels(i, |arg| extent = extent.max(arg.0));
		}
		ensure_whatever!(self.pos() <= end, "overshot end: {} > {}", self.pos(), end);

		Ok(Code(insns))
	}

	fn read_insn<'a>(&mut self, insns: &'a mut Vec<Insn>) -> Result<&'a mut Insn, ReadError> {
		let pos = self.pos();
		insns.push(Insn::new("_label", vec![Arg::Label(Label(pos))]));
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
				for (i, arg) in args.iter().enumerate() {
					self.arg(out, arg)
						.map_err(Box::new)
						.with_context(|_| ArgSnafu {
							name,
							pos: i,
							arg: arg.clone(),
						})?;
				}
				Ok(name)
			}
			Some(iset::Insn::Match { head, on, cases }) => {
				for (i, arg) in head.iter().enumerate() {
					self.arg(out, arg)
						.map_err(Box::new)
						.with_context(|_| ArgSnafu {
							name: "match head",
							pos: i,
							arg: arg.clone(),
						})?;
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
				out.push(Arg::Atom(int_arg(self.iset, v, *ty)?))
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
		use Atom as A;
		match ty {
			T::Label => {
				let v = self.int(self.iset.address_size)?;
				out.push(Arg::Label(Label(cast(v)?)));
			}

			T::Const(int, w) => {
				let v = self.int(*int)?;
				ensure_whatever!(v == *w, "{v} != {w}");
			}

			T::String => out.push(Arg::Atom(A::String(f.string()?))),
			T::TString => out.push(Arg::Atom(A::TString(TString(f.string()?)))),
			T::Text => text(f, out)?,

			T::Pos2 => out.push(Arg::Atom(A::Pos2(f.pos2()?))),
			T::Pos3 => out.push(Arg::Atom(A::Pos3(f.pos3()?))),
			T::RPos3 => out.push(Arg::Atom(A::RPos3(f.pos3()?))),

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
						Insn::new("_goto", vec![Arg::Label(Label(pos))]),
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
					q => out.push(Arg::Atom(A::QuestId(QuestId(q)))),
				}
			},

			T::Menu => {
				for line in f.string()?.split_terminator('\x01') {
					out.push(Arg::Atom(A::TString(TString(line.to_owned()))))
				}
			}

			T::PartySelectMandatory => {
				let mut v = Vec::with_capacity(4);
				for _ in 0..4 {
					match f.u16()? {
						0xFF => v.push(Arg::Atom(A::CharId(CharId::Null))),
						n => v.push(Arg::Atom(A::NameId(NameId(n)))),
					}
				}
				out.push(Arg::Tuple(v));
			}

			T::PartySelectOptional => loop {
				match f.u16()? {
					0xFFFF => break,
					q => out.push(Arg::Atom(A::NameId(NameId(q)))),
				}
			},

			T::TcMembers => {
				// TODO Not 100% sure this bit→nameid mapping is correct, but let's assume it is for now
				let mut v = Vec::with_capacity(4);
				let n = f.u32()?;
				for i in 0..32 {
					if n & (1 << i) != 0 {
						v.push(Arg::Atom(A::NameId(NameId(i))));
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
					v.push(Arg::Atom(A::Int(f.u8()? as i64)));
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

			T::ED7BattlePos => {
				out.push(Arg::Atom(A::BattleId(BattleId(f.u32()?))));
				// This is remapped later
			}

			T::FcPartyEquip => {
				let int = if matches!(out[1], Arg::Atom(A::ItemId(ItemId(600..=799)))) {
					iset::IntType::u8
				} else {
					iset::IntType::Const(0)
				};
				self.arg(out, &iset::Arg::Int(int, iset::IntArg::Int))?;
			}

			T::ScPartySetSlot => {
				let int = if matches!(out[1], Arg::Atom(A::Int(0x7F..=0xFE))) {
					iset::IntType::u8
				} else {
					iset::IntType::Const(0)
				};
				self.arg(out, &iset::Arg::Int(int, iset::IntArg::Int))?;
			}

			T::EffPlayPos => {
				let pos = f.pos3()?;
				if matches!(out[0], Arg::Atom(A::CharId(CharId::Null))) {
					out.push(Arg::Atom(A::Pos3(pos)))
				} else {
					out.push(Arg::Atom(A::RPos3(pos)))
				}
			}
		}
		Ok(())
	}

	fn expr(&mut self) -> Result<Box<Expr>> {
		use code::{AssOp, Atom as A, BinOp, UnOp};
		let mut stack = Vec::new();
		loop {
			let f = &mut self.f;
			let op = f.u8()?;
			if let Ok(op) = BinOp::try_from(op) {
				let rhs = stack.pop().whatever_context("missing operand").strict()?;
				let lhs = stack.pop().whatever_context("missing operand").strict()?;
				stack.push(Expr::Bin(op, Box::new(lhs), Box::new(rhs)))
			} else if let Ok(op) = UnOp::try_from(op) {
				let arg = stack.pop().whatever_context("missing operand").strict()?;
				stack.push(Expr::Unary(op, Box::new(arg)))
			} else if let Ok(op) = AssOp::try_from(op) {
				let arg = stack.pop().whatever_context("missing operand").strict()?;
				stack.push(Expr::Assign(op, Box::new(arg)))
			} else {
				stack.push(match op {
					0x00 => Expr::Atom(A::Int(f.u32()? as i64)),
					0x01 => break,
					0x1C => Expr::Insn(self.insn()?),
					0x1E => Expr::Atom(A::Flag(Flag(f.u16()?))),
					0x1F => Expr::Atom(A::Var(Var(f.u16()?))),
					0x20 => Expr::Atom(A::Attr(Attr(f.u8()?))),
					0x21 => Expr::Atom(A::CharAttr(CharAttr(
						CharId::from_u16(self.iset.game, f.u16()?)?,
						f.u8()?,
					))),
					0x22 => Expr::Rand,
					0x23 => Expr::Atom(A::Global(Global(f.u8()?))),
					op => Err(ValueError::new("Expr", format!("0x{op:02X}")))?,
				})
			};
		}
		if stack.len() == 1 {
			Ok(Box::new(stack.pop().unwrap()))
		} else {
			whatever!("invalid expression stack size {}", stack.len())
		}
	}
}

fn int_arg(iset: &iset::InsnSet, v: i64, ty: iset::IntArg) -> Result<Atom> {
	use iset::IntArg as T;
	use Atom as A;

	Ok(match ty {
		T::Int => A::Int(v),

		T::Time => A::Time(Time(cast(v)?)),
		T::Length => A::Length(Length(cast(v)?)),
		T::Angle => A::Angle(Angle(cast(v)?)),
		T::Angle32 => A::Angle32(Angle32(cast(v)?)),
		T::Speed => A::Speed(Speed(cast(v)?)),
		T::AngularSpeed => A::AngularSpeed(AngularSpeed(cast(v)?)),
		T::Color => A::Color(Color(cast(v)?)),

		T::FileId => A::FileId(FileId(cast(v)?)),

		T::BattleId => A::BattleId(BattleId(cast(v)?)),
		T::BgmId => A::BgmId(BgmId(cast(v)?)),
		T::ItemId => A::ItemId(ItemId(cast(v)?)),
		T::MagicId => A::MagicId(MagicId(cast(v)?)),
		T::NameId => A::NameId(NameId(cast(v)?)),
		T::QuestId => A::QuestId(QuestId(cast(v)?)),
		T::RecipeId => A::RecipeId(RecipeId(cast(v)?)),
		T::ShopId => A::ShopId(ShopId(cast(v)?)),
		T::SoundId => A::SoundId(SoundId(cast(v)?)),
		T::TownId => A::TownId(TownId(cast(v)?)),

		T::FuncId => A::FuncId(FuncId(cast(v & 0xFF)?, cast(v >> 8)?)),
		T::LookPointId => A::LookPointId(LookPointId(cast(v)?)),
		T::EventId => A::EventId(EventId(cast(v)?)),
		T::EntranceId => A::EntranceId(EntranceId(cast(v)?)),
		T::ObjectId => A::ObjectId(ObjectId(cast(v)?)),

		T::ForkId => A::ForkId(ForkId(cast(v)?)),
		T::MenuId => A::MenuId(MenuId(cast(v)?)),
		T::EffId => A::EffId(EffId(cast(v)?)),
		T::EffInstanceId => A::EffInstanceId(EffInstanceId(cast(v)?)),
		T::ChipId => A::ChipId(ChipId(cast(v)?)),
		T::VisId => A::VisId(VisId(cast(v)?)),

		T::CharId => A::CharId(CharId::from_u16(iset.game, cast(v)?)?),

		T::Flag => A::Flag(Flag(cast(v)?)),
		T::Var => A::Var(Var(cast(v)?)),
		T::Global => A::Global(Global(cast(v)?)),
		T::Attr => A::Attr(Attr(cast(v)?)),
		T::CharAttr => {
			let char = CharId::from_u16(iset.game, cast(v & 0xFFFF)?)?;
			let attr: u8 = cast(v >> 16)?;
			A::CharAttr(CharAttr(char, attr))
		}

		T::QuestTask => A::QuestTask(cast(v)?),
		T::QuestFlags => A::QuestFlags(cast(v)?),
		T::SystemFlags => A::SystemFlags(cast(v)?),
		T::LookPointFlags => A::LookPointFlags(cast(v)?),
		T::ObjectFlags => A::ObjectFlags(cast(v)?),
		T::EventFlags => A::EventFlags(cast(v)?),
		T::CharFlags => A::CharFlags(cast(v)?),
		T::CharFlags2 => A::CharFlags2(cast(v)?),
	})
}

fn text(f: &mut Reader, out: &mut Vec<Arg>) -> Result<()> {
	loop {
		let (page, more) = text_page(f)?;
		out.push(Arg::Atom(Atom::Text(page)));
		if !more {
			break;
		}
	}
	Ok(())
}

fn text_page(f: &mut Reader) -> Result<(Text, bool)> {
	let mut buf = String::new();
	let more = loop {
		match f.u8()? {
			0x00 => break false,
			0x01 => buf.push('\n'), // newline
			0x02 => buf.push('\t'), // pause → tab
			0x03 => break true,     // page break
			0x07 => buf.push_str(&format!("♯{}C", f.u8()?)),
			0x0D => buf.push('\r'),
			0x1F => buf.push_str(&format!("♯{}i", f.u16()?)),
			ch @ (0x00..=0x1F) => buf.push_str(&format!("♯{}x", ch)),
			ch @ 0x20.. => match falcom_sjis::decode_char_from(ch, || f.u8().ok()) {
				Ok('♯') => buf.push_str("♯♯"),
				Ok('㈱') => buf.push('♥'),
				Ok(ch) => buf.push(ch),
				Err(enc) => {
					for ch in enc {
						buf.push_str(&format!("♯{}x", ch))
					}
				}
			},
		}
	};
	Ok((Text(TString(buf)), more))
}
