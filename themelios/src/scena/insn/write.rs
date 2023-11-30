use std::collections::BTreeMap;

use gospel::write::{Label as GLabel, Le as _, Writer};
use snafu::prelude::*;
use strict_result::Strict as _;

use super::{Arg, Expr, Insn};
use crate::scena::code::Code;
use crate::scena::{insn_set as iset, CharId, EventId, FuncId, LookPointId};
use crate::types::*;
use crate::util::{cast, ValueError, WriterExt};

#[derive(Debug, Snafu)]
pub enum WriteError {
	#[snafu(context(false))]
	Gospel { source: gospel::write::Error },
	#[snafu(context(false))]
	Encode { source: crate::util::EncodeError },
	#[snafu(context(false))]
	Value { source: crate::util::ValueError },
	#[snafu(whatever, display("{message}"))]
	Whatever { message: String },
}

pub struct InsnWriter<'iset, 'write> {
	f: &'write mut Writer,
	iset: &'iset iset::InsnSet<'iset>,
	labels: BTreeMap<Label, GLabel>,
	label_no: usize,
	battle_pos: Option<&'iset [GLabel]>,
	brk: Option<Label>,
	cont: Option<Label>,
}

macro_rules! expect {
	($pat:pat in $val:expr, $what:expr) => {
		expect!(Some($pat) = $val.next(), $what)
	};
	($pat:pat = $val:expr, $what:expr) => {
		let val = $val;
		let $pat = val else {
			whatever!("expected {}, got: {:?}", $what, val)
		};
	};
}

impl<'iset, 'write> InsnWriter<'iset, 'write> {
	pub fn new(
		f: &'write mut Writer,
		iset: &'iset iset::InsnSet,
		battle_pos: Option<&'iset [GLabel]>,
	) -> Self {
		InsnWriter {
			f,
			iset,
			labels: BTreeMap::new(),
			label_no: 0,
			battle_pos,
			brk: None,
			cont: None,
		}
	}

	pub fn here(&mut self) -> GLabel {
		self.f.here()
	}

	pub fn code(&mut self, code: &Code) -> Result<(), WriteError> {
		for insn in code.iter() {
			self.insn(insn)?;
		}
		Ok(())
	}

	pub fn insn(&mut self, insn: &Insn) -> Result<(), WriteError> {
		match insn.name.as_str() {
			"_label" => {
				let [Arg::Label(label)] = insn.args.as_slice() else {
					whatever!("malformed label")
				};
				let label = self.label(label);
				self.f.place(label);
			}

			"if" => {
				match insn.args.as_slice() {
					[args@.., Arg::Code(yes), Arg::Code(no)] => {
						let mid = self.internal_label(GLabel::new());
						let end = self.internal_label(GLabel::new());
						let mut args = args.to_vec();
						args.push(Arg::Label(mid));
						self.insn(&Insn::new("_if", args))?;
						self.code(yes)?;
						self.insn(&Insn::new("_goto", vec![Arg::Label(end)]))?;
						self.insn(&Insn::new("_label", vec![Arg::Label(mid)]))?;
						self.code(no)?;
						self.insn(&Insn::new("_label", vec![Arg::Label(end)]))?;
					}
					[args@.., Arg::Code(yes)] => {
						let end = self.internal_label(GLabel::new());
						let mut args = args.to_vec();
						args.push(Arg::Label(end));
						self.insn(&Insn::new("_if", args))?;
						self.code(yes)?;
						self.insn(&Insn::new("_label", vec![Arg::Label(end)]))?;
					}
					_ => whatever!("malformed if")
				}
			}

			"break" => {
				ensure_whatever!(insn.args.is_empty(), "malformed break");
				match self.brk {
					Some(l) => self.insn(&Insn::new("_goto", vec![Arg::Label(l)]))?,
					None => whatever!("can't break here"),
				}
			}

			"continue" => {
				ensure_whatever!(insn.args.is_empty(), "malformed continue");
				match self.cont {
					Some(l) => self.insn(&Insn::new("_goto", vec![Arg::Label(l)]))?,
					None => whatever!("can't continue here"),
				}
			}

			"while" => {
				let [args@.., Arg::Code(body)] = insn.args.as_slice() else {
					whatever!("malformed while")
				};
				let start = self.internal_label(GLabel::new());
				let end = self.internal_label(GLabel::new());

				let brk = self.brk.replace(end);
				let cont = self.cont.replace(start);

				let mut args = args.to_vec();
				args.push(Arg::Label(end));
				self.insn(&Insn::new("_label", vec![Arg::Label(start)]))?;
				self.insn(&Insn::new("_if", args))?;
				self.code(body)?;
				self.insn(&Insn::new("continue", vec![]))?;
				self.insn(&Insn::new("_label", vec![Arg::Label(end)]))?;

				self.brk = brk;
				self.cont = cont;
			}

			name => {
				let Some(iargs) = self.iset.insns_rev.get(name) else {
					whatever!("unknown instruction {name}")
				};
				self.args(&insn.args, iargs)?;
			}
		}
		Ok(())
	}

	fn label(&mut self, label: &Label) -> GLabel {
		*self.labels.entry(*label).or_insert_with(GLabel::new)
	}

	fn internal_label(&mut self, label: GLabel) -> Label {
		let l = Label(!self.label_no);
		self.label_no += 1;
		self.labels.insert(l, label);
		l
	}

	fn args(&mut self, args: &[Arg], iargs: &[iset::Arg]) -> Result<(), WriteError> {
		let mut iter = args.iter();
		for iarg in iargs {
			self.arg(args, iarg, &mut iter)?;
		}
		if let Some(arg) = iter.next() {
			whatever!("too many arguments: {arg:?}")
		};
		Ok(())
	}

	fn arg<'c>(
		&mut self,
		args: &[Arg],
		iarg: &iset::Arg,
		iter: &mut impl Iterator<Item = &'c Arg>,
	) -> Result<(), WriteError> {
		match iarg {
			iset::Arg::Int(int, iset::IntArg::Const(val)) => {
				self.int(*int, *val)?;
			}
			iset::Arg::Int(int, iset::IntArg::Address) => {
				expect!(Arg::Label(l) in iter, "label");
				let label = self.label(l);
				match int {
					iset::IntType::u8 => self.f.label8(label),
					iset::IntType::u16 => self.f.label16(label),
					iset::IntType::u32 => self.f.label32(label),
					_ => whatever!("can't write label as {int:?}"),
				}
			}
			iset::Arg::Int(int, iarg) => {
				let Some(val) = iter.next() else {
					whatever!("too few arguments; expected {iarg:?}");
				};
				let val = int_arg(self.iset, val)?;
				self.int(*int, val)?;
			}
			iset::Arg::Misc(iarg) => {
				self.misc(args, iarg, iter)?;
			}
			iset::Arg::Tuple(args) => {
				expect!(Arg::Tuple(val) in iter, "tuple");
				self.args(val, args)?;
			}
		}
		Ok(())
	}

	fn int(&mut self, int: iset::IntType, val: i64) -> Result<(), WriteError> {
		let f = &mut self.f;
		match int {
			iset::IntType::u8 => f.u8(cast(val)?),
			iset::IntType::u16 => f.u16(cast(val)?),
			iset::IntType::u24 => {
				if !(0..(1 << 24)).contains(&val) {
					Err(ValueError::new("u24", val.to_string()))?
				}
				f.u16(val as u16);
				f.u8((val >> 16) as u8);
			}
			iset::IntType::u32 => f.u32(cast(val)?),
			iset::IntType::i8 => f.i8(cast(val)?),
			iset::IntType::i16 => f.i16(cast(val)?),
			iset::IntType::i32 => f.i32(cast(val)?),
			iset::IntType::Const(v) => {
				ensure_whatever!(val == v, "{val} != {v}");
			}
			iset::IntType::ED7Battle => match val {
				0 => {}
				1 => f.u32(0xFFFFFFFF),
				_ => whatever!("invalid ED7Battle"),
			},
		}
		Ok(())
	}

	fn misc<'c>(
		&mut self,
		args: &[Arg],
		iarg: &iset::MiscArg,
		iter: &mut impl Iterator<Item = &'c Arg>,
	) -> Result<(), WriteError> {
		let f = &mut self.f;
		use iset::MiscArg as T;
		match iarg {
			T::String | T::TString => {
				expect!(Arg::String(s) | Arg::TString(TString(s)) in iter, "string");
				f.string(s)?;
			}
			T::Text => text(f, iter)?,

			T::Pos2 => {
				expect!(Arg::Pos2(p) in iter, "pos2");
				f.i32(p.x);
				f.i32(p.z);
			}
			T::Pos3 | T::RPos3 | T::EffPlayPos => {
				expect!(Arg::Pos3(p) | Arg::RPos3(p) in iter, "pos3");
				f.i32(p.x);
				f.i32(p.y);
				f.i32(p.z);
			}

			T::Expr => {
				expect!(Arg::Expr(e) in iter, "expr");
				self.expr(e)?;
			}

			T::Fork => {
				expect!(Arg::Code(c) in iter, "code");
				let l1 = GLabel::new();
				let l2 = GLabel::new();
				self.f.diff8(l1, l2);
				self.f.place(l1);
				self.code(c)?;
				self.f.place(l2);
				if !c.is_empty() {
					self.f.u8(0);
				}
			}

			T::ForkLoop(next) => {
				expect!(Arg::Code(c) in iter, "code");
				let l1 = GLabel::new();
				let l2 = GLabel::new();
				let l1_ = self.internal_label(l1);
				self.f.diff8(l1, l2);
				self.f.place(l1);
				self.code(c)?;
				self.f.place(l2);
				self.insn(&Insn::new(next, vec![]))?;
				self.insn(&Insn::new("_goto", vec![Arg::Label(l1_)]))?;
			}

			T::SwitchTable(count, case) => {
				expect!(Arg::Tuple(cs) in iter, "switch table");
				self.int(*count, cs.len() as i64)?;
				let mut iter = cs.iter();
				while !iter.as_slice().is_empty() {
					self.arg(cs, case, &mut iter)?;
				}
			}

			T::QuestList | T::PartySelectOptional => {
				for val in iter {
					let val = int_arg(self.iset, val)?;
					self.int(iset::IntType::u16, val)?;
				}
				self.f.u16(0xFFFF);
			}

			T::PartySelectMandatory => {
				expect!(Arg::Tuple(val) in iter, "tuple of 4");
				ensure_whatever!(val.len() == 4, "expected tuple of 4, got {val:?}");
				for val in val {
					if let Arg::Char(CharId::Null) = val {
						f.u16(0xFF);
					} else {
						let val = int_arg(self.iset, val)?;
						f.u16(cast(val)?);
					}
				}
			}

			T::TcMembers => {
				expect!(Arg::Tuple(val) in iter, "tuple of name[]");
				let mut n = 0;
				for val in val {
					let val = int_arg(self.iset, val)?;
					ensure_whatever!(val < 32, "name[] < 32");
					n |= 1 << val;
				}
				f.u32(n);
			}

			T::ED7CharAnimation => {
				expect!(Arg::Tuple(val) in iter, "tuple of int");
				f.u8(cast(val.len())?);
				if val.is_empty() {
					f.u8(0)
				}
				for val in val {
					let val = int_arg(self.iset, val)?;
					f.u8(cast(val)?);
				}
			}

			T::Menu => {
				let mut out = String::new();
				for val in iter {
					expect!((Arg::String(s) | Arg::TString(TString(s))) = val, "string");
					out.push_str(s.as_str());
					out.push('\x01');
				}
				f.string(&out)?;
			}

			T::EvoSave => {
				if self.iset.variant == iset::Variant::Evo {
					let ty = iset::Arg::Int(iset::IntType::u8, iset::IntArg::Int);
					self.arg(args, &ty, iter)?;
				}
			}

			T::KaiSoundId => {
				let int = if self.iset.variant == iset::Variant::Kai {
					iset::IntType::u32
				} else {
					iset::IntType::u16
				};
				self.arg(args, &iset::Arg::Int(int, iset::IntArg::SoundId), iter)?;
			}

			T::ED7BattlePos => {
				expect!(id in iter, "battle id");
				let id = int_arg(self.iset, id)?;
				let label = *self
					.battle_pos
					.unwrap()
					.get(id as usize)
					.whatever_context("battle id out of bounds")
					.strict()?;
				f.label32(label);
			}

			T::FcPartyEquip => {
				let item = int_arg(self.iset, &args[1])?;
				let int = if matches!(&item, 600..=799) {
					iset::IntType::u8
				} else {
					iset::IntType::Const(0)
				};
				self.arg(args, &iset::Arg::Int(int, iset::IntArg::Int), iter)?;
			}

			T::ScPartySetSlot => {
				let item = int_arg(self.iset, &args[1])?;
				let int = if matches!(&item, 0x7F..=0xFE) {
					iset::IntType::u8
				} else {
					iset::IntType::Const(0)
				};
				self.arg(args, &iset::Arg::Int(int, iset::IntArg::Int), iter)?;
			}
		}
		Ok(())
	}

	fn expr(&mut self, expr: &Expr) -> Result<(), WriteError> {
		use crate::scena::insn::Term;
		for term in &expr.0 {
			match *term {
				Term::Arg(Arg::Int(n)) => {
					self.f.u8(0x00);
					self.f.u32(cast(n)?);
				}
				Term::Op(op) => self.f.u8(op.into()),
				Term::Insn(ref insn) => {
					self.f.u8(0x1C);
					self.insn(insn)?;
				}
				Term::Arg(Arg::Flag(Flag(v))) => {
					self.f.u8(0x1E);
					self.f.u16(v);
				}
				Term::Arg(Arg::Var(v)) => {
					self.f.u8(0x1F);
					self.f.u16(v);
				}
				Term::Arg(Arg::Attr(v)) => {
					self.f.u8(0x20);
					self.f.u8(v);
				}
				Term::Arg(Arg::CharAttr(id, v)) => {
					self.f.u8(0x21);
					self.f.u16(id.to_u16(self.iset.game)?);
					self.f.u8(v);
				}
				Term::Rand => {
					self.f.u8(0x22);
				}
				Term::Arg(Arg::Global(v)) => {
					self.f.u8(0x23);
					self.f.u8(v);
				}
				Term::Arg(ref v) => whatever!("cannot use {v:?} in Expr"),
			}
		}
		self.f.u8(0x01);
		Ok(())
	}
}

fn int_arg(iset: &iset::InsnSet, arg: &Arg) -> Result<i64, WriteError> {
	Ok(match *arg {
		Arg::Int(v) => v,
		Arg::Scalar(v, _) => v as i64,
		Arg::Angle32(v) => v as i64,
		Arg::Color(v) => v as i64,
		Arg::File(FileId(v)) => v as i64,
		Arg::Battle(BattleId(v)) => v as i64,
		Arg::Bgm(BgmId(v)) => v as i64,
		Arg::Item(ItemId(v)) => v as i64,
		Arg::Magic(MagicId(v)) => v as i64,
		Arg::Name(NameId(v)) => v as i64,
		Arg::Quest(QuestId(v)) => v as i64,
		Arg::Recipe(RecipeId(v)) => v as i64,
		Arg::Shop(ShopId(v)) => v as i64,
		Arg::Sound(SoundId(v)) => v as i64,
		Arg::Town(TownId(v)) => v as i64,
		Arg::Func(FuncId(a, b)) => (a as i64) | (b as i64) << 8,
		Arg::LookPoint(LookPointId(v)) => v as i64,
		Arg::Event(EventId(v)) => v as i64,
		Arg::Entrance(v) => v as i64,
		Arg::Object(v) => v as i64,
		Arg::ForkId(v) => v as i64,
		Arg::MenuId(v) => v as i64,
		Arg::EffId(v) => v as i64,
		Arg::EffInstanceId(v) => v as i64,
		Arg::ChipId(v) => v as i64,
		Arg::VisId(v) => v as i64,
		Arg::Char(v) => v.to_u16(iset.game)? as i64,
		Arg::Flag(Flag(v)) => v as i64,
		Arg::Var(v) => v as i64,
		Arg::Global(v) => v as i64,
		Arg::Attr(v) => v as i64,
		Arg::CharAttr(char, attr) => char.to_u16(iset.game)? as i64 | (attr as i64) << 16,
		Arg::QuestTask(v) => v as i64,
		Arg::QuestFlags(v) => v as i64,
		Arg::SystemFlags(v) => v as i64,
		Arg::LookPointFlags(v) => v as i64,
		Arg::ObjectFlags(v) => v as i64,
		Arg::EventFlags(v) => v as i64,
		Arg::CharFlags(v) => v as i64,
		Arg::CharFlags2(v) => v as i64,
		_ => whatever!("expected integer-valued argument"),
	})
}

fn text<'c>(f: &mut Writer, iter: impl Iterator<Item = &'c Arg>) -> Result<(), WriteError> {
	let mut first = true;
	for val in iter {
		expect!(Arg::Text(t) = val, "text");
		if !std::mem::take(&mut first) {
			f.u8(0x03); // page break
		}
		text_page(f, t)?;
	}
	f.u8(0);
	Ok(())
}

fn text_page(f: &mut Writer, s: &TString) -> Result<(), WriteError> {
	let mut iter = s.chars();
	while let Some(char) = iter.next() {
		if char == '\\' {
			match iter.next() {
				Some('n') => f.u8(0x01),
				Some('f') => f.u8(0x02),
				Some('r') => f.u8(0x0D),
				Some('\\') => f.u8(b'\\'),
				Some(v @ ('0'..='9')) => {
					let mut n = v.to_digit(10).unwrap();
					loop {
						match iter.next() {
							Some(v @ ('0'..='9')) => n = n * 10 + v.to_digit(10).unwrap(),
							Some('C') => {
								f.u8(0x07);
								f.u8(cast(n)?);
								break;
							}
							Some('i') => {
								f.u8(0x1F);
								f.u16(cast(n)?);
								break;
							}
							Some('x') => {
								f.u8(cast(n)?);
								break;
							}
							None => whatever!("unterminated escape sequence"),
							Some(_) => whatever!("illegal escape sequence"),
						}
					}
				}
				None => whatever!("unterminated escape sequence"),
				Some(_) => whatever!("illegal escape sequence"),
			}
		} else if let Some(enc) = falcom_sjis::encode_char(char) {
			f.slice(&enc)
		} else {
			whatever!("cannot encode as shift-jis: {char:?}");
		}
	}
	Ok(())
}
