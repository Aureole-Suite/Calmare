use std::collections::BTreeMap;

use gospel::write::{Label, Le as _, Writer};
use snafu::prelude::*;

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

pub struct InsnWriter<'a, 'b> {
	f: &'b mut Writer,
	iset: &'a iset::InsnSet,
	labels: BTreeMap<usize, Label>,
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

impl<'a, 'b> InsnWriter<'a, 'b> {
	pub fn new(f: &'b mut Writer, iset: &'a iset::InsnSet) -> Self {
		InsnWriter {
			f,
			iset,
			labels: BTreeMap::new(),
		}
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
			name => {
				let Some(iargs) = self.iset.insns_rev.get(name) else {
					whatever!("unknown instruction {name}")
				};
				self.args(&insn.args, iargs)?;
			}
		}
		Ok(())
	}

	fn label(&mut self, label: &usize) -> Label {
		*self.labels.entry(*label).or_insert_with(Label::new)
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
				let l1 = Label::new();
				let l2 = Label::new();
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
				let l1 = Label::new();
				let l2 = Label::new();
				self.f.diff8(l1, l2);
				self.f.place(l1);
				self.code(c)?;
				self.f.place(l2);
				self.insn(&Insn::new(next, vec![]))?;
				// Ugly hack :( Need a _goto to the start of the block, and this is the only way I have to do that
				const KEY: usize = usize::MAX;
				let prev = self.labels.insert(KEY, l1);
				self.insn(&Insn::new("_goto", vec![Arg::Label(KEY)]))?;
				match prev {
					Some(v) => self.labels.insert(KEY, v),
					None => self.labels.remove(&KEY),
				};
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
					expect!(Arg::Name(NameId(i)) = val, "name[]");
					ensure_whatever!(*i < 32, "name[] < 32");
					n |= 1 << i;
				}
				f.u32(n);
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
