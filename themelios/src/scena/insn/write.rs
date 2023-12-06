use std::collections::BTreeMap;

use gospel::write::{Label as GLabel, Le as _, Writer};
use snafu::prelude::*;
use strict_result::Strict as _;

use super::{Arg, Expr, Insn};
use crate::scena::code::Code;
use crate::scena::insn_set as iset;
use crate::scena::*;
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

macro_rules! vec_plus {
	($base:expr $(, $extra:expr)* $(,)?) => {{
		let mut args = $base.to_vec();
		$(args.push($extra);)*
		args
	}}
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
		match insn.parts() {
			("_label", [Arg::Label(label)]) => {
				let label = self.label(label);
				self.f.place(label);
			}
			("_label", _) => whatever!("malformed label"),

			("if", [args @ .., Arg::Code(yes), Arg::Code(no)]) => {
				let mid = self.internal_label(GLabel::new());
				let end = self.internal_label(GLabel::new());
				self.insn(&Insn::new("_if", vec_plus![args, Arg::Label(mid)]))?;
				self.code(yes)?;
				self.insn(&Insn::new("_goto", vec![Arg::Label(end)]))?;
				self.insn(&Insn::new("_label", vec![Arg::Label(mid)]))?;
				self.code(no)?;
				self.insn(&Insn::new("_label", vec![Arg::Label(end)]))?;
			}
			("if", [args @ .., Arg::Code(yes)]) => {
				let end = self.internal_label(GLabel::new());
				self.insn(&Insn::new("_if", vec_plus![args, Arg::Label(end)]))?;
				self.code(yes)?;
				self.insn(&Insn::new("_label", vec![Arg::Label(end)]))?;
			}
			("if", _) => whatever!("malformed if"),

			("break", []) => match self.brk {
				Some(l) => self.insn(&Insn::new("_goto", vec![Arg::Label(l)]))?,
				None => whatever!("can't break here"),
			},
			("break", _) => whatever!("malformed break"),

			("continue", []) => match self.cont {
				Some(l) => self.insn(&Insn::new("_goto", vec![Arg::Label(l)]))?,
				None => whatever!("can't continue here"),
			},
			("continue", _) => whatever!("malformed continue"),

			("while", [args @ .., Arg::Code(body)]) => {
				let start = self.internal_label(GLabel::new());
				let end = self.internal_label(GLabel::new());

				let brk = self.brk.replace(end);
				let cont = self.cont.replace(start);

				self.insn(&Insn::new("_label", vec![Arg::Label(start)]))?;
				self.insn(&Insn::new("_if", vec_plus![args, Arg::Label(end)]))?;
				self.code(body)?;
				self.insn(&Insn::new("continue", vec![]))?;
				self.insn(&Insn::new("_label", vec![Arg::Label(end)]))?;

				self.brk = brk;
				self.cont = cont;
			}
			("while", _) => whatever!("malformed while"),

			("switch", [args @ .., Arg::Code(body)]) => {
				let mut body_out = Vec::new();
				let mut cases = Vec::new();
				let mut default = None;

				for case in body.iter() {
					let label = self.internal_label(GLabel::new());
					match case.parts() {
						("case", [args @ .., Arg::Code(body)]) => {
							cases.push(Arg::Tuple(vec_plus![args, Arg::Label(label)]));
							body_out.push((label, body));
						}
						("default", [Arg::Code(body)]) => {
							ensure_whatever!(default.is_none(), "duplicate default case");
							default = Some(label);
							body_out.push((label, body));
						}
						_ => whatever!("invalid switch case"),
					}
				}

				let end = self.internal_label(GLabel::new());
				let brk = self.brk.replace(end);

				self.insn(&Insn::new(
					"_switch",
					vec_plus![args, Arg::Tuple(cases), Arg::Label(default.unwrap_or(end))],
				))?;
				for (l, code) in body_out {
					self.insn(&Insn::new("_label", vec![Arg::Label(l)]))?;
					self.code(code)?;
				}
				self.insn(&Insn::new("_label", vec![Arg::Label(end)]))?;

				self.brk = brk;
			}
			("switch", _) => whatever!("malformed switch"),

			(name, args) => {
				let Some(iargs) = self.iset.insns_rev.get(name) else {
					whatever!("unknown instruction {name}")
				};
				self.args(args, iargs)?;
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
				self.f.u8(0x01);
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
					if let Arg::CharId(CharId::Null) = val {
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
		match *expr {
			Expr::Arg(Arg::Int(n)) => {
				self.f.u8(0x00);
				self.f.u32(cast(n)?);
			}
			Expr::Bin(op, ref l, ref r) => {
				self.expr(l)?;
				self.expr(r)?;
				self.f.u8(op.into());
			}
			Expr::Unary(op, ref t) => {
				self.expr(t)?;
				self.f.u8(op.into());
			}
			Expr::Assign(op, ref t) => {
				self.expr(t)?;
				self.f.u8(op.into());
			}
			Expr::Insn(ref insn) => {
				self.f.u8(0x1C);
				self.insn(insn)?;
			}
			Expr::Arg(Arg::Flag(Flag(v))) => {
				self.f.u8(0x1E);
				self.f.u16(v);
			}
			Expr::Arg(Arg::Var(Var(v))) => {
				self.f.u8(0x1F);
				self.f.u16(v);
			}
			Expr::Arg(Arg::Attr(Attr(v))) => {
				self.f.u8(0x20);
				self.f.u8(v);
			}
			Expr::Arg(Arg::CharAttr(CharAttr(id, v))) => {
				self.f.u8(0x21);
				self.f.u16(id.to_u16(self.iset.game)?);
				self.f.u8(v);
			}
			Expr::Rand => {
				self.f.u8(0x22);
			}
			Expr::Arg(Arg::Global(Global(v))) => {
				self.f.u8(0x23);
				self.f.u8(v);
			}
			Expr::Arg(ref v) => whatever!("cannot use {v:?} in Expr"),
		}
		Ok(())
	}
}

fn int_arg(iset: &iset::InsnSet, arg: &Arg) -> Result<i64, WriteError> {
	Ok(match *arg {
		Arg::Int(v) => v,
		Arg::Time(Time(v)) => v as i64,
		Arg::Angle(Angle(v)) => v as i64,
		Arg::Angle32(Angle32(v)) => v as i64,
		Arg::Speed(Speed(v)) => v as i64,
		Arg::AngularSpeed(AngularSpeed(v)) => v as i64,
		Arg::Length(Length(v)) => v as i64,
		Arg::Color(Color(v)) => v as i64,
		Arg::FileId(FileId(v)) => v as i64,
		Arg::BattleId(BattleId(v)) => v as i64,
		Arg::BgmId(BgmId(v)) => v as i64,
		Arg::ItemId(ItemId(v)) => v as i64,
		Arg::MagicId(MagicId(v)) => v as i64,
		Arg::NameId(NameId(v)) => v as i64,
		Arg::QuestId(QuestId(v)) => v as i64,
		Arg::RecipeId(RecipeId(v)) => v as i64,
		Arg::ShopId(ShopId(v)) => v as i64,
		Arg::SoundId(SoundId(v)) => v as i64,
		Arg::TownId(TownId(v)) => v as i64,
		Arg::FuncId(FuncId(a, b)) => (a as i64) | (b as i64) << 8,
		Arg::LookPointId(LookPointId(v)) => v as i64,
		Arg::EventId(EventId(v)) => v as i64,
		Arg::EntranceId(EntranceId(v)) => v as i64,
		Arg::ObjectId(ObjectId(v)) => v as i64,
		Arg::ForkId(ForkId(v)) => v as i64,
		Arg::MenuId(MenuId(v)) => v as i64,
		Arg::EffId(EffId(v)) => v as i64,
		Arg::EffInstanceId(EffInstanceId(v)) => v as i64,
		Arg::ChipId(ChipId(v)) => v as i64,
		Arg::VisId(VisId(v)) => v as i64,
		Arg::CharId(v) => v.to_u16(iset.game)? as i64,
		Arg::Flag(Flag(v)) => v as i64,
		Arg::Var(Var(v)) => v as i64,
		Arg::Global(Global(v)) => v as i64,
		Arg::Attr(Attr(v)) => v as i64,
		Arg::CharAttr(CharAttr(char, attr)) => char.to_u16(iset.game)? as i64 | (attr as i64) << 16,
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

fn text_page(f: &mut Writer, s: &Text) -> Result<(), WriteError> {
	let mut iter = s.0.chars();
	while let Some(char) = iter.next() {
		match char {
			'\n' => f.u8(0x01),
			'\t' => f.u8(0x02),
			'\r' => f.u8(0x0D),
			'\0'..='\x1F' => whatever!("unprintable character"),

			'♯' => {
				let mut n = 0;
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
						Some(_) => whatever!("illegal escape sequence (maybe try `#`?)"),
					}
				}
			}

			'♥' => {
				f.slice(&falcom_sjis::encode_char('㈱').unwrap());
			}

			char => {
				if let Some(enc) = falcom_sjis::encode_char(char) {
					f.slice(&enc)
				} else {
					whatever!("cannot encode as shift-jis: {char:?}");
				}
			}
		}
	}
	Ok(())
}
