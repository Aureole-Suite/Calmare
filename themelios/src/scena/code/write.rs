use std::collections::BTreeMap;
use std::iter::Peekable;

use gospel::write::{Label as GLabel, Le as _, Writer};

use super::{Arg, Atom, Code, Expr, Insn};
use crate::gamedata as iset;
use crate::scena::*;
use crate::types::*;
use crate::util::OptionTExt as _;
use crate::util::{bail, ensure};
use crate::util::{cast, encode, Enc};
use crate::util::{ValueError, WriterExt};

#[derive(Debug, thiserror::Error)]
pub enum WriteError {
	#[error("{source}")]
	Gospel {
		#[from]
		source: gospel::write::Error,
		backtrace: Backtrace,
	},
	#[error(transparent)]
	Encode(
		#[from]
		#[backtrace]
		crate::util::EncodeError,
	),
	#[error(transparent)]
	Value(
		#[from]
		#[backtrace]
		crate::util::ValueError,
	),
	#[error("{message}")]
	Whatever {
		message: String,
		backtrace: Backtrace,
	},
}

impl From<std::fmt::Arguments<'_>> for WriteError {
	fn from(message: std::fmt::Arguments<'_>) -> Self {
		Self::Whatever {
			message: message.to_string(),
			backtrace: Backtrace::capture(),
		}
	}
}

pub struct InsnWriter<'iset, 'write> {
	pub f: &'write mut Writer,
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
			bail!("expected {}, got: {:?}", $what, val)
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
			("_label", _) => bail!("malformed label"),

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
			("if", _) => bail!("malformed if"),

			("break", []) => match self.brk {
				Some(l) => self.insn(&Insn::new("_goto", vec![Arg::Label(l)]))?,
				None => bail!("can't break here"),
			},
			("break", _) => bail!("malformed break"),

			("continue", []) => match self.cont {
				Some(l) => self.insn(&Insn::new("_goto", vec![Arg::Label(l)]))?,
				None => bail!("can't continue here"),
			},
			("continue", _) => bail!("malformed continue"),

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
			("while", _) => bail!("malformed while"),

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
							ensure!(default.is_none(), "duplicate default case");
							default = Some(label);
							body_out.push((label, body));
						}
						_ => bail!("invalid switch case"),
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
			("switch", _) => bail!("malformed switch"),

			(name, args) => {
				let Some(iargs) = self.iset.insns_rev.get(name) else {
					bail!("unknown instruction {name}")
				};
				self.args(args, iargs)?;
			}
		}
		Ok(())
	}

	fn label(&mut self, label: &Label) -> GLabel {
		*self.labels.entry(*label).or_default()
	}

	fn internal_label(&mut self, label: GLabel) -> Label {
		let l = Label(!self.label_no);
		self.label_no += 1;
		self.labels.insert(l, label);
		l
	}

	fn args(&mut self, args: &[Arg], iargs: &[iset::Arg]) -> Result<(), WriteError> {
		let mut iter = args.iter().peekable();
		for iarg in iargs {
			self.arg(args, iarg, &mut iter)?;
		}
		if let Some(arg) = iter.peek() {
			bail!("too many arguments: {arg:?}")
		};
		Ok(())
	}

	fn arg<'c>(
		&mut self,
		args: &[Arg],
		iarg: &iset::Arg,
		iter: &mut Peekable<impl Iterator<Item = &'c Arg>>,
	) -> Result<(), WriteError> {
		match iarg {
			iset::Arg::Int(int, iarg) => {
				let Some(val) = iter.next() else {
					bail!("too few arguments; expected {iarg:?}");
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
				ensure!((0..(1 << 24)).contains(&val), ValueError::new("u24", val));
				f.u16(val as u16);
				f.u8((val >> 16) as u8);
			}
			iset::IntType::u32 => f.u32(cast(val)?),
			iset::IntType::i8 => f.i8(cast(val)?),
			iset::IntType::i16 => f.i16(cast(val)?),
			iset::IntType::i32 => f.i32(cast(val)?),
			iset::IntType::Const(v) => {
				ensure!(val == v);
			}
			iset::IntType::ED7Battle => match val {
				0 => {}
				1 => f.u32(0xFFFFFFFF),
				_ => bail!("invalid ED7Battle"),
			},
		}
		Ok(())
	}

	fn misc<'c>(
		&mut self,
		args: &[Arg],
		iarg: &iset::MiscArg,
		iter: &mut Peekable<impl Iterator<Item = &'c Arg>>,
	) -> Result<(), WriteError> {
		let f = &mut self.f;
		use iset::MiscArg as T;
		match iarg {
			T::Label => {
				expect!(Arg::Label(l) in iter, "label");
				let label = self.label(l);
				let int = self.iset.address_size;
				match int {
					iset::IntType::u8 => self.f.label8(label),
					iset::IntType::u16 => self.f.label16(label),
					iset::IntType::u32 => self.f.label32(label),
					_ => bail!("can't write label as {int:?}"),
				}
			}

			T::Const(int, val) => {
				self.int(*int, *val)?;
			}

			T::String => {
				expect!(Arg::Atom(Atom::String(s) | Atom::TString(TString(s))) in iter, "string");
				f.string(s)?;
			}
			T::TString => {
				expect!(Arg::Atom(Atom::String(s) | Atom::TString(TString(s))) in iter, "string");
				f.tstring(s.into(), self.iset)?;
			}
			T::Text => text(f, iter, self.iset)?,

			T::Pos2 => {
				expect!(Arg::Atom(Atom::Pos2(p)) in iter, "pos2");
				f.i32(p.x);
				f.i32(p.z);
			}
			T::Pos3 | T::RPos3 | T::EffPlayPos => {
				expect!(Arg::Atom(Atom::Pos3(p) | Atom::RPos3(p)) in iter, "pos3");
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
				let mut iter = cs.iter().peekable();
				while iter.peek().is_some() {
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
				ensure!(val.len() == 4, "expected tuple of 4, got {val:?}");
				for val in val {
					if let Arg::Atom(Atom::CharId(CharId::Null)) = val {
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
					ensure!(val < 32, "name[] < 32");
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
					expect!(
						Arg::Atom(Atom::String(s) | Atom::TString(TString(s))) = val,
						"string"
					);
					out.push_str(s.as_str());
					out.push('\x01');
				}
				f.tstring(&TString(out), self.iset)?;
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
					.or_whatever("battle id out of bounds")?;
				f.label32(label);
			}

			T::FcPartyEquip => {
				// TODO change these to use peek?
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

			T::f32 => {
				expect!(Arg::Atom(Atom::Float(v)) in iter, "float");
				f.f32(*v);
			}
			T::Cs1_13(a) => {
				if iter.peek().is_some() {
					self.arg(args, a, iter)?;
				}
			}
			T::Cs1_22 => {
				expect!(Arg::Tuple(bools) in iter, "tuple of bool");
				for b in bools {
					let val = int_arg(self.iset, b)?;
					f.u8(cast(val)?);
				}
				expect!(Arg::Tuple(floats) in iter, "tuple of float");
				for v in floats {
					if let Arg::Atom(Atom::Float(v)) = v {
						f.f32(*v);
					} else if let Ok(v) = int_arg(self.iset, v) {
						f.u32(cast(v)?);
					} else {
						bail!("expected float or int")
					}
				}
			}
			T::Cs1_28_34 => todo!(),
			T::Cs1_36(x, a) => {
				let val = int_arg(self.iset, &args[1])?;
				if x.contains(&(val as u16)) {
					self.arg(args, a, iter)?;
				};
			}
			T::Cs1_3C(a) => {
				if iter.peek().is_some() {
					self.arg(args, a, iter)?;
				}
			}
		}
		Ok(())
	}

	fn expr(&mut self, expr: &Expr) -> Result<(), WriteError> {
		use Atom as A;
		match *expr {
			Expr::Atom(A::Int(n)) => {
				self.f.u8(0x00);
				self.f.i32(cast(n)?);
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
			Expr::Atom(A::Flag(Flag(v))) => {
				self.f.u8(0x1E);
				self.f.u16(v);
			}
			Expr::Atom(A::Var(Var(v))) => {
				self.f.u8(0x1F);
				if self.iset.game >= Game::Cs1 {
					self.f.u8(cast(v)?);
				} else {
					self.f.u16(v);
				}
			}
			Expr::Atom(A::Attr(Attr(v))) => {
				self.f.u8(0x20);
				self.f.u8(v);
			}
			Expr::Atom(A::CharAttr(CharAttr(id, v))) => {
				self.f.u8(0x21);
				self.f.u16(id.to_u16(self.iset.game)?);
				self.f.u8(v);
			}
			Expr::Rand => {
				self.f.u8(0x22);
			}
			Expr::Atom(A::Global(Global(v))) => {
				self.f.u8(0x23);
				self.f.u8(v);
			}
			Expr::Atom(ref v) => bail!("cannot use {v:?} in Expr"),
		}
		Ok(())
	}
}

fn int_arg(iset: &iset::InsnSet, arg: &Arg) -> Result<i64, WriteError> {
	use Atom as A;
	expect!(Arg::Atom(sc) = arg, "atom");
	Ok(match *sc {
		A::Int(v) => v,
		A::Time(Time(v)) => v as i64,
		A::Angle(Angle(v)) => v as i64,
		A::Angle32(Angle32(v)) => v as i64,
		A::Speed(Speed(v)) => v as i64,
		A::AngularSpeed(AngularSpeed(v)) => v as i64,
		A::Length(Length(v)) => v as i64,
		A::Color(Color(v)) => v as i64,
		A::FileId(FileId(v)) => v as i64,
		A::BattleId(BattleId(v)) => v as i64,
		A::BgmId(BgmId(v)) => v as i64,
		A::ItemId(ItemId(v)) => v as i64,
		A::MagicId(MagicId(v)) => v as i64,
		A::NameId(NameId(v)) => v as i64,
		A::QuestId(QuestId(v)) => v as i64,
		A::RecipeId(RecipeId(v)) => v as i64,
		A::ShopId(ShopId(v)) => v as i64,
		A::SoundId(SoundId(v)) => v as i64,
		A::TownId(TownId(v)) => v as i64,
		A::FuncId(FuncId(a, b)) => (a as i64) | (b as i64) << 8,
		A::LookPointId(LookPointId(v)) => v as i64,
		A::EventId(EventId(v)) => v as i64,
		A::EntranceId(EntranceId(v)) => v as i64,
		A::ObjectId(ObjectId(v)) => v as i64,
		A::ForkId(ForkId(v)) => v as i64,
		A::MenuId(MenuId(v)) => v as i64,
		A::EffId(EffId(v)) => v as i64,
		A::EffInstanceId(EffInstanceId(v)) => v as i64,
		A::ChipId(ChipId(v)) => v as i64,
		A::VisId(VisId(v)) => v as i64,
		A::CharId(v) => v.to_u16(iset.game)? as i64,
		A::Flag(Flag(v)) => v as i64,
		A::Var(Var(v)) => v as i64,
		A::Global(Global(v)) => v as i64,
		A::Attr(Attr(v)) => v as i64,
		A::CharAttr(CharAttr(char, attr)) => char.to_u16(iset.game)? as i64 | (attr as i64) << 16,
		A::QuestTask(v) => v as i64,
		A::QuestFlags(v) => v as i64,
		A::SystemFlags(v) => v as i64,
		A::LookPointFlags(v) => v as i64,
		A::ObjectFlags(v) => v as i64,
		A::EventFlags(v) => v as i64,
		A::CharFlags(v) => v as i64,
		A::CharFlags2(v) => v as i64,
		A::Float(_)
		| A::String(_)
		| A::Pos2(_)
		| A::Pos3(_)
		| A::RPos3(_)
		| A::TString(_)
		| A::Text(_) => {
			bail!("expected integer-valued argument")
		}
	})
}

fn text<'c>(
	f: &mut Writer,
	iter: impl Iterator<Item = &'c Arg>,
	iset: &iset::InsnSet,
) -> Result<(), WriteError> {
	let escape = if iset.game >= Game::Cs1 {
		escape_ed8
	} else {
		escape_ed67
	};
	let mut first = true;
	for val in iter {
		expect!(Arg::Atom(Atom::Text(t)) = val, "text");
		if !std::mem::take(&mut first) {
			f.u8(0x03); // page break
		}
		text_page(f, t, iset.encoding, escape)?;
	}
	f.u8(0);
	Ok(())
}

fn text_page(
	f: &mut Writer,
	s: &Text,
	enc: Enc,
	mut escape: impl FnMut(&mut Writer, char, u32) -> Result<bool, WriteError>,
) -> Result<(), WriteError> {
	let mut iter = s.0.chars();
	while let (str, Some(char)) = (iter.as_str(), iter.next()) {
		match char {
			'\n' => f.u8(0x01),
			'\t' => f.u8(0x02),
			'\r' => f.u8(0x0D),
			'\0'..='\x1F' => bail!("unprintable character"),

			'♯' => {
				let mut n = 0;
				loop {
					match iter.next() {
						Some(v @ ('0'..='9')) => n = n * 10 + v.to_digit(10).unwrap(),
						Some('x') => {
							f.u8(cast(n)?);
							break;
						}
						Some('♯') => f.slice(&falcom_sjis::encode_char('♯').unwrap()),
						None => bail!("unterminated escape sequence"),
						Some(c) => {
							ensure!(escape(f, c, n)?, "illegal escape sequence");
							break;
						}
					}
				}
			}

			_ => {
				let part = str.split(|c| c < ' ' || c == '♯').next().unwrap();
				let part = part.replace('♥', "㈱");
				f.slice(&encode(&part, enc)?);
				iter = str[part.len()..].chars();
			}
		}
	}
	Ok(())
}

fn escape_ed67(f: &mut Writer, char: char, n: u32) -> Result<bool, WriteError> {
	match char {
		'C' => {
			f.u8(0x07);
			f.u8(cast(n)?);
		}
		'i' => {
			f.u8(0x1F);
			f.u16(cast(n)?);
		}
		_ => return Ok(false),
	}
	Ok(true)
}

fn escape_ed8(f: &mut Writer, char: char, n: u32) -> Result<bool, WriteError> {
	match char {
		'i' => {
			f.u8(0x10);
			f.u16(cast(n)?);
		}
		'J' => {
			f.u8(0x11);
			f.u32(cast(n)?);
		}
		'C' => {
			f.u8(0x12);
			f.u32(cast(n)?);
		}
		'D' => {
			f.u8(0x17);
			f.u16(cast(n)?);
		}
		'E' => {
			f.u8(0x18);
			f.u16(cast(n)?);
		}
		_ => return Ok(false),
	}
	Ok(true)
}
