use gospel::read::{Le as _, Reader};
use gospel::write::{Le as _, Writer};
use snafu::prelude::*;

use crate::scena::{insn_set as iset, CharId};
use crate::types::Flag;
use crate::util::{cast, ValueError};

use super::insn::{Arg, Insn};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum OpKind {
	Unary,
	Binary, // includes comparisons
	Assign,
}

#[derive(
	Debug, Clone, Copy, PartialEq, Eq, num_enum::TryFromPrimitive, num_enum::IntoPrimitive,
)]
#[repr(u8)]
pub enum Op {
	Eq = 0x02,      // ==
	Ne = 0x03,      // !=
	Lt = 0x04,      // <
	Gt = 0x05,      // >
	Le = 0x06,      // <=
	Ge = 0x07,      // >=
	Not = 0x08,     // !
	BoolAnd = 0x09, // &&
	And = 0x0A,     // &
	Or = 0x0B,      // | and ||
	Add = 0x0C,     // +
	Sub = 0x0D,     // -
	Neg = 0x0E,     // -
	Xor = 0x0F,     // ^
	Mul = 0x10,     // *
	Div = 0x11,     // /
	Mod = 0x12,     // %
	Ass = 0x13,     // =
	MulAss = 0x14,  // *=
	DivAss = 0x15,  // /=
	ModAss = 0x16,  // %=
	AddAss = 0x17,  // +=
	SubAss = 0x18,  // -=
	AndAss = 0x19,  // &=
	XorAss = 0x1A,  // ^=
	OrAss = 0x1B,   // |=
	Inv = 0x1D,     // ~
}

impl Op {
	pub fn kind(self) -> OpKind {
		use Op::*;
		match self {
			Not | Neg | Inv => OpKind::Unary,
			Eq | Ne | Lt | Le | Gt | Ge => OpKind::Binary,
			BoolAnd | And | Or | Add | Sub | Xor | Mul | Div | Mod => OpKind::Binary,
			Ass | MulAss | DivAss | ModAss | AddAss | SubAss | AndAss | XorAss | OrAss => {
				OpKind::Assign
			}
		}
	}
}

#[derive(Debug, Clone, PartialEq, Eq)]
#[repr(u8)]
pub enum Term {
	Arg(Arg),
	Op(Op),
	Insn(Box<Insn>),
	Rand, // random 15-bit number
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Expr(pub Vec<Term>);

#[derive(Debug, Snafu)]
pub enum ReadError {
	#[snafu(context(false))]
	Gospel { source: gospel::read::Error },
	#[snafu(context(false))]
	Value { source: ValueError },
	#[snafu(context(false))]
	Insn { source: super::insn::ReadError },
}

#[derive(Debug, Snafu)]
pub enum WriteError {
	#[snafu(context(false))]
	Gospel { source: gospel::write::Error },
	#[snafu(context(false))]
	Value { source: ValueError },
	#[snafu(context(false))]
	Insn { source: super::insn::WriteError },
	#[snafu(whatever, display("{message}"))]
	Whatever { message: String },
}

impl Expr {
	pub fn read(f: &mut Reader, iset: &iset::InsnSet) -> Result<Expr, ReadError> {
		let mut terms = Vec::new();
		loop {
			let op = f.u8()?;
			let term = if let Ok(op) = Op::try_from(op) {
				Term::Op(op)
			} else {
				match op {
					0x00 => Term::Arg(Arg::Int(f.u32()? as i64)),
					0x01 => break,
					0x1C => Term::Insn(Box::new(Insn::read(f, iset)?)),
					0x1E => Term::Arg(Arg::Flag(Flag(f.u16()?))),
					0x1F => Term::Arg(Arg::Var(f.u16()?)),
					0x20 => Term::Arg(Arg::Attr(f.u8()?)),
					0x21 => Term::Arg(Arg::CharAttr(
						CharId::from_u16(iset.game, f.u16()?)?,
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

	pub fn write(f: &mut Writer, iset: &iset::InsnSet, v: &Expr) -> Result<(), WriteError> {
		for term in &v.0 {
			match *term {
				Term::Arg(Arg::Int(n)) => {
					f.u8(0x00);
					f.u32(cast(n)?);
				}
				Term::Op(op) => f.u8(op.into()),
				Term::Insn(ref insn) => {
					f.u8(0x1C);
					Insn::write(f, iset, insn)?;
				}
				Term::Arg(Arg::Flag(Flag(v))) => {
					f.u8(0x1E);
					f.u16(v);
				}
				Term::Arg(Arg::Var(v)) => {
					f.u8(0x1F);
					f.u16(v);
				}
				Term::Arg(Arg::Attr(v)) => {
					f.u8(0x20);
					f.u8(v);
				}
				Term::Arg(Arg::CharAttr(id, v)) => {
					f.u8(0x21);
					f.u16(id.to_u16(iset.game)?);
					f.u8(v);
				}
				Term::Rand => {
					f.u8(0x22);
				}
				Term::Arg(Arg::Global(v)) => {
					f.u8(0x23);
					f.u8(v);
				}
				Term::Arg(ref v) => whatever!("cannot use {v:?} in Expr"),
			}
		}
		f.u8(0x01);
		Ok(())
	}
}
