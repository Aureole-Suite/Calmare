use std::collections::BTreeMap;

use gospel::write::{Label, Le as _, Writer};
use snafu::prelude::*;

use super::{Arg, Insn, Expr};
use crate::scena::code::Code;
use crate::scena::insn::Unit;
use crate::scena::{insn_set as iset, CharId, EventId, FuncId, LookPointId};
use crate::types::*;
use crate::util::WriterExt;

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

impl<'a, 'b> InsnWriter<'a, 'b> {
	pub fn new(f: &'b mut Writer, iset: &'a iset::InsnSet) -> Self {
		InsnWriter {
			f,
			iset,
			labels: BTreeMap::new(),
		}
	}

	pub fn insn(&mut self, insn: &Insn) -> Result<(), WriteError> {
		match insn.name.as_str() {
			"_label" => {
				let [Arg::Label(label)] = insn.args.as_slice() else {
					whatever!("malformed label")
				};
				let here = self.f.here();
				self.labels.insert(*label, here);
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

	fn args(&mut self, args: &[Arg], iargs: &[iset::Arg]) -> Result<(), WriteError> {
		let mut iter = args.iter();
		for arg in iargs {
			self.arg(arg, &mut iter)?;
		}
		if let Some(arg) = iter.next() {
			whatever!("too many arguments: {arg:?}")
		};
		Ok(())
	}

	fn arg<'c>(
		&self,
		arg: &iset::Arg,
		mut iter: impl Iterator<Item = &'c Arg>,
	) -> Result<(), WriteError> {
		match arg {
			iset::Arg::Int(_, _) => todo!(),
			iset::Arg::Misc(_) => todo!(),
			iset::Arg::Tuple(_) => todo!(),
		}
	}

	fn expr(&mut self, expr: &Expr) -> Result<(), WriteError> {
		use crate::scena::insn::Term;
		for term in &expr.0 {
			match *term {
				Term::Arg(Arg::Int(n)) => {
					self.f.u8(0x00);
					self.f.u32(crate::util::cast(n)?);
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
