use gospel::read::Reader;
use gospel::write::Writer;
use snafu::prelude::*;

use crate::scena::insn::{self, Arg, Insn};
use crate::scena::insn_set::InsnSet;

#[derive(Debug, Snafu)]
pub enum ReadError {
	Overshoot {
		pos: usize,
		end: usize,
	},
	Insn {
		context: Vec<(usize, Insn)>,
		pos: usize,
		source: insn::ReadError,
	},
}

#[derive(Debug, Snafu)]
pub enum WriteError {
	Bar,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Code(pub Vec<Insn>);

impl std::ops::Deref for Code {
	type Target = Vec<Insn>;

	fn deref(&self) -> &Self::Target {
		&self.0
	}
}

impl std::ops::DerefMut for Code {
	fn deref_mut(&mut self) -> &mut Self::Target {
		&mut self.0
	}
}

impl Code {
	pub fn read(f: &mut Reader, insn: &InsnSet, end: Option<usize>) -> Result<Code, ReadError> {
		let end = end.expect("inferred end is not yet supported");

		let mut insns = Vec::new();
		while f.pos() < end {
			let pos = f.pos();
			let insn = Insn::read(f, insn).with_context(|_| InsnSnafu {
				pos,
				context: insns
					.iter()
					.rev()
					.take(8)
					.rev()
					.cloned()
					.collect::<Vec<_>>(),
			})?;
			insns.push((pos, insn));
		}
		ensure!(f.pos() == end, OvershootSnafu { pos: f.pos(), end });

		let mut insns2 = Vec::with_capacity(insns.len() * 2 + 1);
		for (pos, insn) in insns {
			insns2.push(Insn::new("_label", vec![Arg::Address(pos)]));
			insns2.push(insn);
		}
		insns2.push(Insn::new("_label", vec![Arg::Address(f.pos())]));
		Ok(Code(insns2))
	}

	pub fn write(code: &mut Writer, insn: &InsnSet, func: &Code) -> Result<(), WriteError> {
		todo!()
	}
}
