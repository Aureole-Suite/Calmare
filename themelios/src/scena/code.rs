use gospel::read::Reader;
use gospel::write::Writer;

use crate::scena::insn::{self, Arg, Insn};
use crate::scena::insn_set::InsnSet;

mod normalize;
pub mod visit;
pub mod visit_mut;

pub use normalize::NormalizeError;

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
	pub fn read(
		f: &mut Reader,
		insn: &InsnSet,
		end: Option<usize>,
	) -> Result<Code, insn::ReadError> {
		let mut code = insn::InsnReader::new(f, insn).code(end)?;
		code.normalize().unwrap();
		Ok(code)
	}

	pub fn write(f: &mut Writer, iset: &InsnSet, code: &Code) -> Result<(), insn::WriteError> {
		insn::InsnWriter::new(f, iset).code(code)
	}

	pub fn normalize(&mut self) -> Result<(), NormalizeError> {
		normalize::normalize(self)
	}
}
