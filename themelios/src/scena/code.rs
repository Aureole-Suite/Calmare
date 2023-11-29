// TODO this module is getting increasingly bare
use crate::scena::insn::{self, Arg, Insn};

pub mod decompile;
pub mod normalize;
pub mod visit;
pub mod visit_mut;

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
