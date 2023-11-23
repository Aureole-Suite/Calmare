// TODO this module is getting increasingly bare
use crate::scena::insn::{self, Arg, Insn};

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
	pub fn normalize(&mut self) -> Result<(), NormalizeError> {
		normalize::normalize(self)
	}
}
