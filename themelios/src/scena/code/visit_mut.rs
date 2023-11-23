use std::ops::ControlFlow;

use super::{
	insn::{Expr, Term},
	Arg, Code, Insn,
};

#[allow(unused_variables)]
pub trait VisitMut {
	fn visit_code_mut(&mut self, code: &mut Code) -> ControlFlow<()> {
		ControlFlow::Continue(())
	}
	fn visit_insn_mut(&mut self, insn: &mut Insn) -> ControlFlow<()> {
		ControlFlow::Continue(())
	}
	fn visit_arg_mut(&mut self, arg: &mut Arg) -> ControlFlow<()> {
		ControlFlow::Continue(())
	}
}

pub trait VisitableMut {
	fn accept_mut(&mut self, f: &mut impl VisitMut);
}

impl<T: VisitableMut> VisitableMut for &mut T {
	fn accept_mut(&mut self, f: &mut impl VisitMut) {
		T::accept_mut(self, f);
	}
}

impl<T: VisitableMut> VisitableMut for [T] {
	fn accept_mut(&mut self, f: &mut impl VisitMut) {
		for v in self {
			v.accept_mut(f);
		}
	}
}

impl<T: VisitableMut> VisitableMut for Vec<T> {
	fn accept_mut(&mut self, f: &mut impl VisitMut) {
		self.as_mut_slice().accept_mut(f)
	}
}

impl VisitableMut for Code {
	fn accept_mut(&mut self, f: &mut impl VisitMut) {
		if f.visit_code_mut(self).is_continue() {
			self.0.accept_mut(f)
		}
	}
}

impl VisitableMut for Insn {
	fn accept_mut(&mut self, f: &mut impl VisitMut) {
		if f.visit_insn_mut(self).is_continue() {
			self.args.accept_mut(f)
		}
	}
}

impl VisitableMut for Arg {
	fn accept_mut(&mut self, f: &mut impl VisitMut) {
		if f.visit_arg_mut(self).is_continue() {
			match self {
				Arg::Tuple(t) => t.accept_mut(f),
				Arg::Code(t) => t.accept_mut(f),
				Arg::Expr(t) => t.accept_mut(f),
				_ => {}
			}
		}
	}
}

impl VisitableMut for Expr {
	fn accept_mut(&mut self, f: &mut impl VisitMut) {
		self.0.accept_mut(f)
	}
}

impl VisitableMut for Term {
	fn accept_mut(&mut self, f: &mut impl VisitMut) {
		match self {
			Term::Arg(t) => t.accept_mut(f),
			Term::Op(_) => {}
			Term::Insn(t) => t.accept_mut(f),
			Term::Rand => {}
		}
	}
}
