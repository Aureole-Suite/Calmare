use std::ops::ControlFlow;

use super::{
	insn::{Expr, Term},
	Arg, Code, Insn,
};

#[allow(unused_variables)]
pub trait Visit {
	fn visit_code(&mut self, code: &Code) -> ControlFlow<()> {
		ControlFlow::Continue(())
	}
	fn visit_insn(&mut self, insn: &Insn) -> ControlFlow<()> {
		ControlFlow::Continue(())
	}
	fn visit_arg(&mut self, arg: &Arg) -> ControlFlow<()> {
		ControlFlow::Continue(())
	}
}

pub trait Visitable {
	fn accept(&self, f: &mut impl Visit);
}

impl<T: Visitable> Visitable for &T {
	fn accept(&self, f: &mut impl Visit) {
		T::accept(self, f);
	}
}

impl<T: Visitable> Visitable for &mut T {
	fn accept(&self, f: &mut impl Visit) {
		T::accept(self, f);
	}
}

impl<T: Visitable> Visitable for [T] {
	fn accept(&self, f: &mut impl Visit) {
		for v in self {
			v.accept(f);
		}
	}
}

impl<T: Visitable> Visitable for Vec<T> {
	fn accept(&self, f: &mut impl Visit) {
		self.as_slice().accept(f)
	}
}

impl Visitable for Code {
	fn accept(&self, f: &mut impl Visit) {
		if f.visit_code(self).is_continue() {
			self.0.accept(f)
		}
	}
}

impl Visitable for Insn {
	fn accept(&self, f: &mut impl Visit) {
		if f.visit_insn(self).is_continue() {
			self.args.accept(f)
		}
	}
}

impl Visitable for Arg {
	fn accept(&self, f: &mut impl Visit) {
		if f.visit_arg(self).is_continue() {
			match self {
				Arg::Tuple(t) => t.accept(f),
				Arg::Code(t) => t.accept(f),
				Arg::Expr(t) => t.accept(f),
				_ => {}
			}
		}
	}
}

impl Visitable for Expr {
	fn accept(&self, f: &mut impl Visit) {
		self.0.accept(f)
	}
}

impl Visitable for Term {
	fn accept(&self, f: &mut impl Visit) {
		match self {
			Term::Arg(t) => t.accept(f),
			Term::Op(_) => {}
			Term::Insn(t) => t.accept(f),
			Term::Rand => {}
		}
	}
}
