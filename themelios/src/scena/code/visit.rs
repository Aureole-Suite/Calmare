use std::ops::ControlFlow;

use super::{
	insn::{Atom, Expr},
	Arg, Code, Insn,
};
use crate::types::Label;

#[allow(unused_variables)]
pub trait Visit {
	fn visit_label(&mut self, arg: &Label) {}
	fn visit_code(&mut self, code: &Code) -> ControlFlow<()> {
		ControlFlow::Continue(())
	}
	fn visit_insn(&mut self, insn: &Insn) -> ControlFlow<()> {
		ControlFlow::Continue(())
	}
	fn visit_expr(&mut self, arg: &Expr) -> ControlFlow<()> {
		ControlFlow::Continue(())
	}
	fn visit_atom(&mut self, arg: &Atom) {}
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
		match self {
			Arg::Label(t) => f.visit_label(t),
			Arg::Tuple(t) => t.accept(f),
			Arg::Code(t) => t.accept(f),
			Arg::Expr(t) => t.accept(f),
			Arg::Atom(t) => t.accept(f),
		}
	}
}

impl Visitable for Atom {
	fn accept(&self, f: &mut impl Visit) {
		f.visit_atom(self)
	}
}

impl Visitable for Expr {
	fn accept(&self, f: &mut impl Visit) {
		if f.visit_expr(self).is_continue() {
			match self {
				Expr::Atom(t) => t.accept(f),
				Expr::Bin(_, l, r) => {
					l.accept(f);
					r.accept(f);
				}
				Expr::Unary(_, t) | Expr::Assign(_, t) => {
					t.accept(f);
				}
				Expr::Insn(t) => t.accept(f),
				Expr::Rand => {}
			}
		}
	}
}

pub fn visit_atoms(val: &(impl Visitable + ?Sized), f: impl FnMut(&Atom)) {
	struct Vis<F> {
		f: F,
	}
	impl<F> Visit for Vis<F>
	where
		F: FnMut(&Atom),
	{
		fn visit_atom(&mut self, arg: &Atom) {
			(self.f)(arg);
		}
	}
	val.accept(&mut Vis { f })
}

pub fn visit_labels(val: &(impl Visitable + ?Sized), f: impl FnMut(&Label)) {
	struct Vis<F> {
		f: F,
	}
	impl<F> Visit for Vis<F>
	where
		F: FnMut(&Label),
	{
		fn visit_label(&mut self, arg: &Label) {
			(self.f)(arg);
		}
	}
	val.accept(&mut Vis { f })
}
