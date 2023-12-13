use std::ops::ControlFlow;

use super::{
	insn::{Atom, Expr},
	Arg, Code, Insn,
};
use crate::types::Label;

#[allow(unused_variables)]
pub trait VisitMut {
	fn visit_label_mut(&mut self, arg: &mut Label) {}
	fn visit_code_mut(&mut self, code: &mut Code) -> ControlFlow<()> {
		ControlFlow::Continue(())
	}
	fn visit_insn_mut(&mut self, insn: &mut Insn) -> ControlFlow<()> {
		ControlFlow::Continue(())
	}
	fn visit_expr_mut(&mut self, arg: &mut Expr) -> ControlFlow<()> {
		ControlFlow::Continue(())
	}
	fn visit_atom_mut(&mut self, arg: &mut Atom) {}
}

pub trait VisitableMut: super::visit::Visitable {
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
		match self {
			Arg::Label(t) => f.visit_label_mut(t),
			Arg::Tuple(t) => t.accept_mut(f),
			Arg::Code(t) => t.accept_mut(f),
			Arg::Expr(t) => t.accept_mut(f),
			Arg::Atom(t) => t.accept_mut(f),
		}
	}
}

impl VisitableMut for Atom {
	fn accept_mut(&mut self, f: &mut impl VisitMut) {
		f.visit_atom_mut(self)
	}
}

impl VisitableMut for Expr {
	fn accept_mut(&mut self, f: &mut impl VisitMut) {
		if f.visit_expr_mut(self).is_continue() {
			match self {
				Expr::Atom(t) => t.accept_mut(f),
				Expr::Bin(_, l, r) => {
					l.accept_mut(f);
					r.accept_mut(f);
				}
				Expr::Unary(_, t) | Expr::Assign(_, t) => {
					t.accept_mut(f);
				}
				Expr::Insn(t) => t.accept_mut(f),
				Expr::Rand => {}
			}
		}
	}
}

pub fn visit_atoms_mut(val: &mut (impl VisitableMut + ?Sized), f: impl FnMut(&mut Atom)) {
	struct Vis<F> {
		f: F,
	}
	impl<F> VisitMut for Vis<F>
	where
		F: FnMut(&mut Atom),
	{
		fn visit_atom_mut(&mut self, arg: &mut Atom) {
			(self.f)(arg);
		}
	}
	val.accept_mut(&mut Vis { f })
}

pub fn visit_labels_mut(val: &mut (impl VisitableMut + ?Sized), f: impl FnMut(&mut Label)) {
	struct Vis<F> {
		f: F,
	}
	impl<F> VisitMut for Vis<F>
	where
		F: FnMut(&mut Label),
	{
		fn visit_label_mut(&mut self, arg: &mut Label) {
			(self.f)(arg);
		}
	}
	val.accept_mut(&mut Vis { f })
}
