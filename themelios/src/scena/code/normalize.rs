use std::collections::{BTreeMap, BTreeSet};
use std::ops::ControlFlow;

use crate::scena::code::visit_mut::visit_labels_mut;
use crate::types::Label;

use super::visit::{Visit, Visitable};
use super::visit_mut::{VisitMut, VisitableMut};
use super::{Arg, Code, Insn};
use snafu::prelude::*;

#[derive(Debug, Snafu)]
pub enum NormalizeError {
	#[snafu(display("duplicate label: {label}"))]
	DuplicateLabel { label: Label },
	#[snafu(display("undefined label: {label}"))]
	UndefinedLabel { label: Label },
}

pub fn normalize(code: &mut impl VisitableMut) -> Result<(), NormalizeError> {
	let used = find_used(code)?;
	remove_unused(code, &used);
	let order = find_order(code);
	visit_labels_mut(code, |l| *l = Label(order[l]));
	Ok(())
}

fn find_used(code: &impl Visitable) -> Result<BTreeSet<Label>, NormalizeError> {
	struct Vis {
		used: BTreeSet<Label>,
		defined: BTreeSet<Label>,
		duplicate: Option<Label>,
	}

	impl Visit for Vis {
		fn visit_insn(&mut self, insn: &Insn) -> ControlFlow<()> {
			if let ("_label", [Arg::Label(l)]) = insn.parts() {
				if !self.defined.insert(*l) {
					self.duplicate = self.duplicate.or(Some(*l))
				}
				ControlFlow::Break(())
			} else {
				ControlFlow::Continue(())
			}
		}

		fn visit_label(&mut self, arg: &Label) {
			self.used.insert(*arg);
		}
	}

	let mut vis = Vis {
		used: BTreeSet::new(),
		defined: BTreeSet::new(),
		duplicate: None,
	};
	code.accept(&mut vis);

	if let Some(label) = vis.duplicate {
		return DuplicateLabelSnafu { label }.fail();
	}
	if let Some(&label) = vis.used.difference(&vis.defined).next() {
		return UndefinedLabelSnafu { label }.fail();
	}
	Ok(vis.used)
}

fn remove_unused(code: &mut impl VisitableMut, used: &BTreeSet<Label>) {
	struct Vis<'a> {
		used: &'a BTreeSet<Label>,
	}

	impl<'a> VisitMut for Vis<'a> {
		fn visit_code_mut(&mut self, code: &mut Code) -> ControlFlow<()> {
			code.retain_mut(|insn| {
				if let ("_label", [Arg::Label(l)]) = insn.parts() {
					self.used.contains(l)
				} else {
					true
				}
			});
			ControlFlow::Continue(())
		}
	}

	let mut vis = Vis { used };
	code.accept_mut(&mut vis);
}

fn find_order(code: &impl Visitable) -> BTreeMap<Label, usize> {
	struct Vis {
		order: BTreeMap<Label, usize>,
	}

	impl Visit for Vis {
		fn visit_insn(&mut self, insn: &Insn) -> ControlFlow<()> {
			if let ("_label", [Arg::Label(l)]) = insn.parts() {
				self.order.insert(*l, self.order.len());
			}
			ControlFlow::Continue(())
		}
	}

	let mut vis = Vis {
		order: BTreeMap::new(),
	};
	code.accept(&mut vis);
	vis.order
}
