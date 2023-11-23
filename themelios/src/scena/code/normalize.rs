use std::collections::{BTreeMap, BTreeSet};
use std::ops::ControlFlow;

use super::visit::{Visit, Visitable};
use super::visit_mut::{VisitMut, VisitableMut};
use super::{Arg, Code, Insn};
use snafu::prelude::*;

#[derive(Debug, Snafu)]
pub enum NormalizeError {
	#[snafu(display("duplicate label: {label}"))]
	DuplicateLabel { label: usize },
	#[snafu(display("undefined label: {label}"))]
	UndefinedLabel { label: usize },
}

pub fn normalize(code: &mut Code) -> Result<(), NormalizeError> {
	let used = find_used(code)?;
	let order = remove_unused(code, &used);
	rename(code, |l| order[&l]);
	Ok(())
}

fn find_used(code: &Code) -> Result<BTreeSet<usize>, NormalizeError> {
	struct Vis {
		used: BTreeSet<usize>,
		defined: BTreeSet<usize>,
		duplicate: Option<usize>,
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

		fn visit_arg(&mut self, arg: &Arg) -> ControlFlow<()> {
			if let Arg::Label(l) = arg {
				self.used.insert(*l);
			}
			ControlFlow::Continue(())
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

fn remove_unused(code: &mut Code, used: &BTreeSet<usize>) -> BTreeMap<usize, usize> {
	struct Vis<'a> {
		used: &'a BTreeSet<usize>,
		order: BTreeMap<usize, usize>,
	}

	impl<'a> VisitMut for Vis<'a> {
		fn visit_code_mut(&mut self, code: &mut Code) -> ControlFlow<()> {
			code.retain_mut(|insn| {
				if let ("_label", [Arg::Label(l)]) = insn.parts() {
					if self.used.contains(l) {
						self.order.insert(*l, self.order.len());
					} else {
						return false;
					}
				}
				true
			});
			ControlFlow::Continue(())
		}
	}

	let mut vis = Vis {
		used,
		order: BTreeMap::new(),
	};
	code.accept_mut(&mut vis);
	vis.order
}

fn rename(code: &mut Code, order: impl FnMut(usize) -> usize) {
	struct Vis<F> {
		order: F,
	}

	impl<F> VisitMut for Vis<F>
	where
		F: FnMut(usize) -> usize,
	{
		fn visit_arg_mut(&mut self, arg: &mut Arg) -> ControlFlow<()> {
			if let Arg::Label(l) = arg {
				*l = (self.order)(*l)
			}
			ControlFlow::Continue(())
		}
	}

	let mut vis = Vis { order };
	code.accept_mut(&mut vis);
}