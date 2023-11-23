use std::collections::{BTreeMap, BTreeSet};
use std::ops::ControlFlow;

use gospel::read::Reader;
use gospel::write::Writer;
use snafu::prelude::*;

use crate::scena::insn::{self, Arg, Insn};
use crate::scena::insn_set::InsnSet;

use self::visit::Visitable;

pub mod visit;

#[derive(Debug, Snafu)]
pub enum NormalizeError {
	#[snafu(display("duplicate label: {label}"))]
	DuplicateLabel { label: usize },
	#[snafu(display("undefined label: {label}"))]
	UndefinedLabel { label: usize },
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
		let mut used = BTreeSet::new();
		let mut defined = BTreeSet::new();
		let mut duplicate = None;
		// No mutations here; if the code is malformed, we don't want mutations
		visit_labels(
			self,
			&mut |df| {
				if !defined.insert(df) {
					duplicate = duplicate.or(Some(df))
				}
			},
			&mut |rf| {
				used.insert(rf);
			},
		);

		if let Some(label) = duplicate {
			return DuplicateLabelSnafu { label }.fail();
		}
		if let Some(&label) = used.difference(&defined).next() {
			return UndefinedLabelSnafu { label }.fail();
		}

		let mut order = BTreeMap::new();
		visit_labels_mut(
			self,
			&mut |df| {
				if used.contains(df) {
					let v = order.len();
					order.insert(*df, v);
					*df = v;
					true
				} else {
					false
				}
			},
			&mut |_| {},
		);
		visit_labels_mut(self, &mut |_| true, &mut |rf| *rf = order[rf]);

		Ok(())
	}
}

struct LabelVisitor<'a, 'b, FRef, FDef> {
	on_def: &'a mut FDef,
	on_ref: &'b mut FRef,
}

impl<'a, 'b, FRef, FDef> visit::Visit for LabelVisitor<'a, 'b, FRef, FDef>
where
	FDef: FnMut(usize),
	FRef: FnMut(usize),
{
	fn visit_insn(&mut self, insn: &Insn) -> ControlFlow<()> {
		if let ("_label", [Arg::Label(l)]) = (insn.name.as_str(), insn.args.as_slice()) {
			(self.on_def)(*l);
			ControlFlow::Break(())
		} else {
			ControlFlow::Continue(())
		}
	}

	fn visit_arg(&mut self, arg: &Arg) -> ControlFlow<()> {
		if let Arg::Label(l) = arg {
			(self.on_ref)(*l);
		}
		ControlFlow::Continue(())
	}
}

impl<'a, 'b, FRef, FDef> visit::VisitMut for LabelVisitor<'a, 'b, FRef, FDef>
where
	FDef: FnMut(&mut usize) -> bool,
	FRef: FnMut(&mut usize),
{
	fn visit_code_mut(&mut self, code: &mut Code) -> ControlFlow<()> {
		code.retain_mut(|insn| {
			if let ("_label", [Arg::Label(l)]) = (insn.name.as_str(), insn.args.as_mut_slice()) {
				(self.on_def)(l)
			} else {
				insn.accept_mut(self);
				true
			}
		});
		ControlFlow::Break(()) // since we're visiting the instructions manually
	}

	fn visit_arg_mut(&mut self, arg: &mut Arg) -> ControlFlow<()> {
		if let Arg::Label(l) = arg {
			(self.on_ref)(l);
		}
		ControlFlow::Continue(())
	}
}

fn visit_labels_mut(
	insns: &mut Code,
	on_def: &mut impl FnMut(&mut usize) -> bool,
	on_ref: &mut impl FnMut(&mut usize),
) {
	insns.accept_mut(&mut LabelVisitor { on_def, on_ref })
}

fn visit_labels(insns: &Code, on_def: &mut impl FnMut(usize), on_ref: &mut impl FnMut(usize)) {
	insns.accept(&mut LabelVisitor { on_def, on_ref })
}
