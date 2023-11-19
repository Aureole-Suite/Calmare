use std::collections::{BTreeMap, BTreeSet};

use gospel::read::Reader;
use gospel::write::Writer;
use snafu::prelude::*;

use crate::scena::insn::{self, Arg, Insn};
use crate::scena::insn_set::InsnSet;

#[derive(Debug, Snafu)]
pub enum ReadError {
	Overshoot {
		pos: usize,
		end: usize,
	},
	Insn {
		context: Vec<(usize, Insn)>,
		pos: usize,
		source: insn::ReadError,
	},
}

#[derive(Debug, Snafu)]
pub enum WriteError {
	#[snafu(context(false))]
	Insn { source: insn::WriteError },
}

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
	pub fn read(f: &mut Reader, insn: &InsnSet, end: Option<usize>) -> Result<Code, ReadError> {
		let end = end.expect("inferred end is not yet supported");

		let mut insns = Vec::new();
		while f.pos() < end {
			let pos = f.pos();
			let insn = Insn::read(f, insn).with_context(|_| InsnSnafu {
				pos,
				context: insns
					.iter()
					.rev()
					.take(8)
					.rev()
					.cloned()
					.collect::<Vec<_>>(),
			})?;
			insns.push((pos, insn));
		}
		ensure!(f.pos() == end, OvershootSnafu { pos: f.pos(), end });

		let mut insns2 = Vec::with_capacity(insns.len() * 2 + 1);
		for (pos, insn) in insns {
			insns2.push(Insn::new("_label", vec![Arg::Label(pos)]));
			insns2.push(insn);
		}
		insns2.push(Insn::new("_label", vec![Arg::Label(f.pos())]));
		let mut code = Code(insns2);
		code.normalize().unwrap();
		Ok(code)
	}

	pub fn write(f: &mut Writer, iset: &InsnSet, code: &Code) -> Result<(), WriteError> {
		let mut writer = insn::InsnWriter::new(f, iset);
		for insn in &code.0 {
			writer.insn(insn)?
		}
		Ok(())
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

fn visit_labels_mut(
	insns: &mut Vec<Insn>,
	on_def: &mut impl FnMut(&mut usize) -> bool,
	on_ref: &mut impl FnMut(&mut usize),
) {
	fn do_args(
		args: &mut [Arg],
		on_def: &mut impl FnMut(&mut usize) -> bool,
		on_ref: &mut impl FnMut(&mut usize),
	) {
		for a in args {
			match a {
				Arg::Label(rf) => on_ref(rf),
				Arg::Code(code) => visit_labels_mut(code, on_def, on_ref),
				Arg::Tuple(args) => do_args(args, on_def, on_ref),
				// Expr can contain Insn, but those never contain labels
				_ => {}
			}
		}
	}
	insns.retain_mut(
		|insn| match (insn.name.as_str(), insn.args.as_mut_slice()) {
			("_label", [Arg::Label(df)]) => on_def(df),
			(_, args) => {
				do_args(args, on_def, on_ref);
				true
			}
		},
	);
}

fn visit_labels(insns: &[Insn], on_def: &mut impl FnMut(usize), on_ref: &mut impl FnMut(usize)) {
	fn do_args(args: &[Arg], on_def: &mut impl FnMut(usize), on_ref: &mut impl FnMut(usize)) {
		for a in args {
			match a {
				Arg::Label(rf) => on_ref(*rf),
				Arg::Code(code) => visit_labels(code, on_def, on_ref),
				Arg::Tuple(args) => do_args(args, on_def, on_ref),
				// Expr can contain Insn, but those never contain labels
				_ => {}
			}
		}
	}
	for insn in insns {
		match (insn.name.as_str(), insn.args.as_slice()) {
			("_label", [Arg::Label(df)]) => on_def(*df),
			(_, args) => do_args(args, on_def, on_ref),
		}
	}
}
