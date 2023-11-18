use std::collections::BTreeMap;

use gospel::write::{Label, Le as _, Writer};
use snafu::prelude::*;

use super::{Arg, Insn};
use crate::scena::code::Code;
use crate::scena::insn::Unit;
use crate::scena::{expr::Expr, insn_set as iset, CharId, EventId, FuncId, LookPointId};
use crate::types::*;
use crate::util::WriterExt;

#[derive(Debug, Snafu)]
pub enum WriteError {
	#[snafu(context(false))]
	Gospel { source: gospel::write::Error },
	#[snafu(context(false))]
	Encode { source: crate::util::EncodeError },
	#[snafu(context(false))]
	Value { source: crate::util::ValueError },
	#[snafu(context(false))]
	Code {
		source: Box<crate::scena::code::WriteError>,
	},
	#[snafu(context(false))]
	Expr {
		source: Box<crate::scena::expr::WriteError>,
	},
	#[snafu(whatever, display("{message}"))]
	Whatever { message: String },
}

pub struct InsnWriter<'a, 'b> {
	f: &'b mut Writer,
	iset: &'a iset::InsnSet,
	labels: BTreeMap<usize, Label>,
}

impl<'a, 'b> InsnWriter<'a, 'b> {
	pub fn new(f: &'b mut Writer, iset: &'a iset::InsnSet) -> Self {
		InsnWriter {
			f,
			iset,
			labels: BTreeMap::new(),
		}
	}

	pub fn insn(&mut self, insn: &Insn) -> Result<(), WriteError> {
		if insn.name == "_label" {
			let [Arg::Label(label)] = insn.args.as_slice() else {
				whatever!("")
			};
			let here = self.f.here();
			self.labels.insert(*label, here);
		} else {
			todo!()
		}
		Ok(())
	}
}
