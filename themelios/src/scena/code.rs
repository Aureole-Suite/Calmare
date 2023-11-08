use gospel::read::Reader;
use gospel::write::Writer;
use snafu::prelude::*;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Code;

pub struct InsnTable;

#[derive(Debug, Snafu)]
pub enum ReadError {
	Foo,
}

#[derive(Debug, Snafu)]
pub enum WriteError {
	Bar,
}

impl Code {
	pub fn read(
		start: &mut Reader,
		insn: &InsnTable,
		end: Option<usize>,
	) -> Result<Code, ReadError> {
		todo!()
	}

	pub fn write(code: &mut Writer, insn: &InsnTable, func: &Code) -> Result<(), WriteError> {
		todo!()
	}
}
