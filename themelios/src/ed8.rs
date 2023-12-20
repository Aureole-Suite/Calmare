use std::ffi::CStr;

use snafu::prelude::*;
use strict_result::Strict as _;

use gospel::read::{Le as _, Reader};
use gospel::write::{Le as _, Writer};

use crate::gamedata as iset;
use crate::scena::code::visit::{Visit, Visitable};
use crate::scena::code::visit_mut::{VisitMut, VisitableMut};
use crate::scena::code::{Code, InsnReader, InsnWriter};
use crate::scena::{ReadError, WriteError};
use crate::util::{cast, list, ReaderExt as _, WriterExt as _};

#[derive(Debug, Clone, PartialEq)]
pub struct Script {
	pub name: String,
	pub functions: Vec<Function>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Function {
	pub name: String,
	pub pad: usize,
	pub data: Data,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Data {
	Code(Code),
	Other(Vec<u8>),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Padding(pub usize);

impl Script {
	pub fn read(iset: &iset::InsnSet, data: &[u8]) -> Result<Script, ReadError> {
		let mut f = Reader::new(data);
		let p = std::array::try_from_fn::<_, 7, _>(|_| Ok(f.u32()? as usize)).strict()?;
		f.check_u32(0xABCDEF00)?;
		ensure_whatever!(f.pos() == p[0], "p[0]");

		let mut script_name = String::default();
		if p[0] == p[1] {
			ensure_whatever!(f.pos() == p[1], "p[1]");
			script_name = f.string()?;
		}

		ensure_whatever!(f.pos() == p[2], "p[2]");
		let funcpos = list(p[3] / 4, || Ok(f.u32()? as usize)).strict()?;

		ensure_whatever!(f.pos() == p[4], "p[4]");
		let funcname: Vec<_> = list(p[5], || Ok(f.u16()? as usize))
			.strict()?
			.into_iter()
			.map(|pos| {
				ensure_whatever!(f.pos() == pos, "funcnamepos");
				Ok(f.string()?)
			})
			.collect::<Result<_, ReadError>>()?;

		if p[0] != p[1] {
			ensure_whatever!(f.pos() == p[1], "p[1]");
			script_name = f.string()?;
		}

		ensure_whatever!(f.pos() == p[6], "p[6]");

		ensure_whatever!(funcpos.len() == funcname.len(), "functbl");

		let mut functions = Vec::with_capacity(funcname.len());

		let start = funcpos.iter().copied();
		let end = funcpos.iter().skip(1).copied().chain([f.len()]);
		let mut iter = funcname.into_iter().zip(start.zip(end));
		while let Some((fname, (pos, end))) = iter.next() {
			ensure_whatever!(f.pos() <= pos, "funcpos");
			let pad = pos - f.pos();
			while f.pos() < pos {
				f.check_u8(0)?;
			}
			let slice = &data[pos..end];
			let data = match fname.as_str() {
				_ if script_name == "face" => todo!(),
				_ if script_name.ends_with("_menu") || script_name.ends_with("_menu_v") => todo!(),
				_ if fname.starts_with("BookData") => {
					ensure_whatever!(fname.ends_with("_99"), "invalid book name");
					let Ok(n) = fname[8..fname.len() - 3].parse::<u16>() else {
						whatever!("invalid book name");
					};
					iter.next(); // Silence clippy for now
					todo!()
				}
				"ActionTable" => todo!(),
				"ConditionTable" => todo!(),
				"AlgoTable" => todo!(),
				"SummonTable" => todo!(),
				"FieldMonsterData" => todo!(),
				"PartTable" => todo!(),
				"AnimeClipTable" => todo!(),
				"ReactionTable" => todo!(),
				"WeaponAttTable" => todo!(),
				"BreakTable" => todo!(),
				"AddCollision" => todo!(),
				"" if slice.starts_with(b"b") => todo!(),
				_ => {
					let mut ir = InsnReader::new(f, iset);
					let code = ir.code_approx(end, |f| {
						f.pos() >= end || f.remaining().starts_with(b"\0")
					})?;
					f = ir.into_inner();
					Data::Code(code)
				}
			};
			functions.push(Function {
				name: fname,
				data,
				pad,
			});
		}

		Ok(Script {
			name: script_name,
			functions,
		})
	}

	pub fn write(insn: &iset::InsnSet, script: &Script) -> Result<Vec<u8>, WriteError> {
		let mut f = Writer::new();
		let head_end = f.ptr32();
		let mut script_name = f.ptr32();
		let mut funcpos = f.ptr32();
		f.u32((script.functions.len() * 4) as u32);
		let mut funcnamepos = f.ptr32();
		let mut funcname = Writer::new();
		f.u32((script.functions.len()) as u32);
		let mut funcbody = f.ptr32();
		f.u32(0xABCDEF00);

		script_name.string(&script.name)?;

		for func in &script.functions {
			funcnamepos.label16(funcname.here());
			funcname.string(&func.name)?;
			for _ in 0..func.pad {
				funcbody.u8(0);
			}
			funcpos.label32(funcbody.here());
			match &func.data {
				Data::Code(code) => InsnWriter::new(&mut funcbody, insn, None).code(code)?,
				Data::Other(data) => funcbody.slice(data),
			}
		}

		f.append(head_end);
		f.append(script_name);
		f.append(funcpos);
		f.append(funcnamepos);
		f.append(funcname);
		f.append(funcbody);

		Ok(f.finish()?)
	}
}

impl Visitable for Function {
	fn accept(&self, f: &mut impl Visit) {
		self.data.accept(f)
	}
}

impl VisitableMut for Function {
	fn accept_mut(&mut self, f: &mut impl VisitMut) {
		self.data.accept_mut(f)
	}
}

impl Visitable for Data {
	fn accept(&self, f: &mut impl Visit) {
		if let Data::Code(code) = self {
			code.accept(f)
		}
	}
}

impl VisitableMut for Data {
	fn accept_mut(&mut self, f: &mut impl VisitMut) {
		if let Data::Code(code) = self {
			code.accept_mut(f)
		}
	}
}
