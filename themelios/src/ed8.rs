use strict_result::Strict as _;

use gospel::read::{Le as _, Reader};
use gospel::write::{Label, Le as _, Writer};

use crate::gamedata as iset;
use crate::scena::code::visit::{Visit, Visitable};
use crate::scena::code::visit_mut::{VisitMut, VisitableMut};
use crate::scena::code::{Code, InsnReader, InsnWriter};
use crate::scena::{ReadError, WriteError};
use crate::types::BgmId;
use crate::util::{bail, ensure, list, ReaderExt as _, WriterExt as _};

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
	Battle(Battle),
	Other(Vec<u8>),
}

impl Script {
	pub fn read(iset: &iset::InsnSet, data: &[u8]) -> Result<Script, ReadError> {
		let mut f = Reader::new(data);
		let p = std::array::try_from_fn::<_, 7, _>(|_| Ok(f.u32()? as usize)).strict()?;
		f.check_u32(0xABCDEF00)?;
		ensure!(f.pos() == p[0]);

		let mut script_name = String::default();
		if p[0] == p[1] {
			ensure!(f.pos() == p[1]);
			script_name = f.string()?;
		}

		ensure!(f.pos() == p[2]);
		let funcpos = list(p[3] / 4, || Ok(f.u32()? as usize)).strict()?;

		ensure!(f.pos() == p[4]);
		let funcname: Vec<_> = list(p[5], || Ok(f.u16()? as usize))
			.strict()?
			.into_iter()
			.map(|pos| {
				ensure!(f.pos() == pos);
				Ok(f.string()?)
			})
			.collect::<Result<_, ReadError>>()?;

		if p[0] != p[1] {
			ensure!(f.pos() == p[1]);
			script_name = f.string()?;
		}

		ensure!(f.pos() == p[6]);

		ensure!(funcpos.len() == funcname.len());

		let mut functions = Vec::with_capacity(funcname.len());

		let start = funcpos.iter().copied();
		let end = funcpos.iter().skip(1).copied().chain([f.len()]);
		let mut iter = funcname.into_iter().zip(start.zip(end));
		while let Some((fname, (pos, end))) = iter.next() {
			ensure!(f.pos() <= pos);
			let pad = (pos - f.pos()) / 4;
			while f.pos() < pos {
				f.check_u8(0)?;
			}
			let data = match fname.as_str() {
				_ if script_name == "face" => todo!(),
				_ if script_name.ends_with("_menu") || script_name.ends_with("_menu_v") => todo!(),
				_ if fname.starts_with("BookData") => {
					ensure!(fname.ends_with("_99"));
					let Ok(n) = fname[8..fname.len() - 3].parse::<u16>() else {
						bail!("invalid book name");
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
				"" if f.remaining().starts_with(b"b") => Data::Battle(Battle::read(&mut f, end)?),
				"" if f.remaining().starts_with(&[0xFF, 0xFF, 0xFF, 0xFF]) => {
					Data::Other(f.slice(end - pos)?.to_vec())
				}
				_ => {
					let mut ir = InsnReader::new(f, iset);
					let code = ir.code_approx(end, |f| {
						f.pos() >= end || f.data()[f.pos()..end].iter().all(|a| *a == 0)
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
		let funcnameend = Label::new();
		f.label32(funcnameend);
		f.u32(0xABCDEF00);

		script_name.string(&script.name)?;

		let mut funcbody = Writer::new();
		let mut iw = InsnWriter::new(&mut funcbody, insn, None);
		for func in &script.functions {
			funcnamepos.label16(funcname.here());
			funcname.string(&func.name)?;
			iw.f.align(4);
			for _ in 0..func.pad {
				iw.f.u32(0);
			}
			funcpos.label32(iw.f.here());
			match &func.data {
				Data::Code(code) => iw.code(code)?,
				Data::Battle(battle) => Battle::write(iw.f, battle)?,
				Data::Other(data) => iw.f.slice(data),
			}
		}
		funcname.place(funcnameend);

		f.append(head_end);
		f.append(script_name);
		f.append(funcpos);
		f.append(funcnamepos);
		f.append(funcname);
		f.align(4);
		f.append(funcbody);

		Ok(f.finish()?)
	}
}

#[derive(Debug, Clone, PartialEq)]
pub struct Battle {
	pub battlefield: String,
	pub unk1: (u16, u16),
	pub bgm: (BgmId, BgmId),
	pub unk2: (u32, u32),
	pub setups: Vec<BattleSetup>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum BattleSetup {
	Normal {
		n: u32,
		monsters: [String; 8],
		chance: [u8; 8],
	},
}

impl Battle {
	pub fn read(f: &mut Reader, end: usize) -> Result<Battle, ReadError> {
		let battlefield = f.sized_string::<16, _>()?;
		let unk1 = (f.u16()?, f.u16()?);
		let bgm = (BgmId(f.u16()?), BgmId(f.u16()?));
		let unk2 = (f.u32()?, f.u32()?);

		let mut setups = Vec::new();
		loop {
			match f.u32()? {
				0xFFFFFFFF => break,
				0xFFFFFFFE => {
					todo!() // cs2 only
				}
				n => {
					let monsters =
						std::array::try_from_fn(|_| Ok(f.sized_string::<16, _>()?)).strict()?;
					let chance = f.array()?;
					f.check(&[0; 8])?;
					setups.push(BattleSetup::Normal {
						n,
						monsters,
						chance,
					});
				}
			}
		}

		f.check(&[0; 24])?;
		f.check_u8(1)?;
		Ok(Battle {
			battlefield,
			unk1,
			bgm,
			unk2,
			setups,
		})
	}

	fn write(f: &mut Writer, battle: &Battle) -> Result<(), WriteError> {
		f.sized_string::<16, _>(&battle.battlefield)?;
		f.u16(battle.unk1.0);
		f.u16(battle.unk1.1);
		f.u16(battle.bgm.0 .0);
		f.u16(battle.bgm.1 .0);
		f.u32(battle.unk2.0);
		f.u32(battle.unk2.1);
		for setup in &battle.setups {
			match setup {
				BattleSetup::Normal {
					n,
					monsters,
					chance,
				} => {
					f.u32(*n);
					for m in monsters {
						f.sized_string::<16, _>(m)?;
					}
					f.slice(chance);
					f.slice(&[0; 8]);
				}
			}
		}
		f.u32(0xFFFFFFFF);
		f.slice(&[0; 24]);
		f.u8(1);
		Ok(())
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
