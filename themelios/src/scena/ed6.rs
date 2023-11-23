use gospel::read::{Le as _, Reader};
use gospel::write::{Le as _, Writer};
use snafu::prelude::*;
use strict_result::Strict;

use crate::scena::code::Code;
use crate::scena::insn::{InsnWriter, InsnReader};
use crate::scena::{insn_set as iset, FuncId};
use crate::types::*;
use crate::util::{self, cast, list, ReaderExt as _, WriterExt as _};

use super::{CharFlags, ChipId, EntryFlags, EventFlags, LookPointFlags};

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Scena {
	pub path: String,          // [Path; フォルダ]
	pub map: String,           // [Map; マップファイル]
	pub town: TownId,          // [Town; 町名]
	pub bgm: BgmId,            // [BGM; BGM 番号]
	pub item_use: FuncId,      // [Item; アイテム使用時イベント]
	pub includes: [FileId; 8], // [Scp0..7; スクリプト(１つだけは必須), これ以降は必要な場合のみ定義する]

	// The script puts cp before ch.
	pub ch: Vec<FileId>, // [Char_Data; キャラデータファイル]
	pub cp: Vec<FileId>, // [Char_Ptn; キャラパターンファイル]

	pub npcs: Vec<Npc>,
	pub monsters: Vec<Monster>,
	pub events: Vec<Event>,
	pub look_points: Vec<LookPoint>,
	pub entries: Vec<Entry>,
	pub functions: Vec<Code>,
}

#[derive(Debug, Snafu)]
pub enum ReadError {
	#[snafu(context(false))]
	Gospel { source: gospel::read::Error },
	#[snafu(context(false))]
	Decode { source: util::DecodeError },
	#[snafu(context(false))]
	Insn { source: super::insn::ReadError },
	#[snafu(whatever, display("{message}"))]
	Whatever { message: String },
}

#[derive(Debug, Snafu)]
pub enum WriteError {
	#[snafu(context(false))]
	Gospel { source: gospel::write::Error },
	#[snafu(context(false))]
	Value { source: util::ValueError },
	#[snafu(context(false))]
	Insn { source: super::insn::WriteError },
	#[snafu(context(false))]
	Encode { source: util::EncodeError },
}

impl Scena {
	pub fn read(insn: &iset::InsnSet, data: &[u8]) -> Result<Scena, ReadError> {
		let mut f = Reader::new(data);

		let path = f.sized_string::<10, _>()?;
		let map = f.sized_string::<14, _>()?;
		let town = TownId(f.u16()?);
		let bgm = BgmId(f.u16()?);
		let item_use = FuncId(f.u16()?, f.u16()?);
		let includes = std::array::try_from_fn(|_| {
			Ok(FileId(match f.u32()? {
				0xFFFFFFFF => 0,
				a => a,
			}))
		})
		.strict()?;
		f.check_u16(0)?;

		let head_end = f.clone().u16()? as usize;

		let ch = (f.ptr16()?, f.u16()?);
		let cp = (f.ptr16()?, f.u16()?);
		let npcs = (f.ptr16()?, f.u16()?);
		let monsters = (f.ptr16()?, f.u16()?);
		let events = (f.ptr16()?, f.u16()?);
		let look_points = (f.ptr16()?, f.u16()?);

		let mut strings = f.ptr16()?;

		let code_start = f.u16()? as usize;
		f.check_u16(0)?;
		let code_end = f.clone().u16()? as usize;
		let func_table = (f.ptr16()?, f.u16()? / 2);

		ensure_whatever!(strings.string()? == "@FileName", "expected @FileName");

		let (mut g, n) = ch;
		let ch = list(n as usize, || Ok(FileId(g.u32()?))).strict()?;

		let (mut g, n) = cp;
		let cp = list(n as usize, || Ok(FileId(g.u32()?))).strict()?;

		let (mut g, n) = npcs;
		let npcs = list(n as usize, || Npc::read(&mut g, &mut strings)).strict()?;

		let (mut g, n) = monsters;
		let monsters = list(n as usize, || Monster::read(&mut g, &mut strings)).strict()?;

		let (mut g, n) = events;
		let events = list(n as usize, || Event::read(&mut g)).strict()?;

		let (mut g, n) = look_points;
		let look_points = list(n as usize, || LookPoint::read(&mut g)).strict()?;

		let (mut g, n) = func_table;
		let func_table = list(n as usize, || Ok(g.u16()? as usize)).strict()?;
		ensure_whatever!(
			func_table.is_empty() || func_table[0] == code_start,
			"Unexpected func table: {func_table:X?} does not start with {code_start:X?}"
		);

		let mut entries = Vec::new();
		while f.pos() < head_end {
			entries.push(Entry::read(&mut f)?);
		}
		ensure_whatever!(f.pos() == head_end, "overshot with entries");

		let mut functions = Vec::with_capacity(func_table.len());
		let mut funcpos = func_table.iter().copied().peekable();
		let f: &mut Reader = &mut f.clone().at(code_start)?;
		let mut ir = InsnReader::new(f, insn);
		while let Some(start) = funcpos.next() {
			ensure_whatever!(ir.pos() == start, "weird function start");
			functions.push(ir.code(funcpos.peek().copied().unwrap_or(code_end))?);
		}
		crate::scena::code::normalize::normalize(&mut functions).unwrap();

		Ok(Scena {
			path,
			map,
			town,
			bgm,
			item_use,
			includes,
			ch,
			cp,
			npcs,
			monsters,
			events,
			look_points,
			entries,
			functions,
		})
	}

	pub fn write(insn: &iset::InsnSet, scena: &Scena) -> Result<Vec<u8>, WriteError> {
		let mut f = Writer::new();

		f.sized_string::<10, _>(&scena.path)?;
		f.sized_string::<14, _>(&scena.map)?;
		f.u16(scena.town.0);
		f.u16(scena.bgm.0);
		f.u16(scena.item_use.0);
		f.u16(scena.item_use.1);
		for i in scena.includes {
			f.u32(match i.0 {
				0 => 0xFFFFFFFF,
				a => a,
			});
		}
		f.u16(0);

		let mut chs = f.ptr16();
		f.u16(cast(scena.ch.len())?);

		let mut cps = f.ptr16();
		f.u16(cast(scena.cp.len())?);

		let mut npcs = f.ptr16();
		f.u16(cast(scena.npcs.len())?);

		let mut monsters = f.ptr16();
		f.u16(cast(scena.monsters.len())?);

		let mut events = f.ptr16();
		f.u16(cast(scena.events.len())?);

		let mut lps = f.ptr16();
		f.u16(cast(scena.look_points.len())?);

		let mut strings = f.ptr16();
		strings.string("@FileName")?;

		let mut code = f.ptr16();
		f.u16(0);
		let mut func_table = f.ptr16();
		f.u16(cast(scena.functions.len() * 2)?);

		for e in &scena.entries {
			e.write(&mut f)?;
		}

		for ch in &scena.ch {
			chs.u32(ch.0);
		}
		chs.u8(0xFF);

		for cp in &scena.cp {
			cps.u32(cp.0);
		}
		cps.u8(0xFF);

		for npc in &scena.npcs {
			npc.write(&mut npcs, &mut strings)?;
		}

		for monster in &scena.monsters {
			monster.write(&mut monsters, &mut strings)?;
		}

		for event in &scena.events {
			event.write(&mut events);
		}

		for lp in &scena.look_points {
			lp.write(&mut lps)?;
		}

		let mut iw = InsnWriter::new(&mut code, insn);
		for func in scena.functions.iter() {
			func_table.label16(iw.here());
			iw.code(func)?;
		}

		f.append(chs);
		f.append(cps);
		f.append(npcs);
		f.append(monsters);
		f.append(events);
		f.append(lps);
		f.append(code);
		f.append(func_table);
		f.append(strings);
		Ok(f.finish()?)
	}
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Entry {
	// [Entry]
	pub pos: Pos3,    // [PlyX, PlyY, PlyZ; Ｘ/Ｙ/Ｚ座標(1m単位)]
	pub chr: u16,     // [PlyChr; キャラパターン] Always 4
	pub angle: Angle, // [PlyVec; キャラ方角]

	pub cam_from: Pos3,            // [CameraFrom: カメラ位置(1m単位)]
	pub cam_at: Pos3,              // [CameraAt; 注目点⟩]
	pub cam_zoom: i32,             // [CameraZoom; ズーム(1mm単位)]
	pub cam_pers: i32,             // [CameraPers; パース]
	pub cam_deg: Angle,            // [CameraDeg; 角度(1度単位)]
	pub cam_limit: (Angle, Angle), // [CameraLimitDeg; カメラの回転可能角度]
	pub north: Angle,              // [NorthDeg; 北角度]

	pub flags: EntryFlags, // [Flag]
	pub town: TownId,      // [Place; 地名]
	pub init: FuncId,      // [Init; 初期化用イベント]
	pub reinit: FuncId,    // [ReInit; ロード後の再初期化用イベント]
}

impl Entry {
	fn read(f: &mut Reader) -> Result<Entry, ReadError> {
		Ok(Entry {
			pos: f.pos3()?,
			chr: f.u16()?,
			angle: Angle(f.i16()?),
			cam_from: f.pos3()?,
			cam_at: f.pos3()?,
			cam_zoom: f.i32()?,
			cam_pers: f.i32()?,
			cam_deg: Angle(f.i16()?),
			cam_limit: (Angle(f.i16()?), Angle(f.i16()?)),
			north: Angle(f.i16()?),
			flags: EntryFlags(f.u16()?),
			town: TownId(f.u16()?),
			init: FuncId(f.u16()?, f.u16()?),
			reinit: FuncId(f.u16()?, f.u16()?),
		})
	}

	fn write(&self, f: &mut Writer) -> Result<(), WriteError> {
		f.pos3(self.pos);
		f.u16(self.chr);
		f.i16(self.angle.0);
		f.pos3(self.cam_from);
		f.pos3(self.cam_at);
		f.i32(self.cam_zoom);
		f.i32(self.cam_pers);
		f.i16(self.cam_deg.0);
		f.i16(self.cam_limit.0 .0);
		f.i16(self.cam_limit.1 .0);
		f.i16(self.north.0);
		f.u16(self.flags.0);
		f.u16(self.town.0);
		f.u16(self.init.0);
		f.u16(self.init.1);
		f.u16(self.reinit.0);
		f.u16(self.reinit.1);
		Ok(())
	}
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Npc {
	// [Player]
	pub name: TString,    // [Name]
	pub pos: Pos3,        // [X, Y, Z]
	pub angle: Angle,     // [ANG]
	pub x: u16,           // [X]
	pub cp: ChipId,       // [Pt]
	pub frame: u16,       // [No]
	pub ch: ChipId,       // [Bs]
	pub flags: CharFlags, // [BXPNAWTDS]
	pub init: FuncId,     // [MOVE_FUNC]
	pub talk: FuncId,     // [EVENT_FUNC]
}

impl Npc {
	fn read(f: &mut Reader, strings: &mut Reader) -> Result<Npc, ReadError> {
		Ok(Npc {
			name: TString(strings.string()?),
			pos: f.pos3()?,
			angle: Angle(f.i16()?),
			x: f.u16()?,
			cp: ChipId(f.u16()?),
			frame: f.u16()?,
			ch: ChipId(f.u16()?),
			flags: CharFlags(f.u16()?),
			init: FuncId(f.u16()?, f.u16()?),
			talk: FuncId(f.u16()?, f.u16()?),
		})
	}

	fn write(&self, f: &mut Writer, strings: &mut Writer) -> Result<(), WriteError> {
		strings.string(self.name.as_str())?;
		f.pos3(self.pos);
		f.i16(self.angle.0);
		f.u16(self.x);
		f.u16(self.cp.0);
		f.u16(self.frame);
		f.u16(self.ch.0);
		f.u16(self.flags.0);
		f.u16(self.init.0);
		f.u16(self.init.1);
		f.u16(self.talk.0);
		f.u16(self.talk.1);
		Ok(())
	}
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Monster {
	// [Monster]
	pub name: TString,
	pub pos: Pos3,
	pub angle: Angle,
	pub chip: ChipId, // This looks like a chip index, but npcs have 4×u16 while this only has 1×u16?
	pub flags: CharFlags,
	pub unk2: i32, // Always -1
	pub battle: BattleId,
	pub flag: Flag, // set when defeated
	pub unk3: u16,
}

impl Monster {
	fn read(f: &mut Reader, strings: &mut Reader) -> Result<Monster, ReadError> {
		Ok(Monster {
			name: TString(strings.string()?),
			pos: f.pos3()?,
			angle: Angle(f.i16()?),
			chip: ChipId(f.u16()?),
			flags: CharFlags(f.u16()?),
			unk2: f.i32()?,
			battle: BattleId(f.u16()? as u32),
			flag: Flag(f.u16()?),
			unk3: f.u16()?,
		})
	}

	fn write(&self, f: &mut Writer, strings: &mut Writer) -> Result<(), WriteError> {
		strings.string(self.name.as_str())?;
		f.pos3(self.pos);
		f.i16(self.angle.0);
		f.u16(self.chip.0);
		f.u16(self.flags.0);
		f.i32(self.unk2);
		f.u16(cast(self.battle.0)?);
		f.u16(self.flag.0);
		f.u16(self.unk3);
		Ok(())
	}
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Event {
	// [Event]
	pub pos1: Pos3,        // [X, Y, Z]
	pub pos2: Pos3,        // [X, Y, Z]
	pub flags: EventFlags, // [  SN6428]
	pub func: FuncId,      // [Scp:Func]
	pub unk1: u16,         // (absent)
}

impl Event {
	fn read(f: &mut Reader) -> Result<Event, ReadError> {
		Ok(Event {
			pos1: f.pos3()?,
			pos2: f.pos3()?,
			flags: EventFlags(f.u16()?),
			func: FuncId(f.u16()?, f.u16()?),
			unk1: f.u16()?,
		})
	}

	fn write(&self, f: &mut Writer) {
		f.pos3(self.pos1);
		f.pos3(self.pos2);
		f.u16(self.flags.0);
		f.u16(self.func.0);
		f.u16(self.func.1);
		f.u16(self.unk1);
	}
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct LookPoint {
	// [LookPoint]
	pub pos: Pos3,             // [X, Y, Z]
	pub radius: Length,        // [R],
	pub bubble_pos: Pos3,      // (absent)
	pub flags: LookPointFlags, // [_N____],
	pub func: FuncId,          // [Scp:Func]
	pub unk1: u16,             // (absent)
}

impl LookPoint {
	fn read(f: &mut Reader) -> Result<LookPoint, ReadError> {
		Ok(LookPoint {
			pos: f.pos3()?,
			radius: Length(f.i32()?),
			bubble_pos: f.pos3()?,
			flags: LookPointFlags(f.u16()?),
			func: FuncId(f.u16()?, f.u16()?),
			unk1: f.u16()?,
		})
	}

	fn write(&self, f: &mut Writer) -> Result<(), WriteError> {
		f.pos3(self.pos);
		f.i32(self.radius.0);
		f.pos3(self.bubble_pos);
		f.u16(cast(self.flags.0)?);
		f.u16(self.func.0);
		f.u16(self.func.1);
		f.u16(self.unk1);
		Ok(())
	}
}
