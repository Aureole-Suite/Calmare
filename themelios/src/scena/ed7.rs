use std::collections::BTreeMap;

use glam::{Mat4, Vec3};
use gospel::read::{Le as _, Reader};
use snafu::prelude::*;
use strict_result::Strict;

use crate::scena::code::Code;
use crate::scena::{insn_set as iset, FuncId};
use crate::types::*;
use crate::util::{list, ReaderExt as _};

use self::battle::BattleRead;

use super::code::visit::visit_args;
use super::code::visit_mut::visit_args_mut;
use super::insn::{Arg, InsnReader};
use super::ReadError;
use super::{CharFlags, ChipId, EntryFlags};

mod battle;

newtype!(AnimId(u32));

#[derive(Debug, Clone, PartialEq)]
pub struct Scena {
	pub path: String,
	pub map: String,
	pub filename: String, // first string in string table (always @FileName in ed6, but valid here)

	pub town: TownId,
	pub bgm: BgmId,
	pub flags: u32,
	pub item_use: FuncId,
	pub unknown_function: FuncId,
	pub system30: u8, // system[30], which is related to car/Merkabah position
	pub includes: [FileId; 6],

	pub chips: Vec<FileId>,
	pub labels: Option<Vec<Label>>,
	pub npcs: Vec<Npc>,
	pub monsters: Vec<Monster>,
	pub events: Vec<Event>,
	pub look_points: Vec<LookPoint>,
	pub animations: Vec<Animation>,
	pub entries: Option<Entry>,

	pub battle: battle::BattleSet,

	pub functions: Vec<Code>,
}

impl Scena {
	pub fn read(iset: &iset::InsnSet, data: &[u8]) -> Result<Scena, ReadError> {
		let mut f = Reader::new(data);

		let path = f.sized_string::<10, _>()?;
		let map = f.sized_string::<10, _>()?;
		let town = TownId(f.u16()?);
		let bgm = BgmId(f.u16()?);
		let flags = f.u32()?;
		let includes = std::array::try_from_fn(|_| {
			Ok(FileId(match f.u32()? {
				0xFFFFFFFF => 0,
				a => a,
			}))
		})
		.strict()?;

		let mut strings = f.ptr32()?;
		let strings_start = strings.pos();
		let filename = strings.string()?;

		let p_chips = f.u16()? as usize;
		let p_npcs = f.u16()? as usize;
		let p_monsters = f.u16()? as usize;
		let p_events = f.u16()? as usize;
		let p_look_points = f.u16()? as usize;

		let p_func_table = f.u16()? as usize;
		let func_count = (f.u16()? / 4) as usize;
		let p_animations = f.u16()? as usize;

		let p_labels = f.u16()? as usize;
		let n_labels = f.u8()? as usize;
		let system30 = f.u8()?;

		let n_chips = f.u8()? as usize;
		let n_npcs = f.u8()? as usize;
		let n_monsters = f.u8()? as usize;
		let n_events = f.u8()? as usize;
		let n_look_points = f.u8()? as usize;

		let item_use = FuncId(f.u8()? as u16, f.u8()? as u16);
		let unknown_function = FuncId(item_use.0, f.u8()? as u16); // uncertain about this, but maybe

		let entries = if f.pos() < p_events {
			Some(Entry::read(&mut f)?)
		} else {
			None
		};

		let mut g = f.clone().at(p_chips)?;
		let chips = list(n_chips, || Ok(FileId(g.u32()?))).strict()?;

		let mut g = f.clone().at(p_npcs)?;
		let npcs = list(n_npcs, || Npc::read(&mut g, &mut strings)).strict()?;

		let mut g = f.clone().at(p_monsters)?;
		let mut monsters = list(n_monsters, || Monster::read(&mut g)).strict()?;
		let monsters_end = g.pos();

		let mut g = f.clone().at(p_events)?;
		let events = list(n_events, || Event::read(&mut g)).strict()?;

		let mut g = f.clone().at(p_look_points)?;
		let look_points = list(n_look_points, || LookPoint::read(&mut g)).strict()?;

		let anim_count = (p_func_table - p_animations) / 12;
		let mut g = f.clone().at(p_animations)?;
		let animations = list(anim_count, || Animation::read(&mut g)).strict()?;

		let mut g = f.clone().at(p_func_table)?;
		let func_table = list(func_count, || Ok(g.u32()? as usize)).strict()?;

		let mut functions = Vec::with_capacity(func_table.len());
		let mut funcpos = func_table.iter().copied().peekable();
		let f: &mut Reader = &mut f.clone().at(func_table[0])?;
		let mut ir = InsnReader::new(f, iset);

		while let Some(start) = funcpos.next() {
			ensure_whatever!(ir.pos() == start, "weird function start");
			functions.push(if let Some(end) = funcpos.peek().copied() {
				ir.code(end)?
			} else {
				ir.code_approx(strings_start, |f| (strings_start - f.pos()) % 8 == 0)?
			});
		}
		crate::scena::code::normalize::normalize(&mut functions).unwrap();
		let code_end = f.pos();

		let labels = if p_labels == 0 {
			None
		} else {
			let mut g = f.clone().at(p_labels)?;
			Some(list(n_labels, || Label::read(&mut g)).strict()?)
		};

		// The battle stuff is not as well-delineated as the other parts, using pointers to individual parts.
		// To be able to roundtrip, I first try to load everything sequentially to prepopulate the mappings.
		// This works so-so on eddec, but that's not worth roundtripping anyway.
		let mut btl = BattleRead::default();
		if btl.preload_sepith(f, code_end..strings_start).is_err()
			|| btl.preload_battles(f, monsters_end..p_animations).is_err()
		{
			btl = BattleRead::default();
		}
		load_battles(f, &mut btl, &mut monsters, &mut functions)?;

		Ok(Scena {
			path,
			map,
			filename,
			town,
			bgm,
			flags,
			item_use,
			unknown_function,
			system30,
			includes,
			chips,
			labels,
			npcs,
			monsters,
			events,
			look_points,
			animations,
			entries,
			functions,
			battle: btl.finish(),
		})
	}
}

fn load_battles(
	f: &Reader,
	btl: &mut BattleRead,
	monsters: &mut [Monster],
	functions: &mut [Code],
) -> Result<(), ReadError> {
	for mons in monsters {
		mons.battle = btl.get_battle(&mut f.clone().at(mons.battle.0 as usize)?)?;
	}
	// If I had an iterator this would be way easier, but unfortunately all I have is internal iteration
	let mut battle_pos = Vec::new();
	visit_args(functions, |arg| {
		if let Arg::Battle(battle) = arg {
			battle_pos.push(*battle)
		}
	});
	let battle_pos = battle_pos
		.into_iter()
		.map(|battle| battle.0)
		.map(|i| Ok((i, btl.get_battle(&mut f.clone().at(i as usize)?)?)))
		.collect::<Result<BTreeMap<_, _>, _>>()
		.strict()?;
	visit_args_mut(functions, |arg| {
		if let Arg::Battle(battle) = arg {
			*battle = battle_pos[&battle.0];
		}
	});
	Ok(())
}

#[derive(Debug, Clone, PartialEq)]
pub struct Label {
	pub name: TString,
	pub pos: Vec3,
	pub unk1: u16,
	pub unk2: u16,
}

impl Label {
	fn read(f: &mut Reader) -> Result<Label, ReadError> {
		Ok(Label {
			pos: f.vec3()?,
			unk1: f.u16()?,
			unk2: f.u16()?,
			name: TString(f.ptr32()?.string()?),
		})
	}
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Npc {
	pub name: TString,
	pub pos: Pos3,
	pub angle: Angle,
	pub flags: CharFlags,
	pub unk2: u16,
	pub chip: ChipId,
	pub init: FuncId,
	pub talk: FuncId,
	pub unk4: u32,
}

impl Npc {
	fn read(f: &mut Reader, strings: &mut Reader) -> Result<Npc, ReadError> {
		Ok(Npc {
			name: TString(strings.string()?),
			pos: f.pos3()?,
			angle: Angle(f.i16()?),
			flags: CharFlags(f.u16()?),
			unk2: f.u16()?,
			chip: ChipId(f.u16()?),
			init: FuncId(f.u8()? as u16, f.u8()? as u16),
			talk: FuncId(f.u8()? as u16, f.u8()? as u16),
			unk4: f.u32()?,
		})
	}
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Monster {
	pub pos: Pos3,
	pub angle: i16,
	pub flags: CharFlags,
	pub battle: BattleId,
	pub flag: Flag,
	pub chip: ChipId,
	pub unk2: u16,
	pub stand_anim: AnimId,
	pub walk_anim: AnimId,
}

impl Monster {
	fn read(f: &mut Reader) -> Result<Monster, ReadError> {
		Ok(Monster {
			pos: f.pos3()?,
			angle: f.i16()?,
			flags: CharFlags(f.u16()?),
			battle: BattleId(f.u16()? as u32),
			flag: Flag(f.u16()?),
			chip: ChipId(f.u16()?),
			unk2: f.u16()?,
			stand_anim: AnimId(f.u32()?),
			walk_anim: AnimId(f.u32()?),
		})
	}
}

#[derive(Debug, Clone, PartialEq)]
pub struct Event {
	pub pos: Vec3,
	pub radius: f32,
	pub transform: Mat4,
	pub unk1: u8,
	pub unk2: u16,
	pub function: FuncId,
	pub unk3: u8,
	pub unk4: u16,
	pub unk5: u32,
	pub unk6: u32,
}

impl Event {
	fn read(f: &mut Reader) -> Result<Event, ReadError> {
		Ok(Event {
			pos: f.vec3()?,
			radius: f.f32()?,
			transform: Mat4::from_cols_array(&std::array::try_from_fn(|_| Ok(f.f32()?)).strict()?),
			unk1: f.u8()?,
			unk2: f.u16()?,
			function: FuncId(f.u8()? as u16, f.u8()? as u16),
			unk3: f.u8()?,
			unk4: f.u16()?,
			unk5: f.u32()?,
			unk6: f.u32()?,
		})
	}
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct LookPoint {
	pub pos: Pos3,
	pub radius: u32,
	pub bubble_pos: Pos3,
	pub unk1: u8,
	pub unk2: u16,
	pub function: FuncId,
	pub unk3: u8,
	pub unk4: u16,
}

impl LookPoint {
	fn read(f: &mut Reader) -> Result<LookPoint, ReadError> {
		Ok(LookPoint {
			pos: f.pos3()?,
			radius: f.u32()?,
			bubble_pos: f.pos3()?,
			unk1: f.u8()?,
			unk2: f.u16()?,
			function: FuncId(f.u8()? as u16, f.u8()? as u16),
			unk3: f.u8()?,
			unk4: f.u16()?,
		})
	}
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Entry {
	pub pos: Pos3,
	pub unk1: u32,

	pub cam_from: Pos3,
	pub cam_pers: i32,
	pub unk2: u16,
	pub cam_deg: Angle,
	pub cam_limit: (Angle, Angle),
	pub cam_at: Pos3,
	pub unk3: u16,
	pub unk4: u16,

	pub flags: EntryFlags,
	pub town: TownId,
	pub init: FuncId,
	pub reinit: FuncId,
}

impl Entry {
	fn read(f: &mut Reader) -> Result<Entry, ReadError> {
		Ok(Entry {
			pos: f.pos3()?,
			unk1: f.u32()?,
			cam_from: f.pos3()?,
			cam_pers: f.i32()?,
			unk2: f.u16()?,
			cam_deg: Angle(f.i16()?),
			cam_limit: (Angle(f.i16()?), Angle(f.i16()?)),
			cam_at: f.pos3()?,
			unk3: f.u16()?,
			unk4: f.u16()?,
			flags: EntryFlags(f.u16()?),
			town: TownId(f.u16()?),
			init: FuncId(f.u8()? as u16, f.u8()? as u16),
			reinit: FuncId(f.u8()? as u16, f.u8()? as u16),
		})
	}
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Animation {
	pub speed: Time,
	pub frames: Vec<u8>,
}

impl Animation {
	fn read(f: &mut Reader) -> Result<Animation, ReadError> {
		let speed = Time(f.u16()? as u32);
		f.check_u8(0)?;
		let count = f.u8()? as usize;
		ensure_whatever!(count <= 8, "too many frames: {count}");
		let frames = f.array::<8>()?;
		let frames = frames[..count].to_owned();
		Ok(Animation { speed, frames })
	}
}
