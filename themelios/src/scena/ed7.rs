use std::collections::BTreeMap;

use glam::{Mat4, Vec3};
use gospel::read::{Le as _, Reader};
use gospel::write::{Le as _, Writer};
use strict_result::Strict;

use crate::gamedata as iset;
use crate::scena::code::visit::visit_atoms;
use crate::scena::code::visit_mut::visit_atoms_mut;
use crate::scena::code::{Atom, Code, InsnReader, InsnWriter};
use crate::scena::{CharFlags, ChipId, EntryFlags, FuncId};
use crate::scena::{ReadError, WriteError};
use crate::types::*;
use crate::util::{cast, ensure, list, OptionTExt as _, ReaderExt as _, WriterExt as _};

use self::battle::{BattleRead, BattleWrite};

pub mod battle;

newtype!(AnimId(u32));
newtype!(ScenaFlags(u32));

#[derive(Debug, Clone, PartialEq)]
pub struct Scena {
	pub path: String,
	pub map: String,
	pub filename: String, // first string in string table (always @FileName in ed6, but valid here)

	pub town: TownId,
	pub bgm: BgmId,
	pub flags: ScenaFlags,
	pub item_use: FuncId,
	pub unknown_function: FuncId,
	pub system30: u8, // system[30], which is related to car/Merkabah position
	pub includes: [FileId; 6],

	pub entries: Option<Entry>,
	pub chips: Vec<FileId>,
	pub labels: Option<Vec<Label>>,
	pub npcs: Vec<Npc>,
	pub monsters: Vec<Monster>,
	pub events: Vec<Event>,
	pub look_points: Vec<LookPoint>,
	pub animations: Vec<Animation>,

	pub btlset: battle::BattleSet,

	pub functions: Vec<Code>,
}

impl Scena {
	pub fn read(iset: &iset::InsnSet, data: &[u8]) -> Result<Scena, ReadError> {
		let mut f = Reader::new(data);

		let path = f.sized_string::<10, _>()?;
		let map = f.sized_string::<10, _>()?;
		let town = TownId(f.u16()?);
		let bgm = BgmId(f.u16()?);
		let flags = ScenaFlags(f.u32()?);
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
		let mut ir = InsnReader::new(f.clone().at(func_table[0])?, iset);

		while let Some(start) = funcpos.next() {
			ensure!(ir.pos() == start, "weird function start");
			functions.push(if let Some(end) = funcpos.peek().copied() {
				ir.code(end)?
			} else {
				ir.code_approx(strings_start, |f| {
					if (strings_start - f.pos()) % 8 == 0 {
						true
					} else if f.remaining().starts_with(&[0]) {
						tracing::warn!("found a9000 null bytes — will not roundtrip");
						true
					} else {
						false
					}
				})?
			});
		}
		crate::scena::code::normalize::normalize(&mut functions).unwrap();
		let code_end = ir.pos();

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
		if btl.preload_sepith(&f, code_end..strings_start).is_err()
			|| btl.preload_battles(&f, monsters_end..p_animations).is_err()
		{
			btl = BattleRead::default();
		}
		load_battles(&f, &mut btl, &mut monsters, &mut functions)?;

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
			entries,
			chips,
			labels,
			npcs,
			monsters,
			events,
			look_points,
			animations,
			btlset: btl.finish(),
			functions,
		})
	}

	pub fn write(insn: &iset::InsnSet, scena: &Scena) -> Result<Vec<u8>, WriteError> {
		let mut f = Writer::new();

		f.sized_string::<10, _>(&scena.path)?;
		f.sized_string::<10, _>(&scena.map)?;
		f.u16(scena.town.0);
		f.u16(scena.bgm.0);
		f.u32(scena.flags.0);
		for i in scena.includes {
			f.u32(match i.0 {
				0 => 0xFFFFFFFF,
				a => a,
			});
		}

		let mut strings = f.ptr32();
		strings.string(&scena.filename)?;

		let mut chips = f.ptr16();
		let mut npcs = f.ptr16();
		let mut monsters = f.ptr16();
		let mut events = f.ptr16();
		let mut look_points = f.ptr16();

		let mut func_table = f.ptr16();
		f.u16(cast(scena.functions.len() * 4)?);
		let mut animations = f.ptr16();

		let mut labels = Writer::new();
		if let Some(l) = &scena.labels {
			f.label16(labels.here());
			f.u8(cast(l.len())?);
		} else {
			f.u16(0);
			f.u8(0);
		}
		f.u8(scena.system30);

		f.u8(cast(scena.chips.len())?);
		f.u8(cast(scena.npcs.len())?);
		f.u8(cast(scena.monsters.len())?);
		f.u8(cast(scena.events.len())?);
		f.u8(cast(scena.look_points.len())?);
		f.u8(cast(scena.item_use.0)?);
		f.u8(cast(scena.item_use.1)?);
		ensure!(scena.item_use.0 == scena.unknown_function.0);
		f.u8(cast(scena.unknown_function.1)?);

		let mut entries = Writer::new();
		let mut code = Writer::new();
		let btl = BattleWrite::write(&scena.btlset)?;

		for chip in &scena.chips {
			chips.u32(chip.0);
		}

		for npc in &scena.npcs {
			npc.write(&mut npcs, &mut strings)?;
		}

		for monster in &scena.monsters {
			monster.write(&mut monsters, &btl.battle_pos)?;
		}

		for event in &scena.events {
			event.write(&mut events)?;
		}

		for lp in &scena.look_points {
			lp.write(&mut look_points)?;
		}

		for entry in &scena.entries {
			entry.write(&mut entries)?;
		}

		for anim in &scena.animations {
			anim.write(&mut animations)?;
		}

		let mut iw = InsnWriter::new(&mut code, insn, Some(&btl.battle_pos));
		for func in &scena.functions {
			func_table.label32(iw.here());
			iw.code(func)?;
		}

		strings.append(btl.strings);

		if let Some(l) = &scena.labels {
			for l in l {
				l.write(&mut labels, &mut strings)?;
			}
		}

		f.append(entries);
		f.append(labels);
		f.append(events);
		f.append(look_points);
		f.append(chips);
		f.append(npcs);
		f.append(monsters);
		f.append(btl.at_rolls);
		f.append(btl.placements);
		f.append(btl.battles);
		f.append(animations);
		f.append(func_table);
		f.append(code);
		f.append(btl.sepith);
		f.append(strings);
		// EDDec has order
		//   header, entries, at_rolls, sepith, placements, battles,
		//   chips, npcs, monsters, events, look_points, labels,
		//   animations, func_table, code, strings
		Ok(f.finish()?)
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
	visit_atoms(functions, |arg| {
		if let Atom::BattleId(battle) = arg {
			battle_pos.push(*battle)
		}
	});
	let battle_pos = battle_pos
		.into_iter()
		.map(|battle| battle.0)
		.map(|i| Ok((i, btl.get_battle(&mut f.clone().at(i as usize)?)?)))
		.collect::<Result<BTreeMap<_, _>, _>>()
		.strict()?;
	visit_atoms_mut(functions, |arg| {
		if let Atom::BattleId(battle) = arg {
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
			name: f.ptr32()?.tstring()?,
		})
	}

	fn write(&self, f: &mut Writer, strings: &mut Writer) -> Result<(), WriteError> {
		f.f32(self.pos.x);
		f.f32(self.pos.y);
		f.f32(self.pos.z);
		f.u16(self.unk1);
		f.u16(self.unk2);
		f.label32(strings.here());
		strings.tstring(&self.name)?;
		Ok(())
	}
}

#[derive(Debug, Clone, PartialEq)]
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
			name: strings.tstring()?,
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

	fn write(&self, f: &mut Writer, strings: &mut Writer) -> Result<(), WriteError> {
		strings.tstring(&self.name)?;
		f.pos3(self.pos);
		f.i16(self.angle.0);
		f.u16(self.flags.0);
		f.u16(self.unk2);
		f.u16(self.chip.0);
		f.u8(cast(self.init.0)?);
		f.u8(cast(self.init.1)?);
		f.u8(cast(self.talk.0)?);
		f.u8(cast(self.talk.1)?);
		f.u32(self.unk4);
		Ok(())
	}
}

#[derive(Debug, Clone, PartialEq)]
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

	fn write(&self, f: &mut Writer, battle_pos: &[gospel::write::Label]) -> Result<(), WriteError> {
		f.pos3(self.pos);
		f.i16(self.angle);
		f.u16(self.flags.0);
		f.label16(
			*battle_pos
				.get(self.battle.0 as usize)
				.or_whatever("battle id out of bounds")?,
		);
		f.u16(self.flag.0);
		f.u16(self.chip.0);
		f.u16(self.unk2);
		f.u32(self.stand_anim.0);
		f.u32(self.walk_anim.0);
		Ok(())
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

	fn write(&self, f: &mut Writer) -> Result<(), WriteError> {
		f.f32(self.pos.x);
		f.f32(self.pos.y);
		f.f32(self.pos.z);
		f.f32(self.radius);
		for v in self.transform.to_cols_array() {
			f.f32(v)
		}
		f.u8(self.unk1);
		f.u16(self.unk2);
		f.u8(cast(self.function.0)?);
		f.u8(cast(self.function.1)?);
		f.u8(self.unk3);
		f.u16(self.unk4);
		f.u32(self.unk5);
		f.u32(self.unk6);
		Ok(())
	}
}

#[derive(Debug, Clone, PartialEq)]
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

	fn write(&self, f: &mut Writer) -> Result<(), WriteError> {
		f.pos3(self.pos);
		f.u32(self.radius);
		f.pos3(self.bubble_pos);
		f.u8(self.unk1);
		f.u16(self.unk2);
		f.u8(cast(self.function.0)?);
		f.u8(cast(self.function.1)?);
		f.u8(self.unk3);
		f.u16(self.unk4);
		Ok(())
	}
}

#[derive(Debug, Clone, PartialEq)]
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

	fn write(&self, f: &mut Writer) -> Result<(), WriteError> {
		f.pos3(self.pos);
		f.u32(self.unk1);
		f.pos3(self.cam_from);
		f.i32(self.cam_pers);
		f.u16(self.unk2);
		f.i16(self.cam_deg.0);
		f.i16(self.cam_limit.0 .0);
		f.i16(self.cam_limit.1 .0);
		f.pos3(self.cam_at);
		f.u16(self.unk3);
		f.u16(self.unk4);
		f.u16(self.flags.0);
		f.u16(self.town.0);
		f.u8(cast(self.init.0)?);
		f.u8(cast(self.init.1)?);
		f.u8(cast(self.reinit.0)?);
		f.u8(cast(self.reinit.1)?);
		Ok(())
	}
}

#[derive(Debug, Clone, PartialEq)]
pub struct Animation {
	pub speed: Time,
	pub frames: Vec<u8>,
}

impl Animation {
	fn read(f: &mut Reader) -> Result<Animation, ReadError> {
		let speed = Time(f.u16()? as u32);
		f.check_u8(0)?;
		let count = f.u8()? as usize;
		ensure!(count <= 8, "too many frames: {count}");
		let frames = f.array::<8>()?;
		let frames = frames[..count].to_owned();
		Ok(Animation { speed, frames })
	}

	fn write(&self, f: &mut Writer) -> Result<(), WriteError> {
		let count = self.frames.len();
		ensure!(count <= 8, "too many frames: {count}");
		let mut frames = [0; 8];
		frames[..count].copy_from_slice(&self.frames);
		f.u16(cast(self.speed.0)?);
		f.u8(0);
		f.u8(count as u8);
		f.slice(&frames);
		Ok(())
	}
}
