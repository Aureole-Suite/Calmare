use std::collections::hash_map::{Entry, HashMap};
use std::ops::Range;

use gospel::read::{Le as _, Reader};
use gospel::write::{Label, Le as _, Writer};
use snafu::prelude::*;
use strict_result::Strict;

use super::ReadError;
use crate::types::*;
use crate::util::{ReaderExt as _, WriterExt as _};

newtype!(SepithId(u16));
newtype!(PlacementId(u16));
newtype!(AtRollId(u16));

#[derive(Debug, Clone, PartialEq, Eq, Default)]
pub struct BattleSet {
	/// The first five, if present, are always the same nonsensical values.
	pub sepith: Vec<[u8; 8]>,
	pub at_rolls: Vec<[u8; 16]>,
	pub placements: Vec<[(u8, u8, Angle); 8]>,
	pub battles: Vec<Battle>,
}

impl BattleSet {
	pub fn is_empty(&self) -> bool {
		self.sepith.is_empty()
			&& self.at_rolls.is_empty()
			&& self.placements.is_empty()
			&& self.battles.is_empty()
	}
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Battle {
	pub flags: u16,
	pub level: u16,
	pub unk1: u8,
	pub vision_range: u8,
	pub move_range: u8,
	pub can_move: u8,
	pub move_speed: u16,
	pub unk2: u16,
	pub battlefield: String,
	pub sepith: Option<SepithId>,
	pub setups: Vec<(u8, BattleSetup)>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct BattleSetup {
	pub enemies: [FileId; 8],
	pub placement: PlacementId,
	pub placement_ambush: PlacementId,
	pub bgm: BgmId,
	pub bgm_ambush: BgmId, // not entirely sure if this is what it is
	pub at_roll: AtRollId,
}

#[derive(Default, Debug)]
pub(crate) struct BattleRead {
	btlset: BattleSet,
	sepith_pos: HashMap<usize, SepithId>,
	at_roll_pos: HashMap<usize, AtRollId>,
	placement_pos: HashMap<usize, PlacementId>,
	battle_pos: HashMap<usize, BattleId>,
}

impl BattleRead {
	pub fn get_sepith(&mut self, f: &mut Reader) -> Result<SepithId, ReadError> {
		match self.sepith_pos.entry(f.pos()) {
			Entry::Occupied(e) => Ok(*e.get()),
			Entry::Vacant(e) => {
				let v = *e.insert(SepithId(self.btlset.sepith.len() as u16));
				self.btlset.sepith.push(f.array()?);
				Ok(v)
			}
		}
	}

	pub fn get_at_roll(&mut self, f: &mut Reader) -> Result<AtRollId, ReadError> {
		match self.at_roll_pos.entry(f.pos()) {
			Entry::Occupied(e) => Ok(*e.get()),
			Entry::Vacant(e) => {
				let v = *e.insert(AtRollId(self.btlset.at_rolls.len() as u16));
				self.btlset.at_rolls.push(f.array()?);
				Ok(v)
			}
		}
	}

	pub fn get_placement(&mut self, f: &mut Reader) -> Result<PlacementId, ReadError> {
		match self.placement_pos.entry(f.pos()) {
			Entry::Occupied(e) => Ok(*e.get()),
			Entry::Vacant(e) => {
				let v = *e.insert(PlacementId(self.btlset.placements.len() as u16));
				self.btlset.placements.push(
					std::array::try_from_fn(|_| Ok((f.u8()?, f.u8()?, Angle(f.i16()?))))
						.strict()?,
				);
				Ok(v)
			}
		}
	}

	pub fn get_battle(&mut self, f: &mut Reader) -> Result<BattleId, ReadError> {
		match self.battle_pos.entry(f.pos()) {
			Entry::Occupied(e) => Ok(*e.get()),
			Entry::Vacant(e) => {
				let v = *e.insert(BattleId(self.btlset.battles.len() as u32));
				let battle = Battle {
					flags: f.u16()?,
					level: f.u16()?,
					unk1: f.u8()?,
					vision_range: f.u8()?,
					move_range: f.u8()?,
					can_move: f.u8()?,
					move_speed: f.u16()?,
					unk2: f.u16()?,
					battlefield: f.ptr32()?.string()?,
					sepith: match f.u32()? {
						0 => None,
						n => Some(self.get_sepith(&mut f.clone().at(n as usize)?)?),
					},
					setups: {
						let mut setups = Vec::new();
						for weight in f.array::<4>()? {
							if weight == 0 {
								continue;
							}
							let setup = BattleSetup {
								enemies: std::array::try_from_fn(|_| Ok(FileId(f.u32()?)))
									.strict()?,
								placement: self.get_placement(&mut f.ptr16()?)?,
								placement_ambush: self.get_placement(&mut f.ptr16()?)?,
								bgm: BgmId(f.u16()?),
								bgm_ambush: BgmId(f.u16()?),
								at_roll: self.get_at_roll(&mut f.ptr32()?)?,
							};
							setups.push((weight, setup));
						}
						setups
					},
				};
				self.btlset.battles.push(battle);
				Ok(v)
			}
		}
	}

	pub fn preload_sepith(&mut self, f: &Reader, range: Range<usize>) -> Result<(), ReadError> {
		let mut f = f.at(range.start)?;
		while f.pos() < range.end {
			self.get_sepith(&mut f)?;
		}
		snafu::ensure_whatever!(f.pos() == range.end, "overshot");
		Ok(())
	}

	pub fn preload_battles(&mut self, f: &Reader, range: Range<usize>) -> Result<(), ReadError> {
		let mut f = f.at(range.start)?;
		while f.pos() < range.end {
			// Heuristic: first field of AT rolls is 100
			if f.clone().u8()? != 100 {
				break;
			}
			self.get_at_roll(&mut f)?;
		}

		while f.pos() < range.end {
			// if both alternatives and field sepith is zero, it's not a placement
			let Ok(v) = f.clone().at(f.pos() + 16).and_then(|mut g| g.u64()) else {
				break;
			};
			if v == 0 {
				break;
			}

			// if there's a valid AT roll pointer for the first alternative, it's probably not a placement
			let Ok(v) = f.clone().at(f.pos() + 64).and_then(|mut g| g.u32()) else {
				break;
			};
			if self.at_roll_pos.contains_key(&(v as usize)) {
				break;
			}

			self.get_placement(&mut f)?;
		}

		while f.pos() < range.end {
			self.get_battle(&mut f)?;
		}

		snafu::ensure_whatever!(f.pos() == range.end, "overshot");
		Ok(())
	}

	pub fn finish(self) -> BattleSet {
		self.btlset
	}
}

pub(crate) struct BattleWrite {
	pub sepith: Writer,
	pub at_rolls: Writer,
	pub placements: Writer,
	pub battles: Writer,
	pub strings: Writer,
	pub battle_pos: Vec<Label>,
}

impl BattleWrite {
	pub fn write(btlset: &BattleSet) -> Result<Self, super::WriteError> {
		let mut sepith = Writer::new();
		let mut at_rolls = Writer::new();
		let mut placements = Writer::new();
		let mut battles = Writer::new();
		let mut strings = Writer::new();

		let mut sepith_pos = Vec::new();
		let mut at_roll_pos = Vec::new();
		let mut placement_pos = Vec::new();
		let mut battle_pos = Vec::new();

		for sep in &btlset.sepith {
			sepith_pos.push(sepith.here());
			sepith.slice(sep);
		}

		for roll in &btlset.at_rolls {
			at_roll_pos.push(at_rolls.here());
			at_rolls.slice(roll);
		}

		for plac in &btlset.placements {
			placement_pos.push(placements.here());
			for p in plac {
				placements.u8(p.0);
				placements.u8(p.1);
				placements.i16(p.2 .0);
			}
		}

		for battle in btlset.battles.iter() {
			battle_pos.push(battles.here());
			battles.u16(battle.flags);
			battles.u16(battle.level);
			battles.u8(battle.unk1);
			battles.u8(battle.vision_range);
			battles.u8(battle.move_range);
			battles.u8(battle.can_move);
			battles.u16(battle.move_speed);
			battles.u16(battle.unk2);
			battles.label32(strings.here());
			strings.string(&battle.battlefield)?;
			if let Some(s) = battle.sepith {
				battles.label32(
					*sepith_pos
						.get(s.0 as usize)
						.whatever_context("field sepith out of bounds")
						.strict()?,
				);
			} else {
				battles.u32(0);
			}
			let mut weights = [0u8; 4];
			let mut h = Writer::new();
			ensure_whatever!(battle.setups.len() <= 4, "too many setups");
			for (i, (weight, setup)) in battle.setups.iter().enumerate() {
				weights[i] = *weight;
				for ms in &setup.enemies {
					h.u32(ms.0);
				}
				h.label16(
					*placement_pos
						.get(setup.placement.0 as usize)
						.whatever_context("placement out of bounds")
						.strict()?,
				);
				h.label16(
					*placement_pos
						.get(setup.placement_ambush.0 as usize)
						.whatever_context("placement out of bounds")
						.strict()?,
				);
				h.u16(setup.bgm.0);
				h.u16(setup.bgm_ambush.0);
				h.label32(
					*at_roll_pos
						.get(setup.at_roll.0 as usize)
						.whatever_context("at roll out of bounds")
						.strict()?,
				);
			}
			battles.array(weights);
			battles.append(h);
		}

		Ok(Self {
			strings,
			sepith,
			at_rolls,
			placements,
			battles,
			battle_pos,
		})
	}
}
