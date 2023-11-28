use std::collections::hash_map::{Entry, HashMap};
use std::ops::Range;
use strict_result::Strict;

use gospel::read::{Le as _, Reader};

use super::ReadError;
use crate::types::*;
use crate::util::ReaderExt as _;

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
	pub setups: Vec<BattleSetup>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct BattleSetup {
	pub weight: u8,
	pub enemies: [FileId; 8],
	pub placement: PlacementId,
	pub placement_ambush: PlacementId,
	pub bgm: BgmId,
	pub bgm_ambush: BgmId, // not entirely sure if this is what it is
	pub at_roll: AtRollId,
}

#[derive(Default, Debug)]
pub struct BattleRead {
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
							setups.push(BattleSetup {
								weight,
								enemies: std::array::try_from_fn(|_| Ok(FileId(f.u32()?)))
									.strict()?,
								placement: self.get_placement(&mut f.ptr16()?)?,
								placement_ambush: self.get_placement(&mut f.ptr16()?)?,
								bgm: BgmId(f.u16()?),
								bgm_ambush: BgmId(f.u16()?),
								at_roll: self.get_at_roll(&mut f.ptr32()?)?,
							});
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
