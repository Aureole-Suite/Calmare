use themelios::scena::ed7::battle::*;
use themelios::types::{BattleId, BgmId, FileId};

use crate::macros::strukt::Field;
use crate::{parse, Parser};
use crate::{PrintBlock, Printer};

crate::macros::newtype_term!(SepithId, "sepith");
crate::macros::newtype_term!(AtRollId, "at_roll");
crate::macros::newtype_term!(PlacementId, "placement");
crate::macros::newtype_hex!(BattleFlags);

impl PrintBlock for BattleSet {
	fn print_block(&self, f: &mut Printer) {
		let junk_sepith = self.sepith.starts_with(&[
			[100, 1, 2, 3, 70, 89, 99, 0],
			[100, 5, 1, 5, 1, 5, 1, 0],
			[100, 5, 1, 5, 1, 5, 1, 0],
			[100, 5, 0, 5, 0, 5, 0, 0],
			[100, 5, 0, 5, 0, 5, 0, 0],
		]);
		if junk_sepith {
			write!(
				f,
				"// NB: the first five sepith sets are seemingly junk data."
			)
			.line();
		}
		for (i, sep) in self.sepith.iter().enumerate() {
			if junk_sepith && i == 5 {
				f.line();
			}
			f.val(SepithId(i as u16));
			let mut tup = f.term("");
			for val in sep {
				tup.field().val(val);
			}
			drop(tup);
			f.line();
		}

		if !self.at_rolls.is_empty() && !self.sepith.is_empty() {
			f.line();
		}
		for (i, roll) in self.at_rolls.iter().enumerate() {
			f.val(AtRollId(i as u16)).no_space().word(":");
			let mut first = true;
			for (name, val) in f.insn_set().at_roll.iter().zip(roll) {
				if *val != 0 {
					if !first {
						f.no_space().word(";");
					}
					first = false;
					f.word(name).val(val);
				}
			}
			f.line();
		}

		if !self.placements.is_empty() && (!self.at_rolls.is_empty() || !self.sepith.is_empty()) {
			f.line();
		}
		for (i, sep) in self.placements.iter().enumerate() {
			f.val(PlacementId(i as u16));
			let mut tup = f.term("");
			for val in sep {
				tup.field().val(val);
			}
			drop(tup);
			f.line();
		}

		for (i, bat) in self.battles.iter().enumerate() {
			f.line();
			f.val(BattleId(i as u32)).val_block(bat);
		}
	}
}

pub struct Setup {
	pub enemies: (
		FileId,
		FileId,
		FileId,
		FileId,
		FileId,
		FileId,
		FileId,
		FileId,
	),
	pub placement: (PlacementId, PlacementId),
	pub bgm: (BgmId, BgmId),
	pub at_roll: AtRollId,
}

crate::macros::strukt::strukt! {
	struct Battle {
		flags, level, unk1, vision_range, move_range, can_move, move_speed,
		unk2, battlefield, sepith,
		setups as setup: Setups,
	}
	struct Setup { enemies, placement, bgm, at_roll }
}

#[derive(Debug, Clone, Default)]
struct Setups {
	value: Vec<(u8, BattleSetup)>,
}

impl Field for Setups {
	type Value = Vec<(u8, BattleSetup)>;

	fn print_field(key: &str, f: &mut Printer, value: &Self::Value) {
		for (weight, setup) in value {
			f.word(key).val(weight).val_block(Setup {
				enemies: setup.enemies.into(),
				placement: (setup.placement, setup.placement_ambush),
				bgm: (setup.bgm, setup.bgm_ambush),
				at_roll: setup.at_roll,
			});
		}
	}

	fn parse_field<'src>(&mut self, word: &'src str, f: &mut Parser<'src>) -> parse::Result<()> {
		Ok(())
	}

	fn is_present(&self) -> bool {
		!self.value.is_empty()
	}

	fn get(self) -> Option<Self::Value> {
		Some(self.value)
	}
}
