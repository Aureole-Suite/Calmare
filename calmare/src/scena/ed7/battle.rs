use themelios::scena::ed7::battle::*;
use themelios::types::{BattleId, BgmId, FileId};

use crate::macros::strukt::{Field, Slot};
use crate::parse::Diagnostic;
use crate::scena::{parse_id, PackedIndices};
use crate::{parse, ParseBlock, Parser};
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

		// TODO put on one line
		// if !self.at_rolls.is_empty() && !self.sepith.is_empty() {
		//	 f.line();
		// }
		for (i, roll) in self.at_rolls.iter().enumerate() {
			f.line();
			f.val(AtRollId(i as u16)).block(|f| {
				for (name, val) in f.insn_set().at_roll.iter().zip(roll) {
					if *val != 0 {
						f.word(name).val(val).line();
					}
				}
			});
			// f.val(AtRollId(i as u16)).no_space().word(":");
			// let mut first = true;
			// for (name, val) in f.insn_set().at_roll.iter().zip(roll) {
			// 	if *val != 0 {
			// 		if !first {
			// 			f.no_space().word(";");
			// 		}
			// 		first = false;
			// 		f.word(name).val(val);
			// 	}
			// }
			// f.line();
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

impl ParseBlock for BattleSet {
	fn parse_block(f: &mut Parser) -> parse::Result<Self> {
		let mut sepith = PackedIndices::new();
		let mut at_rolls = PackedIndices::new();
		let mut placements = PackedIndices::new();
		let mut battles = PackedIndices::new();

		f.lines(|f| {
			let pos = f.pos()?;
			match f.word()? {
				"sepith" => {
					let id = parse_id(f, SepithId)?;
					let span = f.span(pos);
					let val = try {
						let mut tup = f.tuple()?;
						let v = std::array::try_from_fn(|_| tup.field()?.val())?;
						tup.finish()?;
						v
					};
					sepith.insert(span, id.0 as usize, val);
				}
				"at_roll" => {
					let id = parse_id(f, AtRollId)?;
					let span = f.span(pos);
					f.check(":")?;
					let val = parse_at_roll(f);
					at_rolls.insert(span, id.0 as usize, val);
				}
				"placement" => {
					let id = parse_id(f, PlacementId)?;
					let span = f.span(pos);
					let val = try {
						let mut tup = f.tuple()?;
						let v = std::array::try_from_fn(|_| tup.field()?.val())?;
						tup.finish()?;
						v
					};
					placements.insert(span, id.0 as usize, val);
				}
				"battle" => {
					let id = parse_id(f, BattleId)?;
					let span = f.span(pos);
					let val = f.val_block();
					battles.insert(span, id.0 as usize, val);
				}
				_ => return Err(Diagnostic::error(f.span(pos), "invalid declaration")),
			}
			Ok(())
		});

		let sepith = sepith.finish("sepith");
		let at_rolls = at_rolls.finish("at_roll");
		let placements = placements.finish("placement");
		let battles = battles.finish("battle");

		Ok(BattleSet {
			sepith,
			at_rolls,
			placements,
			battles,
		})
	}
}

fn parse_at_roll(f: &mut Parser) -> Result<[u8; 16], Diagnostic> {
	let mut slots = <[Slot<u8>; 16]>::default();
	f.lines(|f| {
		let pos = f.pos()?;
		let word = f.word();
		let word = word?;
		let span = f.span(pos);
		match f.insn_set().at_roll.iter().position(|v| v == word) {
			Some(n) => {
				let val = f.val();
				slots[n].insert(span, val);
			}
			None => return Err(Diagnostic::error(f.span(pos), "invalid at roll field")),
		}
		Ok(())
	});
	Ok(slots.map(|v| v.get().unwrap_or_default()))
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
	count: usize,
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
		if self.count == 4 {
			parse::Diagnostic::error(f.span_of(word), "up to 4 setups allowed").emit();
		}
		self.count += 1;

		let weight = f.val()?;
		let setup = f.val_block::<Setup>()?;
		self.value.push((
			weight,
			BattleSetup {
				enemies: setup.enemies.into(),
				placement: setup.placement.0,
				placement_ambush: setup.placement.1,
				bgm: setup.bgm.0,
				bgm_ambush: setup.bgm.1,
				at_roll: setup.at_roll,
			},
		));

		Ok(())
	}

	fn is_present(&self) -> bool {
		!self.value.is_empty()
	}

	fn get(self) -> Option<Self::Value> {
		Some(self.value)
	}
}
