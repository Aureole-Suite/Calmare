use std::borrow::Cow;
use std::collections::BTreeMap;

use themelios::scena::ed7::battle;

use crate::macros::strukt::{Field, Slot};
use crate::parse::{self, Diagnostic, Span};
use crate::{Parse, ParseBlock, Parser, Print};
use crate::{PrintBlock, Printer};

crate::macros::newtype_term!(battle::SepithId, "sepith");
crate::macros::newtype_term!(battle::AtRollId, "at_roll");
crate::macros::newtype_term!(battle::PlacementId, "placement");

impl PrintBlock for battle::BattleSet {
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
			f.val(battle::SepithId(i as u16));
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
			f.val(battle::AtRollId(i as u16)).no_space().word(":");
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
			f.val(battle::PlacementId(i as u16));
			let mut tup = f.term("");
			for val in sep {
				tup.field().val(val);
			}
			drop(tup);
			f.line();
		}

		// battles
	}
}
