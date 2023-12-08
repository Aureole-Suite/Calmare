use std::borrow::Cow;
use std::collections::BTreeMap;

use themelios::scena::{ed7, ChipId, EventId, FuncId, LocalCharId, LookPointId};
use themelios::types::{BgmId, FileId, TownId};

use crate::macros::strukt::{Field, Slot};
use crate::parse::{self, Diagnostic, Span};
use crate::{Parse, ParseBlock, Parser, Print};
use crate::{PrintBlock, Printer};

crate::macros::newtype_term!(ed7::AnimId, "anim");

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
struct LabelId(u16);
crate::macros::newtype_term!(LabelId, "label");

impl PrintBlock for ed7::Scena {
	fn print_block(&self, f: &mut Printer) {
		// f.kw("scena").suf(":").line().indent(|f| {
		// 	f.kw("name").val(name1).val(name2).val(filename).line();
		// 	f.kw("town").val(town).line();
		// 	f.kw("bgm").val(bgm).line();
		// 	f.kw("flags").val(flags).line();
		// 	f.kw("item_use").val(item_use).line();
		// 	f.kw("unk").val(unk2).val(unk3).line();
		// 	for (i, a) in includes.iter().enumerate() {
		// 		if a.0 != 0 {
		// 			f.kw("scp").val(&(i as u16)).val(a).line();
		// 		}
		// 	}
		// });

		for entry in &self.entries {
			f.line();
			f.word("entry").val_block(entry);
		}

		if !self.chips.is_empty() {
			f.line();
		}
		for (i, chip) in self.chips.iter().enumerate() {
			f.val(ChipId(i as u16)).val(chip).line();
		}

		let mut n = 0;

		for npc in &self.npcs {
			f.line();
			f.word("npc").val(LocalCharId(n)).val_block(npc);
			n += 1;
		}

		for monster in &self.monsters {
			f.line();
			f.word("monster").val(LocalCharId(n)).val_block(monster);
			n += 1;
		}

		for (i, event) in self.events.iter().enumerate() {
			f.line();
			f.val(EventId(i as u16)).val_block(event);
		}

		for (i, lp) in self.look_points.iter().enumerate() {
			f.line();
			f.val(LookPointId(i as u16)).val_block(lp);
		}

		match self.labels.as_deref() {
			None => {}
			Some([]) => {
				f.line();
				write!(
					f,
					"// NB: this line is meaningless, it's just here for roundtripping."
				)
				.line();
				f.word("label").word("null").line();
			}
			Some(labels) => {
				for (i, l) in labels.iter().enumerate() {
					f.line();
					f.val(LabelId(i as u16)).val_block(l);
				}
			}
		}

		// animations
		// sepith (incl. junk)
		// at rolls
		// placements
		// battles

		// for (i, func) in self.functions.iter().enumerate() {
		// 	f.line();
		// 	f.val(LocalFuncId(i as u16)).val_block(func);
		// }
	}
}

crate::macros::strukt::strukt! {
	struct ed7::Entry { pos, unk1, cam_from, cam_pers, unk2, cam_deg, cam_limit, cam_at, unk3, unk4, flags, town, init, reinit, }
	struct ed7::Label { name, pos, unk1, unk2, }
	struct ed7::Npc { name, pos, angle, flags, unk2, chip, init, talk, unk4, }
	struct ed7::Monster { pos, angle, flags, battle, flag, chip, unk2, stand_anim, walk_anim, }
	struct ed7::Event { pos, radius, transform: MatrixField, unk1, function, unk2, unk3, unk4, unk5, unk6, }
	struct ed7::LookPoint { pos, radius, bubble_pos, unk1, unk2, function, unk3, unk4, }
}

#[derive(Debug, Clone)]
struct MatrixField {
	value: Slot<themelios::glam::Mat4>,
}

impl Default for MatrixField {
	fn default() -> Self {
		Self { value: Slot::new() }
	}
}

impl Field for MatrixField {
	type Value = themelios::glam::Mat4;

	fn print_field(key: &str, f: &mut Printer, value: &Self::Value) {
		f.word(key).line().indent(|f| {
			for r in value.transpose().to_cols_array_2d() {
				for c in r {
					f.val(c);
				}
				f.line();
			}

			let k = |v: f32| (v * 1000.).round() / 1000.;
			let (s, r, t) = value.inverse().to_scale_rotation_translation();
			let r = r.to_axis_angle();
			write!(f, "// translate ({:?}, {:?}, {:?})", k(t.x), k(t.y), k(t.z)).line();
			if r.1 != 0. {
				write!(
					f,
					"// rotate ({:?}, {:?}, {:?}) {:?}deg",
					k(r.0.x),
					k(r.0.y),
					k(r.0.z),
					k(r.1.to_degrees())
				)
				.line();
			}
			write!(f, "// scale ({:?}, {:?}, {:?})", k(s.x), k(s.y), k(s.z)).line();
		})
	}

	fn parse_field<'src>(&mut self, word: &'src str, f: &mut Parser<'src>) -> parse::Result<()> {
		Ok(())
	}

	fn is_present(&self) -> bool {
		self.value.span().is_some()
	}

	fn get(self) -> Option<Self::Value> {
		self.value.get()
	}
}
