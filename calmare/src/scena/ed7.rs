use std::borrow::Cow;

use themelios::scena::ed7::AnimId;
use themelios::scena::{ed7, ChipId, EventId, FuncId, LocalCharId, LookPointId};
use themelios::types::{BgmId, FileId, TownId};

use crate::macros::strukt::{Field, Slot};
use crate::parse::{self, Diagnostic, Span};
use crate::{ParseBlock, Parser};
use crate::{PrintBlock, Printer};

use super::{parse_id, LocalFuncId, NpcOrMonster, PackedIndices};

crate::macros::newtype_term!(ed7::AnimId, "anim");
crate::macros::newtype_hex!(ed7::ScenaFlags);

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
struct LabelId(u16);
crate::macros::newtype_term!(LabelId, "label");

mod battle;

impl PrintBlock for ed7::Scena {
	fn print_block(&self, f: &mut Printer) {
		f.word("scena").val_block(Head {
			name: (
				Cow::Borrowed(&self.path),
				Cow::Borrowed(&self.map),
				Cow::Borrowed(&self.filename),
			),
			town: self.town,
			bgm: self.bgm,
			flags: self.flags,
			item_use: self.item_use,
			unknown_function: self.unknown_function,
			system30: self.system30,
			include: self.includes.map(|v| (v != FileId::NONE).then_some(v)),
		});

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

		if !self.animations.is_empty() {
			f.line();
		}
		for (i, anim) in self.animations.iter().enumerate() {
			f.val(AnimId(i as u32)).val(anim.speed);
			let mut tup = f.term("");
			for val in &anim.frames {
				tup.field().val(val);
			}
			drop(tup);
			f.line();
		}

		if !self.btlset.is_empty() {
			f.line();
			f.word("btlset").val_block(&self.btlset);
		}

		for (i, func) in self.functions.iter().enumerate() {
			f.line();
			f.val(LocalFuncId(i as u16)).val_block(func);
		}
	}
}

impl ParseBlock for ed7::Scena {
	fn parse_block(f: &mut Parser) -> parse::Result<Self> {
		let start = f.raw_pos();

		let mut head = None::<Slot<Head>>;
		let mut entries = Slot::new();
		let mut chips = PackedIndices::new();
		let mut npcs_monsters = PackedIndices::new();
		let mut events = PackedIndices::new();
		let mut look_points = PackedIndices::new();
		let mut labels = PackedIndices::new();
		let mut no_labels = None::<(Span, bool)>;
		let mut animations = PackedIndices::new();
		let mut btlset = Slot::new();
		let mut functions = PackedIndices::new();

		f.lines(|f| {
			let pos = f.pos()?;
			match f.word()? {
				"scena" => {
					let val = f.val_block();
					head.get_or_insert_with(Slot::new).insert(f.span(pos), val);
				}
				"entry" => {
					let span = f.span(pos);
					let val = f.val_block();
					entries.insert(span, val);
				}
				"chip" => {
					let id = parse_id(f, ChipId)?;
					let span = f.span(pos);
					let val = f.val::<FileId>();
					chips.insert(f, span, id.0 as usize, val);
				}
				"npc" => {
					let id = f.val::<LocalCharId>()?;
					let span = f.span(pos);
					let val = f.val_block().map(NpcOrMonster::Npc);
					npcs_monsters.insert(f, span, id.0 as usize, val);
				}
				"monster" => {
					let id = f.val::<LocalCharId>()?;
					let span = f.span(pos);
					let val = f.val_block().map(NpcOrMonster::Monster);
					npcs_monsters.insert(f, span, id.0 as usize, val);
				}
				"event" => {
					let id = parse_id(f, EventId)?;
					let span = f.span(pos);
					let val = f.val_block();
					events.insert(f, span, id.0 as usize, val);
				}
				"look_point" => {
					let id = parse_id(f, LookPointId)?;
					let span = f.span(pos);
					let val = f.val_block();
					look_points.insert(f, span, id.0 as usize, val);
				}
				"labels" => {
					let id = parse_id(f, LabelId)?;
					let span = f.span(pos);
					if f.check_word("null").is_ok() {
						if let Some((prev, _)) = no_labels {
							Diagnostic::error(span, "duplicate item")
								.with_note(prev, "previous here")
								.emit();
						}
						no_labels = Some((span, true));
					} else {
						if let Some((prev, true)) = no_labels {
							Diagnostic::error(span, "duplicate item")
								.with_note(prev, "previous here")
								.emit();
						}
						no_labels = Some((span, false));
						let val = f.val_block();
						labels.insert(f, span, id.0 as usize, val);
					}
				}
				"anim" => {
					let id = parse_id(f, AnimId)?;
					let span = f.span(pos);
					let val = (|| {
						let speed = f.val()?;
						let mut tup = f.tuple()?;
						let mut frames = Vec::new();
						while let Some(f) = tup.try_field()? {
							if frames.len() >= 8 {
								return Err(Diagnostic::error(
									f.pos()?.as_span(),
									"up to 8 frames allowed",
								));
							}
							frames.push(f.val()?);
						}
						Ok(ed7::Animation { speed, frames })
					})();
					animations.insert(f, span, id.0 as usize, val);
				}
				"btlset" => {
					let span = f.span(pos);
					let val = f.val_block();
					btlset.insert(span, val);
				}
				"fn" => {
					let id = parse_id(f, LocalFuncId)?;
					let span = f.span(pos);
					let val = f.val_block();
					functions.insert(f, span, id.0 as usize, val);
				}
				_ => return Err(Diagnostic::error(f.span(pos), "invalid declaration")),
			}
			Ok(())
		});

		let chips = chips.finish("chip");
		let (npcs, monsters) = super::chars(npcs_monsters);
		let events = events.finish("event");
		let look_points = look_points.finish("look_point");
		let labels = labels.finish("label");
		let animations = animations.finish("anim");
		let functions = functions.finish("fn");

		let labels = if let Some((_, true)) = no_labels {
			Some(vec![])
		} else if labels.is_empty() {
			None
		} else {
			Some(labels)
		};

		let Some(head) = head else {
			return Err(Diagnostic::error(start.as_span(), "missing `scena` block"));
		};

		let Some(head) = head.get() else {
			return Err(Diagnostic::DUMMY);
		};

		Ok(ed7::Scena {
			path: head.name.0.into_owned(),
			map: head.name.1.into_owned(),
			filename: head.name.2.into_owned(),
			town: head.town,
			bgm: head.bgm,
			flags: head.flags,
			item_use: head.item_use,
			unknown_function: head.unknown_function,
			system30: head.system30,
			includes: head.include.map(|v| v.unwrap_or(FileId::NONE)),
			entries: entries.get(),
			chips,
			npcs,
			monsters,
			events,
			look_points,
			labels,
			animations,
			btlset: btlset.get().unwrap_or_default(),
			functions,
		})
	}
}

struct Head<'a> {
	name: (Cow<'a, str>, Cow<'a, str>, Cow<'a, str>),
	town: TownId,
	bgm: BgmId,
	flags: ed7::ScenaFlags,
	item_use: FuncId,
	unknown_function: FuncId,
	system30: u8,
	include: [Option<FileId>; 6],
}

crate::macros::strukt::strukt! {
	struct Head<'_> {
		name, town, bgm, flags, item_use, unknown_function, system30,
		include: super::Array<6, _>,
	}
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
