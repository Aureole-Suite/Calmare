use std::collections::BTreeMap;

use themelios::scena::{ed6, ChipId, EventId, LocalCharId, LookPointId};
use themelios::types::FileId;

use crate::macros::strukt::Slot;
use crate::parse::{self, Diagnostic, Span};
use crate::{Parse, ParseBlock, Parser};
use crate::{PrintBlock, Printer};

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
struct LocalFuncId(u16);

crate::macros::newtype_term!(LocalFuncId, "fn");

impl PrintBlock for ed6::Scena {
	fn print_block(&self, f: &mut Printer) {
		f.word("scena").block(|f| {
			f.word("name").val(&self.path).val(&self.map).line();
			f.word("town").val(self.town).line();
			f.word("bgm").val(self.bgm).line();
			f.word("item_use").val(self.item_use).line();
			for (i, a) in self.includes.iter().enumerate() {
				if *a != FileId::NONE {
					f.word("scp").val(i as u16).val(a).line();
				}
			}
		});

		for entry in &self.entries {
			f.line();
			f.word("entry").val_block(entry);
		}

		if !self.ch.is_empty() || !self.cp.is_empty() {
			f.line();
		}
		print_chcp(&self.ch, &self.cp, f);

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

		for (i, func) in self.functions.iter().enumerate() {
			f.line();
			f.val(LocalFuncId(i as u16)).val_block(func);
		}
	}
}

#[allow(unused_variables)]
impl ParseBlock for ed6::Scena {
	fn parse_block(f: &mut Parser) -> parse::Result<Self> {
		let mut chcps = PackedIndices::new();
		let mut npcs_monsters = PackedIndices::new();
		let mut events = PackedIndices::new();
		let mut look_points = PackedIndices::new();
		let mut entries = Vec::new();
		let mut functions = PackedIndices::new();

		f.lines(|f| {
			let pos = f.pos();
			match f.word()? {
				"scena" => {}
				"entry" => {
					entries.push(f.val_block()?);
				}
				"chip" => {
					// TODO handle null
					let id = parse_id(f, ChipId)?;
					let span = pos | f.pos();
					let val = (|| {
						let ch = f.val::<FileId>()?;
						let cp = f.val::<FileId>()?;
						Ok((ch, cp))
					})();
					chcps.insert(f, span, id.0 as usize, val);
				}
				"npc" => {
					let id = f.val::<LocalCharId>()?;
					let span = pos | f.pos();
					let val = f.val_block().map(NpcOrMonster::Npc);
					npcs_monsters.insert(f, span, id.0 as usize, val);
				}
				"monster" => {
					let id = f.val::<LocalCharId>()?;
					let span = pos | f.pos();
					let val = f.val_block().map(NpcOrMonster::Monster);
					npcs_monsters.insert(f, span, id.0 as usize, val);
				}
				"event" => {
					let id = parse_id(f, EventId)?;
					let span = pos | f.pos();
					let val = f.val_block();
					events.insert(f, span, id.0 as usize, val);
				}
				"look_point" => {
					let id = parse_id(f, LookPointId)?;
					let span = pos | f.pos();
					let val = f.val_block();
					look_points.insert(f, span, id.0 as usize, val);
				}
				"fn" => {
					let id = parse_id(f, LocalFuncId)?;
					let span = pos | f.pos();
					functions.insert(f, span, id.0 as usize, Err(Diagnostic::DUMMY));
				}
				e => return Err(Diagnostic::error(f.span_of(e), "invalid declaration")),
			}
			Ok(())
		});

		let (ch, cp) = chcps.finish(f, "chip").into_iter().unzip();
		let (npcs, monsters) = chars(f, npcs_monsters);
		let events = events.finish(f, "event");
		let look_points = look_points.finish(f, "look_point");
		let functions = functions.finish(f, "fn");

		use themelios::types::*;
		Ok(ed6::Scena {
			path: String::from(""),
			map: String::from(""),
			town: TownId(0),
			bgm: BgmId(0),
			item_use: themelios::scena::FuncId(0, 0),
			includes: [FileId::NONE; 8],
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
}

fn parse_id<U: Parse, T: Parse>(
	f: &mut Parser<'_>,
	func: impl FnOnce(U) -> T,
) -> parse::Result<T> {
	match f.try_parse(|f| f.term(|f| f.val()))? {
		Some(v) => Ok(func(v)),
		None => {
			let v = f.val()?;
			Ok(v)
		}
	}
}

fn print_chcp(ch: &[FileId], cp: &[FileId], f: &mut Printer) {
	let mut ch = ch.iter();
	let mut cp = cp.iter();
	let mut i = 0;
	loop {
		let ch = ch.next();
		let cp = cp.next();
		if ch.is_none() && cp.is_none() {
			break;
		}
		f.val(ChipId(i));
		if let Some(ch) = ch {
			f.val(ch);
		} else {
			f.word("null");
		}
		if let Some(cp) = cp {
			f.val(cp);
		} else {
			f.word("null");
		}
		f.line();
		i += 1;
	}
}

crate::macros::strukt::strukt! {
	struct ed6::Entry { pos, chr, angle, cam_from, cam_at, cam_zoom, cam_pers, cam_deg, cam_limit, north, flags, town, init, reinit, }
	struct ed6::Npc { name, pos, angle, x, cp, frame, ch, flags, init, talk, }
	struct ed6::Monster { name, pos, angle, chip, flags, unk2, battle, flag, unk3, }
	struct ed6::Event { pos1, pos2, flags, func, unk1, }
	struct ed6::LookPoint { pos, radius, bubble_pos, flags, func, unk1, }
}

#[derive(Debug, Clone)]
struct PackedIndices<V> {
	items: BTreeMap<usize, Slot<V>>,
}

impl<V> Default for PackedIndices<V> {
	fn default() -> Self {
		Self {
			items: BTreeMap::new(),
		}
	}
}

impl<V> PackedIndices<V> {
	pub fn new() -> Self {
		Self::default()
	}

	pub fn insert(&mut self, diag: &mut Parser, s: Span, n: usize, val: parse::Result<V>) {
		self.items
			.entry(n)
			.or_insert_with(|| Slot::new())
			.insert(diag, s, val);
	}

	pub fn items(&self) -> &BTreeMap<usize, Slot<V>> {
		&self.items
	}

	pub fn finish(self, diag: &mut Parser, word: &str) -> Vec<V> {
		let mut vs = Vec::with_capacity(self.items.len());
		let mut expect = 0;
		for (k, slot) in self.items {
			if k != expect {
				Diagnostic::error(slot.span(), format!("missing {word}[{expect}]")).emit(diag);
			}
			expect = k + 1;
			vs.extend(slot.get())
		}
		vs
	}
}

#[derive(Debug, Clone)]
enum NpcOrMonster<A, B> {
	Npc(A),
	Monster(B),
}

fn chars<A, B>(diag: &mut Parser, items: PackedIndices<NpcOrMonster<A, B>>) -> (Vec<A>, Vec<B>) {
	let mut iter = items.items().iter().peekable();
	while let (Some(a), Some(b)) = (iter.next(), iter.peek()) {
		if matches!(&a.1.get_ref(), Some(NpcOrMonster::Monster(_)))
			&& matches!(&b.1.get_ref(), Some(NpcOrMonster::Npc(_)))
		{
			Diagnostic::error(b.1.span(), "npcs mut come before monsters")
				.with_note(a.1.span(), "is after this monster")
				.emit(diag);
		}
	}

	let mut npcs = Vec::new();
	let mut monsters = Vec::new();
	for m in items.finish(diag, "char") {
		match m {
			NpcOrMonster::Npc(n) => npcs.push(n),
			NpcOrMonster::Monster(m) => monsters.push(m),
		}
	}

	(npcs, monsters)
}
