use std::collections::BTreeMap;

use themelios::scena::{ed6, ChipId, EventId, LocalCharId, LookPointId};
use themelios::types::FileId;

use crate::parse::{self, Diagnostic, Emit as _, Span};
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

		f.lines(|f| match f.word()? {
			word @ "scena" => Ok(()),
			word @ "entry" => {
				entries.push(f.val_block()?);
				Ok(())
			}
			word @ "chip" => {
				// TODO handle null
				let (span, id) = parse_id(f, word, ChipId)?;
				chcps.insert(f, span, id.0 as usize, |f| {
					let ch = f.val::<FileId>()?;
					let cp = f.val::<FileId>()?;
					Ok((ch, cp))
				});
				Ok(())
			}
			word @ "npc" => {
				let pos = f.pos();
				let id = f.val::<LocalCharId>()?;
				let span = pos | f.pos();
				npcs_monsters.insert(f, span, id.0 as usize, |f| {
					f.val_block().map(NpcOrMonster::Npc)
				});
				Ok(())
			}
			word @ "monster" => {
				let pos = f.pos();
				let id = f.val::<LocalCharId>()?;
				let span = pos | f.pos();
				npcs_monsters.insert(f, span, id.0 as usize, |f| {
					f.val_block().map(NpcOrMonster::Monster)
				});
				Ok(())
			}
			word @ "event" => {
				let (span, id) = parse_id(f, word, EventId)?;
				events.insert(f, span, id.0 as usize, |f| f.val_block());
				Ok(())
			}
			word @ "look_point" => {
				let (span, id) = parse_id(f, word, LookPointId)?;
				look_points.insert(f, span, id.0 as usize, |f| f.val_block());
				Ok(())
			}
			word @ "fn" => {
				let (span, id) = parse_id(f, word, LocalFuncId)?;
				functions.insert(f, span, id.0 as usize, |f| Err(Diagnostic::DUMMY));
				Ok(())
			}
			e => Err(Diagnostic::error(f.span_of(e), "invalid declaration")),
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

fn parse_id<'src, U: Parse, T: Parse>(
	f: &mut Parser<'src>,
	word: &'src str,
	func: impl FnOnce(U) -> T,
) -> parse::Result<(Span, T)> {
	match f.try_parse(|f| f.term(|f| f.val()))? {
		Some(v) => Ok((f.span_of(word) | f.pos(), func(v))),
		None => {
			let start = f.pos();
			let v = f.val()?;
			Ok((start | f.pos(), v))
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
	items: BTreeMap<usize, (Span, Option<V>)>,
}

impl<V> Default for PackedIndices<V> {
	fn default() -> Self {
		Self {
			items: Default::default(),
		}
	}
}

impl<V> PackedIndices<V> {
	pub fn new() -> Self {
		Self::default()
	}

	pub fn insert(
		&mut self,
		f: &mut Parser,
		s: Span,
		n: usize,
		func: impl FnOnce(&mut Parser) -> parse::Result<V>,
	) {
		let val = func(f).emit(f);
		if let Some((prev, _)) = self.items.insert(n, (s, val)) {
			Diagnostic::error(s, "duplicate item")
				.with_note(prev, "previous here")
				.emit(f);
		}
	}

	pub fn finish(self, diag: &mut Parser, word: &str) -> Vec<V> {
		let mut vs = Vec::with_capacity(self.items.len());
		let mut expect = 0;
		for (k, (s, v)) in self.items {
			if k != expect {
				Diagnostic::error(s, format!("missing {word}[{expect}]")).emit(diag);
			}
			expect = k + 1;
			vs.extend(v)
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
	let misorder = items
		.items
		.iter()
		.skip_while(|a| !matches!(&a.1 .1, Some(NpcOrMonster::Monster(_))))
		.find(|a| matches!(&a.1 .1, Some(NpcOrMonster::Npc(_))));
	if let Some((k, (s, _))) = misorder {
		let (_, (prev, _)) = items.items.range(..k).last().unwrap();
		Diagnostic::error(*prev, "monsters must come after npcs")
			.with_note(*s, "is before this npc")
			.emit(diag);
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
