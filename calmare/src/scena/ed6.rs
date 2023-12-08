use std::borrow::Cow;
use std::collections::BTreeMap;

use themelios::scena::{ed6, ChipId, EventId, FuncId, LocalCharId, LookPointId};
use themelios::types::{BgmId, FileId, TownId};

use crate::macros::strukt::{Slot, Field};
use crate::parse::{self, Diagnostic, Span};
use crate::{Parse, ParseBlock, Parser};
use crate::{PrintBlock, Printer};

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
struct LocalFuncId(u16);

crate::macros::newtype_term!(LocalFuncId, "fn");

impl PrintBlock for ed6::Scena {
	fn print_block(&self, f: &mut Printer) {
		f.word("scena").val_block(Head {
			name: (Cow::Borrowed(&self.path), Cow::Borrowed(&self.map)),
			town: self.town,
			bgm: self.bgm,
			item_use: self.item_use,
			include: self.includes,
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

impl ParseBlock for ed6::Scena {
	fn parse_block(f: &mut Parser) -> parse::Result<Self> {
		let start = f.raw_pos();

		let mut head = None::<Slot<Head>>;
		let mut chcps = PackedIndices::new();
		let mut npcs_monsters = PackedIndices::new();
		let mut events = PackedIndices::new();
		let mut look_points = PackedIndices::new();
		let mut entries = Vec::new();
		let mut functions = PackedIndices::new();

		f.lines(|f| {
			let pos = f.pos()?;
			match f.word()? {
				"scena" => {
					let val = f.val_block();
					head.get_or_insert_with(Slot::new)
						.insert(f, f.span(pos), val);
				}
				"entry" => {
					entries.push(f.val_block()?);
				}
				"chip" => {
					// TODO handle null
					let id = parse_id(f, ChipId)?;
					let span = f.span(pos);
					let val = (|| {
						let ch = f.val::<FileId>()?;
						let cp = f.val::<FileId>()?;
						Ok((ch, cp))
					})();
					chcps.insert(f, span, id.0 as usize, val);
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

		let (ch, cp) = chcps.finish(f, "chip").into_iter().unzip();
		let (npcs, monsters) = chars(f, npcs_monsters);
		let events = events.finish(f, "event");
		let look_points = look_points.finish(f, "look_point");
		let functions = functions.finish(f, "fn");

		let Some(head) = head else {
			return Err(Diagnostic::error(start.as_span(), "missing `scena` block"));
		};

		let Some(head) = head.get() else {
			return Err(Diagnostic::DUMMY);
		};

		Ok(ed6::Scena {
			path: head.name.0.into_owned(),
			map: head.name.1.into_owned(),
			town: head.town,
			bgm: head.bgm,
			item_use: head.item_use,
			includes: head.include,
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

struct Head<'a> {
	name: (Cow<'a, str>, Cow<'a, str>),
	town: TownId,
	bgm: BgmId,
	item_use: FuncId,
	include: [FileId; 8],
}

fn parse_id<U: Parse, T: Parse>(f: &mut Parser<'_>, func: impl FnOnce(U) -> T) -> parse::Result<T> {
	match f.try_parse(|f| f.sqbrack_val())? {
		Some(v) => Ok(func(v)),
		None => Ok(f.val()?),
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
		f.val(ChipId(i)).val(ch).val(cp).line();
		i += 1;
	}
}

crate::macros::strukt::strukt! {
	struct Head<'_> {
		name, town, bgm, item_use,
		include: Includes<8>,
	}
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
		self.items.entry(n).or_default().insert(diag, s, val);
	}

	pub fn items(&self) -> &BTreeMap<usize, Slot<V>> {
		&self.items
	}

	pub fn finish(self, diag: &mut Parser, word: &str) -> Vec<V> {
		let mut vs = Vec::with_capacity(self.items.len());
		let mut expect = 0;
		for (k, slot) in self.items {
			if k != expect {
				Diagnostic::error(slot.span().unwrap(), format!("missing {word}[{expect}]"))
					.emit(diag);
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
			Diagnostic::error(b.1.span().unwrap(), "npcs mut come before monsters")
				.with_note(a.1.span().unwrap(), "is after this monster")
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


#[derive(Debug, Clone)]
pub struct Includes<const N: usize> {
	value: [Slot<FileId>; N],
}

impl<const N: usize> Default for Includes<N> {
	fn default() -> Self {
		Self {
			value: std::array::from_fn(|_| Slot::new()),
		}
	}
}

impl<const N: usize> Field for Includes<N> {
	type Value = [FileId; N];

	fn print_field(key: &str, f: &mut Printer, value: &Self::Value) {
		for (i, value) in value.iter().enumerate() {
			if *value != FileId::NONE {
				f.term(key).field().val(i);
				f.val(value).line();
			}
		}
	}

	fn parse_field<'src>(&mut self, word: &'src str, f: &mut Parser<'src>) -> parse::Result<()> {
		let pos = f.pos()?;
		let n = f.sqbrack_val::<usize>()?;
		let value = f.val();
		self.value[n]
			.insert(f, f.span_of(word) | f.span(pos), value);
		Ok(())
	}

	fn is_present(&self) -> bool {
		true
	}

	fn get(self) -> Option<Self::Value> {
		Some(self.value.map(|v| v.get().unwrap_or(FileId::NONE)))
	}
}
