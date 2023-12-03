use themelios::scena::{ed6, ChipId, EventId, LocalCharId, LookPointId};
use themelios::types::FileId;

use crate::{parse, Parse, ParseContext, Parser};
use crate::{Print, PrintContext, Printer};

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
struct LocalFuncId(u16);

crate::macros::newtype_term!(LocalFuncId, "fn");

impl Print for ed6::Scena {
	fn print(&self, f: &mut Printer, ctx: &mut PrintContext) {
		f.word("scena").block(|f| {
			f.word("name").val(&self.path, ctx).val(&self.map, ctx).line();
			f.word("town").val(self.town, ctx).line();
			f.word("bgm").val(self.bgm, ctx).line();
			f.word("item_use").val(self.item_use, ctx).line();
			for (i, a) in self.includes.iter().enumerate() {
				if *a != FileId::NONE {
					f.word("scp").val(i as u16, ctx).val(a, ctx).line();
				}
			}
		});

		for entry in &self.entries {
			f.line();
			f.word("entry").val(entry, ctx);
		}

		if !self.ch.is_empty() || !self.cp.is_empty() {
			f.line();
		}
		print_chcp(&self.ch, &self.cp, ctx, f);

		let mut n = 0;

		for npc in &self.npcs {
			f.line();
			f.word("npc").val(LocalCharId(n), ctx).val(npc, ctx);
			n += 1;
		}

		for monster in &self.monsters {
			f.line();
			f.word("monster").val(LocalCharId(n), ctx).val(monster, ctx);
			n += 1;
		}

		for (i, event) in self.events.iter().enumerate() {
			f.line();
			f.val(EventId(i as u16), ctx).val(event, ctx);
		}

		for (i, lp) in self.look_points.iter().enumerate() {
			f.line();
			f.val(LookPointId(i as u16), ctx).val(lp, ctx);
		}

		for (i, func) in self.functions.iter().enumerate() {
			f.line();
			f.val(LocalFuncId(i as u16), ctx).val(func, ctx);
		}
	}
}

impl Parse for ed6::Scena {
	fn parse(f: &mut Parser, ctx: &mut ParseContext) -> parse::Result<Self> {
		f.lines(|f| match f.word()? {
			"scena" => Ok(()),
			"entry" => {
				f.space()?;
				let l: ed6::Entry = Parse::parse(f, ctx)?;
				Ok(())
			}
			"chip" => {
				f.space()?;
				let id = Parse::parse(f, ctx)
					.or_else(|_| f.term(|f| Parse::parse(f, ctx)).map(ChipId))?;
				Ok(())
			}
			"npc" => {
				f.space()?;
				let id = Parse::parse(f, ctx)
					.or_else(|_| f.term(|f| Parse::parse(f, ctx)).map(LocalCharId))?;
				f.space()?;
				let l: ed6::Npc = Parse::parse(f, ctx)?;
				Ok(())
			}
			"monster" => {
				f.space()?;
				let id = Parse::parse(f, ctx)
					.or_else(|_| f.term(|f| Parse::parse(f, ctx)).map(LocalCharId))?;
				f.space()?;
				let l: ed6::Monster = Parse::parse(f, ctx)?;
				Ok(())
			}
			"event" => {
				f.space()?;
				let id = Parse::parse(f, ctx)
					.or_else(|_| f.term(|f| Parse::parse(f, ctx)).map(EventId))?;
				f.space()?;
				let l: ed6::Event = Parse::parse(f, ctx)?;
				Ok(())
			}
			"look_point" => {
				f.space()?;
				let id = Parse::parse(f, ctx)
					.or_else(|_| f.term(|f| Parse::parse(f, ctx)).map(LookPointId))?;
				f.space()?;
				let l: ed6::LookPoint = Parse::parse(f, ctx)?;
				println!("{:?} {:?}", id, l);
				Ok(())
			}
			"fn" => {
				f.space()?;
				let id = Parse::parse(f, ctx)
					.or_else(|_| f.term(|f| Parse::parse(f, ctx)).map(LocalFuncId))?;
				println!("fn {:?}", id);
				Ok(())
			}
			e => Err(parse::Diagnostic::error(
				f.span_of(e),
				"invalid declaration",
			)),
		});
		use themelios::types::*;
		Ok(ed6::Scena {
			path: String::from(""),
			map: String::from(""),
			town: TownId(0),
			bgm: BgmId(0),
			item_use: themelios::scena::FuncId(0, 0),
			includes: [FileId::NONE; 8],
			ch: vec![],
			cp: vec![],
			npcs: vec![],
			monsters: vec![],
			events: vec![],
			look_points: vec![],
			entries: vec![],
			functions: vec![],
		})
	}
}

fn print_chcp(ch: &[FileId], cp: &[FileId], ctx: &mut PrintContext, f: &mut Printer) {
	let mut ch = ch.iter();
	let mut cp = cp.iter();
	let mut i = 0;
	loop {
		let ch = ch.next();
		let cp = cp.next();
		if ch.is_none() && cp.is_none() {
			break;
		}
		f.val(ChipId(i), ctx);
		if let Some(ch) = ch {
			f.val(ch, ctx);
		} else {
			f.word("null");
		}
		if let Some(cp) = cp {
			f.val(cp, ctx);
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
