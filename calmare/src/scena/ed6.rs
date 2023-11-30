use crate::{Print, PrintContext, Printer, PrinterExt};
use themelios::scena::{ed6, ChipId, EventId, LocalCharId, LookPointId};
use themelios::types::FileId;

impl Print for ed6::Scena {
	fn print(&self, f: &mut Printer, ctx: &mut PrintContext) {
		f.word("scena").block(|f| {
			f.kv_line("name", (&self.path, &self.map), ctx);
			f.kv_line("town", self.town, ctx);
			f.kv_line("bgm", self.bgm, ctx);
			f.kv_line("item_use", self.item_use, ctx);
			for (i, a) in self.includes.iter().enumerate() {
				if *a != FileId::NONE {
					f.kv_line("scp", (i as u16, a), ctx);
				}
			}
		});
		f.line();

		for entry in &self.entries {
			f.word("entry").block(|f| entry.print(f, ctx));
			f.line();
		}

		print_chcp(&self.ch, &self.cp, ctx, f);
		if !self.ch.is_empty() || !self.cp.is_empty() {
			f.line();
		}

		let mut n = 0;

		for npc in &self.npcs {
			f.word("npc")
				.val(LocalCharId(n), ctx)
				.block(|f| npc.print(f, ctx));
			n += 1;
			f.line();
		}

		for monster in &self.monsters {
			f.word("monster")
				.val(LocalCharId(n), ctx)
				.block(|f| monster.print(f, ctx));
			n += 1;
			f.line();
		}

		for (i, event) in self.events.iter().enumerate() {
			f.val(EventId(i as u16), ctx).block(|f| event.print(f, ctx));
			f.line();
		}

		for (i, lp) in self.look_points.iter().enumerate() {
			f.val(LookPointId(i as u16), ctx)
				.block(|f| lp.print(f, ctx));
			f.line();
		}
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

crate::macros::strukt! {
	struct ed6::Entry { pos, chr, angle, cam_from, cam_at, cam_zoom, cam_pers, cam_deg, cam_limit, north, flags, town, init, reinit, }
	struct ed6::Npc { name, pos, angle, x, cp, frame, ch, flags, init, talk, }
	struct ed6::Monster { name, pos, angle, chip, flags, unk2, battle, flag, unk3, }
	struct ed6::Event { pos1, pos2, flags, func, unk1, }
	struct ed6::LookPoint { pos, radius, bubble_pos, flags, func, unk1, }
}
