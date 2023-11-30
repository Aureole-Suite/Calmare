use crate::{Print, PrintContext, Printer, PrinterExt};
use themelios::scena::{ed6, ChipId, LocalCharId};
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

impl Print for ed6::Entry {
	fn print(&self, f: &mut Printer, ctx: &mut PrintContext) {
		f.kv_line("pos", self.pos, ctx);
		f.kv_line("chr", self.chr, ctx);
		f.kv_line("angle", self.angle, ctx);
		f.kv_line("cam_from", self.cam_from, ctx);
		f.kv_line("cam_at", self.cam_at, ctx);
		f.kv_line("cam_zoom", self.cam_zoom, ctx);
		f.kv_line("cam_pers", self.cam_pers, ctx);
		f.kv_line("cam_deg", self.cam_deg, ctx);
		f.kv_line("cam_limit", self.cam_limit, ctx);
		f.kv_line("north", self.north, ctx);
		f.kv_line("flags", self.flags, ctx);
		f.kv_line("town", self.town, ctx);
		f.kv_line("init", self.init, ctx);
		f.kv_line("reinit", self.reinit, ctx);
	}
}

impl Print for ed6::Npc {
	fn print(&self, f: &mut Printer, ctx: &mut PrintContext) {
		f.kv_line("name", &self.name, ctx);
		f.kv_line("pos", self.pos, ctx);
		f.kv_line("angle", self.angle, ctx);
		f.kv_line("x", self.x, ctx);
		f.kv_line("cp", self.cp, ctx);
		f.kv_line("frame", self.frame, ctx);
		f.kv_line("ch", self.ch, ctx);
		f.kv_line("flags", self.flags, ctx);
		f.kv_line("init", self.init, ctx);
		f.kv_line("talk", self.talk, ctx);
	}
}

impl Print for ed6::Monster {
	fn print(&self, f: &mut Printer, ctx: &mut PrintContext) {
		f.kv_line("name", &self.name, ctx);
		f.kv_line("pos", self.pos, ctx);
		f.kv_line("angle", self.angle, ctx);
		f.kv_line("chip", self.chip, ctx);
		f.kv_line("flags", self.flags, ctx);
		f.kv_line("unk2", self.unk2, ctx);
		f.kv_line("battle", self.battle, ctx);
		f.kv_line("flag", self.flag, ctx);
		f.kv_line("unk3", self.unk3, ctx);
	}
}
