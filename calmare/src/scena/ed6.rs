use crate::{Print, PrintContext, Printer, PrinterExt};
use themelios::scena::ed6;
use themelios::types;

impl Print for ed6::Scena {
	fn print(&self, ctx: &mut PrintContext, f: &mut Printer) {
		f.word("scena").block(|f| {
			f.kv_line("name", (&self.path, &self.map), ctx);
			f.kv_line("town", self.town, ctx);
			f.kv_line("bgm", self.bgm, ctx);
			f.kv_line("item_use", self.item_use, ctx);
			for (i, a) in self.includes.iter().enumerate() {
				if *a != types::FileId::NONE {
					f.kv_line("scp", (i as u16, a), ctx);
				}
			}
		});
		f.line();

		for entry in &self.entries {
			f.word("entry").block(|f| {
				entry.print(ctx, f);
			});
			f.line();
		}
	}
}

impl Print for ed6::Entry {
	fn print(&self, ctx: &mut PrintContext, f: &mut Printer) {
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
