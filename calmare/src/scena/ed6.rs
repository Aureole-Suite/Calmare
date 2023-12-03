use themelios::scena::{ed6, ChipId, EventId, LocalCharId, LookPointId};
use themelios::types::FileId;

use crate::parse::{Diagnostic, Span};
use crate::{parse, Parse, ParseContext, Parser};
use crate::{Print, PrintContext, Printer, PrinterExt};

struct LocalFuncId(u16);

crate::macros::newtype_term!(LocalFuncId, "fn");

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

		for entry in &self.entries {
			f.line();
			f.word("entry").block(|f| entry.print(f, ctx));
		}

		if !self.ch.is_empty() || !self.cp.is_empty() {
			f.line();
		}
		print_chcp(&self.ch, &self.cp, ctx, f);

		let mut n = 0;

		for npc in &self.npcs {
			f.line();
			f.word("npc")
				.val(LocalCharId(n), ctx)
				.block(|f| npc.print(f, ctx));
			n += 1;
		}

		for monster in &self.monsters {
			f.line();
			f.word("monster")
				.val(LocalCharId(n), ctx)
				.block(|f| monster.print(f, ctx));
			n += 1;
		}

		for (i, event) in self.events.iter().enumerate() {
			f.line();
			f.val(EventId(i as u16), ctx).block(|f| event.print(f, ctx));
		}

		for (i, lp) in self.look_points.iter().enumerate() {
			f.line();
			f.val(LookPointId(i as u16), ctx)
				.block(|f| lp.print(f, ctx));
		}

		for (i, func) in self.functions.iter().enumerate() {
			f.line();
			f.val(LocalFuncId(i as u16), ctx).val(func, ctx);
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

impl Parse for ed6::LookPoint {
	fn parse(f: &mut Parser, ctx: &mut ParseContext) -> parse::Result<Self> {
		let mut first_error = true;

		let mut pos = PlainField::default();
		let mut radius = PlainField::default();
		let mut bubble_pos = PlainField::default();
		let mut flags = PlainField::default();
		let mut func = PlainField::default();
		let mut unk1 = PlainField::default();

		let start = f.pos();
		f.lines(|f| {
			let start = f.pos();
			let word = f.word()?;
			let span = start | f.pos();
			match word {
				"pos" => pos.parse_field(f, ctx, span),
				"radius" => radius.parse_field(f, ctx, span),
				"bubble_pos" => bubble_pos.parse_field(f, ctx, span),
				"flags" => flags.parse_field(f, ctx, span),
				"func" => func.parse_field(f, ctx, span),
				"unk1" => unk1.parse_field(f, ctx, span),
				_ => {
					let mut diag = Diagnostic::error(span, "unknown field");
					if first_error {
						first_error = false;
						diag = diag.note(span, "allowed fields are `pos`, `radius`, ...");
					}
					Err(diag)
				}
			}
		});
		let span = start | f.pos();

		#[allow(unused_mut)]
		let mut failures: Vec<&str> = Vec::new();

		let pos = pos.get();
		if pos.is_none() {
			failures.push("`pos`");
		}
		let radius = radius.get();
		if radius.is_none() {
			failures.push("`radius`");
		}
		let bubble_pos = bubble_pos.get();
		if bubble_pos.is_none() {
			failures.push("`bubble_pos`");
		}
		let flags = flags.get();
		if flags.is_none() {
			failures.push("`flags`");
		}
		let func = func.get();
		if func.is_none() {
			failures.push("`func`");
		}
		let unk1 = unk1.get();
		if unk1.is_none() {
			failures.push("`unk1`");
		}

		if failures.is_empty() {
			Ok(Self {
				pos: pos.unwrap(),
				radius: radius.unwrap(),
				bubble_pos: bubble_pos.unwrap(),
				flags: flags.unwrap(),
				func: func.unwrap(),
				unk1: unk1.unwrap(),
			})
		} else {
			Err(Diagnostic::error(span, "missing fields").note(span, failures.join(", ")))
		}
	}
}

pub trait ParseField: Default {
	type Output;
	fn parse_field(
		&mut self,
		f: &mut Parser,
		ctx: &mut ParseContext,
		head_span: Span,
	) -> parse::Result<()>;
	fn get(self) -> Option<Self::Output>;
}

#[derive(Debug, Clone)]
struct PlainField<T> {
	head_span: Option<Span>,
	value: Option<T>,
}

impl<T> Default for PlainField<T> {
	fn default() -> Self {
		Self {
			head_span: None,
			value: None,
		}
	}
}

impl<T: Parse> ParseField for PlainField<T> {
	type Output = T;

	fn parse_field(
		&mut self,
		f: &mut Parser,
		ctx: &mut ParseContext,
		head_span: Span,
	) -> parse::Result<()> {
		if let Some(prev_span) = self.head_span.replace(head_span) {
			Diagnostic::error(head_span, "duplicate item")
				.note(prev_span, "previous here")
				.emit(f);
		}
		f.space()?;
		self.value = Some(T::parse(f, ctx)?);
		Ok(())
	}

	fn get(self) -> Option<Self::Output> {
		self.value
	}
}
