use std::borrow::Cow;

use themelios::ed8::{self};
use themelios::types::{BgmId, FileId, TownId};

use crate::macros::strukt::{Field, Slot};
use crate::parse::{self, Diagnostic};
use crate::{ParseBlock, Parser};
use crate::{PrintBlock, Printer};

impl PrintBlock for ed8::Script {
	fn print_block(&self, f: &mut Printer) {
		f.word("script").val_block(Head {
			name: Cow::Borrowed(&self.name),
		});

		for func in &self.functions {
			if func.pad != 0 {
				f.word("pad").val(func.pad).line();
			}
			f.line();
			match &func.data {
				ed8::Data::Code(code) => {
					f.term("fn").field().val(&func.name);
					f.val_block(code);
				}
				ed8::Data::Battle(battle) => todo!(),
				ed8::Data::Other(byte) => todo!(),
			}
		}
	}
}

impl ParseBlock for ed8::Script {
	fn parse_block(f: &mut Parser) -> parse::Result<Self> {
		let start = f.raw_pos();
		let mut head = <Slot<Head>>::new();
		let mut functions = Vec::new();
		let mut pad = 0;
		f.lines(|f| {
			let pos = f.pos()?;
			let word = f.word()?;
			match word {
				"script" => head.insert(f.span(pos), f.val_block()),
				"pad" => pad += f.val::<usize>()?,
				"fn" => {
					f.check("[")?;
					let name = f.val()?;
					f.check("]")?;
					functions.push(ed8::Function {
						pad,
						name,
						data: ed8::Data::Code(f.val_block()?),
					});
					pad = 0;
				}
				_ => return Err(Diagnostic::error(f.span(pos), "invalid declaration")),
			}
			Ok(())
		});

		if head.span().is_none() {
			return Err(Diagnostic::error(start.as_span(), "missing `script` block"));
		};

		let Some(head) = head.get() else {
			return Err(Diagnostic::DUMMY);
		};

		Ok(ed8::Script {
			name: head.name.into_owned(),
			functions,
		})
	}
}

struct Head<'a> {
	name: Cow<'a, str>,
}

crate::macros::strukt::strukt! {
	struct Head<'_> { name, }
}
