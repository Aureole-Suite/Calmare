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
				ed8::Data::Battle(battle) => {
					f.word("battle").val_block(battle);
				}
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
				"battle" => {
					functions.push(ed8::Function {
						pad,
						name: String::new(),
						data: ed8::Data::Battle(f.val_block()?),
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
	struct ed8::Battle {
		battlefield, unk1, bgm, unk2,
		setups as setup: Setups,
	}
}
#[derive(Debug, Clone, Default)]
struct Setups {
	value: Vec<ed8::BattleSetup>,
}

impl Field for Setups {
	type Value = Vec<ed8::BattleSetup>;

	fn print_field(key: &str, f: &mut Printer, value: &Self::Value) {
		for setup in value {
			match setup {
				ed8::BattleSetup::Normal { n, monsters } => {
					f.word(key).val(n);
					let mut tup = f.term("");
					let mut monsters: &[_] = monsters;
					while monsters.last().is_some_and(|v| *v == Default::default()) {
						monsters = monsters.split_last().unwrap().1;
					}
					for v in monsters {
						tup.field().val(v);
					}
					drop(tup);
					f.line();
				}
			}
		}
	}

	fn parse_field<'src>(&mut self, _word: &'src str, f: &mut Parser<'src>) -> parse::Result<()> {
		let n = f.val()?;
		let mut monsters: [(String, u8); 8] = Default::default();
		let mut i = 0;
		f.tuple(|tup| {
			while let Some(f) = tup.try_field()? {
				if i == 8 {
					Diagnostic::error(f.pos()?.as_span(), "up to 8 monsters allowed").emit();
				}
				let v = f.val()?;
				if i < 8 {
					monsters[i] = v;
				}
				i += 1;
			}
			Ok(())
		})?;
		self.value.push(ed8::BattleSetup::Normal { n, monsters });
		Ok(())
	}

	fn is_present(&self) -> bool {
		!self.value.is_empty()
	}

	fn get(self) -> Option<Self::Value> {
		Some(self.value)
	}
}
