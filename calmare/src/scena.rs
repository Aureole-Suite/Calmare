use std::collections::BTreeMap;

use themelios::scena;

mod code;
mod ed6;
mod ed7;

use crate::macros::strukt::{Field, Slot};
use crate::macros::{newtype_hex, newtype_term};
use crate::parse::{Diagnostic, Emit as _};
use crate::{parse, Parse, Parser};
use crate::{Print, Printer};

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
struct LocalFuncId(u16);

newtype_term!(LocalFuncId, "fn");

impl Print for scena::FuncId {
	fn print(&self, f: &mut Printer) {
		let mut term = f.term("fn");
		term.field().val(self.0);
		term.field().val(self.1);
	}
}

impl Parse for scena::FuncId {
	fn parse(f: &mut Parser) -> parse::Result<Self> {
		f.check_word("fn")?;
		f.check("[")?;
		let a = f.val()?;
		f.check(",")?;
		let b = f.val()?;
		f.check("]")?;
		Ok(scena::FuncId(a, b))
	}
}

newtype_term!(scena::ChipId, "chip");
newtype_term!(scena::LocalCharId, "char");
newtype_hex!(scena::CharFlags);
newtype_term!(scena::EventId, "event");
newtype_hex!(scena::EventFlags);
newtype_term!(scena::LookPointId, "look_point");
newtype_hex!(scena::LookPointFlags);
newtype_term!(scena::EntryId, "entry");
newtype_hex!(scena::EntryFlags);

impl Print for scena::CharId {
	fn print(&self, f: &mut Printer) {
		use scena::CharId as C;
		match self {
			C::Party(v) => f.term("field_party").field().val(v),
			C::Local(v) => f.val(v),
			C::Custom(v) => f.term("custom").field().val(v),
			C::Party2(v) => f.term("party").field().val(v),
			C::Self_ => f.word("self"),
			C::Null => f.word("null"),
			C::Name(v) => f.val(v),
			C::Chest => f.word("chest"),
		};
	}
}

impl Parse for scena::CharId {
	fn parse(f: &mut Parser) -> parse::Result<Self> {
		use scena::CharId as C;
		let v = if f.check_word("field_party").is_ok() {
			C::Party(f.sqbrack_val()?)
		} else if let Some(v) = f.try_val()? {
			C::Local(v)
		} else if f.check_word("custom").is_ok() {
			C::Custom(f.sqbrack_val()?)
		} else if f.check_word("party").is_ok() {
			C::Party2(f.sqbrack_val()?)
		} else if f.check_word("self").is_ok() {
			C::Self_
		} else if f.check_word("null").is_ok() {
			C::Null
		} else if let Some(v) = f.try_val()? {
			C::Name(v)
		} else if f.check_word("chest").is_ok() {
			C::Chest
		} else {
			let word = f.peek_word()?;
			return Err(parse::Diagnostic::info(f.span_of(word), "expected charid"));
		};
		Ok(v)
	}
}

newtype_term!(scena::EntranceId, "entrance"); // defined in ._en file
newtype_term!(scena::ObjectId, "object"); // defined in ._op file
newtype_term!(scena::ForkId, "fork");
newtype_term!(scena::MenuId, "menu");
newtype_term!(scena::EffId, "eff");
newtype_term!(scena::EffInstanceId, "eff_instance");
newtype_term!(scena::VisId, "vis");

newtype_term!(scena::Var, "var");
newtype_term!(scena::Global, "global");
newtype_term!(scena::Attr, "system");

impl Print for scena::CharAttr {
	fn print(&self, f: &mut Printer) {
		f.val(self.0).no_space().word(".").no_space().val(self.1);
	}
}

impl Parse for scena::CharAttr {
	fn parse(f: &mut Parser) -> parse::Result<Self> {
		let char = f.val()?;
		f.no_space().check(".")?;
		let attr = f.no_space().val()?;
		Ok(scena::CharAttr(char, attr))
	}
}

#[derive(Debug, Clone)]
struct PackedIndices<V> {
	name: &'static str,
	items: BTreeMap<usize, Slot<V>>,
}

impl<V> PackedIndices<V> {
	pub fn new(name: &'static str) -> Self {
		Self {
			name,
			items: BTreeMap::new(),
		}
	}

	pub fn insert<'src>(
		&mut self,
		f: &mut Parser<'src>,
		word: &'src str,
		body: impl FnOnce(&mut Parser) -> parse::Result<V>,
	) {
		let Some(pos) = f.pos().emit() else {
			return;
		};

		let id = parse::Result::<usize>::emit(
			try {
				let res = f.check_word(self.name);
				if word != self.name {
					res?;
				}

				f.sqbrack_val()?
			},
		);

		let span = f.span_of(word) | f.span(pos);
		let val = body(f);
		if let Some(id) = id {
			self.items.entry(id).or_default().insert(span, val);
		}
	}

	pub fn items(&self) -> &BTreeMap<usize, Slot<V>> {
		&self.items
	}

	pub fn into_items(self) -> BTreeMap<usize, Slot<V>> {
		self.items
	}

	pub fn finish(self) -> Vec<V> {
		Self::finish_on(self.name, self.items)
	}

	pub fn finish_on(name: &str, items: impl IntoIterator<Item = (usize, Slot<V>)>) -> Vec<V> {
		let items = items.into_iter();
		let mut vs = Vec::with_capacity(items.size_hint().0);
		let mut expect = 0;
		for (k, slot) in items {
			if k != expect {
				Diagnostic::error(slot.span().unwrap(), format!("missing {}[{expect}]", name))
					.emit();
			}
			expect = k + 1;
			vs.extend(slot.get())
		}
		vs
	}
}

#[derive(Debug, Clone)]
enum Actor<A, B> {
	Npc(A),
	Monster(B),
}

fn split_actors<A, B>(items: PackedIndices<Actor<A, B>>) -> (Vec<A>, Vec<B>) {
	let mut iter = items.items().iter().peekable();
	while let (Some(a), Some(b)) = (iter.next(), iter.peek()) {
		if matches!(&a.1.get_ref(), Some(Actor::Monster(_)))
			&& matches!(&b.1.get_ref(), Some(Actor::Npc(_)))
		{
			Diagnostic::error(b.1.span().unwrap(), "npcs mut come before monsters")
				.with_note(a.1.span().unwrap(), "is after this monster")
				.emit();
		}
	}

	let mut npcs = Vec::new();
	let mut monsters = Vec::new();
	for m in items.finish() {
		match m {
			Actor::Npc(n) => npcs.push(n),
			Actor::Monster(m) => monsters.push(m),
		}
	}

	(npcs, monsters)
}

#[derive(Debug, Clone)]
struct Array<const N: usize, T> {
	value: [Slot<T>; N],
}

impl<const N: usize, T> Default for Array<N, T> {
	fn default() -> Self {
		Self {
			value: std::array::from_fn(|_| Slot::new()),
		}
	}
}

impl<const N: usize, T: Print + Parse> Field for Array<N, T> {
	type Value = [Option<T>; N];

	fn print_field(key: &str, f: &mut Printer, value: &Self::Value) {
		for (i, value) in value.iter().enumerate() {
			if let Some(value) = value {
				f.term(key).field().val(i);
				f.val(value).line();
			}
		}
	}

	fn parse_field<'src>(&mut self, word: &'src str, f: &mut Parser<'src>) -> parse::Result<()> {
		let pos = f.pos()?;
		let n = f.sqbrack_val::<usize>()?;
		let value = f.val();
		self.value[n].insert(f.span_of(word) | f.span(pos), value);
		Ok(())
	}

	fn is_present(&self) -> bool {
		true
	}

	fn get(self) -> Option<Self::Value> {
		Some(self.value.map(|v| v.get()))
	}
}
