use super::visit_mut::{VisitMut, VisitableMut};
use super::Code;
use crate::scena::insn::{Arg, Insn};
use crate::types::Label;

pub fn decompile(code: &mut impl VisitableMut) {
	struct Vis;

	impl VisitMut for Vis {
		fn visit_code_mut(&mut self, code: &mut Code) -> std::ops::ControlFlow<()> {
			*code = block(
				Context::new(&mut std::mem::take(&mut code.0).into_iter()),
				None,
				None,
			);
			std::ops::ControlFlow::Continue(())
		}
	}
	code.accept_mut(&mut Vis);
}

struct Context<'a> {
	insns: &'a mut std::vec::IntoIter<Insn>,
	len: usize,
}

impl<'a> std::fmt::Debug for Context<'a> {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		f.debug_tuple("Context").field(&self.as_slice()).finish()
	}
}

impl<'a> Context<'a> {
	fn new(insns: &'a mut std::vec::IntoIter<Insn>) -> Self {
		Self {
			len: insns.as_slice().len(),
			insns,
		}
	}

	fn as_slice(&self) -> &[Insn] {
		&self.insns.as_slice()[..(self.len+1).min(self.insns.as_slice().len())]
	}

	fn until(&mut self, len: usize) -> Context<'_> {
		assert!(len <= self.len);
		self.len -= len;
		Context {
			insns: self.insns,
			len,
		}
	}

	fn lookup(&self, label: Label) -> Option<usize> {
		self.as_slice()
			.iter()
			.map(as_label)
			.enumerate()
			.find(|(_, l)| *l == Some(label))
			.map(|i| i.0)
	}

	fn peek(&self, pos: usize) -> Option<&Insn> {
		self.as_slice().get(pos)
	}
}

impl<'a> Iterator for Context<'a> {
	type Item = Insn;

	fn next(&mut self) -> Option<Self::Item> {
		if self.len == 0 {
			None
		} else {
			self.len -= 1;
			self.insns.next()
		}
	}
}

fn block(mut ctx: Context, cont: Option<Label>, brk: Option<Label>) -> Code {
	let mut out = Vec::new();
	let mut label = None;
	while let Some(insn) = ctx.next() {
		out.push(insn);
		let insn = out.last_mut().unwrap();
		match insn.parts() {
			("_goto", &[Arg::Label(l)]) => {
				if Some(l) == brk {
					*insn = Insn::new("break", vec![])
				} else if Some(l) == cont {
					*insn = Insn::new("continue", vec![])
				}
			}

			("_if", &[.., Arg::Label(l1)]) => {
				let Some(target) = ctx.lookup(l1) else {
					continue;
				};

				insn.args.pop();

				let is_loop = ctx
					.peek(target - 1)
					.and_then(as_goto)
					.is_some_and(|l| Some(l) == label);

				if is_loop {
					let mut body = block(ctx.until(target), label, Some(l1));
					assert_eq!(body.pop().unwrap().name, "continue");
					insn.name = "while".into();
					insn.args.push(Arg::Code(body));
				} else {
					let mut body = block(ctx.until(target), cont, brk);
					let l2 = body
						.last()
						.and_then(as_goto)
						.and_then(|l2| ctx.lookup(l2));
					if let Some(l2) = l2 {
						body.pop();
						let body2 = block(ctx.until(l2), cont, brk);
						insn.name = "if".into();
						insn.args.push(Arg::Code(body));
						insn.args.push(Arg::Code(body2));
					} else {
						insn.name = "if".into();
						insn.args.push(Arg::Code(body));
					}
				}
			}

			_ => {}
		}
		label = as_label(insn);
	}
	Code(out)
}

fn as_goto(insn: &Insn) -> Option<Label> {
	if let ("_goto", [Arg::Label(label)]) = insn.parts() {
		Some(*label)
	} else {
		None
	}
}

fn as_label(insn: &Insn) -> Option<Label> {
	if let ("_label", [Arg::Label(label)]) = insn.parts() {
		Some(*label)
	} else {
		None
	}
}
