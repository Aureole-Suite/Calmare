use std::collections::BTreeMap;

use super::visit_mut::{VisitMut, VisitableMut};
use super::Code;
use crate::scena::insn::{Arg, Insn};
use crate::types::Label;

pub fn decompile(code: &mut impl VisitableMut) {
	struct Vis;

	impl VisitMut for Vis {
		fn visit_code_mut(&mut self, code: &mut Code) -> std::ops::ControlFlow<()> {
			*code = block(Context::new(std::mem::take(&mut code.0)).iter(), None, None);
			std::ops::ControlFlow::Continue(())
		}
	}
	code.accept_mut(&mut Vis);
}

struct Context {
	initial_len: usize,
	labels: BTreeMap<Label, usize>,
	iter: std::vec::IntoIter<Insn>,
}

impl Context {
	fn new(insns: Vec<Insn>) -> Self {
		Self {
			initial_len: insns.len(),
			labels: insns
				.iter()
				.enumerate()
				.filter_map(|(i, insn)| as_label(insn).map(|j| (j, i)))
				.collect(),
			iter: insns.into_iter(),
		}
	}

	fn lookup(&self, label: Label) -> Option<usize> {
		self.labels
			.get(&label)
			.map(|&i| self.iter.len() - (self.initial_len - i))
	}

	fn iter(&mut self) -> ContextIter<'_> {
		ContextIter {
			len: self.iter.as_slice().len(),
			ctx: self,
		}
	}
}

struct ContextIter<'a> {
	ctx: &'a mut Context,
	len: usize,
}

impl<'a> ContextIter<'a> {
	fn as_slice(&self) -> &[Insn] {
		let slice = self.ctx.iter.as_slice();
		&slice[..(self.len + 1).min(slice.len())]
	}

	fn until(&mut self, label: Label) -> ContextIter<'_> {
		let len = self.lookup(label).unwrap();
		assert!(len <= self.len);
		self.len -= len;
		ContextIter { ctx: self.ctx, len }
	}

	fn lookup(&self, label: Label) -> Option<usize> {
		self.ctx.lookup(label).filter(|i| *i <= self.len)
	}

	fn peek(&self, pos: usize) -> Option<&Insn> {
		self.as_slice().get(pos)
	}
}

impl<'a> Iterator for ContextIter<'a> {
	type Item = Insn;

	fn next(&mut self) -> Option<Self::Item> {
		if self.len == 0 {
			None
		} else {
			self.len -= 1;
			self.ctx.iter.next()
		}
	}
}

fn block(mut ctx: ContextIter, cont: Option<Label>, brk: Option<Label>) -> Code {
	let mut out = Vec::new();
	let mut label = None;
	while let Some(insn) = ctx.next() {
		out.push(insn);
		let insn = out.last_mut().unwrap();
		match (insn.name.as_str(), insn.args.as_mut_slice()) {
			("_goto", &mut [Arg::Label(l)]) => {
				if Some(l) == brk {
					*insn = Insn::new("break", vec![])
				} else if Some(l) == cont {
					*insn = Insn::new("continue", vec![])
				}
			}

			("_if", &mut [.., Arg::Label(l1)]) => {
				let Some(target) = ctx.lookup(l1) else {
					continue;
				};
				insn.args.pop();

				let is_loop = ctx
					.peek(target - 1)
					.and_then(as_goto)
					.is_some_and(|l| Some(l) == label);

				if is_loop {
					let mut body = block(ctx.until(l1), label, Some(l1));
					assert_eq!(body.pop().unwrap().name, "continue");
					insn.name = "while".into();
					insn.args.push(Arg::Code(body));
				} else {
					let mut body = block(ctx.until(l1), cont, brk);
					let l2 = body
						.last()
						.and_then(as_goto)
						.filter(|l2| ctx.lookup(*l2).is_some());
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

			("_switch", [.., Arg::Tuple(cases), Arg::Label(default)]) if false => {
				let Some(cases) = find_cases(&ctx, cases, default) else {
					continue;
				};
				insn.args.pop();
				insn.args.pop();

				let mut switch_brk = None;
				for case_end in cases.iter().map(|a| &a.1).skip(1) {
					if let Some(end) = ctx.lookup(*case_end).and_then(|p| ctx.peek(p - 1)).and_then(as_goto) {
						if ctx.lookup(end).is_some() {
							switch_brk = Some(end);
						}
					}
				}

				// There's a couple of edge cases where the last case is the only that has a break.
				// These are currently not handled, might improve it later.

				let mut bodies = Vec::new();
				for end in cases.iter().map(|i| i.1).skip(1).chain(switch_brk) {
					let body = block(ctx.until(end), cont, switch_brk);
					bodies.push(body);
				}

				bodies.push(Code(vec![]));

				let mut out = Vec::new();
				for ((mut case, _), body) in std::iter::zip(cases, bodies) {
					case.args.push(Arg::Code(body));
					out.push(case);
				}

				insn.name = "switch".into();
				insn.args.push(Arg::Code(Code(out)));
			}
			_ => {}
		}
		label = as_label(insn);
	}
	Code(out)
}

fn find_cases(ctx: &ContextIter, cases: &mut [Arg], default: &Label) -> Option<Vec<(Insn, Label)>> {
	let default_pos = ctx.lookup(*default)?;

	let mut out = Vec::new();
	for case in cases.iter() {
		let Arg::Tuple(case) = case else {
			return None;
		};
		let Some(Arg::Label(label)) = case.last() else {
			return None;
		};
		ctx.lookup(*label)?;
		out.push((Insn::new("case", vec![]), *label));
	}

	if !out.is_sorted_by_key(|a| ctx.lookup(a.1).unwrap()) {
		return None;
	}

	for (insn, case) in std::iter::zip(&mut out, cases) {
		let Arg::Tuple(case) = case else {
			return None;
		};
		case.pop();
		std::mem::swap(&mut insn.0.args, case);
	}

	let idx = out.partition_point(|a| ctx.lookup(a.1).unwrap() < default_pos);
	out.insert(idx, (Insn::new("default", vec![]), *default));
	Some(out)
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
