use std::collections::BTreeMap;

use super::visit_mut::{VisitMut, VisitableMut};
use super::{Arg, Code, Insn};
use crate::types::Label;

pub fn decompile(code: &mut impl VisitableMut) {
	struct Vis;

	impl VisitMut for Vis {
		fn visit_code_mut(&mut self, code: &mut Code) -> std::ops::ControlFlow<()> {
			*code = block(
				Context::new(std::mem::take(&mut code.0)).iter(),
				None,
				None,
				false,
			);
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
			.and_then(|&i| self.initial_len.checked_sub(i))
			.and_then(|i| self.iter.len().checked_sub(i))
	}

	fn iter(&mut self) -> ContextIter<'_> {
		ContextIter {
			endlen: 0,
			ctx: self,
		}
	}
}

struct ContextIter<'a> {
	endlen: usize,
	ctx: &'a mut Context,
}

impl<'a> ContextIter<'a> {
	fn as_slice(&self) -> &[Insn] {
		let slice = self.ctx.iter.as_slice();
		&slice[..slice.len() - self.endlen]
	}

	fn until(&mut self, label: Label) -> ContextIter<'_> {
		let pos = self.lookup(label).unwrap();
		ContextIter {
			endlen: self.ctx.iter.as_slice().len() - pos,
			ctx: self.ctx,
		}
	}

	fn as_ref(&mut self) -> ContextIter<'_> {
		ContextIter {
			endlen: self.endlen,
			ctx: self.ctx,
		}
	}

	fn lookup(&self, label: Label) -> Option<usize> {
		self.ctx
			.lookup(label)
			.filter(|i| *i <= self.as_slice().len())
	}

	fn peek(&self, pos: usize) -> Option<&Insn> {
		self.as_slice().get(pos)
	}
}

impl<'a> Iterator for ContextIter<'a> {
	type Item = Insn;

	fn next(&mut self) -> Option<Self::Item> {
		if self.as_slice().is_empty() {
			None
		} else {
			self.ctx.iter.next()
		}
	}
}

fn block(mut ctx: ContextIter, cont: Option<Label>, brk: Option<Label>, switch_tail: bool) -> Code {
	let mut out = Vec::new();
	let mut label = None;
	while let Some(insn) = ctx.next() {
		out.push(insn);
		let insn = out.last_mut().unwrap();
		let label = std::mem::replace(&mut label, as_label(insn));
		match (insn.name.as_str(), insn.args.as_mut_slice()) {
			("_goto", &mut [Arg::Label(l)]) => {
				if brk == Some(l) {
					*insn = Insn::new("break", vec![]);
				} else if cont == Some(l) {
					*insn = Insn::new("continue", vec![]);
				} else if switch_tail && ctx.as_slice().first().and_then(as_label) == Some(l) {
					*insn = Insn::new("break", vec![]);
					break;
				}
			}

			("_if", &mut [.., Arg::Label(l1)]) => {
				let Some(target) = ctx.lookup(l1) else {
					continue;
				};
				insn.args.pop();

				let is_loop = target
					.checked_sub(1)
					.and_then(|p| ctx.peek(p))
					.and_then(as_goto)
					.is_some_and(|l| Some(l) == label);

				if is_loop {
					let mut body = block(ctx.until(l1), label, Some(l1), false);
					assert_eq!(body.pop().unwrap().name, "continue");
					insn.name = "while".into();
					insn.args.push(Arg::Code(body));
					continue;
				}

				let mut body = block(ctx.until(l1), cont, brk, false);
				let l2 = body
					.last()
					.and_then(as_goto)
					.filter(|l2| ctx.lookup(*l2).is_some());
				if let Some(l2) = l2 {
					body.pop();
					let body2 = block(ctx.until(l2), cont, brk, false);
					insn.name = "if".into();
					insn.args.push(Arg::Code(body));
					insn.args.push(Arg::Code(body2));
				} else {
					insn.name = "if".into();
					insn.args.push(Arg::Code(body));
				}
			}

			("_switch", [.., Arg::Tuple(cases), Arg::Label(default)]) => {
				let Some(cases) = find_cases(&ctx, cases, default) else {
					continue;
				};
				insn.args.pop();
				insn.args.pop();

				let mut switch_brk = None;
				for case_end in cases.iter().map(|a| &a.1).skip(1) {
					if let Some(end) = ctx
						.lookup(*case_end)
						.and_then(|p| p.checked_sub(1))
						.and_then(|p| ctx.peek(p))
						.and_then(as_goto)
					{
						if ctx.lookup(end).is_some() {
							switch_brk = Some(end);
						}
					}
				}

				let mut bodies = Vec::new();
				for end in cases.iter().map(|i| i.1).skip(1).chain(switch_brk) {
					let body = block(ctx.until(end), cont, switch_brk, false);
					bodies.push(body);
				}

				let tail = if switch_brk.is_none() {
					let body = block(ctx.as_ref(), cont, None, true);
					if ctx.as_slice().is_empty() {
						bodies.push(Code(vec![]));
						Some(body)
					} else {
						bodies.push(body);
						None
					}
				} else {
					None
				};

				assert_eq!(cases.len(), bodies.len());

				let mut switchbody = Vec::new();
				for ((mut case, _), body) in std::iter::zip(cases, bodies) {
					case.args.push(Arg::Code(body));
					switchbody.push(case);
				}

				insn.name = "switch".into();
				insn.args.push(Arg::Code(Code(switchbody)));

				if let Some(tail) = tail {
					out.extend(tail.0);
					break;
				}
			}
			_ => {}
		}
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
