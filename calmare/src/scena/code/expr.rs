use crate::{PrintContext, Printer, PrinterExt};
use themelios::scena::insn::{Expr, Op, OpKind, Term};

pub fn print(e: &Expr, f: &mut Printer, ctx: &mut PrintContext) {
	let mut stack = Vec::new();
	for t in &e.0 {
		if let Term::Op(op) = t {
			match op.kind() {
				OpKind::Unary => {
					let a = stack.pop().unwrap_or_default();
					stack.push(E::Un(*op, Box::new(a)));
				}
				OpKind::Binary => {
					let b = stack.pop().unwrap_or_default();
					let a = stack.pop().unwrap_or_default();
					stack.push(E::Bin(*op, Box::new(a), Box::new(b)));
				}
				OpKind::Assign => {
					let a = stack.pop().unwrap_or_default();
					stack.push(E::Ass(*op, Box::new(a)));
				}
			}
		} else {
			stack.push(E::Atom(t))
		}
	}

	if stack.is_empty() {
		f.word("(EXPR MISSING)");
	} else {
		for (i, e) in stack.into_iter().enumerate() {
			if i != 0 {
				f.word("¤");
			}
			print_prio(e, 0, f, ctx);
		}
	}
}

#[derive(Default)]
enum E<'a> {
	Atom(&'a Term),
	Bin(Op, Box<E<'a>>, Box<E<'a>>),
	Un(Op, Box<E<'a>>),
	Ass(Op, Box<E<'a>>),
	#[default]
	Error,
}

fn print_prio(e: E, prio: u8, f: &mut Printer, ctx: &mut PrintContext) {
	match e {
		E::Atom(a) => {
			match a {
				Term::Op(_) => unreachable!(),
				Term::Arg(v) => f.val(v, ctx),
				Term::Insn(i) => f.val(&**i, ctx),
				Term::Rand => f.word("random"),
			};
		}
		E::Bin(op, a, b) => {
			let (text, prio2) = op_str(op);
			if prio2 < prio {
				f.word("(").no_space();
			}
			print_prio(*a, prio2, f, ctx);
			f.word(text);
			print_prio(*b, prio2 + 1, f, ctx);
			if prio2 < prio {
				f.no_space().word(")");
			}
		}
		E::Un(op, a) => {
			let (text, prio) = op_str(op);
			f.word(text).no_space();
			print_prio(*a, prio, f, ctx);
		}
		E::Ass(op, a) => {
			let (text, prio) = op_str(op);
			f.word(text);
			print_prio(*a, prio, f, ctx);
		}
		E::Error => {
			write!(f, "(EXPR MISSING)");
		}
	}
}

#[rustfmt::skip]
fn op_str(op: Op) -> (&'static str, u8) {
	match op {
		Op::Eq      => ("==", 4),
		Op::Ne      => ("!=", 4),
		Op::Lt      => ("<",  4),
		Op::Gt      => (">",  4),
		Op::Le      => ("<=", 4),
		Op::Ge      => (">=", 4),
		Op::BoolAnd => ("&&", 3),
		Op::And     => ("&", 3),
		Op::Or      => ("|", 1),
		Op::Add     => ("+", 5),
		Op::Sub     => ("-", 5),
		Op::Xor     => ("^", 2),
		Op::Mul     => ("*", 6),
		Op::Div     => ("/", 6),
		Op::Mod     => ("%", 6),

		Op::Not    => ("!", 10),
		Op::Neg    => ("-", 10),
		Op::Inv    => ("~", 10),

		Op::Ass    => ("=",  0),
		Op::MulAss => ("*=", 0),
		Op::DivAss => ("/=", 0),
		Op::ModAss => ("%=", 0),
		Op::AddAss => ("+=", 0),
		Op::SubAss => ("-=", 0),
		Op::AndAss => ("&=", 0),
		Op::XorAss => ("^=", 0),
		Op::OrAss  => ("|=", 0),
	}
}

