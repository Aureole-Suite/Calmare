use crate::{PrintContext, Printer, PrinterExt};
use themelios::scena::insn::{AssOp, BinOp, Expr, UnOp};

pub fn print(e: &Expr, f: &mut Printer, ctx: &mut PrintContext) {
	print_prio(e, 0, f, ctx)
}

fn print_prio(e: &Expr, prio: u8, f: &mut Printer, ctx: &mut PrintContext) {
	match e {
		Expr::Arg(v) => {
			f.val(v, ctx);
		}
		Expr::Insn(i) => {
			f.val(i, ctx);
		}
		Expr::Rand => {
			f.word("random");
		}
		Expr::Bin(op, a, b) => {
			let (text, prio2) = binop(*op);
			if prio2 < prio {
				f.word("(").no_space();
			}
			print_prio(a, prio2, f, ctx);
			f.word(text);
			print_prio(b, prio2 + 1, f, ctx);
			if prio2 < prio {
				f.no_space().word(")");
			}
		}
		Expr::Unary(op, a) => {
			f.word(unop(*op)).no_space();
			print_prio(a, 10, f, ctx);
		}
		Expr::Assign(op, a) => {
			f.word(assop(*op));
			print_prio(a, 0, f, ctx);
		}
	}
}

#[rustfmt::skip]
fn binop(op: BinOp) -> (&'static str, u8) {
	match op {
		BinOp::Eq      => ("==", 4),
		BinOp::Ne      => ("!=", 4),
		BinOp::Lt      => ("<",  4),
		BinOp::Gt      => (">",  4),
		BinOp::Le      => ("<=", 4),
		BinOp::Ge      => (">=", 4),
		BinOp::BoolAnd => ("&&", 3),
		BinOp::And     => ("&", 3),
		BinOp::Or      => ("|", 1),
		BinOp::Add     => ("+", 5),
		BinOp::Sub     => ("-", 5),
		BinOp::Xor     => ("^", 2),
		BinOp::Mul     => ("*", 6),
		BinOp::Div     => ("/", 6),
		BinOp::Mod     => ("%", 6),
	}
}

#[rustfmt::skip]
fn unop(op: UnOp) -> &'static str {
	match op {
		UnOp::Not => "!",
		UnOp::Neg => "-",
		UnOp::Inv => "~",
	}
}

#[rustfmt::skip]
fn assop(op: AssOp) -> &'static str {
	match op {
		AssOp::Ass    => "=",
		AssOp::MulAss => "*=",
		AssOp::DivAss => "/=",
		AssOp::ModAss => "%=",
		AssOp::AddAss => "+=",
		AssOp::SubAss => "-=",
		AssOp::AndAss => "&=",
		AssOp::XorAss => "^=",
		AssOp::OrAss  => "|=",
	}
}
