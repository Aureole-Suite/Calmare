use themelios::scena::insn::{AssOp, Atom, BinOp, Expr, UnOp};

use crate::parse::{self, Diagnostic};
use crate::{Parser, Printer};

pub fn print(e: &Expr, f: &mut Printer) {
	print_prio(e, 0, f)
}

fn print_prio(e: &Expr, prio: u8, f: &mut Printer) {
	match e {
		Expr::Atom(v) => {
			f.val(v);
		}
		Expr::Insn(i) => {
			f.val(i);
		}
		Expr::Rand => {
			f.word("random");
		}
		Expr::Bin(op, a, b) => {
			let (text, prio2) = binop(*op);
			if prio2 < prio {
				f.word("(").no_space();
			}
			print_prio(a, prio2, f);
			f.word(text);
			print_prio(b, prio2 + 1, f);
			if prio2 < prio {
				f.no_space().word(")");
			}
		}
		Expr::Unary(op, a) => {
			f.word(unop(*op)).no_space();
			print_prio(a, 10, f);
		}
		Expr::Assign(op, a) => {
			f.word(assop(*op));
			print_prio(a, 0, f);
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

pub fn parse(f: &mut Parser) -> parse::Result<Expr> {
	parse_binary(f, 0)
}

fn parse_binary(f: &mut Parser, prec: usize) -> parse::Result<Expr> {
	let mut e = parse_atom(f)?;
	while let Some((op_prec, op)) = parse_binop(f, prec) {
		let e2 = parse_binary(f, op_prec + 1)?;
		e = Expr::Bin(op, Box::new(e), Box::new(e2));
	}
	Ok(e)
}

fn parse_atom(f: &mut Parser) -> parse::Result<Expr> {
	if let Some(op) = parse_unop(f) {
		return Ok(Expr::Unary(op, Box::new(parse_atom(f)?)));
	}

	if f.check("(").is_ok() {
		let e = parse(f)?;
		f.check(")")?;
		return Ok(e);
	}

	if let Some(e) = f.try_parse(parse_lvalue)? {
		return Ok(Expr::Atom(e));
	}

	if let Some(i) = f.try_val()? {
		return Ok(Expr::Insn(i));
	}

	if f.check_word("random").is_ok() {
		return Ok(Expr::Rand);
	}

	Err(Diagnostic::error(f.pos()?.as_span(), "invalid expression"))
}

pub fn parse_lvalue(f: &mut Parser) -> parse::Result<Atom> {
	macro test($($a:ident),*) {
		$(if let Some(v) = f.try_val()?.map(Atom::$a) {
			return Ok(v)
		})*
	}
	test!(Int, Flag, Var, Attr, CharAttr, Global);
	Err(Diagnostic::error(f.pos()?.as_span(), "expected lvalue"))
}

pub fn parse_assignment(f: &mut Parser) -> parse::Result<Expr> {
	let Some(op) = parse_assop(f) else {
		return Err(Diagnostic::error(
			f.pos()?.as_span(),
			"expected assignment operator",
		));
	};
	let rhs = parse(f)?;
	Ok(Expr::Assign(op, Box::new(rhs)))
}

fn parse_unop(f: &mut Parser) -> Option<UnOp> {
	macro op($q:literal => $op:expr) {
		if f.check($q).is_ok() {
			return Some($op);
		}
	}

	op!("!" => UnOp::Not);
	op!("-" => UnOp::Neg);
	op!("~" => UnOp::Inv);
	None
}

fn parse_binop(f: &mut Parser, prec: usize) -> Option<(usize, BinOp)> {
	macro op_prec($op_prec:expr, $q:literal => $op:expr) {
		if prec <= $op_prec && f.check($q).is_ok() {
			return Some(($op_prec, $op));
		}
	}

	op_prec!(4, "==" => BinOp::Eq);
	op_prec!(4, "!=" => BinOp::Ne);
	op_prec!(4, "<=" => BinOp::Le);
	op_prec!(4, "<"  => BinOp::Lt);
	op_prec!(4, ">=" => BinOp::Ge);
	op_prec!(4, ">"  => BinOp::Gt);

	op_prec!(1, "||" => BinOp::Or);
	op_prec!(3, "&&" => BinOp::BoolAnd);

	op_prec!(5, "+"  => BinOp::Add);
	op_prec!(5, "-"  => BinOp::Sub);
	op_prec!(6, "*"  => BinOp::Mul);
	op_prec!(6, "/"  => BinOp::Div);
	op_prec!(6, "%"  => BinOp::Mod);
	op_prec!(1, "|"  => BinOp::Or);
	op_prec!(3, "&"  => BinOp::And);
	op_prec!(2, "^"  => BinOp::Xor);
	None
}

fn parse_assop(f: &mut Parser) -> Option<AssOp> {
	macro op($q:literal => $op:expr) {
		if f.check($q).is_ok() {
			return Some($op);
		}
	}

	op!("="  => AssOp::Ass);
	op!("+=" => AssOp::AddAss);
	op!("-=" => AssOp::SubAss);
	op!("*=" => AssOp::MulAss);
	op!("/=" => AssOp::DivAss);
	op!("%=" => AssOp::ModAss);
	op!("|=" => AssOp::OrAss);
	op!("&=" => AssOp::AndAss);
	op!("^=" => AssOp::XorAss);
	None
}
