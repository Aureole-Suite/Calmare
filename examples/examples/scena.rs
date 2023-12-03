use calmare::parse::{self, Emit as _};
use calmare::Print as _;
use themelios::scena::ed6::Scena;
use themelios::scena::insn_set::{self, Game, Variant};
fn main() -> anyhow::Result<()> {
	unsafe { compact_debug::enable(true) }

	let iset = insn_set::get(Game::Sc, Variant::Base);

	for file in std::env::args().skip(1) {
		println!("running {file}");
		let bytes = std::fs::read(&file)?;
		let mut scena = Scena::read(&iset, &bytes)?;
		// dbg!(&scena);
		themelios::scena::code::decompile::decompile(&mut scena.functions);
		themelios::scena::code::normalize::normalize(&mut scena.functions).unwrap();

		let mut printer = calmare::Printer::new();
		scena.look_points[0].print(&mut printer, &mut calmare::PrintContext {});
		let output = printer.finish();
		print!("{}", output);
		let mut parser = calmare::Parser::new(&output);
		let v: Option<themelios::scena::ed6::LookPoint> =
			calmare::Parse::parse(&mut parser, &mut calmare::ParseContext {}).emit(&mut parser);
		print_diags(&file, &output, parser.diagnostics());
		assert_eq!(v.unwrap(), scena.look_points[0]);

		// println!("{:#?}", scena.functions[0]);
		let bytes2 = Scena::write(&iset, &scena)?;
		if bytes != bytes2 {
			std::fs::write(
				format!("/tmp/scena/{}", file.rsplit_once('/').unwrap().1),
				&bytes2,
			)?;
			let scena2 = Scena::read(&iset, &bytes2)?;
			if scena == scena2 {
				println!("  {file} differs");
			} else {
				println!("  {file} differs significantly!!!!");
			}
		}
	}

	Ok(())
}

pub fn print_diags(filename: &str, source: &str, diags: &[parse::Diagnostic]) {
	use codespan_reporting::diagnostic::{Diagnostic, Label};
	use codespan_reporting::files::SimpleFiles;
	use codespan_reporting::term::termcolor::{ColorChoice, StandardStream};

	let writer = StandardStream::stderr(ColorChoice::Always);
	let config = codespan_reporting::term::Config::default();
	let mut files = SimpleFiles::new();
	let file_id = files.add(filename, source);

	let mut diags = diags.to_owned();
	diags.sort_by_key(|a| (a.text.0.start, a.text.0.end));

	for d in diags {
		let mut l = vec![Label::primary(file_id, d.text.0.as_range()).with_message(&d.text.1)];
		for n in &d.notes {
			l.push(Label::secondary(file_id, n.0.as_range()).with_message(&n.1));
		}
		let d = match d.level {
			parse::Level::Error => Diagnostic::error(),
			parse::Level::Warning => Diagnostic::warning(),
			parse::Level::Info => Diagnostic::help(),
		};
		let d = d.with_labels(l);
		codespan_reporting::term::emit(&mut writer.lock(), &config, &files, &d).unwrap();
	}
}
