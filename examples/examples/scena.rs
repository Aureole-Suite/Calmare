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
		scena.print(&mut calmare::PrintContext {}, &mut printer);
		print!("{}", printer.finish());

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
