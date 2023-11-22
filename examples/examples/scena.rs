use themelios::scena::ed6;
use themelios::scena::insn_set::{self, Game, Variant};
fn main() -> anyhow::Result<()> {
	unsafe { compact_debug::enable(true) }

	let iset = insn_set::get(Game::Sc, Variant::Evo);

	for file in std::env::args().skip(1) {
		println!("running {file}");
		let bytes = std::fs::read(&file)?;
		let scena = ed6::Scena::read(&iset, &bytes)?;
		let bytes2 = ed6::Scena::write(&iset, &scena)?;
		if bytes != bytes2 {
			println!("{file} differs");
			std::fs::write(
				format!("/tmp/scena/{}", file.rsplit_once('/').unwrap().1),
				bytes2,
			)?;
		}
	}

	Ok(())
}
