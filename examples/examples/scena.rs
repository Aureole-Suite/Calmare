use themelios::scena::ed7;
use themelios::scena::insn_set::{self, Game, Variant};
fn main() -> anyhow::Result<()> {
	unsafe { compact_debug::enable(true) }

	let iset = insn_set::get(Game::Zero, Variant::Base);

	for file in std::env::args().skip(1) {
		println!("running {file}");
		let bytes = std::fs::read(&file)?;
		let scena = ed7::Scena::read(&iset, &bytes)?;
		// dbg!(scena);
		// let bytes2 = ed7::Scena::write(&iset, &scena)?;
		// if bytes != bytes2 {
		// 	println!("{file} differs");
		// 	std::fs::write(
		// 		format!("/tmp/scena/{}", file.rsplit_once('/').unwrap().1),
		// 		bytes2,
		// 	)?;
		// }
	}

	Ok(())
}
