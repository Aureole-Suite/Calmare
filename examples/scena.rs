use themelios::scena::ed6;
use themelios::scena::insn_set::Builtin;

fn main() -> anyhow::Result<()> {
	unsafe { compact_debug::enable(true) }

	let iset = Builtin::Fc.get();

	for file in std::env::args().skip(1) {
		println!("{file}");
		let scena = ed6::Scena::read(iset, &std::fs::read(file)?)?;
		println!("{:#?}", scena);
	}

	Ok(())
}
