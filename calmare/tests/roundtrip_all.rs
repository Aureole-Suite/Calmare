#![feature(lazy_cell)]

use std::process::ExitCode;
use std::sync::LazyLock;
use std::{ffi::OsStr, path::Path};

use themelios::gamedata::{self, Encoding, Game, InsnSet, Variant};
use themelios::scena::code::Code;
use themelios::scena::{ReadError, WriteError};

fn compact() {
	static ONCE: std::sync::Once = std::sync::Once::new();
	ONCE.call_once(|| unsafe { compact_debug::enable(true) });
}

static ROOT: LazyLock<&Path> = LazyLock::new(|| Path::new("/home/large/kiseki"));

type Ret = anyhow::Result<ExitCode>;

fn ed67<T: PartialEq + std::fmt::Debug + calmare::PrintBlock + calmare::ParseBlock>(
	dir: &str,
	ext: &str,
	iset: &InsnSet,
	read: impl Fn(&[u8]) -> Result<T, ReadError>,
	write: impl Fn(&T) -> Result<Vec<u8>, WriteError>,
	funcs: impl Fn(&mut T) -> &mut [Code],
) -> Ret {
	compact();
	let dir = ROOT.join(dir);
	let mut failed = false;
	for file in dir.read_dir()? {
		let file = file?;
		if file.path().extension() != Some(OsStr::new(ext)) {
			continue;
		}
		let bytes = std::fs::read(file.path())?;
		let mut scena = read(&bytes)?;
		themelios::scena::code::decompile::decompile(funcs(&mut scena));
		themelios::scena::code::normalize::normalize(funcs(&mut scena))?;
		let output = calmare::print(&scena, iset);
		let (scena2, mut diags) = calmare::parse::<T>(&output, iset);
		diags.retain(|v| v.level == calmare::parse::Level::Error);
		if !diags.is_empty() {
			eprintln!("{:#?}", diags);
		}
		let scena2 = scena2.unwrap();

		if scena != scena2 {
			let output2 = calmare::print(&scena2, iset);
			similar_asserts::assert_eq!(output, output2);
			similar_asserts::assert_eq!(scena, scena2);
		}
	}
	Ok(if failed {
		ExitCode::FAILURE
	} else {
		ExitCode::SUCCESS
	})
}

fn ed6(dir: &str, game: Game, variant: Variant, ext: &str) -> Ret {
	use themelios::scena::ed6::Scena;
	let iset = gamedata::get(game, variant, Encoding::Sjis);
	ed67(
		dir,
		ext,
		&iset,
		|d| Scena::read(&iset, d),
		|s| Scena::write(&iset, s),
		|s| &mut s.functions,
	)
}

fn ed7(dir: &str, game: Game, variant: Variant) -> Ret {
	use themelios::scena::ed7::Scena;
	let iset = gamedata::get(game, variant, Encoding::Sjis);
	ed67(
		dir,
		"bin",
		&iset,
		|d| Scena::read(&iset, d),
		|s| Scena::write(&iset, s),
		|s| &mut s.functions,
	)
}

#[rustfmt::skip]
mod ed6 {
	use super::*;
	#[test] fn fc() -> Ret { ed6("fc.extract/01", Game::Fc, Variant::Base, "_sn") }
	#[test] fn sc() -> Ret { ed6("sc.extract/21", Game::Sc, Variant::Base, "_sn") }
	#[test] fn tc() -> Ret { ed6("3rd.extract/21", Game::Tc, Variant::Base, "_sn") }
	#[test] fn fc_evo() -> Ret { ed6("fc-evo/data/scenario/0", Game::Fc, Variant::Evo, "bin") }
	#[test] fn sc_evo() -> Ret { ed6("sc-evo/data_sc/scenario/1", Game::Sc, Variant::Evo, "bin") }
	#[test] fn tc_evo() -> Ret { ed6("3rd-evo/data_3rd/scenario/2", Game::Tc, Variant::Evo, "bin") }
	#[test] fn fc_psp() -> Ret { ed6("psp/fc/PSP_GAME/USRDIR/data/scenario/0", Game::Fc, Variant::Base, "bin") }
	#[test] fn fc_psp_kai() -> Ret { ed6("psp/fc-kai/PSP_GAME/USRDIR/data/scenario/0", Game::Fc, Variant::Base, "bin") }
	// #[test] fn sc_psp() -> Ret { ed6("psp/sc/PSP_GAME/USRDIR/data_sc/scenario/1", Game::Sc, Variant::Base, "bin") }
	// #[test] fn tc_psp() -> Ret { ed6("psp/3rd/PSP_GAME/USRDIR/data_3rd/scenario/2", Game::Tc, Variant::Base, "bin") }
}

#[rustfmt::skip]
mod ed7 {
	use super::*;
	// #[test] fn zero_gf_jp() -> Ret { ed7("zero-gf/data/scena", Game::Zero, Variant::Base) }
	// #[test] fn zero_gf_en() -> Ret { ed7("zero-gf/data_en/scena", Game::Zero, Variant::Base) }
	#[test] fn zero_jp() -> Ret { ed7("zero/data/scena", Game::Zero, Variant::Kai) }
	#[test] fn zero_en() -> Ret { ed7("zero/data/scena_us", Game::Zero, Variant::Kai) }
	#[test] fn azure_jp() -> Ret { ed7("azure/data/scena", Game::Azure, Variant::Kai) }
	#[test] fn azure_en() -> Ret { ed7("azure/data/scena_us", Game::Azure, Variant::Kai) }

	#[test] fn zero_evo() -> Ret { ed7("zero-evo/data/scena", Game::Zero, Variant::Evo) }
	#[test] fn azure_evo() -> Ret { ed7("azure-evo/data/scena", Game::Azure, Variant::Evo) }
	#[test] fn zero_psp() -> Ret { ed7("psp/zero/PSP_GAME/USRDIR/data/scena", Game::Zero, Variant::Base) }
	#[test] fn azure_psp() -> Ret { ed7("psp/azure/PSP_GAME/USRDIR/data/scena", Game::Azure, Variant::Base) }
}
