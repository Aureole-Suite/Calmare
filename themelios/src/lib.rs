#![feature(array_try_from_fn)]
#![feature(lazy_cell)]
#![feature(is_sorted)]
#![feature(decl_macro)]
#![feature(error_generic_member_access)]

pub use glam;

#[macro_use]
pub mod types;
pub mod scena;
mod util;

pub use themelios_gamedata as gamedata;

pub mod ed8;
