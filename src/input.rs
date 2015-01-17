#![allow(unstable)]

use getopts::{optopt,optflag,getopts,OptGroup,usage};
use std::os;
use std::io::File;

fn print_usage(program: &str, opts: &[OptGroup]) {
	let usage = usage(format!("Usage: {} [FILE | -s S-EXPR] [options]\n \
			Leave empty for interactive shell", program).as_slice(), opts);
	println!("{}", usage);
}

pub fn read_stdlib() -> String {
	let file_bytes = File::open(&Path::new("stdlib.scmrs")).read_to_end().unwrap();
	String::from_utf8(file_bytes).unwrap()
}

/// Get source code to parse and execute from commandline, file, or if no text supplied:
/// return `None` to signal for interactive shell
pub fn get_input() -> Option<String> {
	let args: Vec<String> = os::args();

	let program = args[0].clone();
	let opts = [
		optopt("s", "", "read lisp from supplied string", "S-EXPR"),
		optflag("h", "help", "print this help menu")
	];
	let matches = match getopts(args.tail(), &opts) {
		Ok(m) => m,
		Err(f) => { panic!("{}", f) }
	};
	if matches.opt_present("h") {
		print_usage(program.as_slice(), &opts);
		panic!();
	}
	let source = match matches.opt_str("s") {
		Some(s) => Some(s),
		None if matches.free.len() == 1 => {
			let file_path = &Path::new(matches.free[0].clone());
			let file_bytes = File::open(file_path).read_to_end().unwrap();
			Some(String::from_utf8(file_bytes).unwrap())
		},
		_ => None
	};
	source
}