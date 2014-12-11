use getopts::{optopt,optflag,getopts,OptGroup,usage};
use std::os;
use std::io::File;

fn print_usage(program: &str, opts: &[OptGroup]) {
	println!("{}", usage(format!("Usage: {} <S-EXPR | -f FILE> [options]",
			program).as_slice(),
		opts));
}

pub fn get_input() -> String {
	let args: Vec<String> = os::args();

	let program = args[0].clone();
	let opts = [
		optopt("f", "", "read lisp from a file", "FILE"),
		optflag("h", "help", "print this help menu")
	];
	let matches = match getopts(args.tail(), &opts) {
		Ok(m) => m,
		Err(f) => { panic!(f.to_string()) }
	};
	if matches.opt_present("h") {
		print_usage(program.as_slice(), &opts);
		panic!();
	}
	let input_lisp = match matches.opt_str("f") {
		Some(x) => String::from_utf8(
			File::open(&Path::new(x)).read_to_end().ok().unwrap()
			).ok().unwrap(),
		None => matches.free[0].clone()
	};
	input_lisp
}