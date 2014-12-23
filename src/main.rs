#![feature(globs)]
#![feature(unboxed_closures)]
extern crate getopts;

use std::io;

use lib::{
	Env,
	ProcOrFunc,
	ScmAlert,
	ScmAlertMode,
	scheme_alert,
	scheme_stdlib};
use lib::SEle::SProc;
use parse::{
	split_source_text,
	parse_expressions};

mod cmdline;
mod lib;
mod parse;

fn to_strings(slc: &[&str]) -> Vec<String> {
	slc.iter().map(|s| s.to_string()).collect()
}

/// How many more closing parens there are than opening parens
fn right_paren_surplus(v: &[&str]) -> i32 {
	let mut r_surplus: i32 = 0;
	for s in v.iter() {
		if *s == ")" {
			r_surplus += 1;
		} else if *s == "(" {
			r_surplus -= 1;
		}
	}
	r_surplus
}

fn interactive_shell(env: &mut Env) {
	print!("scheme-rs interactive shell. Copyright (C) 2014  Johan Johansson\n\
		This program is free software released under the GPLv3 license.\n\n\
		>> ");

	let mut token_buf = Vec::with_capacity(100);
	let mut rparen_surplus = 0;
	for line in io::stdin().lock().lines() {
		let line = line.unwrap();
		let splitted = split_source_text(line.as_slice().clone());
		rparen_surplus += right_paren_surplus(splitted.as_slice());
		match rparen_surplus {
			n if rparen_surplus < 0 => {
				token_buf.push_all(to_strings(splitted.as_slice()).as_slice());
				print!("\n>> {}", String::from_char((n * -2) as uint, ' '))
			},
			_ if rparen_surplus > 0 => {
				scheme_alert(ScmAlert::Unexp(")"), &ScmAlertMode::Warn);
				token_buf.clear();
				rparen_surplus = 0;
				print!(">> ");
			},
			_ => {
				token_buf.push_all(to_strings(splitted.as_slice()).as_slice());
				{
					let strs: Vec<&str> = token_buf.iter().map(|s| s.as_slice())
						.collect();
					print!("{}\n>> ", env.eval_sequence(parse_expressions(
								strs.as_slice())));
				}
				token_buf.clear();
				rparen_surplus = 0;
			}
		}
	}
}

fn main(){
	let (std_procs, std_vars) = scheme_stdlib::standard_library();
	let mut vars = Vec::with_capacity(std_vars.len());
	for (name, val) in std_vars.into_iter() {
		vars.push((name.to_string(), val));
	}
	for (name, func) in std_procs.into_iter() {
		vars.push((name.to_string(), SProc(box ProcOrFunc::Func(func))));
	}

	let maybe_input = cmdline::get_input();
	if let Some(input) = maybe_input {
		let begin_wrapped = format!("(begin {})", input);
		let mut parsed = parse_expressions(split_source_text(begin_wrapped.as_slice())
				.as_slice());
		if parsed.len() != 1 {
			panic!("Parsed source is invalid")
		}
		let mut env = Env::new_strict(vars);
		env.elem_value(parsed.pop_head().unwrap());
	} else {
		let mut env = Env::new_lenient(vars);
		interactive_shell(&mut env);
	}
}