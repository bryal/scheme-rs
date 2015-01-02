#![feature(globs)]
#![feature(unboxed_closures)]
extern crate getopts;

use std::io;

use lib::{
	SEle,
	Env,
	LamOrFn,
	ScmAlert,
	ScmAlertMode,
	scheme_alert,
	scheme_stdlib};
use lib::macro::PrecompileEnv;
use parse::{
	split_source_text,
	parse_expressions};

mod input;
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

fn interactive_shell(env: &mut Env, macro_env: &mut PrecompileEnv) {
	print!("scheme-rs interactive shell. Copyright (C) 2014  Johan Johansson\n\
		This program is free software released under the GPLv3 license.\n\n\
		>> ");

	let mut token_buf = Vec::with_capacity(100);
	let mut rparen_surplus = 0;
	for line in io::stdin().lock().lines() {
		let line = line.unwrap();
		let splitted = split_source_text(line.as_slice().clone());
		rparen_surplus += right_paren_surplus(splitted.as_slice());
		if rparen_surplus < 0 {
			token_buf.push_all(to_strings(splitted.as_slice()).as_slice());
			print!(">> {}", String::from_char((rparen_surplus * -2) as uint, ' '))
		} else if rparen_surplus > 0 {
			scheme_alert(ScmAlert::Unexp(")"), &ScmAlertMode::Warn);
			token_buf.clear();
			rparen_surplus = 0;
			print!(">> ");
		} else {
			token_buf.push_all(to_strings(splitted.as_slice()).as_slice());
			{
				let strs: Vec<&str> = token_buf.iter().map(|s| s.as_slice())
					.collect();
				let parsed_and_expanded = parse_expressions(strs.as_slice())
					.into_iter().map(|e| macro_env.expand(e)).collect();
				print!("{}\n>> ", env.eval_sequence(parsed_and_expanded));
			}
			token_buf.clear();
			rparen_surplus = 0;
		}
	}
}

fn main(){
	let (internal_std_procs, internal_std_vars) = scheme_stdlib::standard_library();
	let mut vars = Vec::with_capacity(internal_std_vars.len());
	for (name, val) in internal_std_vars.into_iter() {
		vars.push((name.to_string(), val));
	}
	for (name, func) in internal_std_procs.into_iter() {
		vars.push((name.to_string(), SEle::SProc(box LamOrFn::Fn(func))));
	}

	let mut macro_env = PrecompileEnv::new();
	let mut env = Env::new(vars);

	let stdlib_src = input::read_stdlib();
	env.eval_sequence(parse_expressions(split_source_text(stdlib_src.as_slice()).as_slice())
		.into_iter().map(|e| macro_env.expand(e)).collect());

	if let Some(input) = input::get_input() {
		let begin_wrapped = format!("(begin {})", input);
		let mut parsed = parse_expressions(split_source_text(begin_wrapped.as_slice())
				.as_slice());
		if parsed.len() != 1 {
			panic!("Parsed source is invalid")
		}
		env.eval(macro_env.expand(parsed.pop_head().unwrap()));
	} else {
		env.make_lenient();
		interactive_shell(&mut env, &mut macro_env);
	}
}