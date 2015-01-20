#![feature(slicing_syntax)]
#![feature(box_syntax)]

extern crate getopts;
#[macro_use]
extern crate linked_list;

use std::io;
use std::mem;
use std::iter::repeat;

use lib::{
	SEle,
	Env,
	LamOrFn,
	ScmAlert,
	ScmAlertMode,
	scheme_alert,
	scheme_stdlib};
use lib::scm_macro::PrecompileEnv;

mod input;
mod lib;
mod parse;

fn to_strings(slc: &[&str]) -> Vec<String> {
	slc.iter().map(|s| s.to_string()).collect()
}

/// How many more closing parens there are than opening parens
fn close_paren_surplus(ts: &[parse::Token]) -> i32 {
	use parse::Token::{OpenParen, CloseParen};
	ts.iter().fold(0i32, |acc, t| match *t {
				OpenParen => acc-1,
				CloseParen => acc+1,
				_ => acc })
}

fn interactive_shell(env: &mut Env, macro_env: &mut PrecompileEnv) {
	print!("scheme-rs interactive shell. Copyright (C) 2014  Johan Johansson\n\
		This program is free software released under the GPLv3 license.\n\n\
		>> ");

	let mut tokenizer = parse::Tokenizer::new();
	let mut token_buf = Vec::new();
	let mut rparen_surplus = 0;
	let mut line_n = 1;
	for mut line in io::stdin().lock().lines().map(|l|
			tokenizer.tokenize_source(l.unwrap().as_slice()))
	{
		let tokens = if let Some((_, tokens)) = line.pop() {
				tokens
			} else { continue };
		rparen_surplus += close_paren_surplus(tokens.as_slice());
		if rparen_surplus < 0 {
			token_buf.push((line_n, tokens));
			print!(">> {}", repeat(' ').take((rparen_surplus * -2) as usize)
					.collect::<String>());
			line_n += 1;
		} else if rparen_surplus > 0 {
			scheme_alert(ScmAlert::Unexp(")"), &ScmAlertMode::Warn);
			token_buf.clear();
			line_n = 1;
			rparen_surplus = 0;
			print!(">> ");
		} else {
			token_buf.push((line_n, tokens));
			{
				let parsed_and_expanded = parse::parse_token_lines(
					mem::replace(&mut token_buf, Vec::with_capacity(3)))
						.into_iter().map(|e| macro_env.expand(e)).collect();
				print!("{}\n>> ", env.eval_sequence(parsed_and_expanded));
			}
			line_n = 1;
			rparen_surplus = 0;
		}
	}
}

fn main(){
	let (internal_std_procs, internal_std_vars) = scheme_stdlib::standard_library();
	let mut vars = Vec::with_capacity(internal_std_vars.len());
	for (name, val) in internal_std_vars.into_iter() {
		vars.push((name.to_string(), Some(val)));
	}
	for (name, func) in internal_std_procs.into_iter() {
		vars.push((name.to_string(), Some(SEle::SProc(box LamOrFn::Fn(func)))));
	}

	let mut tokenizer = parse::Tokenizer::new();
	let mut macro_env = PrecompileEnv::new();
	let mut env = Env::new(vars);

	let stdlib_src = input::read_stdlib();

	let tokenized = tokenizer.tokenize_source(stdlib_src.as_slice());
	let parsed = parse::parse_token_lines(tokenized);

	env.eval_sequence(parsed.into_iter().map(|e| macro_env.expand(e)).collect());

	if let Some(input) = input::get_input() {
		let begin_wrapped = format!("(begin {})", input);
		let tokenized = tokenizer.tokenize_source(begin_wrapped.as_slice());
		// debug
		// for &(n, ref t) in tokenized.iter() {
		// 	println!("{}: {:?}", n, t);
		// }
		let mut parsed = parse::parse_token_lines(tokenized);
		if parsed.len() != 1 {
			panic!("Parsed source is invalid")
		}
		env.eval(macro_env.expand(parsed.pop_head().unwrap()));
	} else {
		env.make_lenient();
		interactive_shell(&mut env, &mut macro_env);
	}
}