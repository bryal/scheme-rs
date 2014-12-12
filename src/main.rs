#![feature(globs)]
extern crate getopts;

use std::mem::transmute;
use std::f64::consts::PI;
use std::fmt;
use std::io;
use SEle::*;
use SchemeAlertType as SAT;
use SchemeAlert as SA;
mod cmdline;


enum SchemeAlertType {
	Warn,
	Error
}

impl fmt::Show for SchemeAlertType {
	fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
		match *self {
			SAT::Warn => write!(f, "Warning"),
			SAT::Error => write!(f, "Error")
		}
	}
}

enum SchemeAlert<'a> {
	Unexp(&'a str),
	Undef(&'a str),
}

fn scheme_alert(alert: SA, a_type: SchemeAlertType) {
	match alert {
		SA::Unexp(s) => println!("; {}: Unexpected `{}`", a_type, s),
	}
	if let SAT::Error = a_type {
		panic!()
	}
}

/// An element in an S-Expression list
#[deriving(Clone)]
#[allow(dead_code)]
enum SEle {
	SExpr(Vec<SEle>),
	SBinding(String),
	SNum(f64),
	SStr(String),
	SSymbol(String),
	SBool(bool),
	SPair(Option<Vec<SEle>>)
}

impl fmt::Show for SEle {
	fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
		match *self {
			SExpr(ref x) => {
				let l = x.len();
				(write!(f, "(")).ok();
				for i in range(0, l-1) {
					(write!(f, "{} ", x[i])).ok();
				}
				write!(f, "{})", x[l-1])
			},
			SBinding(ref b) => write!(f, "{}", b),
			SNum(n) => write!(f, "{}", n),
			SStr(ref s) => write!(f, "{}", s),
			SSymbol(ref s) => write!(f, "'{}", s),
			SBool(b) => write!(f, "{}", if b {"#t"} else {"#f"}),
			SPair(ref p) =>
				if let &Some(ref v) = p {
					let l = v.len();
					(write!(f, "(")).ok();
					for i in range(0, l-1) {
						(write!(f, "{} ", v[i])).ok();
					}
					write!(f, "{})", v[l-1])
				} else {
					write!(f, "()")
				},
		}
	}
}

fn unity() -> SEle {
	SPair(None)
}

type List = Vec<SEle>;

fn split_source_text(s: &str) -> Vec<&str> {
	let mut ss = Vec::with_capacity(s.len() / 4);
	let mut start_pos = 0;
	let (mut in_string, mut in_escape, mut in_comment) = (false,false,false);
	let mut in_space = true;
	let mut char_iter = s.char_indices();
	loop {
		// Manual iteration in case we have to skip an iteration sometime
		let (i, c) = match char_iter.next() {
			Some(p) => p,
			None => break
		};

		// First pass: Check for comment
		match c {
			'\n' => in_comment = false,
			';' if !in_string && !in_escape => in_comment = true,
			_ => ()
		}
		if in_comment {
			start_pos = i+1;
			continue
		}
		// Second pass: General splitting
		match c {
			' ' => {
				if !in_string {
					if !in_space && start_pos != i{
						ss.push(s.slice(start_pos, i));
					}
					start_pos = i+1;
				}
				if in_escape { in_escape = false; }
			},
			'\r' | '\n' => {
				if !in_string {
					if !in_space && start_pos != i{
						ss.push(s.slice(start_pos, i));
					}
					// Handle \r\n line endings in case of windows
					if c == '\r' {
						start_pos = i + 2;
						char_iter.next();
					} else {
						start_pos = i + 1;
					}
				}
				if in_escape { in_escape = false; }
			},
			'"' if !in_escape => {
				if in_string {
					ss.push(s.slice(start_pos, i+1));
					start_pos = i+1;
				} else {
					start_pos = i;
				}
				in_string = !in_string;
			},
			'\\' => if !in_escape { in_escape = true; },
			'(' | ')' if !in_string && !in_escape => {
				if !in_space && start_pos != i {
					ss.push(s.slice(start_pos, i));
				}
				ss.push(s.slice(i, i+1));
				start_pos = i+1;
			},
			_ => (),
		}
		// Third pass: Check for space
		match c {
			' ' | '\n' => in_space = true,
			_ => in_space = false
		}
	}
	ss
}

/// Index the matching closing parenthesis in `v` for the opening parenthesis
/// preceding the slice
fn matching_paren(v: &[&str]) -> Option<uint> {
	let mut unclosed_parens: i32 = 0;
	for (i,s) in v.iter().enumerate() {
		if *s == ")" {
			if unclosed_parens == 0 {
				return Some(i);
			}
			unclosed_parens -= 1;
		} else if *s == "(" {
			unclosed_parens += 1;
		}
	}
	None
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

fn parse_binding(s: &str) -> Option<String> {
	Some(s.to_string())
}

fn parse_number(s: &str) -> Option<f64> {
	from_str(s)
}

fn parse_str_literal(s: &str) -> Option<&str> {
	if s.starts_with("\"") && s.ends_with("\"") {
		Some(s.slice(1, s.len()-1))
	} else {
		None
	}
}

fn parse_symbol(s: &str) -> Option<&str> {
	if s.starts_with("'") {
		Some(s.slice_from(1))
	} else {
		None
	}
}

fn parse_bool(s: &str) -> Option<bool> {
	if s.starts_with("#") {
		if s.char_at(1) == 't' {
			Some(true)
		} else if s.char_at(1) == 'f' {
			Some(false)
		} else {
			None
		}
	} else {
		None
	}
}

fn parse_expressions(unparsed: &[&str]) -> List {
	let mut parsed = Vec::with_capacity(unparsed.len());
	let mut i = 0;
	while i < unparsed.len() {
		let current_s = unparsed[i];
		if current_s == "(" {
			let matching_paren = matching_paren(
					unparsed.slice_from(i+1)
				).expect("Unclosed delimiter");
			parsed.push(SExpr(parse_expressions(
						unparsed.slice(i+1,
							i+1+matching_paren))));
			i = i+matching_paren+1;
		} else if let Some(f) = parse_number(current_s) {
			parsed.push(SNum(f));
		} else if let Some(s) = parse_str_literal(current_s) {
			parsed.push(SStr(s.to_string()));
		} else if let Some(s) = parse_symbol(current_s) {
			parsed.push(SSymbol(s.to_string()));
		} else if let Some(b) = parse_bool(current_s) {
			parsed.push(SBool(b));
		} else if let Some(b) = parse_binding(current_s) {
			parsed.push(SBinding(b));
		}
		i += 1;
	}
	parsed
}

fn parse_source_text(src: String) -> List {
	let splitted = split_source_text(src.as_slice());
	parse_expressions(splitted.as_slice())
}

fn into_begin(s: String) -> String {
	format!("(begin {})", s)
}

fn scm_add_iter(env: &mut Env, sum: f64, mut operands: List) -> f64 {
	if let Some(expr) = operands.pop() {
		match env.elem_value(expr) {
			SNum(x) => scm_add_iter(env, sum + x, operands),
			x => panic!("`{}` is NaN", x)
		}
	} else {
		sum
	}
}

// Arguments: Environment, Argument names, Argument values, Body to evaluate
type ScmFn<'a> = |&mut Env, Option<Vec<String>>, List, Option<SEle>|:'a -> SEle;

// TODO: Make use of unboxed closures instead of passing around `arg_names` and `body`
struct FnDef<'a> {
	name: String,
	arg_names: Option<Vec<String>>,
	closure: ScmFn<'a>,
	body: Option<SEle>
}

impl<'a> fmt::Show for FnDef<'a> {
	fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
		write!(f, "{}", self.name)
	}
}

impl<'a> FnDef<'a> {
	fn new_basic(name: String, cls: ScmFn<'a>) -> FnDef<'a> {
		FnDef{name: name, arg_names: None, closure: cls, body: None}
	}

	fn new_full(name: String, args: Vec<String>, cls: ScmFn<'a>, body: SEle) -> FnDef<'a> {
		FnDef{name: name, arg_names: Some(args), closure: cls, body: Some(body)}
	}
}

struct Env<'a> {
	// Name, Argument names, Closure
	fn_defs: Vec<FnDef<'a>>,
	// Name, Value
	var_defs: Vec<(String, SEle)>
}

fn call_fn(env: *mut Env, f_name: &str, args: List) -> SEle {
	let env_brw: &mut Env = unsafe{transmute(&mut *env)};
	let fndef = env_brw.get_fn(f_name);
	(fndef.closure)(unsafe{transmute(&mut *env)}, fndef.arg_names.clone(), args, fndef.body.clone())
}

impl<'a> Env<'a> {
	fn define_var(&mut self, binding: String, val: SEle) {
		let val = self.elem_value(val);
		self.var_defs.push((binding, val));
	}

	fn get_var(&mut self, to_get: &str) -> SEle {
		for vardef in self.var_defs.iter().rev() {
			let &(ref binding, ref var) = vardef;
			if binding.as_slice() == to_get {
				return var.clone();
			} else {
				continue;
			}
		}
		scheme_alert("Variable `{}` is not defined", to_get)
	}

	fn pop_var_def(&mut self) -> (String, SEle) {
		if let Some(var) = self.var_defs.pop() {
			var
		} else {
			panic!("Could not pop variable definition")
		}
	}

	fn pop_var_defs(&mut self, n: uint) {
		for _ in range(0, n) {
			self.pop_var_def();
		}
	}

	fn pop_fn_def(&mut self) -> FnDef {
		if let Some(f) = self.fn_defs.pop() {
			f
		} else {
			panic!("Could not pop fn definition")
		}
	}

	fn pop_fn_defs(&mut self, n: uint) {
		for _ in range(0, n) {
			self.pop_fn_def();
		}
	}

	// Get the closure, argument names, and expression body for a procedure
	fn get_fn(&mut self, f_name: &str) -> &'a mut FnDef {
		for fndef in self.fn_defs.iter_mut().rev() {
			if fndef.name.as_slice() == f_name {
				return fndef;
			} else {
				continue;
			}
		}
		panic!("Procedure `{}` is not defined", f_name)
	}

	// Extract the procedure name and argument names for the `define` header `head`
	fn dismantle_f_head(&mut self, head: SEle) -> (String, Vec<String>) {
		if let SExpr(mut expr) = head {
			if let SBinding(fn_bnd) = expr.remove(0).unwrap() {
				let var_names = expr.into_iter().map(|e|
						if let SBinding(var_bnd) = e {
							var_bnd
						} else {
							panic!("`{}` is not a binding name", e)
						}
					).collect();
				(fn_bnd, var_names)
			} else {
				panic!("`{}` is not a valid procedure binding", expr)
			}
		} else {
			panic!("`{}` is not a valid procedure head", head)
		}
	}

	fn define_fn(&mut self, head: SEle, body: SEle) {
		let (fn_binding, arg_names) = self.dismantle_f_head(head);
		let defined_f: ScmFn<'a> = |env, arg_names_opt, ops, body| {
			let arg_names = arg_names_opt.unwrap();
			let n_args = arg_names.len();
			if n_args != ops.len() {
				panic!("Wrong number of arguments supplied");
			}
			// Bind and push supplied args to environment
			for (k, v) in arg_names.clone().into_iter().zip(ops.into_iter()) {
				env.define_var(k, v);
			}
			let result = env.elem_value(body.unwrap());
			env.pop_var_defs(n_args);
			result
		};
		self.fn_defs.push(FnDef::new_full(fn_binding, arg_names, defined_f, body))
	}

	/// Evaluate the Scheme expression, assuming `expr` is a list of the
	/// expression body starting with a procedure binding
	fn eval_expr(&mut self, mut expr: List) -> SEle {
		match expr.remove(0).unwrap() {
			SBinding(s) => {
				let ele = call_fn(self, s.as_slice(), expr);
				if let SExpr(inner_expr) = ele {
					self.eval_expr(inner_expr)
				} else {
					ele
				}
			},
			x => panic!("`{}` is not a procedure", x)
		}
	}

	// Get the value of the element. If it's an expression, get the evaluation. If it's a
	// binding, get the associated value. If it's a value, return it.
	fn elem_value(&mut self, ele: SEle) -> SEle {
		match ele {
			SExpr(x) => self.eval_expr(x),
			SBinding(x) => self.get_var(x.as_slice()),
			_ => ele
		}
	}

	fn eval_multiple(&mut self, exprs: List) -> SEle {
		let mut it = exprs.into_iter();
		if it.len() == 0 {
			return unity();
		}
		for expr in range(0, it.len()-1).map(|_| it.next()) {
			self.elem_value(expr.unwrap());
		}
		self.elem_value(it.next().expect("Failed to retrieve last expression"))
	}

	// This is run when there are multiple statements in a single expression body, like in a
	// `begin`. Only in these situations can variables and procedures be `define`d, wherefore
	// this method keeps track of variables and procedures in scope.
	fn eval_block(&mut self, exprs: List) -> SEle {
		let (previous_vars, previous_fns) = (self.var_defs.len(), self.fn_defs.len());

		let result = self.eval_multiple(exprs);

		let (current_vars, current_fns) = (self.var_defs.len(), self.fn_defs.len());
		self.pop_var_defs(current_vars - previous_vars);
		self.pop_fn_defs(current_fns - previous_fns);

		result
	}
}

fn scm_sub_iter(env: &mut Env, diff: f64, mut operands: List) -> f64 {
	if let Some(expr) = operands.pop() {
		match env.elem_value(expr) {
			SNum(x) => scm_sub_iter(env, diff - x, operands),
			x => panic!("`{}` is NaN", x)
		}
	} else {
		diff
	}
}

fn scm_div_iter(env: &mut Env, numerator: f64, mut denoms: List) -> f64 {
	if let Some(expr) = denoms.pop() {
		match env.elem_value(expr) {
			SNum(x) => scm_div_iter(env, numerator / x, denoms),
			x => panic!("`{}` is NaN", x)
		}
	} else {
		numerator
	}
}

fn to_strings(slc: &[&str]) -> Vec<String> {
	slc.iter().map(|s| s.to_string()).collect()
}

fn to_strs(slc: &[String]) -> Vec<&str> {
	slc.iter().map(|s| s.as_slice()).collect()
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
				scheme_alert(SA::Unexp(")"), SAT::Warn);
				token_buf.clear();
				rparen_surplus = 0;
				print!(">> ");
			},
			_ => {
				token_buf.push_all(to_strings(splitted.as_slice()).as_slice());
				let parsed = parse_expressions(to_strs(token_buf.as_slice()).as_slice());
				print!("{}\n>> ", env.eval_multiple(parsed));
				token_buf.clear();
				rparen_surplus = 0;
			}
		}
	}
}

fn main(){
	// Scheme Standard library
	let std_procs: Vec<(&str, ScmFn)> = vec![
		("+", |env: &mut Env, _: Option<Vec<String>>, ops, _: Option<SEle>|
			SNum(scm_add_iter(env, 0.0, ops)
		)),
		("-", |env: &mut Env, _: Option<Vec<String>>, mut ops, _: Option<SEle>|
			SNum(if let Some(expr) = ops.remove(0) {
				if let SNum(x) = env.elem_value(expr) {
					scm_sub_iter(env, x, ops)
				} else {
					panic!("NaN")
				}
			} else {
				0.0
			}
		)),
		("/", |env: &mut Env, _: Option<Vec<String>>, mut ops, _: Option<SEle>|
			SNum(if let Some(expr) = ops.remove(0) {
				if let SNum(x) = env.elem_value(expr) {
					if ops.len() < 1 {
						1.0 / x
					} else {
						scm_div_iter(env, x, ops)
					}
				} else {
					panic!("NaN")
				}
			} else {
				panic!("Wrong number of arguments")
			}
		)),
		("define", |env: &mut Env, _: Option<Vec<String>>, mut ops, _: Option<SEle>| {
			if ops.len() != 2 {
				panic!("Wrong number of arguments")
			}
			match ops.remove(0).expect("Could not retrieve binding") {
				SBinding(b) => env.define_var(b, ops.pop().unwrap()),
				SExpr(expr) => env.define_fn(SExpr(expr), ops.pop().unwrap()),
				x => panic!("Invalid binding `{}`", x),
			}
			unity()
				
		}),
		("display", |env: &mut Env, _: Option<Vec<String>>, mut ops, _: Option<SEle>| {
			if ops.len() != 1 {
				panic!("Arity missmatch")
			}
			println!("{}", env.elem_value(ops.pop().expect(
				"Could not retrieve argument")));
			unity()
		}),
		("begin", |env: &mut Env, _: Option<Vec<String>>, ops, _: Option<SEle>| {
			env.eval_block(ops)
		}),
	];
	let std_vars = vec![
		("PI", SNum(PI)),
	];
	let mut procs = Vec::with_capacity(std_procs.len());
	let mut vars = Vec::with_capacity(std_vars.len());
	for (name, cls) in std_procs.into_iter() {
		procs.push(FnDef::new_basic(name.to_string(), cls));
	}
	for (k, v) in std_vars.into_iter() {
		vars.push((k.to_string(), v));
	}
	let mut env = Env{
		fn_defs: procs,
		var_defs: vars,
	};

	let maybe_input = cmdline::get_input();
	if let Some(input) = maybe_input {
		let mut parsed = parse_source_text(into_begin(input));
		println!("{}\n", parsed);
		if parsed.len() != 1 {
			panic!("Parsed source is invalid")
		}
		env.elem_value(parsed.pop().expect("Failed to retrieve expression"));
	} else {
		interactive_shell(&mut env);
	}
}