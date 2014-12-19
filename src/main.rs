#![feature(globs)]
extern crate getopts;

use std::mem::transmute;
use std::f64::consts::PI;
use std::f64::NAN;
use std::fmt;
use std::io;
use std::mem;
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
	WrongType(&'a str, &'a str),
	ArityMiss(&'a str, uint, &'a str, uint),
	Bad(&'a str),
	Undef(&'a str),
	NaN(SEle),
	Unclosed,
}

fn scheme_alert(alert: SA, a_type: &SchemeAlertType) {
	match alert {
		SA::Unexp(s) => println!("; {}: Unexpected `{}`", a_type, s),
		SA::WrongType(exp, got) => println!("; {}: Wrong type. Expected `{}`, found `{}`",
				a_type, exp, got),
		SA::Undef(s) => println!("; {}: Undefined variable `{}`", a_type, s),
		SA::Bad(s) => println!("; {}: Bad `{}`", a_type, s),
		SA::NaN(e) => println!("; {}: `{}: {}` is not a number", a_type, e.variant(), e),
		SA::Unclosed => println!("; {}: Unclosed delimiter", a_type),
		SA::ArityMiss(s, got, vs, exp) => println!("; {}: Arity missmatch in `{}`, {} {} {}",
				a_type, s, got, vs, exp),
	}
	if let &SAT::Error = a_type {
		panic!()
	}
}

struct MoveListItems {
	list: List
}

impl Iterator<SEle> for MoveListItems {
	fn next(&mut self) -> Option<SEle> {
		self.list.pop_head()
	}
}

impl MoveListItems {
	fn peek(&self) -> Option<&SEle> {
		self.list.head()
	}
}

#[deriving(Clone, PartialEq)]
enum List {
	Cons(Box<SEle>, Box<List>),
	Nil
}

impl fmt::Show for List {
	fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
		match *self {
			List::Cons(ref ele, ref link) => write!(f, "{} {}", ele, link),
			List::Nil => write!(f, "")
		}
	}
}

impl List {
	fn new() -> List {
		List::Nil
	}

	fn with_body(head: SEle, tail: List) -> List {
		List::Cons(box head, box tail)
	}

	fn with_head(head: SEle) -> List {
		List::Cons(box head, box List::Nil)
	}

	fn from_vec(vals: Vec<SEle>) -> List {
		let mut list = List::new();
		for val in vals.into_iter() {
			list.push(val);
		}
		list
	}

	fn len(&self) -> uint {
		if self == &List::Nil {
			0
		} else {
			let mut len = 0;
			let mut current = self;
			while let Some(l) = current.tail() {
				current = l;
				len += 1;
			}
			len
		}
	}

	fn into_iter(self) -> MoveListItems {
		MoveListItems{list: self}
	}

	fn swap_val(&mut self, val: &mut SEle) -> Result<(), ()> {
		if let &List::Cons(box ref mut self_val, _) = self {
			mem::swap(val, self_val);
			Ok(())
		} else {
			Err(())
		}
	}

	fn swap_link(&mut self, link: &mut List) -> Result<(), ()> {
		if let &List::Cons(_, box ref mut self_link) = self {
			mem::swap(link, self_link);
			Ok(())
		} else {
			Err(())
		}
	}

	/// Returns a reference the first value in the list.
	fn head(&self) -> Option<&SEle> {
		if let &List::Cons(box ref val, _) = self {
			Some(val)
		} else {
			None
		}
	}

	fn head_mut(&mut self) -> Option<&mut SEle> {
		if let &List::Cons(box ref mut val, _) = self {
			Some(val)
		} else {
			None
		}
	}

	/// Returns a reference a list containing all but the first value.
	fn tail(&self) -> Option<&List> {
		if let &List::Cons(_, box ref list) = self {
			Some(list)
		} else {
			None
		}
	}

	fn tail_mut(&mut self) -> Option<&mut List> {
		if let &List::Cons(_, box ref mut list) = self {
			Some(list)
		} else {
			None
		}
	}

	/// Pops the first value from the list. `self` is now its old tail.
	fn pop_head(&mut self) -> Option<SEle> {
		if let Some(mut cdr) = self.pop_tail() {
			mem::swap(&mut cdr, self);
			if let List::Cons(box val, _) = cdr {
				Some(val)
			} else {
				None
			}
		} else {
			None
		}
	}

	/// Pops the tail from the list. `self` is now only the first entry.
	fn pop_tail(&mut self) -> Option<List> {
		let mut cdr = List::Nil;
		if let &List::Cons(_, _) = self {
			if let Err(_) = self.swap_link(&mut cdr) {
				return None;
			}
			Some(cdr)
		} else {
			None
		}
	}

	/// Pushes an element to the end of the list.
	fn push(&mut self, ele: SEle) {
		match self {
			&List::Cons(_, box List::Nil) => {
				let mut to_swap = List::with_head(ele);
				self.swap_link(&mut to_swap).unwrap();
			},
			&List::Cons(_, box ref mut self_link) => self_link.push(ele),
			_ => {
				let mut new_self = List::Cons(box ele, box List::Nil);
				mem::swap(self, &mut new_self);
			}
		}
	}

	/// Inserts an element at the beginning of the list.
	fn prepush(&mut self, ele: SEle) {
		let mut local_self = List::Nil;
		mem::swap(self, &mut local_self);
		let mut new_self = List::Cons(box ele, box local_self);
		mem::swap(self, &mut new_self);
	}
}

impl FromIterator<SEle> for List {
	fn from_iter<T: Iterator<SEle>>(mut iterator: T) -> List {
		let mut list = List::new();
		for element in iterator {
			list.push(element);
		}
		list
	}
}

impl Index<uint, SEle> for List {
	fn index(&self, index: &uint) -> &SEle {
		let mut current = self;
		for _ in range(0, *index) {
			if let Some(tail) = current.tail() {
				current = tail;
			} else {
				panic!("List index out of range");
			}
		}
		if let Some(val) = current.head() {
			val
		} else {
			panic!("List index out of range")
		}
	}
}

/// An element in an S-Expression list
#[deriving(Clone, PartialEq)]
#[allow(dead_code)]
enum SEle {
	SExpr(List),
	SBinding(String),
	SNum(f64),
	SStr(String),
	SSymbol(String),
	SBool(bool),
	SList(List)
	// TODO: Add procedure type. This is what lambda will return
}

impl SEle {
	fn variant(&self) -> &'static str {
		match self {
			&SExpr(_) => "Expression",
			&SBinding(_) => "Binding",
			&SNum(_) => "Number",
			&SStr(_) => "String",
			&SSymbol(_) => "Symbol",
			&SBool(_) => "Bool",
			&SList(_) => "List"
		}
	}
}

impl fmt::Show for SEle {
	fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
		match *self {
			SExpr(ref xs) | SList(ref xs) => write!(f, "({})", xs),
			SBinding(ref b) => write!(f, "{}", b),
			SNum(n) => write!(f, "{}", n),
			SStr(ref s) => write!(f, "{}", s),
			SSymbol(ref s) => write!(f, "'{}", s),
			SBool(b) => write!(f, "{}", if b {"#t"} else {"#f"}),
		}
	}
}

fn unit() -> SEle {
	SList(List::Nil)
}

fn split_source_text(src: &str) -> Vec<&str> {
	let mut ss = Vec::with_capacity(src.len() / 4);
	let mut start_pos = 0;
	let (mut in_string, mut in_escape, mut in_comment) = (false,false,false);
	let mut in_space = true;
	let mut char_iter = src.char_indices();
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
						ss.push(src.slice(start_pos, i));
					}
					start_pos = i+1;
				}
				if in_escape { in_escape = false; }
			},
			'\r' | '\n' => {
				if !in_string {
					if !in_space && start_pos != i{
						ss.push(src.slice(start_pos, i));
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
					ss.push(src.slice(start_pos, i+1));
					start_pos = i+1;
				} else {
					start_pos = i;
				}
				in_string = !in_string;
			},
			'\\' => if !in_escape { in_escape = true; },
			'(' | ')' if !in_string && !in_escape => {
				if !in_space && start_pos != i {
					ss.push(src.slice(start_pos, i));
				}
				ss.push(src.slice(i, i+1));
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
	if s == "#t" {
		Some(true)
	} else if s == "#f" {
		Some(false)
	} else {
		None
	}
}

fn parse_expressions(unparsed: &[&str]) -> List {
	let mut parsed = List::new();
	let mut i = 0;
	while i < unparsed.len() {
		let current_s = unparsed[i];
		if current_s == "(" {
			if let Some(matching_paren) = matching_paren(unparsed.slice_from(i+1)) {
				let exprs_to_parse = unparsed.slice(i+1, i+1+matching_paren);
				parsed.push(SExpr(parse_expressions(exprs_to_parse)));
				i = i+matching_paren+1;
			} else {
				scheme_alert(SA::Unclosed, &SAT::Error);
			}
		} else if current_s == "'" && i+1 < unparsed.len() && unparsed[i+1] == "(" {
			i += 1;
			if let Some(matching_paren) = matching_paren(unparsed.slice_from(i+1)) {
				let list_item_to_parse = unparsed.slice(i+1, i+1+matching_paren);
				let list = SExpr(parse_expressions(list_item_to_parse));
				// TODO: add macro to create Lists similar to vec![]
				let quoted = SExpr(List::from_vec(vec![SBinding("quote".to_string()),
							list]));
				parsed.push(quoted);
				i = i+matching_paren+1;
			} else {
				scheme_alert(SA::Unclosed, &SAT::Error);
			}
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
	var_defs: Vec<(String, SEle)>,
	// When the `error_mode` is set to `Warn`, all errors are reported as warnings.
	// Useful when run in shell mode and stuff like undefined vars should not crash the session.
	error_mode: SAT
}

impl<'a> Env<'a> {
	fn new_strict(fn_defs: Vec<FnDef<'a>>, var_defs: Vec<(String, SEle)>) -> Env<'a> {
		Env {fn_defs: fn_defs, var_defs: var_defs, error_mode: SAT::Error }
	}

	#[allow(dead_code)]
	fn new_lenient(fn_defs: Vec<FnDef<'a>>, var_defs: Vec<(String, SEle)>) -> Env<'a> {
		Env {fn_defs: fn_defs, var_defs: var_defs, error_mode: SAT::Warn }
	}

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
		scheme_alert(SA::Undef(to_get), &self.error_mode);
		unit()
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
	fn dismantle_fn_head(&mut self, head: SEle) -> (String, Vec<String>) {
		if let SExpr(mut expr) = head {
			if let SBinding(fn_bnd) = expr.pop_head().unwrap() {
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
		let (fn_binding, arg_names) = self.dismantle_fn_head(head);

		let defined_f: ScmFn<'a> = |env, arg_names_opt, ops, body| {
			let evaled_args = env.eval_args(ops);
			let arg_names = arg_names_opt.unwrap();
			let n_args = arg_names.len();
			if n_args != evaled_args.len() {
				scheme_alert(SA::ArityMiss("_", n_args,"!=",evaled_args.len()),
					&env.error_mode);
				return unit();
			}
			// Bind and push supplied args to environment
			for (k, v) in arg_names.clone().into_iter().zip(evaled_args.into_iter()) {
				env.define_var(k, v);
			}
			let result = env.elem_value(body.unwrap());
			env.pop_var_defs(n_args);
			result
		};

		self.fn_defs.push(FnDef::new_full(fn_binding, arg_names, defined_f, body))
	}

	// TODO: Fix to work with empty lists by remedying the `unwrap`
	// TODO: Fix to work with procedures as first element, like the result of a lambda
	/// Evaluate the Scheme expression, assuming `expr` is a list of the
	/// expression body starting with a procedure binding
	fn eval_expr(&mut self, mut expr: List) -> SEle {
		match expr.pop_head().unwrap() {
			SBinding(s) => {
				let ele = call_proc(self, s.as_slice(), expr);
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
		if exprs != List::Nil {
			let mut it = exprs.into_iter();
			while let Some(expr) = it.next() {
				if it.peek() == None {
					return self.elem_value(expr);
				}
				self.elem_value(expr);
			}
		}
		unit()
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

	fn eval_args(&mut self, exprs: List) -> List {
		exprs.into_iter().map(|e| self.elem_value(e)).collect()
	}

	fn elem_is_true(&mut self, ele: SEle) -> bool {
		match ele {
			SBool(b) => b,
			SExpr(_) => {
				let val = self.elem_value(ele);
				self.elem_is_true(val)
			},
			_ => false
		}
	}
}

fn scm_add_iter(env: &mut Env, sum: f64, mut terms: List) -> f64 {
	if let Some(expr) = terms.pop_head() {
		match env.elem_value(expr) {
			SNum(x) => scm_add_iter(env, sum + x, terms),
			x => {
				scheme_alert(SA::NaN(x), &env.error_mode);
				NAN
			}
		}
	} else {
		sum
	}
}

fn scm_sub_iter(env: &mut Env, diff: f64, mut terms: List) -> f64 {
	if let Some(expr) = terms.pop_head() {
		match env.elem_value(expr) {
			SNum(x) => scm_sub_iter(env, diff - x, terms),
			x => {
				scheme_alert(SA::NaN(x), &env.error_mode);
				NAN
			}
		}
	} else {
		diff
	}
}

fn scm_div_iter(env: &mut Env, numerator: f64, mut denoms: List) -> f64 {
	if let Some(expr) = denoms.pop_head() {
		match env.elem_value(expr) {
			SNum(x) => scm_div_iter(env, numerator / x, denoms),
			x => {
				scheme_alert(SA::NaN(x), &env.error_mode);
				NAN
			}
		}
	} else {
		numerator
	}
}

fn call_proc(env: *mut Env, f_name: &str, args: List) -> SEle {
	let env_brw: &mut Env = unsafe{transmute(&mut *env)};
	let fndef = env_brw.get_fn(f_name);
	(fndef.closure)(unsafe{transmute(&mut *env)}, fndef.arg_names.clone(), args,
		fndef.body.clone())
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
				scheme_alert(SA::Unexp(")"), &SAT::Warn);
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

fn elems_wrap_begin(mut elems: List) -> SEle {
	SExpr(List::with_body(SBinding("begin".to_string()), elems))
}

fn main(){
	// Scheme Standard library
	// TODO: any other way than using len to measure num of args? Unefficient with List
	let std_procs: Vec<(&str, ScmFn)> = vec![
		("+", |env: &mut Env, _: Option<Vec<String>>, args, _: Option<SEle>|
			SNum(scm_add_iter(env, 0.0, args)
		)),
		("-", |env: &mut Env, _: Option<Vec<String>>, mut args, _: Option<SEle>|
			SNum(if let Some(expr) = args.pop_head() {
				if let SNum(x) = env.elem_value(expr) {
					scm_sub_iter(env, x, args)
				} else {
					panic!("NaN")
				}
			} else {
				0.0
			}
		)),
		("/", |env: &mut Env, _: Option<Vec<String>>, mut args, _: Option<SEle>|
			SNum(if let Some(expr) = args.pop_head() {
				if let SNum(x) = env.elem_value(expr) {
					if args.len() < 1 {
						1.0 / x
					} else {
						scm_div_iter(env, x, args)
					}
				} else {
					panic!("NaN")
				}
			} else {
				panic!("Wrong number of arguments")
			}
		)),
		("remainder", |env: &mut Env, _: Option<Vec<String>>, mut args, _: Option<SEle>|
			if args.len() != 2 {
				scheme_alert(SA::ArityMiss("remainder", args.len(),"!=",2),
					&env.error_mode);
				unit()
			} else {
				let e1 = env.elem_value(args.pop_head().unwrap());
				let e2 = env.elem_value(args.pop_head().unwrap());
				match (e1, e2) {
					(SNum(n1), SNum(n2)) => SNum(n1 % n2),
					(t1, t2) => {
						scheme_alert(SA::WrongType("Number, Number",
								format!("{}, {}", t1.variant(),
									t2.variant()).as_slice()),
							&env.error_mode);
						unit()
					}
				}
			}
		),
		("define", |env: &mut Env, _: Option<Vec<String>>, mut args, _: Option<SEle>|
			if args.len() < 2 {
				scheme_alert(SA::ArityMiss("define", args.len(),"<",2),
					&env.error_mode);
				unit()
			} else {
				match args.pop_head().unwrap() {
					SBinding(b) =>
						if args.len() != 1 {
							scheme_alert(SA::ArityMiss("define",
								args.len(),"<",2), &env.error_mode);
						} else {
							env.define_var(b, args.pop_head().unwrap());
						},
					SExpr(expr) => env.define_fn(SExpr(expr), elems_wrap_begin(
								args)),
					_ => scheme_alert(SA::Bad("define body"), &env.error_mode),
				}
				unit()
			}		
		),

		("display", |env: &mut Env, _: Option<Vec<String>>, mut args, _: Option<SEle>| {
			if args.len() != 1 {
				panic!("Arity missmatch")
			}
			println!("{}", env.elem_value(args.pop_head().expect(
				"Could not retrieve argument")));
			unit()
		}),
		("begin", |env: &mut Env, _: Option<Vec<String>>, args, _: Option<SEle>| {
			env.eval_block(args)
		}),
		("eqv?", |env: &mut Env, _: Option<Vec<String>>, mut args, _: Option<SEle>|
			if args.len() == 0 || args.len() == 1 {
				SBool(true)
			} else {
				let e1 = args.pop_head().unwrap();
				let v1 = env.elem_value(e1);
				for e2 in args.into_iter() {
					if v1 != env.elem_value(e2) {
						return SBool(false);
					}
				}
				SBool(true)
			}
		),
		("number?", |env: &mut Env, _: Option<Vec<String>>, mut args, _: Option<SEle>|
			if args.len() != 1 {
				scheme_alert(SA::ArityMiss("numbers?", args.len(),"!=",1),
					&env.error_mode);
				unit()
			} else {
				if let SNum(_) = env.elem_value(args.pop_head().unwrap()) {
					SBool(true)
				} else {
					SBool(false)
				}
			}
		),
		("=", |env: &mut Env, _: Option<Vec<String>>, mut args, _: Option<SEle>|
			// TODO: use create macro to artificially construct scheme expression
			if args.clone().into_iter().all(|e| {
					let expr = SExpr(List::from_vec(vec![
						SBinding("number?".to_string()), e]));
					env.elem_is_true(expr)
			})
			{
				// TODO: use List.prepush()?
				args = List::with_body(SBinding("eqv?".to_string()), args);
				SBool(env.elem_is_true(SExpr(args)))
			} else {
				scheme_alert(SA::WrongType("Number", "_"), &env.error_mode);
				unit()
			}
		),
		("cond", |env: &mut Env, _: Option<Vec<String>>, args, _: Option<SEle>| {
			if args.len() < 2 {
				scheme_alert(SA::ArityMiss("cond", args.len(),"<",2), &env.error_mode);
				return unit();
			}
			for clause in args.into_iter() {
				if let SExpr(mut clause_v) = clause {
					if clause_v.len() != 2 {
						scheme_alert(SA::Bad("cond clause"), &env.error_mode);
						return unit();
					}
					let test = clause_v.pop_head().unwrap();
					if let SBinding(maybe_else) = test {
						if maybe_else.as_slice() == "else" {
							return clause_v.pop_head().unwrap();
						}
					} else {
						let test_val = env.elem_value(test);
						if match test_val { SBool(b) => b, _ => true } {
							return env.elem_value(clause_v.pop_head()
									.unwrap());
						}
					}
				} else {
					scheme_alert(SA::Bad("cond clause"), &env.error_mode);
					return unit();
				}
			}
			unit()			
		}),
		("if", |env: &mut Env, _: Option<Vec<String>>, mut args, _: Option<SEle>|
			if args.len() != 3  {
				scheme_alert(SA::ArityMiss("if", args.len(),"!=",3), &env.error_mode);
				unit()
			} else {
				let condition = args.pop_head().unwrap();
				let consequense = args.pop_head().unwrap();
				let alternative = args.pop_head().unwrap();
				let scm_else = SBinding("else".to_string());
				let cond = SExpr(List::from_vec(vec![
					SBinding("cond".to_string()),
					SExpr(List::from_vec(vec![condition, consequense])),
					SExpr(List::from_vec(vec![scm_else, alternative]))
				]));
				env.elem_value(cond)
			}		
		),

		("var-stack", |env: &mut Env, _: Option<Vec<String>>, _, _: Option<SEle>| {
			println!("var stack: {}", env.var_defs);
			unit()
		}),
		("proc-stack", |env: &mut Env, _: Option<Vec<String>>, _, _: Option<SEle>| {
			println!("proc stack: {}", env.fn_defs);
			unit()
		}),
		("newline", |_: &mut Env, _: Option<Vec<String>>, _, _: Option<SEle>| {
			println!("");
			unit()
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
	let mut env = Env::new_strict(procs, vars);

	let maybe_input = cmdline::get_input();
	if let Some(input) = maybe_input {
		let begin_wrapped = format!("(begin {})", input);
		let mut parsed = parse_source_text(begin_wrapped);
		if parsed.len() != 1 {
			panic!("Parsed source is invalid")
		}
		env.elem_value(parsed.pop_head().unwrap());
	} else {
		interactive_shell(&mut env);
	}
}