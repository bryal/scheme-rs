#![feature(globs)]
extern crate getopts;

use std::collections::HashMap;
use std::mem::transmute;
use std::f64::consts::PI;
use std::fmt;
use SEle::*;

mod cmdline;

/// An element in an S-Expression list
#[deriving(Clone)]
#[allow(dead_code)]
enum SEle {
	SExpr(Vec<SEle>),
	SBinding(String),
	SNum(f64),
	SStr(String),
	SBool(bool),
	SPair(Option<Vec<SEle>>)
}

impl fmt::Show for SEle {
	fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
		match *self {
			SExpr(ref x) => {
				let l = x.len();
				write!(f, "(");
				for i in range(0, l-1) {
					write!(f, "{} ", x[i]);
				}
				write!(f, "{})", x[l-1])
			},
			SBinding(ref b) => write!(f, "{}", b),
			SNum(n) => write!(f, "{}", n),
			SStr(ref s) => write!(f, "{}", s),
			SBool(b) => write!(f, "{}", if b {"#t"} else {"#f"}),
			SPair(ref p) =>
				if let &Some(ref v) = p {
					let l = v.len();
					write!(f, "(");
					for i in range(0, l-1) {
						write!(f, "{} ", v[i]);
					}
					write!(f, "{})", v[l-1])
				} else {
					write!(f, "()")
				},
		}
	}
}

type List = Vec<SEle>;

type ScmFn = |&mut Env, List|:'a -> SEle;

struct Env<'a> {
	fn_defs: Vec<(String, ScmFn)>,
	var_defs: Vec<(String, SEle)>
}

fn unity() -> SEle {
	SPair(None)
}

fn call_fn(ssn: *mut Env, f: &str, args: List) -> SEle {
	let ssn_brw: &mut Env = unsafe{transmute(&mut *ssn)};
	let error_msg = format!("Procedure `{}` not defined", f);
	let to_call = ssn_brw.get_fn(f).expect(error_msg.as_slice());
	(*to_call)(unsafe{transmute(&mut *ssn)}, args)
}

impl<'a> Env<'a> {
	fn define_var(&mut self, var: (String, SEle)) {
		self.var_defs.push(var);
	}

	fn get_var(&mut self, to_get: &str) -> SEle {
		for vardef in self.var_defs.iter().rev() {
			let (binding, var) = vardef;
			if binding == to_get {
				return var;
			} else {
				continue;
			}
		}
		panic!("Variable `{}` is not defined", to_get)
	}

	fn pop_var_def(&mut self) -> (&str, SEle) {
		if let Some(var) = self.var_defs.pop() {
			var
		} else {
			panic!("Could not pop variable definition")
		}
	}

	fn pop_var_defs(&mut self, n: u32) {
		for _ in range(0, n) {
			self.pop_var_def();
		}
	}

	fn get_fn(&mut self, to_get: &str) -> &mut ScmFn {
		for fdef in self.fn_defs.iter().rev() {
			let (binding, f) = fdef;
			if binding == to_get {
				return f;
			} else {
				continue;
			}
		}
		panic!("Procedure `{}` is not defined", to_get)
	}

	fn define_fn(&mut self, head: SEle, body: SEle) -> bool {
		if let Some(fn_binding, locals) = self.dismantle_fdef_head(head) {
			let f = |ssn: &mut Env, mut ops: List| {
				// Bind and push supplied args to env
				for var in locals.iter().chain(ops.iter()) {
					self.define_var(var);
				}

			}
			self.fn_defs.insert(head, val)
		} else {
			false
		}
		
	}

	fn dismantle_fdef_head(&mut self, head: SEle) -> (String, Vec<&str>) {
		if let SExpr(expr) = head {
			if let SBinding(fn_bnd) = expr.remove(0).unwrap() {
				let var_defs = expr.into_iter().filter(|e|
						match e {
							SBinding(_) => true,
							_ => false
						}
					).map(|e| {
						let SBinding(var_bnd) = e;
						var_bnd.as_slice()
					}).enumerate()
					.map(|(i, e)| (e, i))
					.collect::<HashMap<&str, uint>>();
				(fn_bnd, var_defs)
			} else {
				panic!("`{}` is not a valid procedure binding", expr)
			}
		} else {
			panic!("`{}` is not a valid procedure head", x)
		}
	}

	fn eval(&mut self, mut expr: List) -> SEle {
		if let SBinding(s) = expr.remove(0).unwrap() {
			let ele = call_fn(self, &s, expr);

			if let SExpr(inner_expr) = ele {
				self.eval(inner_expr)
			} else {
				ele
			}
		} else {
			panic!("Not a procedure")
		}
	}

	fn get_var(&self, key: &String) -> SEle {
		if let Some(ele) = self.var_defs.get(key) {
			ele.clone()
		} else {
			panic!(format!("Variable `{}` not defined", key))
		}
	}

	fn value(&mut self, ele: SEle) -> SEle {
		match ele {
			SExpr(x) => self.eval(x),
			SBinding(x) => self.get_var(&x),
			_ => ele
		}
	}

	fn run_program(&mut self, top_lvl_exprs: List) {
		for expr in top_lvl_exprs.into_iter() {
			println!("\n___\n{}\n=", expr)
			println!("{}", self.value(expr));
		}
	}
}

fn split_source(s: &str) -> Vec<&str> {
	let mut ss = Vec::with_capacity(s.len() / 4);
	let mut start_pos = 0;
	let mut in_string = false;
	let mut in_escape = false;
	let mut in_space = true;
	for (i, c) in s.char_indices() {
		match c {
			' ' | '\n' => {
				if !in_string {
					if !in_space && start_pos != i{
						ss.push(s.slice(start_pos, i));
					}
					start_pos = i+1;
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
		match c {' ' | '\n' => in_space = true, _ => in_space = false}
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

fn parse_expression(unparsed: &[&str]) -> Vec<SEle> {
	let mut parsed = Vec::with_capacity(unparsed.len());
	let mut i = 0;
	while i < unparsed.len() {
		let current_s = unparsed[i];
		if current_s == "(" {
			let matching_paren = matching_paren(
					unparsed.slice_from(i+1)
				).expect("Unclosed delimiter");
			parsed.push(SExpr(parse_expression(
						unparsed.slice(i+1,
							i+1+matching_paren))));
			i = i+matching_paren+1;
		} else if let Some(f) = parse_number(current_s) {
			parsed.push(SNum(f));
		} else if let Some(s) = parse_str_literal(current_s) {
			parsed.push(SStr(s.to_string()));
		} else if let Some(b) = parse_bool(current_s) {
			parsed.push(SBool(b));
		} else if let Some(b) = parse_binding(current_s) {
			parsed.push(SBinding(b));
		}
		i += 1;
	}
	parsed
}

fn parse_source(src: String) -> List {
	let splitted = split_source(src.as_slice());
	parse_expression(splitted.as_slice())
}

fn scm_add_iter(env: &mut Env, sum: f64, mut operands: List) -> f64 {
	if let Some(expr) = operands.pop() {
		if let SNum(x) = env.value(expr) {
			scm_add_iter(env, sum + x, operands)
		} else {
			panic!("NaN")
		}
	} else {
		sum
	}
}

fn scm_sub_iter(env: &mut Env, diff: f64, mut operands: List) -> f64 {
	if let Some(expr) = operands.pop() {
		if let SNum(x) = env.value(expr) {
			scm_add_iter(env, diff - x, operands)
		} else {
			panic!("NaN")
		}
	} else {
		diff
	}
}

fn scm_div_iter(env: &mut Env, numerator: f64, mut denoms: List) -> f64 {
	if let Some(expr) = denoms.pop() {
		if let SNum(x) = env.value(expr) {
			scm_add_iter(env, numerator / x, denoms)
		} else {
			panic!("NaN")
		}
	} else {
		numerator
	}
}

fn main(){
	// Scheme Standard library
	let std_procs = vec![
		("+", |ssn: &mut Env, ops: List|
			SNum(scm_add_iter(ssn, 0.0, ops))),
		("-", |ssn: &mut Env, mut ops: List|
			SNum(if let Some(expr) = ops.remove(0) {
				if let SNum(x) = ssn.value(expr) {
					scm_sub_iter(ssn, x, ops)
				} else {
					panic!("NaN")
				}
			} else {
				0.0
			})),
		("/", |ssn: &mut Env, mut ops: List|
			SNum(if let Some(expr) = ops.remove(0) {
				if let SNum(x) = ssn.value(expr) {
					if ops.len() < 1 {
						1.0 / x
					} else {
						scm_div_iter(ssn, x, ops)
					}
				} else {
					panic!("NaN")
				}
			} else {
				panic!("Wrong number of arguments")
			})),
		("define", |ssn: &mut Env, mut ops: List| {
			if ops.len() != 2 {
				panic!("Wrong number of arguments")
			}
			if let Some(ele) = ops.remove(0) {
				match ele {
					SBinding(b) => ssn.define_var(b,
						ops.remove(0).unwrap()),
					SExpr(expr) => env.define_fn(
						ops.remove(0).unwrap(),
						ops.remove(0).unwrap()),
					x => panic!("Invalid binding `{}`", x),
				}
			}
			unity()
				
		}),
	];
	let std_vars = vec![
		("PI", SNum(PI)),
	];
	let (mut procs, mut vars) = (HashMap::new(), HashMap::new());
	for (k,v) in std_procs.into_iter() {
		procs.insert(k.to_string(),v);
	}
	for (k,v) in std_vars.into_iter() {
		vars.insert(k.to_string(),v);
	}
	let mut env = Env{
		fn_defs: procs,
		var_defs: vars,
	};

	let input = cmdline::get_input();
	let parsed = parse_source(input);
	println!("{}\n", parsed);
	
	env.run_program(parsed);
}