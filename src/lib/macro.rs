use super::{
	unit,
	List,
	SEle};
use super::SEle::*;

#[deriving(Clone, PartialEq)]
pub struct Transformer {
	keywords: Vec<String>,
	// Clauses of (pattern, template)
	clauses: Vec<(List, SEle)>,
}

impl Transformer {
	pub fn new(keywords: Vec<String>, clauses: Vec<(List, SEle)>) -> Transformer {
		Transformer{keywords: keywords, clauses: clauses}
	}
}

pub type FnMacro = fn(&mut PrecompileEnv, List) -> SEle;

#[deriving(Clone)]
pub enum Macro {
	Trans(Transformer),
	Fn(&'static FnMacro)
}

pub struct PrecompileEnv {
	macro_stack: Vec<(String, Macro)>,
}

impl PrecompileEnv {
	pub fn new() -> PrecompileEnv {
		PrecompileEnv{macro_stack: Vec::with_capacity(10)}
	}

	fn get_macro(&self, to_get: &str) -> Option<Macro> {
		for &(ref binding, ref macro) in self.macro_stack.iter().rev() {
			if *binding == to_get {
				return Some(macro.clone());
			}
		}
		None
	}

	/* macro example:
	(define-syntax multiply
	  (with)
	  ((_ x with y) (* x y)))
	*/
	fn define_syntax(&mut self, name: String, maybe_keywords: List, clauses: List) {
		let keywords = maybe_keywords.into_iter().map(|ele| {
			if let SEle::SBinding(keyword) = ele { keyword }
			else { panic!("`{}` is not a valid keyword.", ele) }}).collect();
		let mut ret_clauses = vec![];
		for clause in clauses.into_iter() {
			if let SExpr(mut rule) = clause {
				if rule.len() != 2 {
					panic!("Bad syntax rule. {} != 2. {}", rule.len(), rule)
				}
				if let (Some(SExpr(pattern)), Some(template)) =
					(rule.pop_head(), rule.pop_head())
				{
					ret_clauses.push((pattern, template));
				}
			}
		}
		self.macro_stack.push((name, Macro::Trans(Transformer::new(keywords,ret_clauses))));
	}

	fn expand_expr(&mut self, mut expr: List) -> SEle {
		if let Some(head) = expr.pop_head() {
			let expanded_head = self.expand(head);
			if let SBinding(head_binding) = expanded_head {
				if head_binding.as_slice() == "define-syntax" {
					if let (Some(SBinding(name)), Some(SExpr(keywords)),
							clauses) =
						(expr.pop_head(), expr.pop_head(), expr)
					{
						self.define_syntax(name, keywords, clauses);
						unit()
					} else {
						panic!("Invalid `define-syntax`");
					}
				} else if let Some(macro) = self.get_macro(head_binding.as_slice())
				{
					match macro {
						Macro::Trans(ref transformer) => {
							let expanded = transform(List::with_body(
									SBinding(head_binding),
									expr), transformer);
							self.expand(expanded)
						},
						Macro::Fn(func) => (*func)(self, expr),
					}
				} else {
					let mut ret_expr = List::with_head(SBinding(head_binding));
					for child in expr.into_iter() {
						ret_expr.push(self.expand(child));
					}
					SExpr(ret_expr)
				}
			} else {
				let mut ret_expr = List::with_head(expanded_head);
				for child in expr.into_iter() {
					ret_expr.push(self.expand(child));
				}
				SExpr(ret_expr)
			}
		} else {
			SExpr(expr)
		}
	}

	pub fn expand(&mut self, ele: SEle) -> SEle {
		match ele {
			SExpr(x) => {
				self.expand_expr(x)
			},
			_ => ele
		}
	}
}

fn bind_transform_pattern<'a>(expr: List, pattern: &'a List, keywords: &Vec<String>)
	-> Option<Vec<(&'a str, SEle)>>
{
	let mut ret_stack = Vec::with_capacity(4);
	if expr.len() != pattern.len() {
		return None;
	}
	for (sele, maybe_binding) in expr.into_iter().zip(pattern.iter()) {
		if let &SBinding(ref pattern_binding) = maybe_binding {
			if keywords.as_slice().contains(pattern_binding) {
				if let SBinding(maybe_keyword) = sele {
					if maybe_keyword == *pattern_binding {
						continue
					}
					else { return None }
				} else { return None }
			}
			if *pattern_binding != "_" {
				ret_stack.push((pattern_binding.as_slice(), sele));
			}
		} else {
			panic!("Invalid pattern.")
		}
	}
	Some(ret_stack)
}

fn apply_template(template: SEle, binding_stack: &Vec<(&str, SEle)>) -> SEle {
	match template {
		SBinding(this_binding) => {
				for &(ref binding, ref bound) in binding_stack.iter() {
					if this_binding == *binding {
						return bound.clone();
					}
				}
				SBinding(this_binding)
			},
		SExpr(expr) =>
			SExpr(expr.into_iter().map(|e| apply_template(e, binding_stack)).collect()),
		// TODO: Maybe more needs to be added here, not sure.
		_ => template,
	}
}

fn transform(expr: List, transformer: &Transformer) -> SEle {
	for &(ref pattern, ref template) in transformer.clauses.iter() {
		if let Some(bound_patterns_stack) =
			// NOTE: expr.clone() should not be needed here? compiler bugg maybe.
			bind_transform_pattern(expr.clone(), pattern, &transformer.keywords)
		{
			return apply_template(template.clone(), &bound_patterns_stack);
		}
	}
	panic!("The expression did not match any pattern in macro. `{}`", expr)
}

pub fn expand_macros(ele: SEle) -> SEle {
	let mut macro_env = PrecompileEnv{macro_stack: vec![]};
	macro_env.expand(ele)
}

// fn mac_quote(env: &mut Env, args: List) -> SEle {
// 	let mut acc = 0.0;
// 	for expr in args.into_iter() {
// 		let ele = env.eval(expr);
// 		if let SNum(x) = ele {
// 			acc += x;
// 		} else {
// 			scheme_alert(ScmAlert::NaN(ele), &env.error_mode);
// 		}
// 	}
// 	SNum(acc)
// }

// static MAC_QUOTE: &'static FnMacro = &(mac_quote as FnMacro);

// pub fn standard_macros() -> Vec<(&'static str, &'static FnMacro)> {
// 	// Scheme Standard macro library
// 	let std_macros = vec![
// 		("quote", MAC_QUOTE),
// 	];

// 	std_macros
// }