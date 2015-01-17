use std::iter::repeat;

use super::{
	unit,
	List,
	SEle};
use super::SEle::*;

#[derive(Clone, PartialEq)]
pub struct Transformer {
	keywords: Vec<String>,
	// Clauses of (pattern, template)
	clauses: Vec<(List<SEle>, SEle)>,
}

impl Transformer {
	pub fn new(keywords: Vec<String>, clauses: Vec<(List<SEle>, SEle)>) -> Transformer {
		Transformer{keywords: keywords, clauses: clauses}
	}
}

pub type FnMacro = fn(&mut PrecompileEnv, List<SEle>) -> SEle;

#[derive(Clone)]
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
		for &(ref binding, ref macro_) in self.macro_stack.iter().rev() {
			if *binding == to_get {
				return Some(macro_.clone());
			}
		}
		None
	}

	/* macro example:
	(define-syntax multiply
	  (with)
	  ((_ x with y) (* x y)))
	*/
	fn define_syntax(&mut self, name: String, maybe_keywords: List<SEle>, clauses: List<SEle>) {
		let keywords = maybe_keywords.into_iter().map(|ele| {
			if let SEle::SBinding(keyword) = ele { keyword }
			else { panic!("`{:?}` is not a valid keyword.", ele) }}).collect();
		let mut ret_clauses = vec![];
		for clause in clauses.into_iter() {
			if let SExpr(mut rule) = clause {
				if rule.len() != 2 {
					panic!("Bad syntax rule. {:?} != 2. {:?}", rule.len(), rule)
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

	fn expand_expr(&mut self, mut expr: List<SEle>) -> SEle {
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
				} else if let Some(macro_) = self.get_macro(head_binding.as_slice())
				{
					match macro_ {
						Macro::Trans(ref transformer) => {
							let expanded = transform(List::with_body(
									SBinding(head_binding),
									expr), transformer);
							self.expand(expanded)
						},
						Macro::Fn(func) => (*func)(self, expr),
					}
				} else {
					let mut ret_expr = list![SBinding(head_binding)];
					for child in expr.into_iter() {
						ret_expr.push(self.expand(child));
					}
					SExpr(ret_expr)
				}
			} else {
				let mut ret_expr = list![expanded_head];
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
		let exp = match ele {
			SExpr(x) => {
				self.expand_expr(x)
			},
			_ => ele
		};
		exp
	}
}

/// Create variadic macro definitions for the elements in `elems_to_bind` based on the pattern
/// `pattern_ele`.
fn match_ellipsis(pattern_ele: &SEle, elems_to_bind: List<SEle>) -> Vec<(&str, SEle)> {
	if let &SBinding(ref binding) = pattern_ele {
		// (a ...) for (x y z) => a = (x y z)
		vec![(binding.as_slice(), SList(elems_to_bind))]
	} else if let &SExpr(ref pattern_list) = pattern_ele {
		println!("elippeles: {:?}", elems_to_bind);
		// if elems_to_bind.is_empty() {
		// 	return None;
		// }
		// ((a b) ...) for ((x y) (z α)) => a = (x z) & b = (y α)
		let mut bound_elems_lists = repeat(list![]).take(pattern_list.len())
			.collect::<Vec<_>>();
		for elem_list in elems_to_bind.into_iter() {
			for (elem, list) in elem_list.into_expr().into_iter().zip(
					bound_elems_lists.iter_mut())
			{
				list.push(elem);
			}
		}
		let approx_total_elems = bound_elems_lists.len() * pattern_list.len();
		bound_elems_lists.into_iter().zip(pattern_list.iter()).fold(
				Vec::with_capacity(approx_total_elems),
				|mut acc, (list, pattern_ele2)| {
					acc.extend(match_ellipsis(pattern_ele2, list).into_iter());
					acc })
	} else {
		panic!("{:?} `{:?}` can not be matched as variadic.",
			pattern_ele.variant(), pattern_ele)
	}
}

fn bind_transform_pattern<'a>(expr: List<SEle>, pattern: &'a List<SEle>, keywords: &[String])
	-> Option<Vec<(&'a str, SEle)>>
{
	if pattern.is_empty() {
		return Some(vec![]);
	} else if expr.is_empty()  { // TODO: .is_empty() for better performance
		return None;
	}
	let mut ret_stack = Vec::with_capacity(4);
	let (mut expr_it, mut pattern_it) = (expr.into_iter(), pattern.iter());
	while let (Some(elem_to_bind), Some(pattern_ele)) = (expr_it.next(), pattern_it.next()) {
		if if let Some(&SBinding(ref maybe_ellipsis)) = pattern_it.peek() {
				*maybe_ellipsis == "..." } else { false }
		{
			// If `pattern_ele` is followed by an ellipsis, it's variadic.
			ret_stack.extend(match_ellipsis(pattern_ele, List::with_body(elem_to_bind,
							expr_it.collect())).into_iter());
			return Some(ret_stack);
		} else if let &SBinding(ref binding) = pattern_ele {
			if *binding == "_" {
				continue;
			} else if keywords.contains(binding) {
				if if let SBinding(maybe_keyword) = elem_to_bind {
						maybe_keyword == *binding } else { false }
				{
					continue;
				} else {
					return None;
				}
			}
			ret_stack.push((binding.as_slice(), elem_to_bind));
		} else if let &SExpr(ref pattern_list) = pattern_ele {
			if let SExpr(sele_expr) = elem_to_bind {
				if let Some(macro_defs) =
					bind_transform_pattern(sele_expr, pattern_list, keywords)
				{
					ret_stack.extend(macro_defs.into_iter())
				} else { return None; }
			} else { return None; }
		} else {
			panic!("Invalid pattern.")
		}
	}
	Some(ret_stack)
}

// Try to apply the vars in `binding_stack` to the expression `template`
fn apply_template(template: SEle, binding_stack: &Vec<(&str, SEle)>) -> SEle {
	if let SBinding(this_binding) = template {
		for &(ref binding, ref bound) in binding_stack.iter() {
			if this_binding == *binding {
				return bound.clone();
			}
		}
		SBinding(this_binding)
	} else if let SExpr(templ_expr) = template {
		let mut applied = list![];
		let mut templ_expr_it = templ_expr.into_iter();
		while let Some(elem) = templ_expr_it.next() {
			if if let Some(&SBinding(ref maybe_ellipsis)) = templ_expr_it.peek() {
					*maybe_ellipsis == "..." } else { false }
			{
				// An element followed by an ellipsis in a template must be a
				// variadic binding.
				let try_expand = apply_template(elem, binding_stack);
				if let SList(to_expand) = try_expand {
					applied.extend(to_expand.into_iter().map(|x|
							apply_template(x, binding_stack)));
					templ_expr_it.next(); // Skip the ellipsis
					continue;
				} else {
					panic!("NOOOOO")
				}
			} else {
				applied.push(apply_template(elem, binding_stack));
			}
		}
		SExpr(applied)
	} else {
		template
	}
}

/// Transorm the expression according to pattern -> template of the transformer
fn transform(expr: List<SEle>, transformer: &Transformer) -> SEle {
	for &(ref pattern, ref template) in transformer.clauses.iter() {
		if let Some(&SBinding(ref last_ele)) = pattern.last_node().head() {
			if *last_ele != "..." && pattern.len() != expr.len() {
				continue;
			}
		} else {
			panic!("Pattern in macro is empty.");
		}
		if let Some(bound_patterns_stack) = bind_transform_pattern(expr.clone(), pattern,
				transformer.keywords.as_slice())
		{
			return apply_template(template.clone(), &bound_patterns_stack);
		}
	}
	panic!("The expression did not match any pattern in macro. `{:?}`", expr)
}

pub fn expand_macros(ele: SEle) -> SEle {
	let mut macro_env = PrecompileEnv{macro_stack: vec![]};
	macro_env.expand(ele)
}