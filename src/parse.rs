use std::mem;
use std::fmt;

use lib::{
	List,
	SEle,
	scheme_alert};
use lib::SEle::*;
use lib::ScmAlert;
use lib::ScmAlertMode;

#[derive(PartialEq)]
pub enum Token {
	OpenParen,
	CloseParen,
	Quote,
	Octothorpe,
	StrLit(String),
	Ident(String), // Any non-delim char sequence not in a string
}
impl fmt::Show for Token {
	fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
		match *self {
			Token::OpenParen => write!(f, "("),
			Token::CloseParen => write!(f, ")"),
			Token::Quote => write!(f, "'"),
			Token::Octothorpe => write!(f, "#"),
			Token::StrLit(ref s) => write!(f, "\"{}\"", s),
			Token::Ident(ref s) => write!(f, "{}", s),
		}
	}
}

// TODO: Raw string literal. Maybe R"delim(raw text)delim"

static DELIMITERS: [char; 13] = [' ', '\t', ';', '"', '\'', '`', '|', '(', ')', '[', ']', '{', '}'];

fn iterator_travel_n<T: Iterator>(it: &mut T, n: usize) {
	for _ in (0..n) {
		if let None = it.next() {
			break;
		}
	}
}

fn char_is_number(c: char) -> bool {
	match c {
		'0'...'9' => true,
		_ => false
	}
}

fn escape_raw_str(s: &str) -> String {
	let mut v = Vec::with_capacity(s.len());
	for b in s.bytes() {
		let c = b as char;
		match c {
			'\t' | '\r' | '\n' | '"' | '\\' => for ch in c.escape_default() {
				v.push(ch as u8)
			},
			_ => v.push(b),
		}
	}
	String::from_utf8(v).unwrap()
}

fn str_lit_exit(s: &str) -> Option<usize> {
	s.find_str("\"")
}
fn raw_str_lit_exit(s: &str) -> Option<usize> {
	s.find_str(")_\"")
}
fn ident_exit(s: &str) -> usize {
	s.chars().take_while(|c| !DELIMITERS.contains(c)).count()
}

pub struct Tokenizer {
	in_str_lit: bool,
	in_raw_str_lit: bool,
	comp_str_start_line: usize,
	comp_str_lit: String,
}
impl Tokenizer {
	pub fn new() -> Tokenizer {
		Tokenizer{
			in_str_lit: false,
			in_raw_str_lit: false,
			comp_str_start_line: 0,
			comp_str_lit: String::with_capacity(6) }
	}

	fn get_rest_of_str_lit<'a>(&self, src_line: &'a str) -> (Option<usize>, &'a str) {
		let maybe_p = if self.in_str_lit {
				str_lit_exit(src_line)
			} else if self.in_raw_str_lit {
				raw_str_lit_exit(src_line)
			} else {
				panic!("Not in string literal");
			};
		if let Some(p) = maybe_p {
			(Some(p), &src_line[0..p])
		} else {
			(None, &src_line[0..])
		}
	}

	fn match_chars_to_tokens(&mut self, src_line: &str, mut tokens: Vec<Token>) -> Vec<Token> {
		let mut char_iter = src_line.char_indices();
		while let Some((i, c)) = char_iter.next() {
			match c {
				';' => break,
				' ' | '\t' => continue,
				'(' => tokens.push(Token::OpenParen),
				')' => tokens.push(Token::CloseParen),
				'\'' => tokens.push(Token::Quote),
				// '#' => tokens.push(Token::Octothorpe),
				'"' => match str_lit_exit(&src_line[i+1..]) {
					None => {
						self.comp_str_lit = src_line[i+1..].to_string();
						self.in_str_lit = true;
						break
					},
					Some(str_len) => {
						tokens.push(Token::StrLit(
							src_line[i+1..i+1+str_len].to_string()));
						iterator_travel_n(&mut char_iter, str_len+1);
					}
				},
				'R' if &src_line[i+1..i+4] == "\"_(" =>
					match raw_str_lit_exit(&src_line[i+4..])
				{
					None => {
						self.comp_str_lit = src_line[i+4..].to_string();
						self.in_raw_str_lit = true;
						break;
					},
					Some(str_len) => {
						tokens.push(Token::StrLit(escape_raw_str(
							&src_line[i+4..i+4+str_len])));
						iterator_travel_n(&mut char_iter, str_len+6);
					}
				},
				x => {
					let ident_len = ident_exit(&src_line[i..]);
					tokens.push(Token::Ident(
						src_line[i..i+ident_len].to_string()));
					iterator_travel_n(&mut char_iter, ident_len-1);
				}
			}
		}
		tokens
	}

	fn tokenize_line(&mut self, mut src_line: &str) -> Option<Vec<Token>> {
		let mut tokens = Vec::with_capacity(3);

		// Handle multi line string
		if self.in_str_lit || self.in_raw_str_lit {
			let (maybe_end, s) = self.get_rest_of_str_lit(src_line);
			self.comp_str_lit.push_str(s);
			if let Some(end_pos) = maybe_end {
				self.in_str_lit = false;
				self.in_raw_str_lit = false;
				let str_lit = mem::replace(&mut self.comp_str_lit,
					String::with_capacity(6));
				tokens.push(Token::StrLit(str_lit));
				let filler = if self.in_str_lit { 1 } else { 3 };
				src_line = src_line[end_pos+filler..].trim_left();
			} else {
				return None;
			}
		}

		let tokens = self.match_chars_to_tokens(src_line, tokens);
		if tokens.len() == 0 {
			None
		} else {
			Some(tokens)
		}
	}
	pub fn tokenize_source(&mut self, src: &str) -> Vec<(usize, Vec<Token>)> {
		let mut ret_lines = Vec::with_capacity(src.len() / 40);

		for (line_n, src_line) in src.lines()
			.enumerate()
			.map(|(n, l)| (n+1, l.trim()))
			.filter(|&(_, l)| !l.is_empty() && !l.starts_with(";"))
		{
			let maybe_tokenized = self.tokenize_line(src_line);
			if let Some(tokenized) = maybe_tokenized {
				ret_lines.push((line_n, tokenized))
			} else {
				continue
			}
		}

		ret_lines
	}
}

/// Index the matching closing parenthesis in `v` for the opening parenthesis
/// preceding the slice
fn matching_paren(ts: &[(usize, Token)]) -> Option<usize> {
	let mut unclosed_parens: i32 = 0;
	for (i, t) in ts.iter().enumerate() {
		match t.1 {
			Token::CloseParen => if unclosed_parens == 0 {
					return Some(i)
				} else {
					unclosed_parens -= 1
				},
			Token::OpenParen => unclosed_parens += 1,
			_ => ()
		}
	}
	None
}

fn parse_bool(s: &str) -> Option<bool> {
	match s {
		"#t" => Some(true),
		"#f" => Some(false),
		_ => None
	}
}
fn parse_number(s: &str) -> Option<f64> {
	s.replace("_", "").parse()
}
fn parse_binding(s: &str) -> Option<String> {
	Some(s.to_string())
}


/// Tokens are parsed to `SEle`'s so that they can be evaluated as expressions.
fn parse_tokens(unparsed: &[(usize, Token)]) -> List<SEle> {
	let mut parsed = list![];
	let mut i = 0;
	let unparsed_len = unparsed.len();
	while i < unparsed_len {
		// TODO: fix line numbering
		let (line_n, ref current_token) = unparsed[i];
		match *current_token {
			Token::OpenParen =>
				if let Some(mp) = matching_paren(&unparsed[i+1..]) {
					parsed.push(SExpr(parse_tokens(&unparsed[i+1..i+1+mp])));
					i+= mp+1;
				} else {
					scheme_alert(ScmAlert::Unclosed(line_n),
						&ScmAlertMode::Error);
				},
			Token::Quote if i+1 < unparsed_len && unparsed[i+1].1 == Token::OpenParen =>
				{
					i += 1;
					if let Some(mp) = matching_paren(&unparsed[i+1..]) {
						let expr = list![
							SBinding("quote".to_string()),
							SExpr(parse_tokens(
									&unparsed[i+1..i+1+mp]))];
						parsed.push(SExpr(expr));
						i += mp+1;
					} else {
						scheme_alert(ScmAlert::Unclosed(line_n),
							&ScmAlertMode::Error);
					}
				},
			Token::Quote => if let Token::Ident(ref s) = unparsed[i+1].1 {
					parsed.push(SSymbol(s.clone()));
					i += 1;
				},
			Token::StrLit(ref s) => {parsed.push(SStr(s.clone()));},
			Token::Ident(ref s) => if let Some(b) = parse_bool(s.as_slice()) {
					parsed.push(SBool(b));
				} else if let Some(f) = parse_number(s.as_slice()) {
					parsed.push(SNum(f));
				} else {
					parsed.push(SBinding(s.clone()));
				},
			_ => ()
		}
		i += 1;
	}
	parsed
}

pub fn parse_token_lines(unparsed: Vec<(usize, Vec<Token>)>) -> List<SEle> {
	let aprox_new_len = unparsed.len() * 5;
	let lined_tokens = unparsed.into_iter().fold(Vec::with_capacity(aprox_new_len),
		|mut acc, (line_n, tokens)| {
			acc.extend(tokens.into_iter().map(|t| (line_n, t)));
			acc });
	parse_tokens(lined_tokens.as_slice())
}