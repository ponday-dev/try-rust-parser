use std::collections::HashMap;

use crate::{choice, join, regex};

use super::parser::*;

#[derive(Debug, Clone, PartialEq)]
pub enum Value {
    Null,
    False,
    True,
    Number(f64),
    String(String),
    Array(Vec<Value>),
    Object(HashMap<String, Value>),
}

fn lstring<'a>(target: &'a str) -> impl Parser<()> + 'a {
    lexeme(string(target))
}

fn lcharacter(c: char) -> impl Parser<()> {
    lexeme(character(c))
}

fn null(s: &str) -> Option<(Value, &str)> {
    let p = lstring("null");
    let p = map(p, |_| Value::Null);
    p(s)
}

fn true_(s: &str) -> Option<(Value, &str)> {
    let p = lstring("true");
    let p = map(p, |_| Value::True);
    p(s)
}

fn false_(s: &str) -> Option<(Value, &str)> {
    let p = lstring("false");
    let p = map(p, |_| Value::False);
    p(s)
}

fn number(s: &str) -> Option<(Value, &str)> {
    const PATTERN: &str = r"^-?(0|[1-9][0-9]*)(\.[0-9]+)?([eE][+-]?[0-9]+)?";
    let p = regex!(PATTERN, |s| s.parse::<f64>().ok());
    let p = lexeme(p);
    let p = map(p, |x| Value::Number(x));
    p(s)
}

fn hex_code(code: &str) -> Option<char> {
    code.strip_prefix(r"\u").and_then(|hex| {
        u32::from_str_radix(hex, 16)
            .ok()
            .and_then(|cp| char::from_u32(cp))
    })
}

fn escape(s: &str) -> Option<char> {
    match s {
        "\\\"" => Some('"'),
        "\\\\" => Some('\\'),
        "\\/" => Some('/'),
        "\\b" => Some('\x08'),
        "\\f" => Some('\x0C'),
        "\\n" => Some('\n'),
        "\\r" => Some('\r'),
        "\\t" => Some('\t'),
        _ => None,
    }
}

fn json_character(s: &str) -> Option<(char, &str)> {
    choice![
        regex!(r#"^[^"\\[:cntrl:]]"#, |s| s.chars().next()),
        regex!(r#"^\\u[0-9a-fA-F]{4}"#, hex_code),
        regex!(r#"^\\."#, escape)
    ](s)
}

fn json_string_raw(s: &str) -> Option<(String, &str)> {
    let p = crate::join![character('"'), many(json_character), character('"')];
    let p = lexeme(p);
    let p = map(p, |((_, chars), _)| chars.into_iter().collect());
    p(s)
}

fn json_string(s: &str) -> Option<(Value, &str)> {
    map(json_string_raw, Value::String)(s)
}

fn array(s: &str) -> Option<(Value, &str)> {
    let p = join![
        lcharacter('['),
        separated(json_value, lcharacter(',')),
        lcharacter(']')
    ];
    let p = map(p, |((_, values), _)| Value::Array(values));
    p(s)
}

fn object(s: &str) -> Option<(Value, &str)> {
    let p = join![
        lcharacter('{'),
        separated(key_value, lcharacter(',')),
        lcharacter('}')
    ];
    let p = map(p, |((_, key_values), _)| {
        let h = HashMap::from_iter(key_values.into_iter());
        Value::Object(h)
    });
    p(s)
}

fn key_value(s: &str) -> Option<((String, Value), &str)> {
    let p = join![json_string_raw, lcharacter(':'), json_value];
    let p = map(p, |((key, _), value)| (key, value));
    p(s)
}

fn json_value(s: &str) -> Option<(Value, &str)> {
    choice![null, false_, true_, number, json_string, array, object](s)
}

pub fn parse(s: &str) -> Option<Value> {
    json_value(s).and_then(|(value, rest)| {
        if rest.chars().all(|c| c.is_ascii_whitespace()) {
            Some(value)
        } else {
            None
        }
    })
}
