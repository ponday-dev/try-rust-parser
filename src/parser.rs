use regex::Regex;

pub trait Parser<T>: Fn(&str) -> Option<(T, &str)> {}
impl<T, F> Parser<T> for F where F: Fn(&str) -> Option<(T, &str)> {}

pub fn digits(s: &str) -> Option<(i64, &str)> {
    let end = s.find(|c: char| !c.is_ascii_digit()).unwrap_or(s.len());
    match s[..end].parse() {
        Ok(value) => Some((value, &s[end..])),
        Err(_) => None,
    }
}

fn generalize_lifetime<T, F>(f: F) -> F
where
    F: Fn(&str) -> Option<(T, &str)>,
{
    f
}

pub fn character(c: char) -> impl Parser<()> {
    generalize_lifetime(move |s| {
        let mut chars = s.chars();
        if chars.next() == Some(c) {
            Some(((), chars.as_str()))
        } else {
            None
        }
    })
}

pub fn lexeme<T>(parser: impl Parser<T>) -> impl Parser<T> {
    generalize_lifetime(move |s: &str| parser(s.trim_start()))
}

pub fn string<'a>(target: &'a str) -> impl Parser<()> + 'a {
    generalize_lifetime(move |s: &str| s.strip_prefix(target).map(|rest| ((), rest)))
}

pub fn map<A, B>(parser: impl Parser<A>, f: impl Fn(A) -> B) -> impl Parser<B> {
    generalize_lifetime(move |s| parser(s).map(|(value, rest)| (f(value), rest)))
}

pub fn choice<T>(parser1: impl Parser<T>, parser2: impl Parser<T>) -> impl Parser<T> {
    generalize_lifetime(move |s| parser1(s).or_else(|| parser2(s)))
}

#[macro_export]
macro_rules! choice {
    ($parser0:expr, $($parser:expr),*) => {{
        let p = $parser0;
        $(
            let p = $crate::parser::choice(p, $parser);
        )*
        p
    }};
}

pub fn join<A, B>(parser1: impl Parser<A>, parser2: impl Parser<B>) -> impl Parser<(A, B)> {
    generalize_lifetime(move |s| {
        parser1(s).and_then(|(value1, rest1)| {
            parser2(rest1).map(|(value2, rest2)| ((value1, value2), rest2))
        })
    })
}

#[macro_export]
macro_rules! join {
    ($parser0:expr, $($parser:expr),*) => {{
        let p = $parser0;
        $(
            let p = $crate::parser::join(p, $parser);
        )*
        p
    }};
}

pub fn many<T>(parser: impl Parser<T>) -> impl Parser<Vec<T>> {
    generalize_lifetime(move |s| {
        let mut t = s;
        let mut ret = vec![];
        while let Some((value, rest)) = parser(t) {
            ret.push(value);
            t = rest;
        }
        Some((ret, t))
    })
}

pub fn separated<T>(parser: impl Parser<T>, separator: impl Parser<()>) -> impl Parser<Vec<T>> {
    generalize_lifetime(move |s| {
        let mut t = s;
        let mut ret = vec![];

        match parser(t) {
            Some((value, rest)) => {
                ret.push(value);
                t = rest;
            }
            None => return Some((ret, t)),
        }

        while let Some((_, rest)) = separator(t) {
            t = rest;
            match parser(t) {
                Some((value, rest)) => {
                    ret.push(value);
                    t = rest;
                }
                None => return Some((ret, t)),
            }
        }

        Some((ret, t))
    })
}

pub fn regex<'a, T>(re: &'a Regex, f: impl Fn(&str) -> Option<T> + 'a) -> impl Parser<T> + 'a {
    generalize_lifetime(move |s| {
        re.find(s).and_then(|matched| {
            f(matched.as_str()).map(|value| {
                let rest = &s[matched.end()..];
                (value, rest)
            })
        })
    })
}

#[macro_export]
macro_rules! regex {
    ($pattern:expr, $f:expr) => {{
        use once_cell::sync::Lazy;
        use regex::Regex;
        static RE: Lazy<Regex> = Lazy::new(|| Regex::new($pattern).unwrap());
        $crate::parser::regex(&RE, $f)
    }};
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_digits() {
        assert_eq!(digits("123*456"), Some((123, "*456")));
        assert_eq!(digits("ABC*DEF"), None);
    }

    #[test]
    fn test_character() {
        assert_eq!(character('A')("ABC"), Some(((), "BC")));
        assert_eq!(character('A')("123"), None);
    }

    #[test]
    fn test_lexeme() {
        assert_eq!(lexeme(digits)("  1A2"), Some((1, "A2")));
    }

    #[test]
    fn test_string() {
        let parser = string("hello");
        assert_eq!(parser("hello world"), Some(((), " world")));
        assert_eq!(parser("helo world"), None);
    }

    #[test]
    fn test_map() {
        let parser = map(digits, |x| x + 1);
        assert_eq!(parser("1"), Some((2, "")));
        assert_eq!(parser("X"), None);
    }

    #[test]
    fn test_choice() {
        let parser = choice(digits, map(string("null"), |_| 0));
        assert_eq!(parser("1234"), Some((1234, "")));
        assert_eq!(parser("null"), Some((0, "")));
        assert_eq!(parser("foo"), None);
    }

    #[test]
    fn test_choice_macro() {
        let parser = choice![
            map(string("zero"), |_| 0),
            map(string("one"), |_| 1),
            digits
        ];

        assert_eq!(parser("zero"), Some((0, "")));
        assert_eq!(parser("one"), Some((1, "")));
        assert_eq!(parser("42"), Some((42, "")));
        assert_eq!(parser("foo"), None);
    }

    #[test]
    fn test_join() {
        let plus_minus = choice(map(character('+'), |_| '+'), map(character('-'), |_| '-'));
        let parser = join(plus_minus, digits);

        assert_eq!(parser("+123"), Some((('+', 123), "")));
        assert_eq!(parser("-123"), Some((('-', 123), "")));
        assert_eq!(parser("-abc"), None);
        assert_eq!(parser("*123"), None);
    }

    #[test]
    fn test_join_macro() {
        let parser = join![lexeme(digits), lexeme(digits), lexeme(digits)];
        assert_eq!(parser("10 20 30"), Some((((10, 20), 30), "")));
        assert_eq!(parser("10 20 AA"), None);
    }

    #[test]
    fn test_many() {
        let parser = many(lexeme(digits));
        assert_eq!(parser("10 20 30"), Some((vec![10, 20, 30], "")));
        assert_eq!(parser(""), Some((vec![], "")));
        assert_eq!(parser("10 hello"), Some((vec![10], " hello")));
    }

    #[test]
    fn test_separated() {
        let parser = separated(digits, character(','));
        assert_eq!(parser("1,2,3"), Some((vec![1, 2, 3], "")));
        assert_eq!(parser(""), Some((vec![], "")));
    }

    #[test]
    fn test_regex() {
        let re = Regex::new(r"^[a-zA-Z_][a-zA-Z0-9_]*").unwrap();
        let parser = regex(&re, |s| Some(s.to_owned()));
        assert_eq!(parser("x_1=0"), Some(("x_1".to_owned(), "=0")));
        assert_eq!(parser("0_1=-"), None);
    }

    #[test]
    fn test_regex_macro() {
        let parser = regex!(r"^[a-zA-Z_][a-zA-Z0-9_]*", |s| Some(s.to_owned()));
        assert_eq!(parser("x_1=0"), Some(("x_1".to_owned(), "=0")));
        assert_eq!(parser("0_1=-"), None);
    }
}
