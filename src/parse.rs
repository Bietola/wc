use lazy_static::lazy_static;
use regex::Regex;

/**********************/
/* Parser XML element */
/**********************/

#[derive(Clone, Debug, PartialEq, Eq, Default)]
pub struct Element {
    name: String,
    attributes: Vec<(String, String)>,
    children: Vec<Element>,
}

/***********************/
/* Generic parser type */
/***********************/
type ParseResult<'a, T> = Result<(&'a str, T), &'a str>;

pub trait Parser<'a, T> {
    fn parse(&self, input: &'a str) -> ParseResult<'a, T>;
}

impl<'a, F, T> Parser<'a, T> for F
where
    F: Fn(&'a str) -> ParseResult<T>,
{
    fn parse(&self, input: &'a str) -> ParseResult<'a, T> {
        self(input)
    }
}

/**********************/
/* Parser combinators */
/**********************/

pub fn literal<'a>(expected: &'static str) -> impl Parser<'a, ()> {
    move |input| {
        let re = Regex::new(&format!("^{}.*", expected)).unwrap();

        if re.is_match(input) {
            Ok((&input[expected.len()..], ()))
        } else {
            Err(input)
        }
    }
}

pub fn identifier(input: &str) -> ParseResult<String> {
    lazy_static! {
        static ref RE: Regex = Regex::new(r"^[a-zA-z][\w\d-]*").unwrap();
    }

    match RE.find(input) {
        Some(mtc) => {
            let mtcstr = mtc.as_str();
            Ok((&input[mtcstr.len()..], mtcstr.to_owned()))
        }
        None => Err(input),
    }
}

pub fn quoted_string(input: &str) -> ParseResult<String> {
    right(
        literal("\""),
        left(
            map(zero_or_more(pred(any_char, |c| *c != '\"')), |output| {
                output.into_iter().collect()
            }),
            literal("\""),
        ),
    )
    .parse(input)
}

pub fn pair<'a, P1, P2, R1, R2>(parser1: P1, parser2: P2) -> impl Parser<'a, (R1, R2)>
where
    P1: Parser<'a, R1>,
    P2: Parser<'a, R2>,
{
    move |input| match parser1.parse(input) {
        Ok((rest1, res1)) => match parser2.parse(rest1) {
            Ok((rest2, res2)) => Ok((rest2, (res1, res2))),
            Err(err) => Err(err),
        },
        Err(err) => Err(err),
    }
}

pub fn map<'a, P, F, A, B>(parser: P, fun: F) -> impl Parser<'a, B>
where
    F: Fn(A) -> B,
    P: Parser<'a, A>,
{
    move |input| match parser.parse(input) {
        Ok((rest, res)) => Ok((rest, fun(res))),
        Err(err) => Err(err),
    }
}

pub fn right<'a, P1, P2, R1, R2>(parser1: P1, parser2: P2) -> impl Parser<'a, R2>
where
    P1: Parser<'a, R1>,
    P2: Parser<'a, R2>,
{
    map(pair(parser1, parser2), |(_, rhs)| rhs)
}

pub fn left<'a, P1, P2, R1, R2>(parser1: P1, parser2: P2) -> impl Parser<'a, R1>
where
    P1: Parser<'a, R1>,
    P2: Parser<'a, R2>,
{
    map(pair(parser1, parser2), |(lhs, _)| lhs)
}

pub fn at_least<'a, P, A>(parser: P, required_num: usize) -> impl Parser<'a, Vec<A>>
where
    P: Parser<'a, A>,
{
    move |mut input| {
        let mut result = Vec::new();

        while let Ok((rest, res)) = parser.parse(input) {
            result.push(res);
            input = rest;
        }

        if result.len() >= required_num {
            Ok((input, result))
        } else {
            Err(input)
        }
    }
}

pub fn zero_or_more<'a, P, A>(parser: P) -> impl Parser<'a, Vec<A>>
where
    P: Parser<'a, A>,
{
    at_least(parser, 0)
}

pub fn one_or_more<'a, P, A>(parser: P) -> impl Parser<'a, Vec<A>>
where
    P: Parser<'a, A>,
{
    at_least(parser, 1)
}

pub fn pred<'a, P, A, F>(parser: P, the_pred: F) -> impl Parser<'a, A>
where
    P: Parser<'a, A>,
    F: Fn(&A) -> bool,
{
    move |input| match parser.parse(input) {
        Ok((rest, res)) => {
            if the_pred(&res) {
                Ok((rest, res))
            } else {
                Err(input)
            }
        }
        err @ Err(_) => err,
    }
}

pub fn any_char<'a>(input: &'a str) -> ParseResult<'a, char> {
    match input.chars().next() {
        Some(c) => Ok((&input[c.len_utf8()..], c)),
        None => Err(input),
    }
}

pub fn whitespace_char<'a>(input: &'a str) -> ParseResult<'a, char> {
    pred(any_char, |c| c.is_whitespace()).parse(input)
}

pub fn space1<'a>() -> impl Parser<'a, Vec<char>> {
    one_or_more(whitespace_char)
}

pub fn space0<'a>() -> impl Parser<'a, Vec<char>> {
    one_or_more(whitespace_char)
}

pub fn xml_ele<'a>(input: &'a str) -> ParseResult<'a, Element> {
    // Support parsers. Their name indicate what they parse
    let attr_pair = pair(identifier, right(literal("="), quoted_string));

    let attr_lst = zero_or_more(right(whitespace_char, attr_pair));

    let ele = right(literal("<"), pair(identifier, left(attr_lst, literal(">"))));

    map(ele, |(name, attributes)| Element {
        name,
        attributes,
        ..Default::default()
    })
    .parse(input)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn literal_parser() {
        let parse_joe = literal("Hello Joe!");
        assert_eq!(Ok(("", ())), parse_joe.parse("Hello Joe!"));
        assert_eq!(
            Ok((" Hello Robert!", ())),
            parse_joe.parse("Hello Joe! Hello Robert!")
        );
        assert_eq!(Err("Hello Mike!"), parse_joe.parse("Hello Mike!"));
    }

    #[test]
    fn identifier_parser() {
        assert_eq!(
            Ok((", identifier", "hello-there".into())),
            identifier("hello-there, identifier")
        );
    }

    #[test]
    fn pair_parser() {
        assert_eq!(
            pair(pair(literal("<"), identifier), literal(">")).parse("<hello-there>"),
            Ok(("", (((), "hello-there".into()), ()))),
        )
    }

    #[test]
    fn right_parser() {
        let tag = right(literal("<"), identifier);

        assert_eq!(tag.parse("<hello-there"), Ok(("", "hello-there".into())),)
    }

    #[test]
    fn left_parser() {
        let tag = left(literal("<"), identifier);

        assert_eq!(tag.parse("<hello-there"), Ok(("", ())),)
    }

    #[test]
    fn at_least_parser() {
        // At least three letters `a`.
        let parse_letters_a = at_least(literal("a"), 3);

        assert_eq!(
            parse_letters_a.parse("aaaa"),
            Ok(("", vec![(), (), (), ()])),
        );

        assert_eq!(parse_letters_a.parse("aaba"), Err("ba"),);
    }

    #[test]
    fn zero_or_more_letters_a() {
        let parse_letters_a = zero_or_more(literal("a"));

        assert_eq!(parse_letters_a.parse("aaa"), Ok(("", vec![(), (), ()])),);

        assert_eq!(parse_letters_a.parse(""), Ok(("", vec![])),);
    }

    #[test]
    fn one_or_more_letters_a() {
        let parse_letters_a = one_or_more(literal("a"));

        assert_eq!(parse_letters_a.parse("aaba"), Ok(("ba", vec![(), ()])),);

        assert_eq!(parse_letters_a.parse("ba"), Err("ba"));
    }

    #[test]
    fn any_char_parser() {
        assert_eq!(any_char("hello"), Ok(("ello", 'h')));
        assert_eq!(any_char(r#""hello"#), Ok(("hello", '"')));
    }

    #[test]
    fn any_char_simple_word() {
        assert_eq!(
            zero_or_more(any_char).parse("hello"),
            Ok(("", "hello".to_owned().chars().collect()))
        )
    }

    #[test]
    fn any_char_simple_sentence() {
        assert_eq!(
            zero_or_more(any_char).parse("hello there"),
            Ok(("", "hello there".to_owned().chars().collect()))
        )
    }

    #[test]
    fn any_char_quoted_sentence() {
        assert_eq!(
            zero_or_more(any_char).parse(r#""hello there""#),
            Ok(("", r#""hello there""#.to_owned().chars().collect()))
        )
    }

    #[test]
    fn predicate_combinator() {
        let parser = pred(any_char, |c| *c == 'o');

        assert_eq!(parser.parse("omg"), Ok(("mg", 'o')));
    }

    #[test]
    fn quoted_string_short_sentence() {
        let ex_str = r#""hello there""#;

        assert_eq!(quoted_string(ex_str), Ok(("", ex_str.into())));
    }

    #[test]
    fn quoted_string_nested_quotes() {
        let ex_str = r#""hello there", he said"#;

        assert_eq!(quoted_string(ex_str), Ok(("", ex_str.into())));
    }

    #[test]
    fn parse_element_with_attributes() {
        assert_eq!(
            xml_ele(r#"<single-element attribute="value">"#),
            Ok((
                "",
                Element {
                    name: "single-element".into(),
                    attributes: vec![("attribute".into(), "value".into())],
                    ..Default::default()
                }
            )),
        );
    }
}
