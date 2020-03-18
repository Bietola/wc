use lazy_static::lazy_static;
use regex::Regex;

/****************/
/* Parser trait */
/****************/

pub type ParseResult<'a, T> = Result<(&'a str, T), &'a str>;

pub trait Parser<'a, Output> {
    fn parse(&self, input: &'a str) -> ParseResult<'a, Output>;

    /// Same to free-function equivalent, but encloses resulting parser in dynamic type (`BoxedParser`).
    /// Useful for when type length and/or compilation times get out of hand.
    fn map<F, NewOutput>(self, fun: F) -> BoxedParser<'a, NewOutput>
    where
        Self: Sized + 'a,
        Output: 'a,
        NewOutput: 'a,
        F: Fn(Output) -> NewOutput + 'a,
    {
        BoxedParser::new(map(self, fun))
    }

    /// Same to free-function equivalent, but encloses resulting parser in dynamic type (`BoxedParser`).
    /// Useful for when type length and/or compilation times get out of hand.
    fn pred<Pred>(self, the_pred: Pred) -> BoxedParser<'a, Output>
    where
        Self: Sized + 'a,
        Output: 'a,
        Pred: Fn(&Output) -> bool + 'a,
    {
        BoxedParser::new(pred(self, the_pred))
    }

    /// Same to free-function equivalent, but encloses resulting parser in dynamic type (`BoxedParser`).
    /// Useful for when type length and/or compilation times get out of hand.
    fn and_then<NewOutput, F>(self, fun: F) -> BoxedParser<'a, NewOutput>
    where
        Self: Sized + 'a,
        Output: 'a,
        NewOutput: 'a,
        F: Fn(Output) -> BoxedParser<'a, NewOutput> + 'a,
    {
        BoxedParser::new(and_then(self, fun))
    }
}

impl<'a, F, T> Parser<'a, T> for F
where
    F: Fn(&'a str) -> ParseResult<T>,
{
    fn parse(&self, input: &'a str) -> ParseResult<'a, T> {
        self(input)
    }
}

/************************************************************/
/* Dynamic parser for when static types get too complicated */
/************************************************************/

pub struct BoxedParser<'a, Output> {
    parser: Box<dyn Parser<'a, Output> + 'a>,
}

impl<'a, Output> BoxedParser<'a, Output> {
    fn new<P>(parser: P) -> Self
    where
        P: Parser<'a, Output> + 'a,
    {
        Self {
            parser: Box::new(parser),
        }
    }
}

impl<'a, Output> Parser<'a, Output> for BoxedParser<'a, Output> {
    fn parse(&self, input: &'a str) -> ParseResult<'a, Output> {
        self.parser.parse(input)
    }
}

/*************************/
/* Free function parsers */
/*************************/

/// Match a specified literal string
pub fn literal<'a>(expected: &'static str) -> impl Parser<'a, ()> {
    move |input: &'a str| {
        if let Some(found) = input.find(expected) {
            // Parser must match `expected` string in first position of parsed sentence
            if found == 0 {
                Ok((&input[expected.len()..], ()))
            } else {
                Err(input)
            }
        } else {
            Err(input)
        }
    }
}

/// Match classic pascal-like identifier (`^[a-zA-z][\w\d-]*`)
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

/// Match a simple string.
pub fn simple_string<'a>() -> impl Parser<'a, String> {
    zero_or_more(any_char.pred(|c| *c != '\"')).map(|output| output.into_iter().collect())
}

/// Match any string surrounded by quotes
pub fn quoted_string<'a>() -> impl Parser<'a, String> {
    right(literal("\""), left(simple_string(), literal("\"")))
}

/// Match first using `parser1` and then `parser2` (in that order), then return the results of both
/// in a pair wrapped in a `ParseResult`
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

/// Functor-like `map` implementation for the `Parser` type
/// TODO: write what this actually does
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

/// Monad-like `>>` implementation for the `Parser` type
/// TODO: write what this actually does
pub fn and_then<'a, P1, R1, R2, F>(parser: P1, fun: F) -> impl Parser<'a, R2>
where
    P1: Parser<'a, R1>,
    F: Fn(R1) -> BoxedParser<'a, R2>,
{
    move |input| match parser.parse(input) {
        Ok((rest, res)) => fun(res).parse(rest),
        Err(err) => Err(err),
    }
}

/// Like `pair`, but discard the result of the left parser (`parser1`)
pub fn right<'a, P1, P2, R1, R2>(parser1: P1, parser2: P2) -> impl Parser<'a, R2>
where
    R1: 'a,
    R2: 'a,
    P1: Parser<'a, R1> + 'a,
    P2: Parser<'a, R2> + 'a,
{
    pair(parser1, parser2).map(|(_, rhs)| rhs)
}

/// Like `pair`, but discard the result of the right parser (`parser2`)
pub fn left<'a, P1, P2, R1, R2>(parser1: P1, parser2: P2) -> impl Parser<'a, R1>
where
    R1: 'a,
    R2: 'a,
    P1: Parser<'a, R1> + 'a,
    P2: Parser<'a, R2> + 'a,
{
    pair(parser1, parser2).map(|(lhs, _)| lhs)
}

/// Sorrounds center parser with two parsers, ignoring the sorrounding parsers.
pub fn sorround<'a, P1, P2, P3, R1, R2, R3>(
    opening_par: P1,
    center_par: P2,
    closing_par: P3,
) -> impl Parser<'a, R2>
where
    R1: 'a,
    R2: 'a,
    R3: 'a,
    P1: Parser<'a, R1> + 'a,
    P2: Parser<'a, R2> + 'a,
    P3: Parser<'a, R3> + 'a,
{
    right(opening_par, left(center_par, closing_par))
}

/// Matches like `parser`, but allows matches to be sorrounded by albitrary whitespace.
pub fn whitespace_wrap<'a, P, Output>(parser: P) -> impl Parser<'a, Output>
where
    Output: 'a,
    P: Parser<'a, Output> + 'a,
{
    sorround(space0(), parser, space0())
}

/// Returns parser that tries to match using `parser1` first and, if it fails, using `parser2`
/// later. If all match attempts fail, the parse error from `parser2` is issued.
pub fn either<'a, P1, P2, R>(parser1: P1, parser2: P2) -> impl Parser<'a, R>
where
    P1: Parser<'a, R>,
    P2: Parser<'a, R>,
{
    move |input| match parser1.parse(input) {
        ok @ Ok(_) => ok,
        Err(_) => parser2.parse(input),
    }
}

/// Match anything containing at least `required_num` recurring instances of `parser` matches
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

/// Match zero or more instances of `parser` matches
pub fn zero_or_more<'a, P, A>(parser: P) -> impl Parser<'a, Vec<A>>
where
    P: Parser<'a, A>,
{
    at_least(parser, 0)
}

/// Match one or more instances of `parser` matches
pub fn one_or_more<'a, P, A>(parser: P) -> impl Parser<'a, Vec<A>>
where
    P: Parser<'a, A>,
{
    at_least(parser, 1)
}

/// Match one or more instances of `parser` matches
pub fn opt<'a, P, A>(parser: P) -> impl Parser<'a, Option<A>>
where
    P: Parser<'a, A>,
{
    move |input| match parser.parse(input) {
        Ok((rest, res)) => Ok((rest, Some(res))),
        Err(_) => Ok((input, None)),
    }
}

/// Match next instance of `parser` match and pipe result to `the_pred`; return valid match only if `the_pred`
/// is valid on said result
/// TODO: use monad
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
        Err(err) => Err(err),
    }
}

/// Match any UTF character
pub fn any_char(input: &str) -> ParseResult<'_, char> {
    match input.chars().next() {
        Some(c) => Ok((&input[c.len_utf8()..], c)),
        None => Err(input),
    }
}

/// Match any whitespace character (corresponding to the `char::is_whitespace` funcion)
pub fn whitespace_char(input: &str) -> ParseResult<'_, char> {
    any_char.pred(|c| c.is_whitespace()).parse(input)
}

/// Match one ore more spaces
pub fn space1<'a>() -> impl Parser<'a, Vec<char>> {
    one_or_more(whitespace_char)
}

/// Match zero ore more spaces
pub fn space0<'a>() -> impl Parser<'a, Vec<char>> {
    zero_or_more(whitespace_char)
}

/*********/
/* Tests */
/*********/

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
    fn opt_optional_exclamation_point() {
        let phrase = one_or_more(left(simple_string(), space0()));
        let phrase_with_opt_exclamation = left(phrase, opt(literal("!")));

        // With exclamation.
        assert_eq!(
            Ok(("", vec!["uga".into(), "uguga".into()])),
            phrase_with_opt_exclamation.parse("ug ugug")
        );

        // Without.
        assert_eq!(
            Ok(("", vec!["ug".into(), "ugug".into()])),
            phrase_with_opt_exclamation.parse("ug ugug!")
        );
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

        // TODO: check a more coincise way of doing this
        assert_eq!(
            quoted_string().parse(ex_str),
            Ok(("", (&ex_str[1..ex_str.len() - 1]).into()))
        );
    }

    #[test]
    fn whitespace_wrap_identifier_sorrounded_by_whitespace() {
        assert_eq!(
            whitespace_wrap(identifier).parse("\n\n  hello  \n\n"),
            Ok(("", "hello".into())),
        );
    }

    #[test]
    fn whitespace_wrap_identifier_no_whitespace_on_one_side() {
        assert_eq!(
            whitespace_wrap(identifier).parse("hello\n\n"),
            Ok(("", "hello".into())),
        );
    }
}
