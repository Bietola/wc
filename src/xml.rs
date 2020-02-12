use crate::parse::*;

/**********************/
/* XML element struct */
/**********************/

#[derive(Clone, Debug, PartialEq, Eq, Default)]
pub struct Element {
    name: String,
    attributes: Vec<(String, String)>,
    children: Vec<Element>,
}

/// Match xml element argument along with its value
///
/// e.g. `hello="there"`
fn attr_pair<'a>() -> impl Parser<'a, (String, String)> {
    pair(identifier, right(literal("="), quoted_string()))
}

/// Match xml element argument list
fn attr_list<'a>() -> impl Parser<'a, Vec<(String, String)>> {
    zero_or_more(right(space1(), attr_pair()))
}

/// Utility that matches internal structures of element
fn ele_internals<'a>() -> impl Parser<'a, Element> {
    let raw_parser = pair(identifier, attr_list());

    raw_parser.map(|(name, attributes)| Element {
        name,
        attributes,
        ..Default::default()
    })
}

/// Match XML opening element
///
/// e.g. in `<hello-there arg="val" arg2="val2"> ... <\hello-there>` the following is matched:
/// `<hello-there arg=...>`.
fn open_ele<'a>() -> impl Parser<'a, Element> {
    sorround(literal("<"), ele_internals(), literal(">"))
}

/// Match XML closing element
///
/// e.g. as in `<\closing>`
fn close_ele<'a>() -> impl Parser<'a, Element> {
    sorround(literal("<\\"), ele_internals(), literal(">"))
}

/// Match XML element into dedicated structure (`Element`)
pub fn single_ele<'a>() -> impl Parser<'a, Element> {
    sorround(literal("<"), ele_internals(), literal("\\>"))
}

/// Match XML tree
pub fn tree(input: &str) -> ParseResult<Element> {
    // Try to match single element
    if let ok @ Ok(_) = single_ele().parse(input) {
        return ok;
    }

    // Match real element tree
    // Start with matching open element
    open_ele()
        .parse(input)
        // Go on parsing
        .and_then(
            |(
                rest,
                Element {
                    name: open_name,
                    attributes,
                    ..
                },
            )| {
                // Match children
                match one_or_more(right(space0(), tree)).parse(rest) {
                    // Go on parsing
                    Ok((rest, children)) => {
                        // Match closing element
                        right(space0(), close_ele()).parse(rest).and_then(
                            |(
                                rest,
                                Element {
                                    name: close_name, ..
                                },
                            )| {
                                // Return an error if the closing element name doesn't correspond to the opening element's.
                                if close_name == open_name {
                                    // Final result
                                    Ok((
                                        rest,
                                        Element {
                                            name: open_name,
                                            attributes,
                                            children,
                                        },
                                    ))
                                } else {
                                    Err(rest)
                                }
                            },
                        )
                    }
                    // TODO: redundant...
                    Err(err) => Err(err),
                }
            },
        )
}

#[cfg(test)]
mod tests {
    use super::*;
    use indoc::indoc;
    use pretty_assertions::assert_eq;

    #[test]
    fn parse_element_with_attributes() {
        assert_eq!(
            single_ele().parse(r#"<single-element attribute="value"\>"#),
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

    #[test]
    fn parse_big_xml_tree() {
        let str_tree = indoc!(
            r#"
            <parent attr="I'm an attribute">

                <child1\>

                <big_child name="big child">
                    <nested_child\>
                <\big_child>

                <child2 name="child" last_name="two"\>

            <\parent>"#
        );

        assert_eq!(
            tree(str_tree),
            Ok((
                "",
                Element {
                    name: "parent".into(),
                    attributes: vec![("attr".into(), "I'm an attribute".into())],
                    children: vec![
                        Element {
                            name: "child1".into(),
                            ..Default::default()
                        },
                        Element {
                            name: "big_child".into(),
                            attributes: vec![("name".into(), "big child".into())],
                            children: vec![Element {
                                name: "nested_child".into(),
                                ..Default::default()
                            }],
                        },
                        Element {
                            name: "child2".into(),
                            attributes: vec![
                                ("name".into(), "child".into()),
                                ("last_name".into(), "two".into())
                            ],
                            ..Default::default()
                        }
                    ],
                }
            ))
        );
    }
}
