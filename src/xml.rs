use crate::parse::*;

/**********************/
/* XML element struct */
/**********************/

/// Representation of generic XML Element
#[derive(Clone, Debug, PartialEq, Eq, Default)]
pub struct Element {
    name: String,
    attributes: Vec<(String, String)>,
    children: Vec<Element>,
}

/***************/
/* XML parsers */
/***************/

/// Some useful type aliases
type RawAttrLst = Vec<(String, String)>;
type RawSingleEle = (String, RawAttrLst);

/// Match xml element argument along with its value
///
/// e.g. `hello="there"`
fn attr_pair<'a>() -> impl Parser<'a, (String, String)> {
    pair(identifier, right(literal("="), quoted_string()))
}

/// Match xml element argument list
fn attr_list<'a>() -> impl Parser<'a, RawAttrLst> {
    zero_or_more(right(space1(), attr_pair()))
}

/// Utility that matches internal structures of element
fn ele_internals<'a>() -> impl Parser<'a, RawSingleEle> {
    pair(identifier, attr_list())
}

/// Match XML opening element
fn open_ele<'a>() -> impl Parser<'a, RawSingleEle> {
    sorround(literal("<"), ele_internals(), literal(">"))
}

/// Match XML closing element
fn close_ele<'a>(opening_name: String) -> impl Parser<'a, String> {
    sorround(literal("<\\"), identifier, literal(">"))
        .pred(move |closing_name| closing_name == &opening_name)
}

/// Match generic XML ele (with or without children) sorrounded by whitespace
fn ele<'a>() -> impl Parser<'a, Element> {
    whitespace_wrap(either(single_ele(), tree()))
}

/// Match single XML element (e.g. `<\hello-there>`, which has no children)
/// into dedicated structure (`Element`)
pub fn single_ele<'a>() -> impl Parser<'a, Element> {
    sorround(literal("<"), ele_internals(), literal("\\>")).map(|(name, attributes)| Element {
        name,
        attributes,
        children: vec![],
    })
}

/// Match XML tree (NB. must have children)
pub fn tree<'a>() -> impl Parser<'a, Element> {
    println!("hello there!");

    open_ele().and_then(|(name, attributes)| {
        pair(zero_or_more(ele()), close_ele(name)).map(move |(children, name)| Element {
            name,
            attributes: attributes.clone(),
            children,
        })
    })
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
            tree().parse(str_tree),
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
