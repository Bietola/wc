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

/// Match XML element into dedicated structure (`Element`)
pub fn xml_ele(input: &str) -> ParseResult<'_, Element> {
    // Support parsers. Their name indicate what they parse
    let attr_pair = pair(identifier, right(literal("="), quoted_string()));

    let attr_lst = zero_or_more(right(space1(), attr_pair));

    let ele = right(
        literal("<"),
        pair(identifier, left(attr_lst, literal("\\>"))),
    );

    map(ele, |(name, attributes)| Element {
        name,
        attributes,
        ..Default::default()
    })
    .parse(input)
}

pub fn xml_tree(_input: &str) -> ParseResult<Element> {
    unimplemented!()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn parse_element_with_attributes() {
        assert_eq!(
            xml_ele(r#"<single-element attribute="value"\>"#),
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
        let str_tree = r#"
            <parent attr="I'm an attribute">

                <child1\>

                <child2 name="child" last_name="two"\>

                <big_child name="big child"\>
                    <nested_child\>
                <\big_child>

            <\parent>
        "#;

        assert_eq!(xml_tree(str_tree), Ok(("", Default::default(),)));
    }
}
