use super::statements::{parse_attribute, parse_attribute_double_only};
use super::*;
use crate::parser::types::parse_type_modifiers_after;

/// Parse multiple declarators on a declaration
pub fn parse_init_declarators(input: &[LexToken]) -> ParseResult<Vec<InitDeclarator>> {
    parse_list_nonempty(parse_token(Token::Comma), parse_init_declarator)(input)
}

/// Parse the declarator and initialiser of a declaration
fn parse_init_declarator(input: &[LexToken]) -> ParseResult<InitDeclarator> {
    let (input, declarator) = parse_declarator(input)?;
    let (input, location_annotations) = parse_multiple(parse_location_annotation)(input)?;
    let (input, init) = parse_initializer(input)?;
    let init_decl = InitDeclarator {
        declarator,
        location_annotations,
        init,
    };
    Ok((input, init_decl))
}

/// Parse the declarator part of a declaration
pub fn parse_declarator(input: &[LexToken]) -> ParseResult<Declarator> {
    parse_declarator_internal(input, false)
}

/// Parse the declarator part of a type id
pub fn parse_abstract_declarator(input: &[LexToken]) -> ParseResult<Declarator> {
    parse_declarator_internal(input, true)
}

/// Parse the declarator part of a declaration or type id
fn parse_declarator_internal(
    input: &[LexToken],
    abstract_declarator: bool,
) -> ParseResult<Declarator> {
    if let Ok((input, _)) = parse_token(Token::Asterix)(input) {
        let (input, attributes) = parse_multiple(parse_attribute)(input)?;

        let mut modifiers = TypeModifierSet::default();
        let input = parse_type_modifiers_after(input, &mut modifiers);

        let (input, inner) = parse_declarator_internal(input, abstract_declarator)?;

        let decl = Declarator::Pointer(PointerDeclarator {
            attributes,
            qualifiers: modifiers,
            inner: Box::new(inner),
        });
        return Ok((input, decl));
    };

    if let Ok((input, _)) = parse_token(Token::Ampersand)(input) {
        let (input, attributes) = parse_multiple(parse_attribute)(input)?;
        let (input, inner) = parse_declarator_internal(input, abstract_declarator)?;

        let decl = Declarator::Reference(ReferenceDeclarator {
            attributes,
            inner: Box::new(inner),
        });
        return Ok((input, decl));
    };

    let (input, mut decl) = if abstract_declarator {
        (input, Declarator::Empty)
    } else {
        let (input, identifier) = parse_variable_name(input)?;
        let (input, identifier_attributes) = parse_multiple(parse_attribute_double_only)(input)?;

        let decl = Declarator::Identifier(
            ScopedIdentifier::unqualified(identifier),
            identifier_attributes,
        );

        (input, decl)
    };

    let mut input = input;
    while let Ok((next, array_size)) = parse_arraydim(input) {
        let (next, array_attributes) = parse_multiple(parse_attribute_double_only)(next)?;

        decl = Declarator::Array(ArrayDeclarator {
            inner: Box::new(decl),
            array_size,
            attributes: array_attributes,
        });

        input = next;
    }

    Ok((input, decl))
}

/// Parse register(), packoffset(), and semantic annotations
pub fn parse_location_annotation(input: &[LexToken]) -> ParseResult<LocationAnnotation> {
    // Attempt to consume a :
    // If successful then we expect to parse some form of annotation
    let input = match parse_token(Token::Colon)(input) {
        Ok((input, _)) => input,
        Err(_) => return ParseErrorReason::wrong_token(input),
    };

    match input {
        [LexToken(Token::Register, _), ..] => {
            let (input, register) = parse_register(input)?;
            Ok((input, LocationAnnotation::Register(register)))
        }
        [LexToken(Token::PackOffset, _), ..] => {
            let (input, packoffset) = parse_packoffset(input)?;
            Ok((input, LocationAnnotation::PackOffset(packoffset)))
        }
        _ => {
            let (input, semantic) = parse_semantic(input)?;
            Ok((input, LocationAnnotation::Semantic(semantic)))
        }
    }
}

/// Parse a register slot for a resource
fn parse_register(input: &[LexToken]) -> ParseResult<Register> {
    let (input, _) = parse_token(Token::Register)(input)?;
    let (input, _) = parse_token(Token::LeftParen)(input)?;
    let identifier_input_start = input;
    let (mut input, id) = match_identifier(input)?;

    let mut slot = None;
    let mut space = None;

    if let Some(index) = parse_space_identifier(&id.0) {
        space = Some(index);
    } else {
        // Identifiers must have at least one character
        // Only single byte chars should be valid identifiers so the split is safe
        let (register_type_char, register_index_str) = id.0.split_at(1);

        let slot_type = match register_type_char {
            "t" => RegisterType::T,
            "u" => RegisterType::U,
            "b" => RegisterType::B,
            "s" => RegisterType::S,
            _ => {
                return ParseErrorReason::InvalidSlotType(id.0.clone())
                    .into_result(identifier_input_start)
            }
        };

        let index = {
            if register_index_str.chars().all(|c| char::is_ascii_digit(&c)) {
                match register_index_str.parse::<u32>() {
                    Ok(v) => v,
                    Err(_) => {
                        return ParseErrorReason::InvalidSlotIndex(id.0.clone())
                            .into_result(identifier_input_start)
                    }
                }
            } else {
                return ParseErrorReason::InvalidSlotIndex(id.0.clone())
                    .into_result(identifier_input_start);
            }
        };

        slot = Some(RegisterSlot { slot_type, index });
    }

    if space.is_none() {
        // Parse an optional space identifier
        let (rest, space_identifier) = match parse_token(Token::Comma)(input) {
            Ok((space_start, _)) => match match_identifier(space_start) {
                Ok((input, id)) => (input, Some((id, space_start))),
                Err(err) => return Err(err),
            },
            Err(_) => (input, None),
        };

        // Convert the space identifier into a space index
        space = match space_identifier {
            Some((id, space_start)) => match parse_space_identifier(&id.0) {
                Some(index) => Some(index),
                None => {
                    return ParseErrorReason::InvalidSpaceIdentifier(id.0.clone())
                        .into_result(space_start)
                }
            },
            None => None,
        };

        input = rest;
    }

    let (input, _) = parse_token(Token::RightParen)(input)?;

    let register = Register { slot, space };
    Ok((input, register))
}

/// Attempt to turn a space identifier into a space index
fn parse_space_identifier(space_identifier: &str) -> Option<u32> {
    if let Some(index_string) = space_identifier.strip_prefix("space") {
        if let Ok(index) = index_string.parse::<u32>() {
            Some(index)
        } else {
            None
        }
    } else {
        None
    }
}

/// Parse a packoffset annotation
pub fn parse_packoffset(_: &[LexToken]) -> ParseResult<PackOffset> {
    unimplemented!("packoffset")
}

/// Parse a semantic
pub fn parse_semantic(input: &[LexToken]) -> ParseResult<Semantic> {
    match input.first() {
        Some(LexToken(Token::Id(Identifier(name)), _)) => {
            let semantic = match name[..].to_lowercase().as_str() {
                "sv_dispatchthreadid" => Semantic::DispatchThreadId,
                "sv_groupid" => Semantic::GroupId,
                "sv_groupindex" => Semantic::GroupIndex,
                "sv_groupthreadid" => Semantic::GroupThreadId,
                "sv_vertexid" => Semantic::VertexId,
                "sv_instanceid" => Semantic::InstanceId,
                "sv_primitiveid" => Semantic::PrimitiveId,
                "sv_position" => Semantic::Position,
                "sv_target" => Semantic::Target(0),
                "sv_target0" => Semantic::Target(0),
                "sv_target1" => Semantic::Target(1),
                "sv_target2" => Semantic::Target(2),
                "sv_target3" => Semantic::Target(3),
                "sv_target4" => Semantic::Target(4),
                "sv_target5" => Semantic::Target(5),
                "sv_target6" => Semantic::Target(6),
                "sv_target7" => Semantic::Target(7),
                "sv_depth" => Semantic::Depth,
                "sv_depthgreaterequal" => Semantic::DepthGreaterEqual,
                "sv_depthlessequal" => Semantic::DepthLessEqual,
                _ => Semantic::User(name.clone()),
            };
            Ok((&input[1..], semantic))
        }
        _ => ParseErrorReason::wrong_token(input),
    }
}

#[test]
fn test_register() {
    use test_support::*;
    let register = ParserTester::new(parse_register);

    register.check(
        "register(t0)",
        Register {
            slot: Some(RegisterSlot {
                slot_type: RegisterType::T,
                index: 0,
            }),
            space: None,
        },
    );

    register.check(
        "register(t1)",
        Register {
            slot: Some(RegisterSlot {
                slot_type: RegisterType::T,
                index: 1,
            }),
            space: None,
        },
    );

    register.check(
        "register(u1)",
        Register {
            slot: Some(RegisterSlot {
                slot_type: RegisterType::U,
                index: 1,
            }),
            space: None,
        },
    );

    register.expect_fail(
        "register(p0)",
        ParseErrorReason::InvalidSlotType("p0".to_string()),
        9,
    );

    register.expect_fail(
        "register(tY)",
        ParseErrorReason::InvalidSlotIndex("tY".to_string()),
        9,
    );

    register.expect_fail(
        "register(pY)",
        ParseErrorReason::InvalidSlotType("pY".to_string()),
        9,
    );

    register.check(
        "register(t4294967295)",
        Register {
            slot: Some(RegisterSlot {
                slot_type: RegisterType::T,
                index: u32::MAX,
            }),
            space: None,
        },
    );

    register.expect_fail(
        "register(t4294967296)",
        ParseErrorReason::InvalidSlotIndex("t4294967296".to_string()),
        9,
    );

    register.check(
        "register(t1, space2)",
        Register {
            slot: Some(RegisterSlot {
                slot_type: RegisterType::T,
                index: 1,
            }),
            space: Some(2),
        },
    );

    register.expect_fail(
        "register(t1, space)",
        ParseErrorReason::InvalidSpaceIdentifier("space".to_string()),
        13,
    );

    register.check(
        "register(t1, space4294967295)",
        Register {
            slot: Some(RegisterSlot {
                slot_type: RegisterType::T,
                index: 1,
            }),
            space: Some(u32::MAX),
        },
    );

    register.expect_fail(
        "register(t1, space4294967296)",
        ParseErrorReason::InvalidSpaceIdentifier("space4294967296".to_string()),
        13,
    );

    register.check(
        "register(space2)",
        Register {
            slot: None,
            space: Some(2),
        },
    );

    register.expect_fail("register(space2, space3)", ParseErrorReason::WrongToken, 15);

    // This parser does not expect semicolon on the end
    register.expect_fail("register(t0);", ParseErrorReason::TokensUnconsumed, 12);
}
