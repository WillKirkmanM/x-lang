use crate::Rule;
use pest::iterators::Pair;
use x_ast::Type;

pub fn parse_type(pair: Pair<Rule>) -> Type {
    match pair.as_rule() {
        // Case 1: Handle container rules by recursively calling on their content.
        Rule::type_specifier | Rule::return_type_ann => {
            parse_type(pair.into_inner().next().unwrap())
        }

        // Case 2: The main `type` rule. This is the dispatcher.
        Rule::r#type => {
            let mut inner = pair.into_inner();
            let first_child = inner.next().unwrap();

            // If the inner type is a reference, delegate to ref_type handling.
            if first_child.as_rule() == Rule::ref_type {
                return parse_type(first_child);
            }

            // Otherwise the first child is the base type (basic_type or custom_type).
            let base_type_pair = first_child;
            let mut parsed_type = parse_type(base_type_pair);

            // Any remaining children that are array postfixes wrap the base type.
            let array_dimensions = inner.filter(|p| p.as_rule() == Rule::array_postfix).count();
            for _ in 0..array_dimensions {
                parsed_type = Type::Array(Box::new(parsed_type));
            }
            parsed_type
        }

        // Bracket-style array: `[ T ]`
        Rule::array_type => {
            // inner contains the contained `type`
            let inner_type_pair = pair.into_inner().next().unwrap();
            let inner_ty = parse_type(inner_type_pair);
            Type::Array(Box::new(inner_ty))
        }

        // Case 3: A basic, primitive type.
        Rule::basic_type => match pair.as_str() {
            "i32" => Type::Int,
            "f64" => Type::Float,
            "bool" => Type::Bool,
            "str" => Type::String,
            "void" => Type::Void,
            _ => unreachable!("Unknown basic type: {}", pair.as_str()),
        },

        // Case 4: A custom struct type with optional generic arguments.
        Rule::custom_type => {
            let mut inner = pair.into_inner();
            let name = inner.next().unwrap().as_str().to_string();

            // Grammar's generic_arg can be a lifetime or an identifier.
            // Ignore lifetime arguments; treat identifier args as type parameters.
            let type_args: Vec<Type> = inner
                .filter_map(|p| match p.as_rule() {
                    Rule::lifetime => None,
                    _ => Some(Type::TypeParameter(p.as_str().to_string())),
                })
                .collect();

            if type_args.is_empty() {
                Type::Custom(name)
            } else {
                Type::GenericInstance { name, type_args }
            }
        }

        // fallback: treat a bare identifier token as a custom type name
        Rule::value_identifier => Type::Custom(pair.as_str().to_string()),

        // Case 5: A reference type, like `&T` or `&mut T[]`.
        Rule::ref_type => {
            let mut inner = pair.into_inner();
            let is_mut = inner.peek().map(|p| p.as_rule()) == Some(Rule::kw_mut);
            if is_mut {
                inner.next();
            }

            if inner.peek().map(|p| p.as_rule()) == Some(Rule::lifetime) {
                inner.next();
            }

            // Next child is the base type (basic_type or custom_type)
            let base_type_pair = inner.next().unwrap();
            let mut inner_ty = parse_type(base_type_pair);

            // Apply any array postfixes to the referenced type.
            let array_dimensions = inner.filter(|p| p.as_rule() == Rule::array_postfix).count();
            for _ in 0..array_dimensions {
                inner_ty = Type::Array(Box::new(inner_ty));
            }

            Type::Ref {
                is_mut,
                inner: Box::new(inner_ty),
            }
        }

        _ => unreachable!(
            "parse_type received an unexpected rule: {:?}",
            pair.as_rule()
        ),
    }
}
