use crate::Rule;
use pest::iterators::Pair;
use x_ast::{ExternParam, Statement};

pub fn parse_extern_fn_decl(pair: Pair<Rule>) -> Statement {
    let mut inner = pair.into_inner();
    let name = inner.next().unwrap().as_str().to_string();

    let mut params = Vec::new();
    let mut return_type = None;

    while let Some(next_pair) = inner.peek() {
        match next_pair.as_rule() {
            Rule::extern_params => {
                let params_pair = inner.next().unwrap();
                for param_pair in params_pair.into_inner() {
                    let mut param_inner = param_pair.into_inner();
                    let param_name = param_inner.next().unwrap().as_str().to_string();
                    let type_name = param_inner.next().unwrap().as_str().to_string();
                    params.push(ExternParam {
                        name: param_name,
                        type_name,
                    });
                }
            }
            Rule::type_specifier => {
                return_type = Some(inner.next().unwrap().as_str().to_string());
            }
            _ => break,
        }
    }

    Statement::ExternFunctionDecl {
        name,
        params,
        return_type,
    }
}
