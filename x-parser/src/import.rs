use pest::iterators::Pair;
use x_ast::Statement;

use crate::Rule;

pub fn parse_import(pair: Pair<Rule>) -> Statement {
    let import_type_pair = pair.into_inner().next().unwrap();

    match import_type_pair.as_rule() {
        Rule::module_import => {
            let mut inner_rules = import_type_pair.into_inner();
            let module = inner_rules.next().unwrap().as_str().to_string();
            let item = inner_rules.next().unwrap().as_str().to_string();
            Statement::Import { module, item }
        }
        Rule::file_import => {
            let path_str = import_type_pair.into_inner().next().unwrap().as_str();
            let path = path_str.trim_matches('"').to_string();
            Statement::FileImport { path }
        }
        _ => unreachable!(
            "Unexpected rule in import: {:?}",
            import_type_pair.as_rule()
        ),
    }
}
