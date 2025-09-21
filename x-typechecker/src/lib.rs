use std::{
    collections::{HashMap, HashSet},
    path::Path,
};
use x_ast::{Program, Statement, StructDef, Type};

mod expression;
mod function;
mod generics;
mod infer;
mod statement;
mod r#struct;
mod substitute;
mod r#trait;

#[derive(Debug, Clone)]
pub struct FunctionDef {
    pub name: String,
    pub generic_params: Option<Vec<String>>,
    pub params: Vec<(String, Type)>,
    pub return_type: Type,
    pub body: Box<Vec<Statement>>,
    pub is_pure: bool,
    pub is_memoised: bool,
    pub is_multi: bool,
    pub is_throws: bool,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct FunctionSignature {
    pub param_types: Vec<Type>,
    pub return_type: Type,
}

#[derive(Debug, Clone)]
pub struct TraitDef {
    pub name: String,
    pub methods: HashMap<String, FunctionSignature>,
}

#[derive(Debug, Clone)]
pub struct ImplDef {
    pub trait_name: String,
    pub type_name: Type,
    pub methods: HashMap<String, String>,
}

#[derive(Debug, Default)]
pub struct SymbolTable {
    variables: Vec<HashMap<String, Type>>,
    functions: HashMap<String, FunctionSignature>,
    structs: HashMap<String, Vec<(String, Type)>>,
    generic_functions: HashMap<String, FunctionDef>,
    generic_structs: HashMap<String, StructDef>,
    instantiations: HashMap<(String, Vec<String>), String>,
    struct_inst_args: HashMap<String, Vec<Type>>,
    traits: HashMap<String, TraitDef>,
    impls_for_type: HashMap<Type, Vec<String>>,
    trait_ast_defs: HashMap<String, x_ast::TraitDef>,
    multi_functions: HashMap<String, Vec<FunctionSignature>>,
}

impl SymbolTable {
    pub fn new() -> Self {
        let mut s = Self::default();
        s.variables.push(HashMap::new());
        s
    }
    pub fn enter_scope(&mut self) {
        self.variables.push(HashMap::new());
    }
    pub fn exit_scope(&mut self) {
        self.variables.pop();
    }
    pub fn add_variable(&mut self, name: String, ty: Type) {
        self.variables.last_mut().unwrap().insert(name, ty);
    }
    pub fn get_variable(&self, name: &str) -> Option<&Type> {
        self.variables
            .iter()
            .rev()
            .find_map(|scope| scope.get(name))
    }
    pub fn add_function(&mut self, name: String, sig: FunctionSignature) {
        self.functions.insert(name, sig);
    }
    pub fn get_function(&self, name: &str) -> Option<&FunctionSignature> {
        self.functions.get(name)
    }
    pub fn add_struct(&mut self, name: String, fields: Vec<(String, Type)>) {
        self.structs.insert(name, fields);
    }
}

pub struct TypeChecker {
    symbols: SymbolTable,
    program: Program,
    processed_files: HashSet<String>,
}

impl TypeChecker {
    pub fn new() -> Self {
        let mut symbols = SymbolTable::new();

        symbols.add_function(
            "print".to_string(),
            FunctionSignature {
                param_types: vec![Type::Float],
                return_type: Type::Void,
            },
        );
        symbols.add_function(
            "print_str".to_string(),
            FunctionSignature {
                param_types: vec![Type::String],
                return_type: Type::Void,
            },
        );

        Self {
            symbols,
            program: Program {
                statements: Vec::new(),
            },
            processed_files: HashSet::new(),
        }
    }

    pub fn check(&mut self, program: Program) -> Result<Program, String> {
        let all_statements = self.collect_all_statements(program.statements, Path::new("."))?;
        self.program.statements = all_statements;

        self.register_generics()?;
        self.register_signatures()?;
        self.register_traits_and_impls()?;

        self.synthesise_default_methods()?;

        let mut i = 0;
        while i < self.program.statements.len() {
            let mut stmt = self.program.statements[i].clone();
            self.check_statement(&mut stmt)?;
            self.program.statements[i] = stmt;
            i += 1;
        }
        Ok(self.program.clone())
    }

    fn synthesise_default_methods(&mut self) -> Result<(), String> {
        let mut synthesis_plan = Vec::new();

        for (i, stmt) in self.program.statements.iter().enumerate() {
            if let Statement::ImplDecl(impl_def) = stmt {
                let implemented_methods: HashSet<String> = impl_def
                    .methods
                    .iter()
                    .filter_map(|m| {
                        if let Statement::Function { name, .. } = m {
                            Some(name.clone())
                        } else {
                            None
                        }
                    })
                    .collect();

                if let Some(trait_ast) = self.symbols.trait_ast_defs.get(&impl_def.trait_name) {
                    for (method_name, method_stmt_opt) in &trait_ast.methods {
                        if !implemented_methods.contains(method_name) {
                            if let Some(func_stmt) = method_stmt_opt {
                                synthesis_plan.push((
                                    i,
                                    func_stmt.clone(),
                                    impl_def.type_name.clone(),
                                ));
                            }
                        }
                    }
                }
            }
        }

        for (impl_index, mut synthesised_method, self_type) in synthesis_plan {
            println!(
                "[TypeChecker] Synthesising default method for type {:?}",
                self_type
            );

            let type_map = HashMap::from([("Self".to_string(), self_type)]);
            self.substitute_types_in_statement(&mut synthesised_method, &type_map);

            if let Some(Statement::ImplDecl(impl_def)) = self.program.statements.get_mut(impl_index)
            {
                impl_def.methods.push(synthesised_method);
            }
        }

        Ok(())
    }

    fn register_signatures(&mut self) -> Result<(), String> {
        for stmt in &self.program.statements {
            match stmt {
                Statement::Function {
                    name,
                    params,
                    return_type,
                    is_multi,
                    ..
                } => {
                    let sig = FunctionSignature {
                        param_types: params.iter().map(|(_, t)| t.clone()).collect(),
                        return_type: return_type.clone(),
                    };
                    if *is_multi {
                        self.symbols
                            .multi_functions
                            .entry(name.clone())
                            .or_default()
                            .push(sig);
                    } else {
                        self.symbols.add_function(name.clone(), sig);
                    }
                }

                Statement::StructDecl(s) => {
                    self.symbols.add_struct(s.name.clone(), s.fields.clone());
                }
                _ => {}
            }
        }
        Ok(())
    }

    fn types_compatible(&self, expected: &Type, actual: &Type) -> bool {
        expected == actual || (matches!(expected, Type::Float) && matches!(actual, Type::Int))
    }

    fn resolve_type(&mut self, ty: &Type) -> Result<Type, String> {
        if let Type::GenericInstance { name, type_args } = ty {
            let concrete_args = type_args
                .iter()
                .map(|arg| self.resolve_type(arg))
                .collect::<Result<Vec<_>, _>>()?;

            if self.symbols.generic_structs.contains_key(name) {
                let mangled_name = self.instantiate_struct(name, &concrete_args)?;
                return Ok(Type::Custom(mangled_name));
            }
        }
        Ok(ty.clone())
    }
}
