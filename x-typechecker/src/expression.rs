use std::collections::HashMap;

use x_ast::{Expr, Operator, Statement, Type, UnaryOperator};

use crate::{FunctionSignature, TypeChecker};

impl TypeChecker {
    pub fn check_expression(&mut self, expr: &mut Expr) -> Result<Type, String> {
        match expr {
            Expr::FunctionCall { name, args } => {
                // First, evaluate argument types once
                let arg_types: Vec<Type> = args
                    .iter_mut()
                    .map(|a| self.check_expression(a))
                    .collect::<Result<_, _>>()?;

                // Handle generic instantiation if needed (non-multi generics)
                if self.symbols.generic_functions.contains_key(name) {
                    let mut type_map = HashMap::new();
                    let generic_func = self.symbols.generic_functions.get(name).unwrap();
                    for (param, arg_ty) in generic_func.params.iter().zip(arg_types.iter()) {
                        self.infer_types_from_match(&param.ty, arg_ty, &mut type_map)?;
                    }
                    let concrete_args: Vec<Type> = generic_func
                        .generic_params
                        .as_ref()
                        .unwrap()
                        .iter()
                        .map(|gp| type_map.get(gp).cloned().unwrap_or(Type::Unknown))
                        .collect();
                    if concrete_args.iter().any(|t| matches!(t, Type::Unknown)) {
                        return Err(format!(
                            "Could not infer all generic arguments for function '{}'",
                            name
                        ));
                    }
                    let mangled_name = self.instantiate_function(name, &concrete_args)?;
                    *name = mangled_name;
                }

                // Multi-dispatch resolution: if this name has multi variants, pick the matching one.
                if let Some(candidates) = self.symbols.multi_functions.get(name) {
                    let mut matches: Vec<&FunctionSignature> = candidates
                        .iter()
                        .filter(|sig| {
                            sig.param_types.len() == arg_types.len()
                                && sig
                                    .param_types
                                    .iter()
                                    .zip(arg_types.iter())
                                    .all(|(exp, act)| self.types_compatible(exp, act))
                        })
                        .collect();
                    match matches.len() {
                        0 => {
                            // Diagnostic: show expected signatures
                            let expects = candidates
                                .iter()
                                .map(|s| format!("{:?}", s.param_types))
                                .collect::<Vec<_>>()
                                .join(" | ");
                            return Err(format!(
                              "No matching multi overload for '{}'. Got {:?}, expected one of: {}",
                              name, arg_types, expects
                          ));
                        }
                        1 => {
                            return Ok(matches.remove(0).return_type.clone());
                        }
                        _ => {
                            return Err(format!(
                              "Ambiguous multi overload for '{}': arguments {:?} match multiple candidates",
                              name, arg_types
                          ));
                        }
                    }
                }

                // If the function signature is still not found, it might be an anonymous function
                if self.symbols.get_function(name).is_none() {
                    // Find the anonymous function definition in the program's statements.
                    let anon_fn_expr = self.program.statements.iter().find_map(|stmt| {
                        if let Statement::VariableDecl {
                            name: var_name,
                            value,
                            ..
                        } = stmt
                        {
                            if var_name == name {
                                if let Expr::AnonymousFunction { .. } = value {
                                    return Some(value.clone());
                                }
                            }
                        }
                        None
                    });

                    if let Some(Expr::AnonymousFunction {
                        params, mut body, ..
                    }) = anon_fn_expr
                    {
                        // Check for correct number of arguments.
                        if params.len() != arg_types.len() {
                            return Err(format!(
                                "Called function '{}' with {} arguments, but it is defined with {}",
                                name,
                                arg_types.len(),
                                params.len()
                            ));
                        }

                        self.symbols.enter_scope();

                        // Add parameters to the new scope using types from the call site.
                        for (param_def, arg_type) in params.iter().zip(arg_types.iter()) {
                            let param_name = &param_def;
                            self.symbols
                                .add_variable(param_name.to_string(), arg_type.clone());
                        }

                        // Type-check the function's body to find its return type.
                        // We conservatively set the inferred return type to Unknown unless we can detect a return.
                        let inferred_return_type = Type::Unknown;
                        for stmt in &mut body {
                            if let Statement::VariableDecl { value, .. } = stmt {
                                self.check_expression(value)?;
                            }
                        }

                        self.symbols.exit_scope();

                        // Create the complete signature with the inferred return type and add it.
                        let sig = FunctionSignature {
                            param_types: arg_types.clone(),
                            return_type: inferred_return_type,
                        };
                        self.symbols.add_function(name.clone(), sig);
                    }
                }

                // Regular function lookup/check
                // If the function name isn't registered but there's a variable
                // with the same name bound to an anonymous function, synthesise a
                // provisional signature using the call-site argument types. This
                // is a pragmatic fallback so `let mul = |x,y| { ... }; mul(4,1)`
                // can resolve without requiring parameter annotations.
                if self.symbols.get_function(name).is_none() {
                    for stmt in &self.program.statements {
                        if let Statement::VariableDecl {
                            name: var_name,
                            value,
                            ..
                        } = stmt
                        {
                            if var_name == name {
                                if let Expr::AnonymousFunction { .. } = value {
                                    // Use the already-computed arg_types as the parameter types.
                                    let sig = FunctionSignature {
                                        param_types: arg_types.clone(),
                                        return_type: Type::Unknown,
                                    };
                                    self.symbols.add_function(name.clone(), sig);
                                    break;
                                }
                            }
                        }
                    }
                }

                let sig = self
                    .symbols
                    .get_function(name)
                    .ok_or(format!("Unknown function {}", name))?
                    .clone();
                for (i, at) in arg_types.iter().enumerate() {
                    // Accept cases where a generic-instantiated parameter (GenericInstance)
                    // has been monomorphised into a concrete named struct like `Pair_i32_bool`.
                    // by treating `Custom` names that start with `"<GenericName>_"` as matches.

                    let expected = &sig.param_types[i];
                    // If the argument type is Unknown but the argument expression is a function call,
                    // try to recover the actual return type from the called function's signature or AST.
                    let mut actual = at.clone();
                    if actual == Type::Unknown {
                        if let Expr::FunctionCall {
                            name: inner_name, ..
                        } = &args[i]
                        {
                            // try symbol table first
                            if let Some(inner_sig) = self.symbols.get_function(inner_name) {
                                if inner_sig.return_type != Type::Unknown {
                                    actual = inner_sig.return_type.clone();
                                }
                            } else {
                                for stmt in &self.program.statements {
                                    if let Statement::Function {
                                        name: fn_name,
                                        return_type,
                                        ..
                                    } = stmt
                                    {
                                        if fn_name == inner_name && *return_type != Type::Unknown {
                                            actual = return_type.clone();
                                            break;
                                        }
                                    }
                                }
                            }
                        }
                    }

                    if let (Type::GenericInstance { name: gen_name, .. }, Type::Custom(cname)) =
                        (expected, at)
                    {
                        if cname.starts_with(&format!("{}_", gen_name)) {
                            continue;
                        }
                    }
                    // ref/coercion handling:
                    // 1. If both expected and actual are references, compare mutability + inner types.
                    // 2. If expected is a reference but actual isn't, that's an error.
                    // 3. If expected is non-reference but actual is a reference, unwrap the actual and compare.
                    match (expected, &actual) {
                        (
                            Type::Ref {
                                is_mut: exp_mut,
                                inner: exp_inner,
                                is_unique: _,
                            },
                            Type::Ref {
                                is_mut: act_mut,
                                inner: act_inner,
                                is_unique: _,
                            },
                        ) => {
                            // If callee expects a mutable reference, caller must provide a mutable one.
                            if *exp_mut && !*act_mut {
                                return Err(format!(
                                  "Type mismatch in call to '{}'. Expected mutable reference for argument {}, but got immutable reference.",
                                  name, i
                              ));
                            }
                            if !self.types_compatible(&*exp_inner, &*act_inner) {
                                return Err(format!(
                                  "Type mismatch in call to '{}'. Expected {:?} for argument {}, but got {:?}.",
                                  name, expected, i, at
                              ));
                            }
                        }
                        (Type::Ref { .. }, _other) => {
                            // expected a reference but caller passed a non-reference
                            return Err(format!(
                              "Type mismatch in call to '{}'. Expected reference type {:?} for argument {}, but got {:?}.",
                              name, expected, i, at
                          ));
                        }
                        (expected_nonref, Type::Ref { inner, .. }) => {
                            // caller passed a reference but callee expects a plain value -> allow by unwrapping
                            if !self.types_compatible(expected_nonref, &*inner) {
                                return Err(format!(
                                  "Type mismatch in call to '{}'. Expected {:?} for argument {}, but got {:?}.",
                                  name, expected_nonref, i, at
                              ));
                            }
                        }
                        (expected_nonref, other_actual) => {
                            if !self.types_compatible(expected_nonref, other_actual) {
                                return Err(format!(
                                  "Type mismatch in call to '{}'. Expected {:?} for argument {}, but got {:?}.",
                                  name, expected_nonref, i, at
                              ));
                            }
                        }
                    }
                }
                Ok(sig.return_type)
            }

            Expr::FieldAccess { object, field } => {
                let object_type = self.check_expression(object)?;

                let base_type = match &object_type {
                    Type::Ref { inner, .. } => &**inner,
                    _ => &object_type,
                };

                let struct_name = if let Type::Custom(s) = base_type {
                    s.clone()
                } else {
                    return Err(format!(
                        "Cannot access member on non-custom type {:?}",
                        object_type
                    ));
                };

                // 1. First, try to resolve it as a struct field
                if let Some(fields) = self.symbols.structs.get(&struct_name) {
                    if let Some((_, field_type)) = fields.iter().find(|(f_name, _)| f_name == field)
                    {
                        return Ok(field_type.clone());
                    }
                }

                // 2. If not a field, try to resolve it as a method from a trait
                if let Some(implemented_traits) = self.symbols.impls_for_type.get(base_type) {
                    for trait_name in implemented_traits {
                        if let Some(trait_def) = self.symbols.traits.get(trait_name) {
                            if trait_def.methods.contains_key(field) {
                                return Ok(Type::Unknown);
                            }
                        }
                    }
                }

                // 2b. If not a field or trait method, check for an inherent method.
                if let Some(_method_sig) = self.symbols.get_struct_method(&struct_name, field) {
                    return Ok(Type::Unknown);
                }

                // 3. If it's neither a field nor a known method, then it's an error.
                Err(format!(
                    "Struct '{}' has no field or method named '{}'",
                    struct_name, field
                ))
            }
            Expr::StructInstantiate(init) => {
                for (_, field_expr) in &mut init.fields {
                    self.check_expression(field_expr)?;
                }
                Ok(Type::Custom(init.name.clone()))
            }
            Expr::TypeLiteral(t) => {
                let resolved = self.resolve_type(t)?;

                *expr = Expr::TypeLiteral(resolved.clone());
                Ok(resolved)
            }
            Expr::Identifier(name) => self
                .symbols
                .get_variable(name)
                .cloned()
                .ok_or_else(|| format!("Unknown variable {}", name)),
            Expr::Int(_) => Ok(Type::Int),
            Expr::Float(_) => Ok(Type::Float),
            Expr::String(_) => Ok(Type::String),
            Expr::Boolean(_) => Ok(Type::Bool),
            Expr::BinaryOp { left, op, right } => {
                let l = self.check_expression(left)?;
                let r = self.check_expression(right)?;
                match op {
                    Operator::Add | Operator::Subtract | Operator::Multiply | Operator::Divide => {
                        if l == r && (l == Type::Int || l == Type::Float) {
                            Ok(l)
                        } else {
                            Err(format!(
                                "Arithmetic on incompatible types: {:?} and {:?}",
                                l, r
                            ))
                        }
                    }
                    _ => {
                        if l == r {
                            Ok(Type::Bool)
                        } else {
                            Err(format!(
                                "Comparison on incompatible types: {:?} and {:?}",
                                l, r
                            ))
                        }
                    }
                }
            }
            Expr::UnaryOp { op, expr } => {
                let inner_type = self.check_expression(expr)?;
                match op {
                    UnaryOperator::Negate => {
                        if inner_type == Type::Int || inner_type == Type::Float {
                            Ok(inner_type)
                        } else {
                            Err(format!(
                                "Cannot apply negation operator '-' to non-numeric type '{:?}'",
                                inner_type
                            ))
                        }
                    }
                    UnaryOperator::LogicalNot => {
                        if self.types_compatible(&Type::Bool, &inner_type) {
                            Ok(Type::Bool)
                        } else {
                            Err(format!(
                                "Cannot apply logical not '!' to non-boolean type '{:?}'",
                                inner_type
                            ))
                        }
                    }
                    UnaryOperator::BitwiseNot => {
                        if inner_type == Type::Int {
                            Ok(Type::Int)
                        } else {
                            Err(format!(
                                "Cannot apply bitwise not '~' to non-integer type '{:?}'",
                                inner_type
                            ))
                        }
                    }
                    _ => Err(format!("Unsupported unary operator '{:?}'", op)),
                }
            }
            Expr::AddressOf {
                is_mut,
                expr: inner,
            } => {
                let t = self.check_expression(inner)?;
                match &**inner {
                    Expr::Identifier(_) | Expr::FieldAccess { .. } => Ok(Type::Ref {
                        is_mut: *is_mut,
                        inner: Box::new(t),
                        is_unique: false,
                    }),
                    _ => Err("& can only take the address of an lvalue".to_string()),
                }
            }
            Expr::Deref { expr: inner } => {
                let t = self.check_expression(inner)?;
                if let Type::Ref { inner, .. } = t {
                    Ok(*inner)
                } else {
                    Err("Cannot deref a non-reference value".to_string())
                }
            }
            Expr::Assignment { target, value } => {
                self.check_expression(target)?;
                self.check_expression(value)?;
                Ok(Type::Void)
            }
            Expr::AnonymousFunction { body, .. } => {
                self.symbols.enter_scope();
                for stmt in body {
                    self.check_statement(stmt)?;
                }
                self.symbols.exit_scope();
                Ok(Type::Unknown)
            }
            _ => Err(format!("Unsupported expression type: {:?}", expr)),
        }
    }
}
