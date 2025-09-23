use std::collections::HashMap;
use x_ast::{Expr, Program, Statement, Type};

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash, Default)]
struct Region(usize);

#[derive(Clone, Debug)]
struct Borrow {
    region: Region,
    is_mut: bool,
}

#[derive(Default)]
struct Scope {
    region: Region,
    vars: HashMap<String, VarInfo>,
}

#[derive(Clone, Debug)]
struct VarInfo {
    ty: Type,
    initialised: bool,
    moved: bool,
    borrows: Vec<Borrow>,
    field_borrows: HashMap<String, Vec<Borrow>>,
    ref_target: Option<(String, Region, bool)>,
    field_ref_targets: HashMap<String, (String, Region, bool)>,
}

impl Default for VarInfo {
    fn default() -> Self {
        Self {
            ty: Type::Unknown,
            initialised: false,
            moved: false,
            borrows: Vec::new(),
            field_borrows: HashMap::new(),
            ref_target: None,
            field_ref_targets: HashMap::new(),
        }
    }
}

pub struct BorrowChecker {
    scopes: Vec<Scope>,
    next_region_id: usize,
}

impl BorrowChecker {
    pub fn new() -> Self {
        Self {
            scopes: vec![Scope {
                region: Region(0),
                ..Default::default()
            }],
            next_region_id: 1,
        }
    }

    fn current_region(&self) -> Region {
        self.scopes.last().unwrap().region
    }
    fn enter_scope(&mut self) {
        let r = Region(self.next_region_id);
        self.next_region_id += 1;
        self.scopes.push(Scope {
            region: r,
            ..Default::default()
        });
    }
    fn exit_scope(&mut self) -> Result<(), String> {
        let dead = self.scopes.pop().expect("at least one scope");
        // Drop borrows created in this region
        for scope in self.scopes.iter_mut().rev() {
            for vi in scope.vars.values_mut() {
                vi.borrows.retain(|b| b.region != dead.region);
                // Any outer var holding a ref into `dead.region` is an error (escape)
                if let Some((_tgt, reg, _m)) = vi.ref_target.as_ref() {
                    if *reg == dead.region {
                        return Err("Reference does not live long enough (borrowed value does not outlive the current scope)".to_string());
                    }
                }
            }
        }
        Ok(())
    }

    fn declare_var(&mut self, name: &str, ty: Type) {
        let scope = self.scopes.last_mut().unwrap();
        scope.vars.insert(
            name.to_string(),
            VarInfo {
                ty,
                initialised: true,
                moved: false,
                borrows: Vec::new(),
                field_borrows: HashMap::new(),
                ref_target: None,
                field_ref_targets: HashMap::new(),
            },
        );
    }

    fn lookup_var_mut(&mut self, name: &str) -> Option<(&mut VarInfo, Region)> {
        for s in self.scopes.iter_mut().rev() {
            if let Some(v) = s.vars.get_mut(name) {
                return Some((v, s.region));
            }
        }
        None
    }
    fn lookup_var(&self, name: &str) -> Option<(&VarInfo, Region)> {
        for s in self.scopes.iter().rev() {
            if let Some(v) = s.vars.get(name) {
                return Some((v, s.region));
            }
        }
        None
    }

    fn is_copy_type(t: &Type) -> bool {
        matches!(
            t,
            Type::Int | Type::Float | Type::Bool | Type::String | Type::Ref { .. }
        )
    }

    pub fn check(&mut self, program: &Program) -> Result<(), String> {
        for s in &program.statements {
            self.check_statement(s)?;
        }
        Ok(())
    }

    fn check_statement(&mut self, stmt: &Statement) -> Result<(), String> {
        match stmt {
            Statement::StructDecl(_) => Ok(()),
            Statement::Function { params, body, .. } => {
                self.enter_scope();
                for param in params {
                    self.declare_var(&param.name, param.ty.clone());
                }
                for st in body.as_ref().unwrap().iter() {
                    self.check_statement(st)?;
                }
                self.exit_scope()
            }
            Statement::Block { statements } => {
                self.enter_scope();
                for st in statements {
                    self.check_statement(st)?;
                }
                self.exit_scope()
            }
            Statement::VariableDecl {
                name,
                type_ann,
                value,
            } => {
                // Check RHS effects first
                self.check_expr(value)?;

                // A variable is only initialised if it's not just a type literal placeholder.
                let is_initialised = !matches!(value, Expr::TypeLiteral(_));

                // Moves from identifiers
                if let Expr::Identifier(src) = value {
                    let dst_ty = type_ann.clone().unwrap_or_else(|| {
                        self.lookup_var(src)
                            .map(|(v, _)| v.ty.clone())
                            .unwrap_or(Type::Unknown)
                    });
                    if is_initialised {
                        if !Self::is_copy_type(&dst_ty) {
                            self.move_ident(src)?;
                        } else {
                            self.use_ident(src)?;
                        }
                    }
                }

                // If RHS is a reference (&) or a mutable reference (&mut), record the borrow and reference target
                let mut vi = VarInfo {
                    ty: type_ann.clone().unwrap_or(Type::Unknown),
                    initialised: is_initialised,
                    moved: false,
                    borrows: Vec::new(),
                    field_borrows: HashMap::new(),
                    ref_target: None,
                    field_ref_targets: HashMap::new(),
                };

                if let Expr::AddressOf { is_mut, expr } = value {
                    match &**expr {
                        Expr::Identifier(tgt) => {
                            let is_unique_borrow = matches!(
                                type_ann,
                                Some(Type::Ref {
                                    is_unique: true,
                                    ..
                                })
                            );
                            vi.ty = Type::Ref {
                                is_mut: *is_mut,
                                is_unique: is_unique_borrow,
                                inner: Box::new(
                                    self.lookup_var(tgt)
                                        .map(|(v, _)| v.ty.clone())
                                        .unwrap_or(Type::Unknown),
                                ),
                            };
                            vi.ref_target = Some((tgt.clone(), self.current_region(), *is_mut));
                        }
                        Expr::FieldAccess {
                            object,
                            field: _field,
                        } => {
                            if let Expr::Identifier(tgt) = &**object {
                                let is_unique_borrow = matches!(
                                    type_ann,
                                    Some(Type::Ref {
                                        is_unique: true,
                                        ..
                                    })
                                );

                                vi.ty = Type::Ref {
                                    is_mut: *is_mut,
                                    is_unique: is_unique_borrow,
                                    inner: Box::new(
                                        self.lookup_var(tgt)
                                            .map(|(v, _)| v.ty.clone())
                                            .unwrap_or(Type::Unknown),
                                    ),
                                };
                                vi.ref_target = Some((tgt.clone(), self.current_region(), *is_mut));
                            }
                        }
                        _ => {}
                    }
                }
                // If copying an existing ref, propagate its ref_target
                if let Expr::Identifier(src) = value {
                    if let Some((src_vi, _)) = self.lookup_var(src) {
                        if matches!(src_vi.ty, Type::Ref { .. }) {
                            vi.ref_target = src_vi.ref_target.clone();
                            vi.ty = src_vi.ty.clone();
                        }
                    }
                }

                self.scopes
                    .last_mut()
                    .unwrap()
                    .vars
                    .insert(name.clone(), vi);
                Ok(())
            }
            Statement::Return { value } => {
                if let Some(v) = value {
                    if self.is_reference_expr(v)? {
                        return Err("Cannot return a reference value (lifetime may not live long enough). Add explicit lifetime support first.".to_string());
                    }
                    self.check_expr(v)?;
                }
                Ok(())
            }
            Statement::Expression { expr } => self.check_expr(expr),
            _ => Ok(()),
        }
    }

    fn is_reference_expr(&self, e: &Expr) -> Result<bool, String> {
        match e {
            Expr::AddressOf { .. } => Ok(true),
            Expr::Identifier(name) => Ok(self
                .lookup_var(name)
                .map_or(false, |(v, _)| matches!(v.ty, Type::Ref { .. }))),
            _ => Ok(false),
        }
    }

    fn create_borrow(
        &mut self,
        name: &str,
        field: Option<&str>,
        is_mut: bool,
    ) -> Result<(), String> {
        let region = self.current_region();
        let (vi, _) = self
            .lookup_var_mut(name)
            .ok_or_else(|| format!("Borrow of undeclared variable '{}'", name))?;
        if vi.moved {
            return Err(format!("Cannot borrow moved value '{}'", name));
        }
        if let Some(f) = field {
            let fb = vi.field_borrows.entry(f.to_string()).or_default();
            if is_mut {
                if !fb.is_empty() {
                    return Err(format!(
                        "Cannot mutably borrow '{}.{}' because it is already borrowed",
                        name, f
                    ));
                }
            } else {
                if fb.iter().any(|b| b.is_mut) {
                    return Err(format!(
                        "Cannot immutably borrow '{}.{}' because it is already mutably borrowed",
                        name, f
                    ));
                }
            }
            fb.push(Borrow { region, is_mut });
        } else {
            if is_mut {
                if !vi.borrows.is_empty() {
                    return Err(format!(
                        "Cannot mutably borrow '{}' because it is already borrowed",
                        name
                    ));
                }
            } else if vi.borrows.iter().any(|b| b.is_mut) {
                return Err(format!(
                    "Cannot immutably borrow '{}' because it is already mutably borrowed",
                    name
                ));
            }
            vi.borrows.push(Borrow { region, is_mut });
        }
        Ok(())
    }

    fn move_ident(&mut self, name: &str) -> Result<(), String> {
        let (vi, _) = self
            .lookup_var_mut(name)
            .ok_or_else(|| format!("Use of undeclared variable '{}'", name))?;
        if vi.moved {
            return Err(format!(
                "Variable '{}' was moved and cannot be used again",
                name
            ));
        }
        if !vi.borrows.is_empty() {
            return Err(format!("Cannot move '{}' because it is borrowed", name));
        }
        vi.moved = true;
        Ok(())
    }
    fn use_ident(&mut self, name: &str) -> Result<(), String> {
        let (vi, _) = self
            .lookup_var_mut(name)
            .ok_or_else(|| format!("Use of undeclared variable '{}'", name))?;
        if vi.moved {
            return Err(format!("Use-after-move of variable '{}'", name));
        }
        Ok(())
    }

    fn check_expr(&mut self, expr: &Expr) -> Result<(), String> {
        match expr {
            Expr::Identifier(name) => self.use_ident(name),
            Expr::AddressOf { is_mut, expr } => match &**expr {
                Expr::Identifier(tgt) => self.create_borrow(tgt, None, *is_mut),
                Expr::FieldAccess { object, field } => {
                    if let Expr::Identifier(tgt) = &**object {
                        self.create_borrow(tgt, Some(field.as_str()), *is_mut)
                    } else {
                        Err("& on non-lvalue".to_string())
                    }
                }
                _ => Err("& on non-lvalue".to_string()),
            },
            Expr::Deref { expr } => self.check_expr(expr),
            Expr::FunctionCall { args, .. } => {
                // Evaluate call arguments in a temporary region so any borrows produced
                // by taking addresses (AddressOf) during argument evaluation are
                // released immediately after the call returns. Moves and other
                // effects (like marking a variable as moved) still persist.
                self.enter_scope();
                for a in args {
                    if let Expr::Identifier(id) = a {
                        let ty = self
                            .lookup_var(id)
                            .map(|(v, _)| v.ty.clone())
                            .unwrap_or(Type::Unknown);
                        if !Self::is_copy_type(&ty) {
                            // moving the identifier is a global effect (persisting outside temp region)
                            self.move_ident(id)?;
                            continue;
                        }
                    }
                    // this may create borrows (AddressOf) which get recorded in the
                    // temporary region and will be dropped by exit_scope below.
                    self.check_expr(a)?;
                }
                // Drop borrows created while evaluating the call arguments.
                self.exit_scope()?;
                Ok(())
            }
            Expr::FieldAccess { object, .. } => self.check_expr(object),
            Expr::Array(elements) => {
                for e in elements {
                    self.check_expr(e)?;
                }
                Ok(())
            }
            Expr::ArrayAccess { array, index } => {
                self.check_expr(array)?;
                self.check_expr(index)
            }
            Expr::BinaryOp { left, right, .. } => {
                self.check_expr(left)?;
                self.check_expr(right)
            }
            Expr::UnaryOp { expr, .. } => self.check_expr(expr),
            Expr::Int(_)
            | Expr::Float(_)
            | Expr::Boolean(_)
            | Expr::String(_)
            | Expr::TypeLiteral(_) => Ok(()),
            Expr::Assignment { target, value } => {
                self.check_expr(value)?;

                let new_ref_target: Option<(String, Region, bool)> = match &**value {
                    Expr::AddressOf {
                        is_mut,
                        expr: ref_expr,
                    } => {
                        if let Expr::Identifier(ref_tgt_name) = &**ref_expr {
                            let (_, ref_tgt_region) =
                                self.lookup_var(ref_tgt_name).ok_or_else(|| {
                                    format!("Cannot find target of reference '{}'", ref_tgt_name)
                                })?;
                            Some((ref_tgt_name.clone(), ref_tgt_region, *is_mut))
                        } else {
                            None
                        }
                    }
                    Expr::Identifier(src_name) => self
                        .lookup_var(src_name)
                        .and_then(|(src_vi, _)| src_vi.ref_target.clone()),
                    _ => None,
                };

                match &**target {
                    Expr::Identifier(name) => {
                        let (vi, _) = self
                            .lookup_var_mut(name)
                            .ok_or_else(|| format!("Assignment to undeclared variable '{}'", name))?;

                        if vi.moved {
                            return Err(format!("Cannot assign to moved variable '{}'", name));
                        }

                        vi.ref_target = new_ref_target;

                        vi.initialised = true;
                        Ok(())
                    }
                    Expr::FieldAccess { object, field } => {
                        if let Expr::Identifier(obj_name) = &**object {
                            let (vi, _) = self
                                .lookup_var_mut(obj_name)
                                .ok_or_else(|| format!("Assignment to undeclared variable '{}'", obj_name))?;

                            if vi.moved {
                                return Err(format!("Cannot assign to field of moved variable '{}'", obj_name));
                            }

                            if !vi.borrows.is_empty() {
                                return Err(format!("Cannot assign to '{}.{}' because '{}' is currently borrowed", obj_name, field, obj_name));
                            }
                            if let Some(fb) = vi.field_borrows.get(field) {
                                if !fb.is_empty() {
                                    return Err(format!("Cannot assign to '{}.{}' because that field is currently borrowed", obj_name, field));
                                }
                            }

                            if let Some(rt) = new_ref_target {
                                vi.field_ref_targets.insert(field.clone(), rt);
                            }

                            vi.initialised = true;
                            Ok(())
                        } else {
                            Err("Assignment to non-identifier field owners is not supported".to_string())
                        }
                    }
                    _ => Err("Assignment to non-identifier l-values is not yet supported by the borrow checker".to_string()),
                }
            }
            Expr::StructInstantiate(init) => {
                for (_, field_expr) in &init.fields {
                    self.check_expr(field_expr)?;
                }
                Ok(())
            }
            Expr::AnonymousFunction { .. } => Ok(()),
        }
    }
}
