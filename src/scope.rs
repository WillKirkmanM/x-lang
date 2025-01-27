use std::collections::HashMap;

use inkwell::{builder::Builder, values::PointerValue};

pub struct ScopeStack<'ctx> {
    scopes: Vec<HashMap<String, PointerValue<'ctx>>>,
}

impl<'ctx> ScopeStack<'ctx> {
    pub fn new() -> Self {
        let mut scopes = Vec::new();
        scopes.push(HashMap::new());
        Self { scopes }
    }
    pub fn push_scope(&mut self) {
        self.scopes.push(HashMap::new());
    }
    pub fn pop_scope(&mut self) {
        if self.scopes.len() > 1 {
            self.scopes.pop();
        }
    }
    pub fn get(&self, name: &str) -> Option<&PointerValue<'ctx>> {
        for scope in self.scopes.iter().rev() {
            if let Some(val) = scope.get(name) {
                return Some(val);
            }
        }
        None
    }
    pub fn insert(&mut self, name: String, value: PointerValue<'ctx>) {
        if let Some(scope) = self.scopes.last_mut() {
            scope.insert(name, value);
        }
    }
    pub fn get_or_create(
        &mut self,
        name: &str,
        builder: &Builder<'ctx>,
        i32_type: inkwell::types::IntType<'ctx>,
    ) -> PointerValue<'ctx> {
        if let Some(ptr) = self.get(name) {
            *ptr
        } else {
            let alloca = builder.build_alloca(i32_type, name).unwrap();
            if self.scopes.is_empty() {
                self.push_scope();
            }
            self.scopes
                .last_mut()
                .unwrap()
                .insert(name.to_string(), alloca);
            alloca
        }
    }
    pub fn update(&mut self, name: &str, value: PointerValue<'ctx>) -> bool {
        for scope in self.scopes.iter_mut().rev() {
            if scope.contains_key(name) {
                scope.insert(name.to_string(), value);
                return true;
            }
        }
        false
    }
    pub fn get_mut(&mut self, name: &str) -> Option<&mut PointerValue<'ctx>> {
        for scope in self.scopes.iter_mut().rev() {
            if let Some(val) = scope.get_mut(name) {
                return Some(val);
            }
        }
        None
    }
}