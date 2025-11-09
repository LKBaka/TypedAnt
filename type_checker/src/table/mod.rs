use std::{cell::RefCell, collections::HashMap, rc::Rc};

use crate::Type;

pub enum SymbolScope {
    Global, Local
}

#[derive(Debug, Clone)]
pub enum SymbolType {
    Variable(Type),
    Function {
        params_type: Vec<Type>,
        ret_type: Type,
    },
}

pub struct Symbol {
    pub name: Rc<str>,
    pub ty: SymbolType,
}

pub struct TypeTable {
    pub outer: Option<Rc<RefCell<TypeTable>>>,

    var_map: HashMap<Rc<str>, Symbol>,
}

impl TypeTable {
    pub fn new() -> Self {
        Self {
            outer: None,
            var_map: HashMap::new(),
        }
    }

    pub fn get(&self, name: &str) -> Option<&Symbol> {
        match self.var_map.get(name) {
            Some(it) => Some(it),
            None => return None
        }
    }

    pub fn insert_var(&mut self, symbol: Symbol) {
        self.var_map.insert(symbol.name.clone(), symbol);
    }

    pub fn define_var(&mut self, name: &str, var_ty: Type) -> Symbol {
        Symbol {
            name: name.into(),
            ty: SymbolType::Variable(var_ty)
        }
    }
}