use std::{collections::HashMap, cell::RefCell, rc::Rc};

use crate::types::*;

#[derive(Debug, Clone)]
pub struct ContextCore<'a> {
    pub base_units: HashMap<&'a str, UnitAtom<'a>>,
    pub vars: HashMap<&'a str, Quantity<'a>>,
    pub parent: Option<Context<'a>>,
}

impl<'a> Default for ContextCore<'a> {
    fn default() -> Self {
        ContextCore {
            base_units: HashMap::new(),
            vars: HashMap::new(),
            parent: None,
        }
    }
}

impl<'a> ContextCore<'a> {
    pub fn assign(&mut self, name: &'a str, value: Quantity<'a>) -> Option<Quantity<'a>> {
        if self.base_units.contains_key(name) {
            None
        } else {
            self.vars.insert(name, value.clone());
            Some(value)
        }
    }

    pub fn var(&mut self, name: &'a str) -> Option<Quantity<'a>> {
        let found = self.vars.get_mut(name).map(|x| x.clone());

        if found.is_none() {
            if let Some(p) = &mut self.parent {
                return p.var(name);
            }
        }

        found
    }

    pub fn define(&mut self, name: &'a str) -> Option<Quantity<'a>> {
        if self.base_units.contains_key(name) {
            None
        } else {
            self.base_units.insert(name, UnitAtom::base(name));
            Some(UnitAtom::base(name).into())
        }
    }

    pub fn unit(&mut self, name: &'a str) -> Option<Quantity<'a>> {
        // self.base_units.get(name).map(|x| x.clone().into())
        let found = self.base_units.get_mut(name).map(|x| x.clone().into());

        if found.is_none() {
            if let Some(p) = &mut self.parent {
                return p.unit(name);
            }
        }

        found
    }

    pub fn unit_or_var(&mut self, name: &'a str) -> Option<Quantity<'a>> {
        if let Some(x) = self.unit(name) {
            Some(x)
        } else if let Some(x) = self.var(name) {
            Some(x)
        } else {
            None
        }
    }
}

#[derive(Debug, Clone)]
pub struct Context<'a>(Rc<RefCell<ContextCore<'a>>>);

impl<'a> Context<'a> {
    pub fn new() -> Self {
        Context(Rc::new(RefCell::new(Default::default())))
    }

    pub fn from_parent(parent: Context<'a>) -> Self {
        let core = ContextCore {
            base_units: HashMap::new(),
            vars: HashMap::new(),
            parent: Some(parent)
        };

        Context(Rc::new(RefCell::new(core)))
    }

    #[allow(unused_assignments)]
    pub fn into_parent(mut self) {
        let parent = self.0.borrow().clone().parent;

        match parent {
            Some(parent) => self = parent,
            None => panic!("not parent was defined!"),
        }
    }

    pub fn assign(&mut self, name: &'a str, value: Quantity<'a>) -> Option<Quantity<'a>> {
        self.0.borrow_mut().assign(name, value)
    }

    pub fn var(&mut self, name: &'a str) -> Option<Quantity<'a>> {
        self.0.borrow_mut().var(name)
    }

    pub fn define(&mut self, name: &'a str) -> Option<Quantity<'a>> {
        self.0.borrow_mut().define(name)
    }

    pub fn unit(&mut self, name: &'a str) -> Option<Quantity<'a>> {
        self.0.borrow_mut().unit(name)
    }

    pub fn unit_or_var(&mut self, name: &'a str) -> Option<Quantity<'a>> {
        self.0.borrow_mut().unit_or_var(name)
    }
}

impl<'a> Node<'a> {
    pub fn eval(self, mut cntxt: Context<'a>) -> Option<Quantity<'a>> {
        use NodeType::*;

        match self.typ {
            Def(name) => cntxt.define(name),
            Add(lhs, rhs) => lhs.eval(cntxt.clone())? + rhs.eval(cntxt)?,
            Sub(lhs, rhs) => lhs.eval(cntxt.clone())? - rhs.eval(cntxt)?,
            Mul(lhs, rhs) => lhs.eval(cntxt.clone())? * rhs.eval(cntxt)?,
            Div(lhs, rhs) => lhs.eval(cntxt.clone())? / rhs.eval(cntxt)?,
            Unit(name) => cntxt.unit_or_var(name),
            Num(num) => Some(Quantity::num(num)),
            Assign(name, val) => {
                let val = val.eval(cntxt.clone())?;
                cntxt.assign(name, val)
            }
            AddAssign(lhs, rhs) => {
                let var = cntxt.var(lhs)?;
                let res = var + rhs.eval(cntxt.clone())?;
                cntxt.assign(lhs, res?)
            }
            SubAssign(lhs, rhs) => {
                let var = cntxt.var(lhs)?;
                let res = var - rhs.eval(cntxt.clone())?;
                cntxt.assign(lhs, res?)
            }
            MulAssign(lhs, rhs) => {
                let var = cntxt.var(lhs)?;
                let res = var * rhs.eval(cntxt.clone())?;
                cntxt.assign(lhs, res?)
            }
            DivAssign(lhs, rhs) => {
                let var = cntxt.var(lhs)?;
                let res = var / rhs.eval(cntxt.clone())?;
                cntxt.assign(lhs, res?)
            }
            Scope(vec) => {
                let mut res: Option<Quantity<'a>> = None;

                cntxt = Context::from_parent(cntxt);

                for e in vec {
                    res = Some(e.eval(cntxt.clone())?);
                }

                cntxt.into_parent();

                res
            }
            Err => unreachable!(),
        }
    }
}
