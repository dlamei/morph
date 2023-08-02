use std::{cell::RefCell, collections::HashMap, ops::Range, rc::Rc};

use crate::error::*;
use crate::types::*;

#[derive(Debug, Clone, Default)]
pub struct ContextCore<'a> {
    pub base_units: HashMap<&'a str, Quantity<'a>>,
    pub vars: HashMap<&'a str, Quantity<'a>>,
    pub parent: Option<Context<'a>>,
}

impl<'a> ContextCore<'a> {
    pub fn assign(
        &mut self,
        name: &'a str,
        mut value: Quantity<'a>,
        span: Range<usize>,
    ) -> RuntimeResult<'a> {
        if self.base_units.contains_key(name) {
            Err(MorphError::custom(
                value.span.into(),
                format!("base unit {} is not assignable", name),
                ErrorType::UnsupportedAttribute,
            ))
        } else {
            value.span = span;
            self.vars.insert(name, value.clone());
            Ok(value)
        }
    }

    pub fn var(&mut self, name: &'a str, span: Range<usize>) -> RuntimeResult<'a> {
        let found = self.vars.get_mut(name).map(|x| {
            let mut ret = x.clone();
            ret.span = span.clone();
            ret
        });

        if found.is_none() {
            if let Some(p) = &mut self.parent {
                return p.var(name, span);
            }
        }

        found.ok_or(MorphError::custom(
            span.into(),
            format!("use of undeclared variable '{}'", name),
            ErrorType::UndefinedIdent,
        ))
    }

    pub fn define(&mut self, name: &'a str, span: Range<usize>) -> RuntimeResult<'a> {
        if self.base_units.contains_key(name) {
            Err(MorphError::custom(
                span.into(),
                format!("base unit {} is not redefinable", name),
                ErrorType::UnsupportedAttribute,
            ))
        } else {
            self.base_units
                .insert(name, Quantity::base(name, span.clone()));
            Ok(Quantity::base(name, span))
        }
    }

    pub fn unit(&mut self, name: &'a str, span: Range<usize>) -> RuntimeResult<'a> {
        // self.base_units.get(name).map(|x| x.clone().into())
        let found = self.base_units.get_mut(name).map(|x| {
            let mut ret = x.clone();
            ret.span = span.clone();
            ret
        });

        if found.is_none() {
            if let Some(p) = &mut self.parent {
                return p.unit(name, span);
            }
        }

        found.ok_or(MorphError::custom(
            span.into(),
            format!("use of undeclared unit '{}'", name),
            ErrorType::UndefinedIdent,
        ))
    }

    pub fn unit_or_var(&mut self, name: &'a str, span: Range<usize>) -> RuntimeResult<'a> {
        if let Ok(x) = self.unit(name, span.clone()) {
            Ok(x)
        } else if let Ok(x) = self.var(name, span.clone()) {
            Ok(x)
        } else {
            Err(MorphError::custom(
                span.into(),
                format!("use of undeclared unit/variable '{}'", name),
                ErrorType::UndefinedIdent,
            ))
        }
    }
}

#[derive(Debug, Clone)]
pub struct Context<'a>(Rc<RefCell<ContextCore<'a>>>);

impl<'a> Context<'a> {
    pub fn new() -> Self {
        Context(Rc::new(RefCell::new(Default::default())))
    }

    pub fn from_parent(parent: &Context<'a>) -> Self {
        let core = ContextCore {
            base_units: HashMap::new(),
            vars: HashMap::new(),
            parent: Some(parent.clone()),
        };

        Context(Rc::new(RefCell::new(core)))
    }

    // #[allow(unused_assignments)]
    // pub fn into_parent(mut self) {
    //     let parent = self.0.borrow().clone().parent;

    //     match parent {
    //         Some(parent) => self = parent,
    //         None => panic!("not parent was defined!"),
    //     }
    // }

    pub fn assign(
        &mut self,
        name: &'a str,
        value: Quantity<'a>,
        span: Range<usize>,
    ) -> RuntimeResult<'a> {
        self.0.borrow_mut().assign(name, value, span)
    }

    pub fn var(&mut self, name: &'a str, span: Range<usize>) -> RuntimeResult<'a> {
        self.0.borrow_mut().var(name, span)
    }

    pub fn define(&mut self, name: &'a str, span: Range<usize>) -> RuntimeResult<'a> {
        self.0.borrow_mut().define(name, span)
    }

    pub fn unit(&mut self, name: &'a str, span: Range<usize>) -> RuntimeResult<'a> {
        self.0.borrow_mut().unit(name, span)
    }

    pub fn unit_or_var(&mut self, name: &'a str, span: Range<usize>) -> RuntimeResult<'a> {
        self.0.borrow_mut().unit_or_var(name, span)
    }
}

impl<'a> Node<'a> {
    pub fn eval(self, cntxt: &mut Context<'a>) -> RuntimeResult<'a> {
        use NodeType::*;

        let span = self.span.clone();

        match self.typ {
            Def(name) => cntxt.define(name, span),
            Add(lhs, rhs) => lhs.eval(cntxt)? + rhs.eval(cntxt)?,
            Sub(lhs, rhs) => lhs.eval(cntxt)? - rhs.eval(cntxt)?,
            Mul(lhs, rhs) => lhs.eval(cntxt)? * rhs.eval(cntxt)?,
            Div(lhs, rhs) => lhs.eval(cntxt)? / rhs.eval(cntxt)?,
            Pow(_, _) => todo!(),
            UnrySub(val) => {
                let neg = -val.eval(cntxt)?;
                neg
            }
            UnryNot(_) => todo!(),
            Unit(name) => cntxt.unit_or_var(name, span),
            Num(num) => Ok(Quantity::num(num, span)),
            Assign(name, val) => {
                let val = val.eval(cntxt)?;
                cntxt.assign(name, val, span)
            }
            AddAssign(lhs, rhs) => {
                let var = cntxt.var(lhs, span.clone())?;
                let res = var + rhs.eval(cntxt)?;
                cntxt.assign(lhs, res?, span)
            }
            SubAssign(lhs, rhs) => {
                let var = cntxt.var(lhs, span.clone())?;
                let res = var - rhs.eval(cntxt)?;
                cntxt.assign(lhs, res?, span)
            }
            MulAssign(lhs, rhs) => {
                let var = cntxt.var(lhs, span.clone())?;
                let res = var * rhs.eval(cntxt)?;
                cntxt.assign(lhs, res?, span)
            }
            DivAssign(lhs, rhs) => {
                let var = cntxt.var(lhs, span.clone())?;
                let res = var / rhs.eval(cntxt)?;
                cntxt.assign(lhs, res?, span)
            }
            Scope(vec) => {
                let mut res: Option<RuntimeResult<'a>> = None;

                let mut child = Context::from_parent(cntxt);

                for e in vec {
                    res = Some(Ok(e.eval(&mut child)?));
                }

                res.unwrap_or(Ok(Quantity::num(0, span)))
            }
            ParseError => Err(MorphError::custom(
                span.into(),
                "encountered error from parse stage",
                ErrorType::UndefinedSyntax,
            )),
        }
    }
}
