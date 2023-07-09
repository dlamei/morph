use std::{cmp, fmt, ops, ops::Range, str::FromStr, stringify};

use logos::Logos;
use paste::paste;

// use num_traits::Pow;
// use num_traits::pow::Pow;
use rust_decimal::prelude::*;
use rust_decimal_macros::dec;

pub type NumType = Decimal;

use crate::error::*;

fn decimal<'a>(lex: &mut logos::Lexer<'a, Token<'a>>) -> Option<Decimal> {
    Decimal::from_str(lex.slice()).ok()
}

#[derive(Logos, Debug, PartialEq, Clone)]
#[logos(skip r"([ \t\f]+|//.*)")]
#[logos(subpattern unicode_ident = r"\p{XID_Start}\p{XID_Continue}*")]
#[logos(subpattern ascii_ident = r"[_a-zA-Z][_0-9a-zA-Z]*")]
pub enum Token<'a> {
    #[token("*")]
    Mul,
    #[token("/")]
    Div,
    #[token("+")]
    Add,
    #[token("-")]
    Sub,
    #[token("^")]
    Pow,
    #[token("!")]
    Not,
    #[token("=")]
    Assign,
    #[token("+=")]
    AddAssign,
    #[token("-=")]
    SubAssign,
    #[token("*=")]
    MulAssign,
    #[token("/=")]
    DivAssign,

    #[token("==")]
    Equal,
    #[token("!=")]
    NeEqual,
    #[token(">=")]
    GreaterEqual,
    #[token("<=")]
    LesserEqual,
    #[token(">")]
    Greater,
    #[token("<")]
    Lesser,

    #[token("(")]
    LParen,
    #[token(")")]
    RParen,
    #[token("{")]
    LCurly,
    #[token("}")]
    RCurly,

    #[regex("def")]
    Def,
    #[regex("if")]
    If,
    #[regex("else")]
    Else,

    #[regex("(?&unicode_ident)", |lex| lex.slice())]
    Unit(&'a str),

    #[regex(r"(([0-9]+)(\.[0-9]+))", decimal)]
    #[regex("[0-9]+", decimal)]
    Num(NumType),

    #[regex(r";|\n")]
    NL,

    LexErr(&'a str),
}

impl<'a> Token<'a> {
    pub const NUM: Self = Self::Num(dec!(0));
    pub const UNIT: Self = Self::Unit("...");
}

impl fmt::Display for Token<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use Token::*;

        let res = match self {
            Mul => "*",
            Div => "/",
            Add => "+",
            Sub => "-",
            Pow => "^",
            Not => "!",
            Assign => "=",
            AddAssign => "+=",
            SubAssign => "-=",
            MulAssign => "*=",
            DivAssign => "/=",
            LParen => "(",
            RParen => ")",
            LCurly => "{",
            RCurly => "}",
            Def => "def",
            If => "if",
            Else => "else",
            Unit(_) => "UNIT",
            Num(_) => "NUM",
            NL => r"(\n or ;)",
            LexErr(msg) => return write!(f, "Lexer Error: {msg}"),
            Equal => "==",
            NeEqual => "!=",
            GreaterEqual => ">=",
            LesserEqual => "<=",
            Greater => ">",
            Lesser => "<",
        };

        write!(f, "{}", res)
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum NodeType<'a> {
    Def(&'a str),
    IfElse {
        cond: Box<Node<'a>>,
        ifBody: Box<Node<'a>>,
        elseBody: Option<Box<Node<'a>>>,
    },

    Add(Box<Node<'a>>, Box<Node<'a>>),
    Sub(Box<Node<'a>>, Box<Node<'a>>),
    Mul(Box<Node<'a>>, Box<Node<'a>>),
    Div(Box<Node<'a>>, Box<Node<'a>>),
    Pow(Box<Node<'a>>, Box<Node<'a>>),

    UnryNot(Box<Node<'a>>),
    UnrySub(Box<Node<'a>>),

    Unit(&'a str),
    Num(NumType),

    Assign(&'a str, Box<Node<'a>>),
    AddAssign(&'a str, Box<Node<'a>>),
    SubAssign(&'a str, Box<Node<'a>>),
    MulAssign(&'a str, Box<Node<'a>>),
    DivAssign(&'a str, Box<Node<'a>>),

    Equal(Box<Node<'a>>, Box<Node<'a>>),
    NeEqual(Box<Node<'a>>, Box<Node<'a>>),
    GreaterEqual(Box<Node<'a>>, Box<Node<'a>>),
    LesserEqual(Box<Node<'a>>, Box<Node<'a>>),
    Greater(Box<Node<'a>>, Box<Node<'a>>),
    Lesser(Box<Node<'a>>, Box<Node<'a>>),

    Scope(Vec<Node<'a>>),

    ParseError,
}

impl<'a> NodeType<'a> {
    pub fn if_else(cond: Box<Node<'a>>, if_body: Box<Node<'a>>, else_body: Option<Box<Node<'a>>>) -> Self {
        NodeType::IfElse { cond, ifBody: if_body, elseBody: else_body }
    }
}

pub fn merge_span(r1: &Range<usize>, r2: &Range<usize>) -> Range<usize> {
    let mut smaller = r1;
    let mut bigger = r2;
    if r2.start < r1.start {
        smaller = r2;
        bigger = r1;
    }

    smaller.start..bigger.end
}

#[derive(Debug, Clone)]
pub struct Node<'a> {
    pub typ: NodeType<'a>,
    pub span: Range<usize>,
}

macro_rules! wrap_binop_node {
    (binop: $fn_name: ident -> $op: ident) => {
        pub fn $fn_name(lhs: Node<'a>, rhs: Node<'a>) -> Self {
            let span = merge_span(&lhs.span, &rhs.span);
            let typ = NodeType::$op(lhs.into(), rhs.into());
            Node { typ, span }
        }
    };
}

impl<'a> Node<'a> {
    pub fn new<I: Into<Range<usize>>>(typ: NodeType<'a>, range: I) -> Self {
        Self {
            typ,
            span: range.into(),
        }
    }

    pub fn err<I: Into<Range<usize>>>(range: I) -> Self {
        Self {
            typ: NodeType::ParseError,
            span: range.into(),
        }
    }

    #[allow(dead_code)]
    pub fn assign(&mut self, other: Node<'a>) {
        if let NodeType::Unit(name) = self.typ {
            let range = merge_span(&self.span, &other.span);
            let typ = NodeType::Assign(name, other.into());
            *self = Node { typ, span: range };
        } else {
            panic!("You can only Assign to the Node::Unit enum");
        }
    }

    wrap_binop_node!(binop: equal -> Equal);
    wrap_binop_node!(binop: mul -> Mul);
    wrap_binop_node!(binop: div -> Div);
    wrap_binop_node!(binop: add -> Add);
    wrap_binop_node!(binop: sub -> Sub);
    wrap_binop_node!(binop: pow -> Pow);
}

impl<'a> From<Node<'a>> for NodeType<'a> {
    fn from(n: Node<'a>) -> Self {
        n.typ
    }
}

impl<'a> fmt::Display for Node<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        use NodeType::*;

        match &self.typ {
            Def(name) => write!(f, "(def {})", name),
            IfElse { cond, ifBody, elseBody} => {
                write!(f, "if {} {} ", cond, ifBody)?;
                if let Some(body) = elseBody {
                    write!(f, "else {}", body)?;
                }
                Ok(())
            },
            Add(left, right) => write!(f, "({} + {})", left, right),
            Sub(left, right) => write!(f, "({} - {})", left, right),
            Mul(left, right) => write!(f, "({} * {})", left, right),
            Div(left, right) => write!(f, "({} / {})", left, right),
            Pow(left, right) => write!(f, "({} ^ {})", left, right),
            UnryNot(val) => write!(f, "!({})", val),
            UnrySub(val) => write!(f, "-({})", val),
            Unit(unit) => write!(f, "{}", unit),
            Num(num_type) => write!(f, "{}", num_type),
            Assign(name, val) => write!(f, "({} = {})", name, val),
            AddAssign(name, val) => write!(f, "({} += {})", name, val),
            SubAssign(name, val) => write!(f, "({} -= {})", name, val),
            MulAssign(name, val) => write!(f, "({} *= {})", name, val),
            DivAssign(name, val) => write!(f, "({} /= {})", name, val),
            Equal(lhs, rhs) => write!(f, "({} == {})", lhs, rhs),
            NeEqual(lhs, rhs) => write!(f, "({} != {})", lhs, rhs),
            GreaterEqual(lhs, rhs) => write!(f, "({} >= {})", lhs, rhs),
            LesserEqual(lhs, rhs) => write!(f, "({} <= {})", lhs, rhs),
            Greater(lhs, rhs) => write!(f, "({} > {})", lhs, rhs),
            Lesser(lhs, rhs) => write!(f, "({} < {})", lhs, rhs),
            Scope(nodes) => {
                writeln!(f, "{{")?;
                for n in nodes {
                    writeln!(f, "{}", n)?;
                }
                write!(f, "}}")
            }
            ParseError => write!(f, "Error"),
        }
    }
}

impl<'a> cmp::PartialEq for Node<'a> {
    fn eq(&self, other: &Self) -> bool {
        self.typ == other.typ
    }
}

macro_rules! impl_node_op {

    (binop: $op: ident -> $n_op: ident) => {
        impl<'a> std::ops::$op<Node<'a>> for Node<'a> {
            type Output = Node<'a>;

            paste! {
                fn [<$op:lower>](self, rhs: Node<'a>) -> Self::Output {
                    let span = merge_span(&self.span, &rhs.span);
                    let typ = NodeType::$n_op(self.into(), rhs.into());
                    Node {typ, span}
                }
            }
        }
    };

    (binop: $op: ident) => {
        impl_node_op!(binop: $op -> $op);
    };

    (assign: $op: ident) => {
        impl<'a> std::ops::$op<Node<'a>> for Node<'a> {
            paste! {
                fn [<$op:snake>](&mut self, other: Self) {
                    // Self::Output::$op(self.into(), rhs.into())
                    if let NodeType::Unit(name) = self.typ {
                        let span = merge_span(&self.span, &other.span);
                        let typ = NodeType::$op(name, other.into());
                        *self = Node {typ, span}
                    } else {
                        panic!("You can only {} to the Node::Unit enum", stringify!($op))
                    }
                }
            }
        }
    };
}

impl_node_op!(binop: Mul);
impl_node_op!(binop: Div);
impl_node_op!(binop: Add);
impl_node_op!(binop: Sub);
impl_node_op!(binop: BitXor -> Pow);

impl_node_op!(assign: AddAssign);
impl_node_op!(assign: SubAssign);
impl_node_op!(assign: MulAssign);
impl_node_op!(assign: DivAssign);

#[derive(Debug, PartialEq, Clone, Copy)]
pub struct UnitAtom<'a> {
    name: &'a str,
    exp: Decimal,
}

impl<'a> UnitAtom<'a> {
    pub fn base(name: &'a str) -> Self {
        Self { name, exp: dec!(1) }
    }
}

impl<'a> fmt::Display for UnitAtom<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if self.exp.is_integer() && self.exp == dec!(1) {
            write!(f, "{}", self.name)
        } else {
            write!(f, "{}^{}", self.name, self.exp)
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct Unit<'a>(Vec<UnitAtom<'a>>);

impl<'a> Unit<'a> {
    pub fn none() -> Self {
        Unit(vec![])
    }

    pub fn has_units(&self) -> bool {
        !self.0.is_empty()
    }

    pub fn pow(mut self, exp: Decimal) -> Self {
        for u in &mut self.0 {
            u.exp *= exp;
        }
        self
    }
}

impl<'a> From<UnitAtom<'a>> for Unit<'a> {
    fn from(value: UnitAtom<'a>) -> Self {
        Unit(vec![value])
    }
}

impl<'a> fmt::Display for Unit<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if self.0.is_empty() {
            return Ok(());
        }

        write!(f, "[")?;

        if let Some((last, rest)) = self.0.split_last() {
            for u in rest {
                write!(f, "{} ", u)?;
            }

            write!(f, "{}]", last)?;
        }

        Ok(())
    }
}

impl<'a> ops::Mul for Unit<'a> {
    type Output = Unit<'a>;

    fn mul(self, rhs: Self) -> Self::Output {
        let mut merged: Vec<_> = Vec::new();

        for u in self.0 {
            merged.push(u);
        }

        for u in rhs.0 {
            merged.push(u);
        }

        merged.sort_by(|a, b| a.name.cmp(b.name));

        let mut current: Option<UnitAtom> = None;

        let mut result: Vec<_> = Vec::new();

        for u in merged {
            if let Some(mut c) = current {
                if c.name == u.name {
                    c.exp += u.exp;
                    current = Some(c);
                } else {
                    result.push(c);
                    current = Some(u);
                }
            } else {
                current = Some(u)
            }
        }

        if let Some(c) = current {
            result.push(c);
        }

        result.retain(|unit| !unit.exp.is_zero());
        result.sort_by(|a, _| match a.exp.is_sign_negative() {
            true => cmp::Ordering::Greater,
            false => cmp::Ordering::Less,
        });

        Unit(result)
    }
}

#[allow(clippy::suspicious_arithmetic_impl)]
impl<'a> ops::Div for Unit<'a> {
    type Output = Unit<'a>;

    fn div(self, rhs: Self) -> Self::Output {
        let mut res = rhs.clone();
        res.0.iter_mut().for_each(|u| u.exp *= dec!(-1));

        self * res
    }
}

#[derive(Debug, Clone)]
pub struct Quantity<'a> {
    pub value: Decimal,
    pub unit: Unit<'a>,
    pub span: Range<usize>,
}

macro_rules! impl_quantity_cmp {

    ($sym: tt -> $fn_name: ident) => {
        pub fn $fn_name(&self, rhs: &Quantity<'a>) -> RuntimeResult<'a> {
            let span = merge_span(&self.span, &rhs.span);

            if self.unit != rhs.unit {
                return Err(MorphError::custom(
                    span,
                    format!(
                        "non-conformable units for '{}': ({} + {})",
                        stringify!($sym),
                        self.unit, rhs.unit
                    ),
                    ErrorType::TypeError,
                ));
            }

            if self.value $sym rhs.value {
                Ok(Quantity::num(dec!(1), span))
            } else {
                Ok(Quantity::num(dec!(0), span))
            }
        }
    }

}

impl<'a> Quantity<'a> {
    pub fn new<I: Into<Decimal>>(value: I, unit: Unit<'a>, span: Range<usize>) -> Self {
        Self {
            value: value.into(),
            unit,
            span,
        }
    }

    pub fn base(name: &'a str, span: Range<usize>) -> Self {
        Self {
            value: dec!(1),
            unit: UnitAtom::base(name).into(),
            span,
        }
    }

    pub fn num<I: Into<Decimal>>(value: I, span: Range<usize>) -> Self {
        Self {
            value: value.into(),
            unit: Unit::none(),
            span,
        }
    }

    pub fn equal(&self, rhs: &Quantity<'a>) -> RuntimeResult<'a> {
        let span = merge_span(&self.span, &rhs.span);

        if self == rhs {
            Ok(Quantity::num(dec!(1), span))
        } else {
            Ok(Quantity::num(dec!(0), span))
        }
    }

    pub fn not(self) -> RuntimeResult<'a> {
        if self.value.is_zero() {
            Ok(Quantity::num(dec!(1), self.span))
        } else {
            Ok(Quantity::num(dec!(0), self.span))
        }
    }

    impl_quantity_cmp!(>= -> greater_eq);
    impl_quantity_cmp!(> -> greater);
    impl_quantity_cmp!(<= -> lesser_eq);
    impl_quantity_cmp!(< -> lesser);
}

impl<'a> fmt::Display for Quantity<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{} {}", self.value, self.unit)
    }
}

impl<'a> ops::Add for Quantity<'a> {
    // type Output = Option<Quantity<'a>>;
    type Output = RuntimeResult<'a>;

    fn add(self, rhs: Self) -> Self::Output {
        let mut res = self.clone();

        if self.unit == rhs.unit {
            res.value += rhs.value;
            Ok(res)
        } else {
            let span = merge_span(&self.span, &rhs.span);
            Err(MorphError::custom(
                span,
                format!(
                    "non-conformable units for '+': ({} + {})",
                    self.unit, rhs.unit
                ),
                ErrorType::TypeError,
            ))
        }
    }
}

impl<'a> ops::Sub for Quantity<'a> {
    type Output = RuntimeResult<'a>;

    fn sub(self, rhs: Self) -> Self::Output {
        let mut res = rhs.clone();
        res.value *= dec!(-1);
        self + res
    }
}

impl<'a> ops::Mul for Quantity<'a> {
    type Output = RuntimeResult<'a>;

    fn mul(self, rhs: Self) -> Self::Output {
        let span = merge_span(&self.span, &rhs.span);

        if self.value.is_zero() || rhs.value.is_zero() {
            return Ok(Quantity::num(dec!(0), span));
        }

        let res = self.unit * rhs.unit;
        Ok(Quantity::new(self.value * rhs.value, res, span))
    }
}

impl<'a> ops::Div for Quantity<'a> {
    type Output = RuntimeResult<'a>;

    fn div(self, rhs: Self) -> Self::Output {
        let span = merge_span(&self.span, &rhs.span);

        if rhs.value.is_zero() {
            Err(MorphError::custom(
                span,
                "division by zero",
                ErrorType::ZeroDivision,
            ))
        } else {
            let res = self.unit / rhs.unit;
            Ok(Quantity::new(self.value / rhs.value, res, span))
        }
    }
}

impl<'a> ops::BitXor for Quantity<'a> {
    type Output = RuntimeResult<'a>;

    fn bitxor(self, rhs: Self) -> Self::Output {
        let span = merge_span(&self.span, &rhs.span);

        if !rhs.unit.has_units() {
            let mut res = self.clone();
            // res.value = Decimal::powd(&res.value, rhs.value);
            panic!("pow not implemented");
            res.unit = res.unit.pow(rhs.value);
            Ok(res)
        } else {
            Err(MorphError::custom(
                span,
                format!("exponent can't contain a unit, found: {rhs}"),
                ErrorType::TypeError,
            ))
        }
    }
}

impl<'a> cmp::PartialEq for Quantity<'a> {
    fn eq(&self, other: &Self) -> bool {
        self.value == other.value && self.unit == other.unit
    }
}
