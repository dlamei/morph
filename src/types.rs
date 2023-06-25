use std::fmt::Display;

use logos::Logos;
use paste::paste;

#[derive(Logos, Debug, PartialEq, Clone)]
#[logos(skip r"[ \t\f]+")]
#[logos(subpattern unicode_ident = r"\p{XID_Start}\p{XID_Continue}*")]
#[logos(subpattern ascii_ident = r"[_a-zA-Z][_0-9a-zA-Z]*")]
pub enum Token<'a> {
    #[token("*")]
    Mul,
    #[token("/")]
    Div,
    #[token("+")]
    Add,
    #[token("++")]
    AddAdd,
    #[token("-")]
    Sub,
    #[token("--")]
    SubSub,
    #[token("=")]
    Assign,
    #[token("+=")]
    AddEq,
    #[token("-=")]
    SubEq,
    #[token("*=")]
    MulEq,
    #[token("/=")]
    DivEq,

    #[token("(")]
    LParen,
    #[token(")")]
    RParen,

    #[regex("def")]
    Def,

    #[regex("(?&unicode_ident)", |lex| lex.slice())]
    Unit(&'a str),

    #[regex("[0-9]+", |lex| lex.slice().parse().ok())]
    #[regex(r"(([0-9]+)(\.[0-9]+))", |lex| lex.slice().parse().ok())]
    Num(NumType),

    #[regex(r"\n")]
    #[token(";")]
    NL,

    LexErr(&'a str),
}

impl Display for Token<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let res = match self {
            Token::Mul => "*",
            Token::Div => "/",
            Token::Add => "+",
            Token::AddAdd => "++",
            Token::Sub => "-",
            Token::SubSub => "--",
            Token::Assign => "=",
            Token::AddEq => "+=",
            Token::SubEq => "-=",
            Token::MulEq => "*=",
            Token::DivEq => "/=",
            Token::LParen => "(",
            Token::RParen => ")",
            Token::Def => "def",
            Token::Unit(_) => "UNIT",
            Token::Num(_) => "NUM",
            Token::NL => "NextExpr",
            Token::LexErr(msg) => return write!(f, "Lexer Error: {msg}"),
        };

        write!(f, "{}", res)
    }
}

pub type NumType = i64;

#[derive(Debug, PartialEq)]
pub enum Node<'a> {
    Def(&'a str),

    Add(Box<Node<'a>>, Box<Node<'a>>),
    Sub(Box<Node<'a>>, Box<Node<'a>>),
    Mul(Box<Node<'a>>, Box<Node<'a>>),
    Div(Box<Node<'a>>, Box<Node<'a>>),
    Unit(&'a str),
    Num(NumType),

    // UnrySub(Box<Node<'a>>),
    Body(Vec<Node<'a>>),

    Err,
}

impl Node<'_> {
    pub fn to_code(&self) -> String {
        use Node::*;

        match &self {
            Def(name) => format!("def {}", name),
            Add(l, r) => format!("{} + {}", l.to_code(), r.to_code()),
            Sub(l, r) => format!("{} - {}", l.to_code(), r.to_code()),
            Mul(l, r) => format!("{} * {}", l.to_code(), r.to_code()),
            Div(l, r) => format!("{} / {}", l.to_code(), r.to_code()),
            // UnrySub(v) => format!("- {}", v.to_code()),
            Unit(name) => name.to_string(),
            Num(num) => num.to_string(),
            Body(bod) => bod
                .iter()
                .map(|x| "\n".to_owned() + &x.to_code())
                .reduce(|e1, e2| e1 + &e2)
                .unwrap_or_default(),
            Err => "ERROR".to_owned(),
        }
    }
}

macro_rules! impl_node_op {
    ($op: ident) => {
        impl<'a> std::ops::$op<Node<'a>> for Node<'a> {
            type Output = Node<'a>;

            paste! {
                fn [<$op:lower>](self, rhs: Node<'a>) -> Self::Output {
                    Self::Output::$op(self.into(), rhs.into())
                }
            }
        }
    };
}

impl_node_op!(Mul);
impl_node_op!(Div);
impl_node_op!(Add);
impl_node_op!(Sub);
