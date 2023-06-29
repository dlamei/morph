use std::fmt;

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
    #[token("-")]
    Sub,
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

    #[regex(r"\n", |lex| lex.slice())]
    #[token(";", |lex| lex.slice())]
    NL(&'a str),

    LexErr(&'a str),
}

impl<'a> Token<'a> {
    pub const NUM: Self = Self::Num(0);
    pub const UNIT: Self = Self::Unit("...");
}

impl fmt::Display for Token<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let res = match self {
            Token::Mul => "*",
            Token::Div => "/",
            Token::Add => "+",
            Token::Sub => "-",
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
            Token::NL("\n") => "\\n",
            Token::NL(c) => *c,
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

    Body(Vec<Node<'a>>),

    Err,
}

impl<'a> fmt::Display for Node<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Node::Def(name) => write!(f, "(def {})", name),
            Node::Add(left, right) => write!(f, "({} + {})", left, right),
            Node::Sub(left, right) => write!(f, "({} - {})", left, right),
            Node::Mul(left, right) => write!(f, "({} * {})", left, right),
            Node::Div(left, right) => write!(f, "({} / {})", left, right),
            Node::Unit(unit) => write!(f, "{}", unit),
            Node::Num(num_type) => write!(f, "{}", num_type),
            Node::Body(nodes) => {
                writeln!(f, "Body(")?;
                for node in nodes {
                    writeln!(f, "{}", node)?;
                }
                writeln!(f, ")")
            }
            Node::Err => write!(f, "Err"),
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
