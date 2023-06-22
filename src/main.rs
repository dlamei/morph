#![allow(dead_code)]

use std::fmt::Display;
use chumsky::{
    input::{Stream, ValueInput},
    prelude::*,
};
use logos::Logos;

#[derive(Logos, Debug, PartialEq, Clone)]
#[logos(skip r"[ \t\n\f]+")]
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
    #[token("(")]
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

    LParen,
    #[token(")")]
    RParen,

    #[regex("def")]
    Def,

    #[regex("[a-zA-Z]+", |lex| lex.slice())]
    Unit(&'a str),

    #[regex("[0-9]+", |lex| lex.slice().parse().ok())]
    #[regex(r"(([0-9]+)(\.[0-9]+))", |lex| lex.slice().parse().ok())]
    Num(f64),

    Error,
}

impl Display for Token<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let res = match self {
            Token::Mul => "*".to_string(),
            Token::Div => "/".to_string(),
            Token::Add => "+".to_string(),
            Token::AddAdd => "++".to_string(),
            Token::Sub => "-".to_string(),
            Token::SubSub => "--".to_string(),
            Token::Assign => "=".to_string(),
            Token::AddEq => "+=".to_string(),
            Token::SubEq => "-=".to_string(),
            Token::MulEq => "*=".to_string(),
            Token::DivEq => "/=".to_string(),
            Token::LParen => "(".to_string(),
            Token::RParen => ")".to_string(),
            Token::Def => "def".to_string(),
            Token::Unit(name) => name.to_string(),
            Token::Num(v) => v.to_string(),
            Token::Error => "ERROR".to_string(),
        };

        write!(f, "{}", res)
    }
}

#[derive(Debug, PartialEq)]
enum Node {
    Def(String),

    Add(Box<Node>, Box<Node>),
    Sub(Box<Node>, Box<Node>),
    Mul(Box<Node>, Box<Node>),
    Div(Box<Node>, Box<Node>),
    Unit(String),
    Num(f64),

    UnrySub(Box<Node>),
}

impl Node {

    fn to_string(&self) -> String {
        use Node::*;

        match &self {
            Def(name) => format!("def {}", name),
            Add(l, r) => format!("{} + {}", l.to_string(), r.to_string()),
            Sub(l, r) => format!("{} - {}", l.to_string(), r.to_string()),
            Mul(l, r) => format!("{} * {}", l.to_string(), r.to_string()),
            Div(l, r) => format!("{} / {}", l.to_string(), r.to_string()),
            UnrySub(v) => format!("- {}", v.to_string()),
            Unit(name) => name.to_string(),
            Num(num) => num.to_string(),
        }
    }
}


fn parser<'a, I>() -> impl Parser<'a, I, Node, extra::Err<Rich<'a, Token<'a>>>>
where
    I: ValueInput<'a, Token = Token<'a>, Span = SimpleSpan>,
{
    let atom = select!(
        Token::Unit(x) => Node::Unit(x.to_owned()),
    );
    atom
}

const SRC: &str = r"
    meter
";

fn main() {

    let token_iter = Token::lexer(SRC)
        .spanned()
        .map(|(tok, span)| match tok {
            Ok(tok) => (tok, span.into()),
            Err(()) => (Token::Error, span.into()),
        });

    let token_stream = Stream::from_iter(token_iter)
        .spanned((SRC.len()..SRC.len()).into());

    println!("{:?}", parser().parse(token_stream).into_result())
}
