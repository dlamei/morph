#![allow(dead_code)]

use logos::Logos;
use std::error;
use std::fmt;

#[derive(Logos, Debug, PartialEq)]
#[logos(skip r"[ \t\n\f]+")] // Ignore this regex pattern between tokens
pub enum Token {
    // Tokens can be literal strings, of any length.
    #[token("=")]
    Assign,

    #[token("*")]
    Mul,
    #[token("/")]
    Div,
    #[token("+")]
    Add,
    #[token("-")]
    Sub,
    #[token("(")]
    LParen,
    #[token(")")]
    RParen,

    #[regex("[a-zA-Z]+", |lex| lex.slice().to_owned())]
    Unit(String),

    #[regex("[0-9]+", |lex| lex.slice().parse().ok())]
    #[regex(r"(([0-9]+)(\.[0-9]+))", |lex| lex.slice().parse().ok())]
    Num(f64),
}

pub trait TokenIter: std::iter::Iterator<Item = Result<Token, ()>> {
    fn peek(&mut self) -> Option<&Self::Item>;
}
//Item = Result<Token, ()>

impl<I: std::iter::Iterator<Item = Result<Token, ()>>> TokenIter for std::iter::Peekable<I> {
    fn peek(&mut self) -> Option<&Self::Item> {
        std::iter::Peekable::peek(self)
    }
}

//impl<I: std::iter::Iterator> TokenIter for std::iter::Peekable<I> {
//    fn peek(&mut self) -> Option<&Self::Item> {
//        std::iter::Peekable::peek(self)
//    }
//}

#[derive(Debug, Default)]
struct Error {
    msg: String,
}

impl Error {
    fn from(&mut self, msg: String) -> Self {
        Error {msg}
    }
}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.msg)
    }
}

impl error::Error for Error {}

#[derive(Debug, PartialEq)]
enum Node {
    Add(Box<Node>, Box<Node>),
    Sub(Box<Node>, Box<Node>),
    Mul(Box<Node>, Box<Node>),
    Div(Box<Node>, Box<Node>),
    Unit(String),
    Num(f64),
}

impl Node {
    fn to_string(&self) -> String {
        use Node::*;

        match self {
            Add(l, r) => format!("{} + {}", l, r),
            Sub(l, r) => format!("{} - {}", l, r),
            Mul(l, r) => format!("{} * {}", l, r),
            Div(l, r) => format!("{} / {}", l, r),
            Unit(name) => name.to_string(),
            Num(num) => num.to_string(),
        }
    }
}

impl fmt::Display for Node {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.to_string())
    }
}

fn atom<I>(iter: &mut I) -> Result<Node, ()> 
    where
        I: TokenIter,
{
    use Token::*;

    if iter.peek().is_none() {
        return Err(())
    }
    let tok = iter.next().unwrap();
    let tok = tok?;

    match tok {
        Unit(name) => Ok(Node::Unit(name)),
        Num(f64) => Ok(Node::Num(f64)),
        _ => panic!("atom should not be called with {:?}", tok)
    }
}

macro_rules! unwrap_or_ret {
    ($e: expr, $err: expr) => {{
        let n = $e;
        if n.is_none() {
            return Err($err);
        }
        n.unwrap()
    }};
}

fn parse_expr<I>(iter: &mut I) -> Result<Node, ()> 
    where
        I: TokenIter,
{
    let tok = unwrap_or_ret!(iter.next(), ())?;
    Err(())
}

fn main() {
    let code: &str = "mm * u";
    let lex = Token::lexer(code);
    let mut iter = lex.into_iter().peekable();

    println!("{:?}", parse_expr(&mut iter));
    //print!("{:?}", parse(&mut lex));
}
