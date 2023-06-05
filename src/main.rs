#![allow(dead_code)]

use logos::Logos;
use std::fmt;
use std::iter::Iterator;
use std::ops::Range;

#[derive(Logos, Debug, PartialEq)]
#[logos(skip r"[ \t\n\f]+")] 
pub enum Token {
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

    LexErr(String),
}

pub trait TokenIter: Iterator<Item = (Token, Range<usize>)> {
    fn peek(&mut self) -> Option<&Self::Item>;
}

impl<I: Iterator<Item = (Token, Range<usize>)>> TokenIter for std::iter::Peekable<I> {
    fn peek(&mut self) -> Option<&Self::Item> {
        std::iter::Peekable::peek(self)
    }
}


#[derive(Debug, Default)]
struct Error {
    msg: String,
}

#[derive(Debug, PartialEq)]
enum NodeType {
    Add(Box<Node>, Box<Node>),
    Sub(Box<Node>, Box<Node>),
    Mul(Box<Node>, Box<Node>),
    Div(Box<Node>, Box<Node>),
    Unit(String),
    Num(f64),

    UnrySub(Box<Node>),
}

#[derive(Debug, PartialEq)]
struct Node {
    typ: NodeType,
    range: Range<usize>,
}

impl Node {
    fn new(typ: NodeType, range: Range<usize>) -> Self {
        Node {typ, range}
    }

    fn to_string(&self) -> String {
        use NodeType::*;

        match &self.typ {
            Add(l, r) => format!("{} + {}", l, r),
            Sub(l, r) => format!("{} - {}", l, r),
            Mul(l, r) => format!("{} * {}", l, r),
            Div(l, r) => format!("{} / {}", l, r),
            UnrySub(v) => format!("- {v}"),
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

fn atom<I: TokenIter>(iter: &mut I) -> Result<Node, String> 
{
    assert!(iter.peek().is_some());

    let (tok, range) = iter.next().unwrap();

    let typ = match tok {
        Token::Unit(name) => NodeType::Unit(name),
        Token::Num(f64) => NodeType::Num(f64),
        Token::LexErr(msg) => return Err(msg),

        Token::Sub => NodeType::UnrySub(Box::new(parse_expr(iter)?)),

        _ => panic!("atom should not be called with {:?}", tok)
    };

    Ok(Node::new(typ, range))
}

fn parse_expr<I: TokenIter>(iter: &mut I) -> Result<Node, String> 
{
    atom(iter)
}

fn lex_code<'a>(code: &'a str) -> impl TokenIter + 'a {
    let lex = Token::lexer(code);

    lex.spanned().map(|(tok, rang)| 
    (match tok {
        Ok(v) => v,
        Err(_) => Token::LexErr(code[rang.clone()].to_owned()),
    }, rang)
    ).peekable()
}

fn main() {
    let code: &str = "mm * u";

    let mut iter = lex_code(code);

    println!("{:?}", parse_expr(&mut iter).map(|x| x.to_string()));
    //println!("{:?}", parse_expr(&mut iter));
    //print!("{:?}", parse(&mut lex));
}
