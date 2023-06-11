#![allow(dead_code)]

use logos::Logos;
use std::fmt::Display;
use std::iter::Iterator;
use std::ops::Range;

use paste::paste;

#[macro_export]
macro_rules! enum_or {

    ($x:pat) => {
        paste!(Self::$x)
    };

    ($x1:pat, $($x2:pat),*) => {
        paste!(Self::$x1) | enum_or!($($x2),*)
    };
}

#[macro_export]
macro_rules! enum_match {

    ($self:ident, $default:expr, $n:expr,) => {$default};

    ($self:ident, $default:expr, $n:expr, [$($x1:pat),*] $([$($x2:pat),*])*) => {

        if let enum_or!($($x1),*) = $self {
            $n
        } else {
            enum_match!($self, $default, $n+1, $([$($x2),*])*)
        }
    };
}

#[macro_export]
macro_rules! priority_func {

    ($name:tt -> $typ:ty, $default:expr, $([$($x:pat),*])*) =>
    {
        pub fn $name(&self) -> $typ {
            enum_match!(self, $default, $default + 1, $([$($x),*])*)
        }
    };
}

#[macro_export]
macro_rules! assign_func {

    ($name: tt -> $typ:ty, $default: expr, $([$val:expr; $($x2:pat),*])*) => {
        pub fn $name(&self) -> $typ {
            #[allow(unreachable_patterns)]
            match self {
                $($(paste!(Self::$x2))|* => $val,)*
                    _ => $default,
            }
        }
    };
}

#[derive(Logos, Debug, PartialEq, Clone)]
#[logos(skip r"[ \t\n\f]+")]
pub enum Token {
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

    #[regex("[a-zA-Z]+", |lex| lex.slice().to_owned())]
    Unit(String),

    #[regex("[0-9]+", |lex| lex.slice().parse().ok())]
    #[regex(r"(([0-9]+)(\.[0-9]+))", |lex| lex.slice().parse().ok())]
    Num(f64),

    LexErr(String),
    EOF,
}

impl Display for Token {
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
            Token::LexErr(err) => err.to_string(),
            Token::EOF => "EOF".to_string(),
        };

        write!(f, "{}", res)
    }
}

impl Token {
    priority_func!(precedence -> i32, 0,
        [Assign, AddEq, SubEq, MulEq, DivEq]
        [Add, Sub]
        [Mul, Div]
    );

    fn is_op(&self) -> bool {
        self.precedence() != 0
    }
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
    Def(String),

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
        Node { typ, range }
    }

    fn to_string(&self) -> String {
        use NodeType::*;

        match &self.typ {
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

//impl fmt::Display for Node {
//    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
//        write!(f, "{}", self.to_string())
//    }
//}

fn parse_def<I: TokenIter>(iter: &mut I) -> Result<NodeType, String> {
    let tok = iter.next();

    if let Some((Token::Unit(name), _)) = tok {
        return Ok(NodeType::Def(name));
    } else if let Some((tok, _)) = tok {
        return Err(format!("def expectes unit name, found: {}", tok));
    } else {
        assert!(false, "expected EOF");
    }

    todo!();
}

fn atom<I: TokenIter>(iter: &mut I) -> Result<Node, String> {
    assert!(iter.peek().is_some());

    let (tok, range) = iter.next().unwrap();

    let typ = match tok {
        Token::Unit(name) => NodeType::Unit(name),
        Token::Num(f64) => NodeType::Num(f64),
        Token::LexErr(msg) => return Err(msg),
        Token::Def => parse_def(iter)?,

        Token::Sub => NodeType::UnrySub(Box::new(parse_expr(iter)?)),

        _ => panic!("atom should not be called with {:?}", tok),
    };

    Ok(Node::new(typ, range))
}

macro_rules! check_for_eof {
    ($e: expr) => {
        match $e {
            None => return Err("no EOF at the end of token stream!".to_string()),
            Some((Token::EOF, _)) => true,
            _ => false,
        }
    };
}

fn apply_op(op: Token, lhs: Node, rhs: Node) -> Node {
    let range = lhs.range.start..rhs.range.end;

    use Token::*;

    Node::new(
        match op {
            Add => NodeType::Add(lhs.into(), rhs.into()),
            Sub => NodeType::Sub(lhs.into(), rhs.into()),
            Mul => NodeType::Mul(lhs.into(), rhs.into()),
            Div => NodeType::Div(lhs.into(), rhs.into()),
            _ => panic!("apply_op called with: {:?}", op),
        },
        range,
    )
}

fn parse_sub_expr<I: TokenIter>(
    iter: &mut I,
    mut lhs: Node,
    precedence: i32,
) -> Result<Node, String> {
    if check_for_eof!(iter.peek()) {
        return Ok(lhs);
    }

    let mut lookahead = iter.peek().unwrap().0.clone();

    while lookahead.is_op() && lookahead.precedence() > precedence {
        let op = lookahead;

        iter.next();
        if check_for_eof!(iter.peek()) {
            return Ok(lhs);
        }

        let mut rhs = atom(iter)?;

        if check_for_eof!(iter.peek()) {
            lhs = apply_op(op, lhs, rhs);
            break;
        }

        lookahead = iter.peek().unwrap().0.clone();

        while lookahead.is_op() && lookahead.precedence() > op.precedence() {
            rhs = parse_sub_expr(iter, rhs, op.precedence())?;

            if check_for_eof!(iter.peek()) {
                break;
            }
            lookahead = iter.peek().unwrap().0.clone();
        }

        lhs = apply_op(op, lhs, rhs);
    }

    Ok(lhs)
}

fn parse_expr<I: TokenIter>(iter: &mut I) -> Result<Node, String> {
    if iter.peek().is_none() {
        return Err("no EOF at the end of token stream!".to_string());
    }

    let lhs = atom(iter)?;
    parse_sub_expr(iter, lhs, -1)
}

fn lex_code(code: &str) -> impl TokenIter + '_ {
    let lex = Token::lexer(code);

    lex.spanned()
        .map(|(tok, rang)| {
            (
                match tok {
                    Ok(v) => v,
                    Err(_) => Token::LexErr(code[rang.clone()].to_owned()),
                },
                rang,
            )
        })
        .chain(std::iter::once((Token::EOF, 0..0)))
        .peekable()
}

fn main() {
    let code: &str = "def @";

    let mut iter = lex_code(code);

    println!("{:?}", parse_expr(&mut iter));
    //println!("{:?}", parse_expr(&mut iter));
    //print!("{:?}", parse(&mut lex));
}
