#![allow(dead_code)]

use chumsky::{
    input::{Stream, ValueInput},
    prelude::*,
};
use logos::Logos;
use std::fmt::Display;

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

    #[regex("[a-zA-Z]+", |lex| lex.slice())]
    Unit(&'a str),

    #[regex("[0-9]+", |lex| lex.slice().parse().ok())]
    #[regex(r"(([0-9]+)(\.[0-9]+))", |lex| lex.slice().parse().ok())]
    Num(f64),

    Error(&'a str),
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
            Token::Error(msg) => format!("ERROR({})", msg),
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

macro_rules! replace_expected {
    ($err: ident ::<$I: ty>, $exp_tok: expr) => {{
        let found = $err
            .found()
            .map(|x| ::chumsky::util::MaybeRef::Val(x.to_owned()));
        let exp = $exp_tok.map(|x| Some(chumsky::util::MaybeRef::Val(x)));
        let span = $err.span().clone();
        ::chumsky::error::Error::<$I>::replace_expected_found($err, exp, found, span)
    }};
}

type ParseError<'a> = Rich<'a, Token<'a>>;

fn parser<'a, I>() -> impl Parser<'a, I, Node, extra::Err<ParseError<'a>>>
where
    I: ValueInput<'a, Token = Token<'a>, Span = SimpleSpan>,
{
    let expr = recursive(|expr| {
        let atom = expr
            .delimited_by(just(Token::LParen), just(Token::RParen))
            .or(select!(
                Token::Num(x) => Node::Num(x),
                Token::Unit(x) => Node::Unit(x.to_owned()),
            ));

        let unary = just(Token::Sub)
            .repeated()
            .foldr(atom, |_op, rhs| Node::UnrySub(Box::new(rhs)));

        let def = just(Token::Def)
            .ignore_then(select!(Token::Unit(x) => Node::Def(x.to_owned())))
            .map_err(|err: ParseError| replace_expected!(err::<I>, [Token::Unit("...")]));

        let product = unary.clone().foldl(
            choice((
                just(Token::Mul).to(Node::Mul as fn(_, _) -> _),
                just(Token::Div).to(Node::Div as fn(_, _) -> _),
            ))
            .then(unary)
            .repeated(),
            |lhs, (op, rhs)| op(Box::new(lhs), Box::new(rhs)),
        );

        let sum = product.clone().foldl(
            choice((
                just(Token::Add).to(Node::Add as fn(_, _) -> _),
                just(Token::Sub).to(Node::Sub as fn(_, _) -> _),
            ))
            .then(product)
            .repeated(),
            |lhs, (op, rhs)| op(Box::new(lhs), Box::new(rhs)),
        );

        sum.or(def)
    });

    //let def = just(Token::Def)
    //    .ignore_then( select!( Token::Unit(name) => Node::Def(name.to_owned())));

    expr
}

const SRC: &str = r"
    $
";

fn main() {
    let token_iter = Token::lexer(SRC).spanned().map(|(tok, span)| match tok {
        Ok(tok) => (tok, span.into()),
        Err(()) => (Token::Error(&SRC[span.clone()]), span.into()),
    });

    let token_stream = Stream::from_iter(token_iter).spanned((SRC.len()..SRC.len()).into());

    let res = parser().parse(token_stream).into_result();

    match res {
        Ok(res) => println!("{}", res.to_string()),
        Err(e) => println!("{:?}", e),
    }
}
