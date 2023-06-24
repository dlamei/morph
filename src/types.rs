use logos::Logos;
use std::fmt::Display;

use chumsky::{input::ValueInput, prelude::*};

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
    Num(f64),

    #[regex(r"\n")]
    #[token(";")]
    NextExpr,

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
            Token::NextExpr => "NextExpr",
            Token::LexErr(msg) => return write!(f, "Lexer Error: {msg}"),
        };

        write!(f, "{}", res)
    }
}

#[derive(Debug, PartialEq)]
pub enum Node<'a> {
    Def(&'a str),

    Add(Box<Node<'a>>, Box<Node<'a>>),
    Sub(Box<Node<'a>>, Box<Node<'a>>),
    Mul(Box<Node<'a>>, Box<Node<'a>>),
    Div(Box<Node<'a>>, Box<Node<'a>>),
    Unit(&'a str),
    Num(f64),

    UnrySub(Box<Node<'a>>),

    Body(Vec<Node<'a>>),
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
            UnrySub(v) => format!("- {}", v.to_code()),
            Unit(name) => name.to_string(),
            Num(num) => num.to_string(),
            Body(bod) => bod
                .iter()
                .map(|x| "\n".to_owned() + &x.to_code())
                .reduce(|e1, e2| e1 + &e2)
                .unwrap_or_default(),
        }
    }
}

pub trait Parsable<'a>
where
    Self: Sized,
{
    type Token: PartialEq;

    fn parser<I>() -> Boxed<'a, 'a, I, Self, extra::Err<Rich<'a, Self::Token>>>
    where
        I: ValueInput<'a, Token = Self::Token, Span = SimpleSpan>;
}

macro_rules! merge_expected {
    ($err: ident ::<$I: ty>, $exp_tok: expr) => {{
        let found = $err
            .found()
            .map(|x| ::chumsky::util::MaybeRef::Val(x.to_owned()));
        let exp = $exp_tok.map(|x| Some(chumsky::util::MaybeRef::Val(x)));
        let span = $err.span().clone();
        ::chumsky::error::Error::<$I>::merge_expected_found($err, exp, found, span)
    }};
}

pub type ParseError<'a> = Rich<'a, Token<'a>>;

impl<'a> Parsable<'a> for Node<'a> {
    type Token = Token<'a>;

    fn parser<I>() -> Boxed<'a, 'a, I, Self, extra::Err<ParseError<'a>>>
    where
        I: ValueInput<'a, Token = Self::Token, Span = SimpleSpan>,
    {
        let expr = recursive(|expr| {
            let atom = expr
                .delimited_by(just(Token::LParen), just(Token::RParen))
                .or(select!(
                    Token::Num(x) => Node::Num(x),
                    Token::Unit(x) => Node::Unit(x),
                ))
                .map_err(|err: ParseError| {
                    merge_expected!(err::<I>, [Token::Num(0.), Token::Unit("...")])
                });

            let unary = just(Token::Sub)
                .repeated()
                .foldr(atom, |_op, rhs| Node::UnrySub(Box::new(rhs)));

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

            sum
        });

        let def = just(Token::Def)
            .ignore_then(select!(Token::Unit(x) => Node::Def(x)))
            .map_err(|err: ParseError| merge_expected!(err::<I>, [Token::Unit("...")]));

        let expr = expr.or(def);

        let body = expr
            .separated_by(just(Token::NextExpr).repeated())
            .allow_trailing()
            .allow_leading()
            .collect::<Vec<_>>()
            .map(Node::Body);

        body.boxed()
    }
}
