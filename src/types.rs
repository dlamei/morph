use std::fmt;

use chumsky::{
    error::{RichPattern, RichReason},
    prelude::Input,
    span::SimpleSpan,
    util::MaybeRef,
};

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

fn flat_merge_rich_reason<'a, T, L>(
    r1: RichReason<'a, T, L>,
    r2: RichReason<'a, T, L>,
) -> RichReason<'a, T, L>
where
    T: PartialEq,
    L: PartialEq,
{
    match (r1, r2) {
        (
            RichReason::ExpectedFound {
                expected: mut this_expected,
                found,
            },
            RichReason::ExpectedFound {
                expected: mut other_expected,
                ..
            },
        ) => {
            // Try to avoid allocations if we possibly can by using the longer vector
            if other_expected.len() > this_expected.len() {
                core::mem::swap(&mut this_expected, &mut other_expected);
            }
            for expected in other_expected {
                if !this_expected[..].contains(&expected) {
                    this_expected.push(expected);
                }
            }
            RichReason::ExpectedFound {
                expected: this_expected,
                found,
            }
        }
        (RichReason::Many(mut m1), RichReason::Many(m2)) => {
            m1.extend(m2);
            RichReason::Many(m1)
        }
        (RichReason::Many(mut m), other) => {
            m.push(other);
            RichReason::Many(m)
        }
        (this, RichReason::Many(mut m)) => {
            m.push(this);
            RichReason::Many(m)
        }
        (this, other) => RichReason::Many(vec![this, other]),
    }
}

#[derive(Debug)]
pub enum ParseErrorType {
    CouldNotLex,
    UndefinedSyntax,
    Other,
}

impl ParseErrorType {
    fn desc(&self) -> &'static str {
        match self {
            ParseErrorType::CouldNotLex => "CouldNotLex: found illegal character",
            ParseErrorType::UndefinedSyntax => "UndefinedSynax: parser encountered syntax error",
            ParseErrorType::Other => "",
        }
    }
}

impl fmt::Display for ParseErrorType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.desc())
    }
}

impl Default for ParseErrorType {
    fn default() -> Self {
        Self::Other
    }
}

#[derive(Debug)]
pub struct ParseError<'a> {
    pub span: SimpleSpan<usize>,
    pub reason: Box<RichReason<'a, Token<'a>, &'static str>>,
    pub typ: ParseErrorType,
}

impl fmt::Display for ParseError<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.reason)
    }
}

impl<'a> ParseError<'a> {
    pub fn span(&self) -> &SimpleSpan {
        &self.span
    }

    pub fn reason(&self) -> &RichReason<'a, Token<'a>, &'a str> {
        &self.reason
    }

    #[allow(dead_code)]
    pub fn into_reason(self) -> RichReason<'a, Token<'a>, &'a str> {
        *self.reason
    }

    pub fn found(&self) -> Option<&Token<'a>> {
        self.reason.found()
    }

    #[allow(dead_code)]
    pub fn expected(&self) -> impl ExactSizeIterator<Item = &RichPattern<'a, Token<'a>, &'a str>> {
        fn push_expected<'a, 'b, T, L>(
            reason: &'b RichReason<'a, T, L>,
            v: &mut Vec<&'b RichPattern<'a, T, L>>,
        ) {
            match reason {
                RichReason::ExpectedFound { expected, .. } => v.extend(expected.iter()),
                RichReason::Custom(_) => {}
                RichReason::Many(many) => many.iter().for_each(|r| push_expected(r, v)),
            }
        }
        let mut v = Vec::new();
        push_expected(&self.reason, &mut v);
        v.into_iter()
    }
}

impl<'a, I: Input<'a, Token = Token<'a>, Span = SimpleSpan<usize>>> chumsky::error::Error<'a, I>
    for ParseError<'a>
where
    I::Token: PartialEq,
{
    #[inline]
    fn expected_found<E: IntoIterator<Item = Option<MaybeRef<'a, I::Token>>>>(
        expected: E,
        found: Option<MaybeRef<'a, I::Token>>,
        span: I::Span,
    ) -> Self {
        Self {
            span,
            reason: Box::new(RichReason::ExpectedFound {
                expected: expected
                    .into_iter()
                    .map(|tok| {
                        tok.map(RichPattern::Token)
                            .unwrap_or(RichPattern::EndOfInput)
                    })
                    .collect(),
                found,
            }),
            typ: Default::default(),
        }
    }

    #[inline]
    fn merge(self, other: Self) -> Self {
        let new_reason = flat_merge_rich_reason(*self.reason, *other.reason);
        Self {
            span: self.span,
            reason: Box::new(new_reason),
            typ: Default::default(),
        }
    }

    #[inline]
    fn merge_expected_found<E: IntoIterator<Item = Option<MaybeRef<'a, I::Token>>>>(
        mut self,
        new_expected: E,
        found: Option<MaybeRef<'a, I::Token>>,
        _span: I::Span,
    ) -> Self {
        match &mut *self.reason {
            RichReason::ExpectedFound { expected, found: _ } => {
                for new_expected in new_expected {
                    let new_expected = new_expected
                        .map(RichPattern::Token)
                        .unwrap_or(RichPattern::EndOfInput);
                    if !expected[..].contains(&new_expected) {
                        expected.push(new_expected);
                    }
                }
            }
            RichReason::Many(m) => m.push(RichReason::ExpectedFound {
                expected: new_expected
                    .into_iter()
                    .map(|tok| {
                        tok.map(RichPattern::Token)
                            .unwrap_or(RichPattern::EndOfInput)
                    })
                    .collect(),
                found,
            }),
            RichReason::Custom(_) => {
                let old = core::mem::replace(&mut *self.reason, RichReason::Many(Vec::new()));
                self.reason = Box::new(RichReason::Many(vec![
                    old,
                    RichReason::ExpectedFound {
                        expected: new_expected
                            .into_iter()
                            .map(|tok| {
                                tok.map(RichPattern::Token)
                                    .unwrap_or(RichPattern::EndOfInput)
                            })
                            .collect(),
                        found,
                    },
                ]));
            }
        }
        self
    }

    #[inline]
    fn replace_expected_found<E: IntoIterator<Item = Option<MaybeRef<'a, I::Token>>>>(
        mut self,
        new_expected: E,
        new_found: Option<MaybeRef<'a, I::Token>>,
        span: I::Span,
    ) -> Self {
        self.span = span;
        match &mut *self.reason {
            RichReason::ExpectedFound { expected, found } => {
                expected.clear();
                expected.extend(new_expected.into_iter().map(|tok| {
                    tok.map(RichPattern::Token)
                        .unwrap_or(RichPattern::EndOfInput)
                }));
                *found = new_found;
            }
            _ => {
                self.reason = Box::new(RichReason::ExpectedFound {
                    expected: new_expected
                        .into_iter()
                        .map(|tok| {
                            tok.map(RichPattern::Token)
                                .unwrap_or(RichPattern::EndOfInput)
                        })
                        .collect(),
                    found: new_found,
                });
            }
        }
        self
    }
}

// pub type ParseError<'a> = chumsky::error::Rich<'a, Token<'a>>;
pub type ParseResult<'a> = chumsky::ParseResult<Node<'a>, ParseError<'a>>;
