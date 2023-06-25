use crate::parser::*;
use crate::types::*;

use logos::Logos;

use chumsky::{input::Stream, prelude::Input, Parser};

pub type ParseResult<'a> = Result<Node<'a>, Vec<ParseError<'a>>>;

pub fn parse(src: &str) -> ParseResult {
    let token_iter = Token::lexer(src).spanned().map(|(tok, span)| match tok {
        Ok(tok) => (tok, span.into()),
        Err(()) => (Token::LexErr(&src[span.clone()]), span.into()),
    });

    let token_stream = Stream::from_iter(token_iter).spanned((src.len()..src.len()).into());

    Node::parser().parse(token_stream).into_result()
}
