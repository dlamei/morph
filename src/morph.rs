use crate::types::*;

use ariadne::Source;
use ariadne::{Color, Label, Report, ReportKind};
use logos::Logos;

use chumsky::{input::Stream, prelude::Input, Parser};

pub fn parse(src: &str) -> ParseResult {
    let token_iter = Token::lexer(src).spanned().map(|(tok, span)| match tok {
        Ok(tok) => (tok, span.into()),
        Err(()) => (Token::LexErr(&src[span.clone()]), span.into()),
    });

    let token_stream = Stream::from_iter(token_iter).spanned((src.len()..src.len()).into());

    Node::parser().parse(token_stream)
}

pub fn run(file_name: &str, src: &str) {
    let res = parse(src);

    for err in res.errors() {
        Report::build(ReportKind::Error, file_name, err.span().start)
            // .with_message(err.to_string()) //TODO:: General error code + short overview of error
            .with_message(err.typ.to_string()) //TODO:: General error code + short overview of error
            .with_code(err.err_code())
            .with_label(
                Label::new((file_name, err.span().into_range()))
                    .with_message(err.reason().to_string())
                    .with_color(Color::Red),
            )
            .finish()
            .eprint((file_name, Source::from(src)))
            .unwrap();
    }

    if let Some(ast) = res.output() {
        println!("{}", ast);
    } else {
        println!("ERROR: nothing was parsed");
    }

    // match res {
    //     Ok(n) => println!("{}", n.to_code()),
    //     Err(errs) => {
    //         for err in errs {
    //             Report::build(ReportKind::Error, (), err.span().start)
    //                 .with_message(err.to_string())
    //                 .with_label(
    //                     Label::new(err.span().into_range())
    //                         .with_message(err.reason().to_string())
    //                         .with_color(Color::Red),
    //                 )
    //                 .finish()
    //                 .eprint(Source::from(src))
    //                 .unwrap();
    //         }
    //     }
    // }
}

#[cfg(test)]
pub mod test_utils {
    use crate::morph;
    use crate::types::*;

    macro_rules! bod {
        ($($x:expr),*) => {{
            #[allow(unused_mut)]
            let mut vec = Vec::new();
            $(vec.push($x);)*
            crate::types::Node::Body(vec)
        }};
    }

    macro_rules! eq {
        ($lhs:expr, $rhs:expr) => {
            assert_eq!($lhs, $rhs)
        };
    }

    macro_rules! get_reason {
        ($parse: expr) => {
            $parse.into_errors().swap_remove(0).into_reason()
        };

        ($parse: expr, $n: literal) => {
            $parse.into_errors().swap_remove($n).into_reason()
        };
    }

    pub(crate) use bod;
    pub(crate) use eq;
    pub(crate) use get_reason;

    pub fn u(name: &str) -> Node {
        Node::Unit(name)
    }

    pub fn d(name: &str) -> Node {
        Node::Def(name)
    }

    pub fn n(val: NumType) -> Node<'static> {
        Node::Num(val)
    }

    type Reason<'a> = chumsky::error::RichReason<'a, Token<'a>, &'a str>;
    type Pattern<'a> = chumsky::error::RichPattern<'a, Token<'a>, &'a str>;

    pub fn cmp_expected<'a>(r1: Reason<'a>, expected: &[Token<'a>]) -> bool {
        let expected_toks = if let Reason::ExpectedFound { expected: e, .. } = r1 {
            e
        } else {
            return false;
        };

        let expected = expected.iter().map(|x| Pattern::Token((*x).clone().into()));

        for e1 in expected {
            for e2 in &expected_toks {
                if e1 == *e2 {
                    return true;
                }
            }
        }

        false
    }

    pub fn parse(code: &str) -> morph::ParseResult {
        morph::parse(code)
    }

    pub fn nodes(code: &str) -> Node {
        morph::parse(code).unwrap()
    }
}
