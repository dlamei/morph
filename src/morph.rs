use crate::error::*;
use crate::eval::*;
use crate::types::*;

use logos::Logos;

use chumsky::{input::Stream, prelude::Input, Parser};

pub fn parse(src: &str) -> ParseResult {
    let token_iter = Token::lexer(src).spanned().map(|(tok, span)| match tok {
        Ok(tok) => (tok, span.into()),
        Err(()) => (Token::LexErr(&src[span.clone()]), span.into()),
    });

    let token_stream = Stream::from_iter(token_iter).spanned((src.len()..src.len()).into());

    Node::parser().parse(token_stream).into_output_errors()
}

pub fn run(file_name: &str, src: &str) {
    let (root, errors) = parse(src);

    for err in &errors {
        err.report(file_name, src);
    }

    if !errors.is_empty() {
        return;
    }

    if let Some(ast) = root {
        let mut context = Context::new();
        match ast.eval(&mut context) {
            Ok(res) => println!("{}", res),
            Err(err) => err.report(file_name, src),
        }
        // if let Ok(res) = ast.clone().eval(context) {
        //     println!("{}", res);
        // } else {

        // }
    } else {
        println!("ERROR: nothing was parsed");
    }
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
            crate::types::NodeType::Scope(vec)
        }};
    }

    macro_rules! eq {
        ($lhs:expr, $rhs:expr) => {
            assert_eq!($lhs, $rhs)
        };
    }

    pub(crate) use bod;
    pub(crate) use eq;
    // use rust_decimal::Decimal;

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

    pub fn node_types(code: &str) -> NodeType {
        morph::parse(code).0.unwrap().into()
    }

    pub fn nodes(code: &str) -> Node {
        morph::parse(code).0.unwrap()
    }

    pub fn run(code: &'static str) -> morph::RuntimeResult<'static> {
        let ast = nodes(code);
        let mut context = morph::Context::new();
        ast.eval(&mut context)
        // morph::run("<TEST>", code)
    }
}

#[cfg(test)]
mod test {
    use crate::{
        morph::test_utils::*,
        types::{Quantity, Unit},
    };
    use rust_decimal::Decimal;

    fn n<I: Into<Decimal>>(val: I) -> Quantity<'static> {
        Quantity::num(val, 0..0)
    }

    fn u<I: Into<Decimal>>(val: I, unit: &'static str) -> Quantity<'static> {
        Quantity::new(val, Unit::base(unit), 0..0)
    }

    #[test]
    fn unary_op() {
        eq!(run("!0").unwrap(), n(1));
        eq!(run("!1").unwrap(), n(0));
        eq!(run("def m; !m").unwrap(), u(0, "m"));
        eq!(run("def m; !0m").unwrap(), u(1, "m"));
    }

    #[test]
    fn expr() {
        eq!(run("def m; def s; m * s"), u(1, "m") * u(1, "s"))
    }
}
