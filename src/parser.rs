use std::ops;

use crate::types::*;

use chumsky::{input::ValueInput, prelude::*};

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

macro_rules! cast_enum {
    ($exp: expr => $cast: tt $body:block) => {{
        #[allow(unused_parens)]
        if let $cast = $exp {
            $body
        } else {
            unreachable!()
        }
    }};
}

pub type ParseError<'a> = Rich<'a, Token<'a>>;

impl<'a> Node<'a> {
    pub fn parser<I>() -> Boxed<'a, 'a, I, Self, extra::Err<ParseError<'a>>>
    where
        I: ValueInput<'a, Token = Token<'a>, Span = SimpleSpan>,
    {
        let unit = select!(Token::Unit(x) => Node::Unit(x));
        let num = select!(Token::Num(x) => Node::Num(x));

        let num_unit = num.then(unit).map(|x| {
            cast_enum!(x => (Node::Num(n), Node::Unit(name)) {
                return Node::Mul(Node::Num(n).into(), Node::Unit(name).into())
            });
        });

        let expr = recursive(|expr| {
            let atom = choice((
                expr.delimited_by(just(Token::LParen), just(Token::RParen)),
                num_unit,
                unit,
                num,
            ))
            .map_err(|err: ParseError| {
                merge_expected!(
                    err::<I>,
                    [Token::Num(Default::default()), Token::Unit("...")]
                )
            });

            let unary = just(Token::Sub)
                .repeated()
                .foldr(atom, |_op, rhs| Node::Num(-1) * rhs);

            let product = unary.clone().foldl(
                choice((
                    just(Token::Mul).to(ops::Mul::mul as fn(_, _) -> _),
                    just(Token::Div).to(ops::Div::div as fn(_, _) -> _),
                ))
                .then(unary)
                .repeated(),
                |lhs, (op, rhs)| op(lhs, rhs),
            );

            product.clone().foldl(
                choice((
                    just(Token::Add).to(ops::Add::add as fn(_, _) -> _),
                    just(Token::Sub).to(ops::Sub::sub as fn(_, _) -> _),
                ))
                .then(product)
                .repeated(),
                |lhs, (op, rhs)| op(lhs, rhs),
            )
        });

        let def = just(Token::Def)
            .ignore_then(unit)
            .map(|u| cast_enum!(u => (Node::Unit(name)) {return Node::Def(name)}))
            .map_err(|err: ParseError| merge_expected!(err::<I>, [Token::Unit("...")]));

        let expr = choice((expr, def));

        let body = just(Token::NL)
            .repeated()
            .ignore_then(
                expr.then_ignore(just(Token::NL).repeated().at_least(1).or(end())), // .recover_with(via_parser(
                                                                                    //         end().map(|_| {
                                                                                    //             println!("end!");
                                                                                    //             Node::Err
                                                                                    //         })
                                                                                    //     ))
                                                                                    // .recover_with(via_parser(
                                                                                    //         none_of(Token::NL).repeated().map(|x| {
                                                                                    //             println!("mapped: {:?}", x);
                                                                                    //             Node::Err
                                                                                    //         })
                                                                                    //     ))
            )
            // .map(|x| {println!("parsed: {:?}", x); x})
            .repeated()
            .collect::<Vec<_>>()
            .map(Node::Body);

        body.boxed()
    }
}


#[cfg(test)]
mod test {
    use crate::test_utils::*;

    #[test]
    fn basic_expr() {
        eq!(nodes(""), bod!());
        eq!(nodes("def m \n m * s"), bod!(d("m"), u("m") * u("s")));
        eq!(
            nodes("a + b * c + d"),
            bod!(u("a") + u("b") * u("c") + u("d"))
        );
        eq!(nodes("a * (b + c) * d"), bod!(u("a") * (u("b") + u("c")) * u("d")));
    }

    #[test]
    fn def() {
        eq!(nodes("def m"), bod!(d("m")));
        assert!(parse("def \n").is_err());
        assert!(parse("def 2").is_err());
    }

    #[test]
    fn add() {
        eq!(nodes("m + s"), bod!(u("m") + u("s")));
    }

    #[test]
    fn sub() {
        eq!(nodes("m - s"), bod!(u("m") - u("s")));
    }

    #[test]
    fn mul() {
        eq!(nodes("m * s"), bod!(u("m") * u("s")));
    }

    #[test]
    fn div() {
        eq!(nodes("m / s"), bod!(u("m") / u("s")));
    }

    #[test]
    fn unary_op() {
        eq!(nodes("2 meter"), bod!(n(2) * u("meter")));
        eq!(nodes("345meter"), bod!(n(345) * u("meter")));
        eq!(nodes("- meter"), bod!(n(-1) * u("meter")));
        eq!(
            nodes("- meter * -s"),
            bod!((n(-1) * u("meter")) * (n(-1) * u("s")))
        );
        a!(parse("+ meter").is_err());
    }

    #[test]
    fn syntax_err() {
        a!(parse("def me$ter").is_err());
        a!(parse("me<er").is_err());
    }
}
