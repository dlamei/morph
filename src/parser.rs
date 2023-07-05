use std::ops;

use crate::error::*;
use crate::types::*;

use chumsky::{input::ValueInput, prelude::*, primitive};
use rust_decimal_macros::dec;

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

macro_rules! set_err_type {
    ($err: tt, $typ: expr) => {{
        $err.set_type($typ);
        $err
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

macro_rules! parse {
    (num) => {
        select!(Token::Num(x) => NodeType::Num(x)).map_with_span(Node::new)
    };

    (unit) => {
        select!(Token::Unit(x) => NodeType::Unit(x)).map_with_span(Node::new)
    };

    (num * unit) => {
        parse!(num).then(parse!(unit)).map(|(x1, x2)| x1 * x2)
    };
}

impl<'a> Node<'a> {
    pub fn syntax_err<I>() -> Boxed<'a, 'a, I, Node<'a>, extra::Err<MorphError<'a>>>
    where
        I: ValueInput<'a, Token = Token<'a>, Span = SimpleSpan>,
    {
        primitive::select::<'_, _, I, &str, extra::Err<MorphError<'a>>>(|x, _| match x {
            Token::LexErr(err) => Some(err),
            _ => None,
        })
        .validate(|x, span, emit| {
            emit.emit(MorphError::custom(
                span,
                format!("unsupported character: {}", x),
                ErrorType::CouldNotLex,
            ))
        })
        .map_with_span(|_, span| Node::err(span))
        .boxed()
    }

    pub fn expression<I>(
        scope_parser: impl Parser<'a, I, Node<'a>, extra::Err<MorphError<'a>>> + 'a,
    ) -> Boxed<'a, 'a, I, Node<'a>, extra::Err<MorphError<'a>>>
    where
        I: ValueInput<'a, Token = Token<'a>, Span = SimpleSpan>,
    {
        let expr = recursive(|expr| {
            let atom = choice((
                expr.delimited_by(just(Token::LParen), just(Token::RParen)),
                scope_parser.delimited_by(just(Token::LCurly), just(Token::RCurly)),
                parse!(num * unit),
                parse!(unit),
                parse!(num),
            ))
            .map_err(|err: MorphError| merge_expected!(err::<I>, [Token::NUM, Token::UNIT]));

            let unary = just(Token::Sub)
                .map_with_span(|_, span| Node::new(NodeType::Num(num!(-1)), span))
                .repeated()
                .foldr(atom, |op, rhs| op * rhs)
                .boxed();

            let product = unary.clone().foldl(
                choice((
                    just(Token::Mul).to(ops::Mul::mul as fn(_, _) -> _),
                    just(Token::Div).to(ops::Div::div as fn(_, _) -> _),
                ))
                .then(unary)
                .repeated(),
                |lhs, (op, rhs)| op(lhs, rhs),
            );

            let sum = product.clone().foldl(
                choice((
                    just(Token::Add).to(ops::Add::add as fn(_, _) -> _),
                    just(Token::Sub).to(ops::Sub::sub as fn(_, _) -> _),
                ))
                .then(product)
                .repeated(),
                |lhs, (op, rhs)| op(lhs, rhs),
            );

            let assign = parse!(unit).foldl(
                choice((
                    just(Token::Assign).to(Node::assign as fn(&mut Node<'a>, Node<'a>)),
                    just(Token::AddAssign)
                        .to(ops::AddAssign::add_assign as fn(&mut Node<'a>, Node<'a>)),
                    just(Token::SubAssign)
                        .to(ops::SubAssign::sub_assign as fn(&mut Node<'a>, Node<'a>)),
                    just(Token::MulAssign)
                        .to(ops::MulAssign::mul_assign as fn(&mut Node<'a>, Node<'a>)),
                    just(Token::DivAssign)
                        .to(ops::DivAssign::div_assign as fn(&mut Node<'a>, Node<'a>)),
                ))
                .then(sum.clone())
                .repeated()
                .at_least(1),
                |mut lhs: Node, (op, rhs): (_, Node)| {
                    op(&mut lhs, rhs);
                    lhs
                },
            );

            let def = just(Token::Def)
                .ignore_then(parse!(unit))
                .map_with_span(|x: Node, span: SimpleSpan| cast_enum!(x.typ => (NodeType::Unit(name)) {return Node::new(NodeType::Def(name), span)}))
                .map_err(|err: MorphError| merge_expected!(err::<I>, [Token::UNIT]));

            choice((assign, sum, def))
        });

        choice((expr, Self::syntax_err())).boxed()
    }

    pub fn parser<I>() -> Boxed<'a, 'a, I, Node<'a>, extra::Err<MorphError<'a>>>
    where
        I: ValueInput<'a, Token = Token<'a>, Span = SimpleSpan>,
    {
        let scope = recursive(|scope| {
            just(Token::NL)
                .repeated()
                .ignore_then(
                    Self::expression(scope)
                        .then_ignore(choice((
                            just(Token::NL).repeated().at_least(1),
                            just(Token::RCurly).to(()),
                            end()
                        )))
                        .map_err(|mut err: MorphError| {
                            set_err_type!(err, ErrorType::UndefinedSyntax)
                        })
                        .recover_with(via_parser(
                            none_of([Token::NL, Token::RCurly])
                                .repeated()
                                .at_least(1)
                                .map_with_span(|_, span| Node::err(span)),
                        )),
                )
                .repeated()
                .collect::<Vec<_>>()
                .map_with_span(|x, span| Node::new(NodeType::Scope(x), span))
        });

        // let scopes =
        //     just(Token::NL)
        //     .repeated()
        //     .ignore_then(
        //         expr.then_ignore(choice((just(Token::NL).repeated().at_least(1), end())))
        //             .map_err(|mut err: ParseError| {
        //                 set_err_type!(err, ParseErrorType::UndefinedSyntax)
        //             })
        //             .recover_with(via_parser(
        //                 none_of([Token::NL])
        //                     .repeated()
        //                     .at_least(1)
        //                     .map_with_span(|_, span| Node::err(span)),
        //             )),
        //     )
        //     .repeated()
        //     .collect::<Vec<_>>()
        //     .map_with_span(|x, span| Node::new(NodeType::Scope(x), span));

        let empty = just(Token::NL)
            .repeated()
            .then(end())
            .map(|(_, _)| Node::new(NodeType::Scope(vec![]), 0..0));

        empty.or(scope).boxed()
    }
}

#[cfg(test)]
mod test {
    use crate::{error::*, morph::test_utils::*, types::*};

    #[test]
    fn basic_expr() {
        eq!(nodes(""), bod!());
        eq!(nodes(";;\n;\n\n"), bod!());
        eq!(nodes("def m \n m * s"), bod!(d("m"), u("m") * u("s")));
        eq!(
            nodes("a + b * c + d"),
            bod!(u("a") + u("b") * u("c") + u("d"))
        );
        eq!(
            nodes("a * (b + c) * d"),
            bod!(u("a") * (u("b") + u("c")) * u("d"))
        );
    }

    #[test]
    fn def() {
        eq!(nodes("def m"), bod!(d("m")));
        assert!(!parse("def \n").1.is_empty());
        assert!(!parse("def 2").1.is_empty());
        assert!(cmp_expected(
            parse("def ; meter").1.swap_remove(0).into_reason(),
            &[Token::UNIT]
        ));
    }

    #[test]
    fn binop() {
        eq!(nodes("m / s"), bod!(u("m") / u("s")));
        eq!(nodes("m * s"), bod!(u("m") * u("s")));
        eq!(nodes("m - s"), bod!(u("m") - u("s")));
        eq!(nodes("m + s"), bod!(u("m") + u("s")));
    }

    #[test]
    fn assign() {
        let mut a = u("a");
        a.assign(n(2) / n(3) + n(2));
        eq!(nodes("a = 2 / 3 + 2"), bod!(a));

        let mut b = u("b");
        b += u("m") + n(3) * n(2);
        eq!(nodes("b += m + 3 * 2"), bod!(b));

        let mut c = u("c");
        c *= u("m") / u("s");
        eq!(nodes("c *= m / s"), bod!(c));

        let mut d = u("d");
        d /= n(0);
        eq!(nodes("d /= 0"), bod!(d));
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
        assert!(!parse("+ meter").1.is_empty());
    }

    #[test]
    fn syntax_err() {
        assert!(!parse("def me$ter").1.is_empty());
        assert!(!parse("me<er").1.is_empty());
    }

    #[test]
    fn error_recover() {
        eq!(parse("def ; meter; +second; dir; --m").1.len(), 2)
    }

    #[test]
    fn error_type() {
        eq!(parse("$").1[0].typ, ErrorType::CouldNotLex);
        eq!(parse("+ meter").1[0].typ, ErrorType::UndefinedSyntax);
    }
}
