mod morph;
mod parser;
mod types;

use ariadne::{Color, Label, Report, ReportKind, Source};

const SRC: &str = r"
def meter

3 meter

- meter * s


second
joules 
";

fn main() {
    let res = morph::parse(SRC);

    match res {
        Ok(n) => println!("{}", n.to_code()),
        Err(errs) => {
            for err in errs {
                Report::build(ReportKind::Error, (), err.span().start)
                    .with_message(err.to_string())
                    .with_label(
                        Label::new(err.span().into_range())
                            .with_message(err.reason().to_string())
                            .with_color(Color::Red),
                    )
                    .finish()
                    .eprint(Source::from(SRC))
                    .unwrap();
            }
        }
    }
}

#[cfg(test)]
mod test_utils {
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

    macro_rules! a {
    ($cond:expr) => {
        if !$cond {
            panic!("assertion failed: {}", stringify!($cond));
        }
    };
    ($cond:expr, $($arg:tt)+) => {
        if !$cond {
            panic!("assertion failed: {}: {}", stringify!($cond), format_args!($($arg)+));
        }
    };
}

    pub(crate) use bod;
    pub(crate) use eq;
    pub(crate) use a;

    pub fn u(name: &str) -> Node {
        Node::Unit(name)
    }

    pub fn d(name: &str) -> Node {
        Node::Def(name)
    }

    pub fn n(val: NumType) -> Node<'static> {
        Node::Num(val)
    }

    pub fn parse(code: &str) -> morph::ParseResult {
        morph::parse(code)
    }

    pub fn nodes(code: &str) -> Node {
        morph::parse(code).unwrap()
    }
}
