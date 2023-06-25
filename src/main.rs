mod morph;
mod types;

use ariadne::{Color, Label, Report, ReportKind, Source};

const SRC: &str = r"
def meter

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
mod tests {
    use crate::morph;

    #[test]
    fn basic_expr() {
        assert!(morph::parse("def meter").is_ok());
        assert!(morph::parse("def $eter").is_err());
        assert!(morph::parse("meter * s").is_ok());
        assert!(morph::parse("def meter \n meter * s").is_ok());
    }
}
