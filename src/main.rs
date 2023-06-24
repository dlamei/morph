mod morph;
mod types;

use ariadne::{Color, Label, Report, ReportKind, Source};

const SRC: &str = r"
def $
m - meter
d
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
    #[test]
    fn it_works() {
        let result = 2 + 2;
        assert_eq!(result, 4);
    }
}
