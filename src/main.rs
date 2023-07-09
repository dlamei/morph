mod error;
mod eval;
mod morph;
mod parser;
mod types;

const SRC: &str = r"
def m
meter = m
meters = m
km = 1000 m
cm = 0.01 m
mm = 0.1 cm
def s

1m != 1
";

fn main() {
    morph::run("<STDIN>", SRC);
}
