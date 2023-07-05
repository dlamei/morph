mod error;
mod eval;
mod morph;
mod parser;
mod types;

const SRC: &str = r"
def m
meter = m
def s

3 * meter / s / 0
";

fn main() {
    morph::run("<STDIN>", SRC);
}
