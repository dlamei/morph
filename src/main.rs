mod error;
mod eval;
mod morph;
mod parser;
mod types;

const SRC: &str = r"
def m

d = {
    d = 3m
}

d
";

fn main() {
    morph::run("<STDIN>", SRC);
}
