mod error;
mod eval;
mod morph;
mod parser;
mod types;

const SRC: &str = r"
def m
meter = m
meters = m
def s
second = s
seconds = s


a = { res = 3 m / s; res *= 1 s } //TODO: fix return from scope
a
";

fn main() {
    morph::run("<STDIN>", SRC);
}
