mod morph;
mod parser;
mod types;

const SRC: &str = r"
b = m + 3 * 2
";

fn main() {
    morph::run("<STDIN>", SRC);
}
