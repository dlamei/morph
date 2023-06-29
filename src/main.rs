mod morph;
mod parser;
mod types;

const SRC: &str = r"
def dir;

3 mster

- meter * s

3.4

second
joules";

fn main() {
    morph::run("<STDIN>", SRC);
}
