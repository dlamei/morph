mod morph;
mod parser;
mod types;

const SRC: &str = r"
$

3 d

+ meter * s

3

meter d $
joules";

fn main() {
    morph::run("<STDIN>", SRC);
}
