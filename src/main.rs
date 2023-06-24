mod morph;
mod types;

const SRC: &str = r"
    m + meter
    s / d
";

fn main() {
    morph::parse(SRC);
}
