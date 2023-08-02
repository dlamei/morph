mod error;
mod eval;
mod morph;
mod parser;
mod types;

use std::env;

const SRC: &str = r"
def m
meter = m
meters = m
def s
second = s
seconds = s


a = 2;
!a
";

fn get_input() -> String {
    let mut input = String::new();

    loop {
        std::io::stdin()
            .read_line(&mut input)
            .expect("Failed to read line");

        let (round_open, round_close) = (
            input.chars().filter(|&c| c == '(').count(),
            input.chars().filter(|&c| c == ')').count(),
        );
        let (curly_open, curly_close) = (
            input.chars().filter(|&c| c == '{').count(),
            input.chars().filter(|&c| c == '}').count(),
        );

        if round_open == round_close && curly_open == curly_close {
            break;
        } else {
            let mut next_input = String::new();
            std::io::stdin()
                .read_line(&mut next_input)
                .expect("Failed to read line");
            input.push_str(&next_input);
        }
    }

    input
}

fn main() {
    let args: Vec<String> = env::args().collect();

    if Some("repl".to_string()) == args.get(1).map(|x| x.to_owned().to_lowercase()) {
        let mut input = String::new();

        while input.to_lowercase() != "exit".to_string() {
            input = get_input();
            morph::run("<STDIN>", &input);
        }
    } else {
        morph::run("<STDIN>", SRC);
    }
}
