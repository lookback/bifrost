extern crate clap;

mod conv_pass;
mod conv_rust;
mod filter;
mod parser;
mod token;

use conv_pass::Pass;
use conv_rust::Rust;
use filter::filter_ast;
use parser::*;
use std::fs::File;
use std::io::prelude::Read;

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct Output {
    pub file_name: String,
    pub contents: String,
}

fn main() {
    let m = clap::App::new("bifrost")
        .arg(
            clap::Arg::with_name("types")
                .help("Filter to only these types")
                .multiple(true)
                .takes_value(true)
                .long("types")
                .short("t"),
        )
        .arg(
            clap::Arg::with_name("lang")
                .help("Language to convert to")
                .required(true)
                .possible_values(&["none", "rust"]),
        )
        .arg(
            clap::Arg::with_name("source")
                .help("Graphql source file to use")
                .required(true),
        )
        .get_matches();

    let source = m.value_of("source").unwrap();
    let lang = m.value_of("lang").unwrap();
    let types: Vec<&str> = m
        .values_of("types")
        .map(|t| t.collect())
        .unwrap_or_else(|| vec![]);
    let contents = read_file(&source).expect("Failed to read file");

    let output = match lang {
        "none" => {
            let f = filter_ast(&parse::<Pass>(&contents).expect("Parse failed"), &types);
            format!("{}", f)
        }
        "rust" => {
            let f = filter_ast(&parse::<Rust>(&contents).expect("Parse failed"), &types);
            format!("{}", f)
        }
        _ => panic!("Unknown output"),
    };

    print!("{}", output);
}

fn read_file(source: &str) -> std::io::Result<String> {
    let mut file = File::open(source)?;
    let mut contents = String::new();
    file.read_to_string(&mut contents)?;
    Ok(contents)
}

impl ::std::fmt::Display for Output {
    fn fmt(&self, w: &mut ::std::fmt::Formatter) -> ::std::fmt::Result {
        write!(w, "File: {}\n{}", self.file_name, self.contents)
    }
}