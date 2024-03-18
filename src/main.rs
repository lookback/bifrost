#![warn(clippy::all)]
#![allow(clippy::inefficient_to_string)]

extern crate clap;

mod conv_kotlin;
mod conv_pass;
mod conv_rust;
mod conv_swift;
mod conv_ts;
mod filter;
mod indent;
mod parser;
mod token;

use crate::conv_kotlin::Kotlin;
use crate::conv_pass::Pass;
use crate::conv_rust::Rust;
use crate::conv_swift::Swift;
use crate::conv_ts::TypeScript;
use crate::filter::filter_ast;
use crate::parser::*;
use std::fmt::Display;
use std::fmt::{Formatter, Result};
use std::fs::File;
use std::io::prelude::Read;

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct Output {
    pub file_name: String,
    pub contents: String,
}

fn main() {
    let cmd = std::env::args().collect::<Vec<String>>().join(" ");

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
            clap::Arg::with_name("swift-all-optional")
                .help("If all values for swift should be optionals")
                .long("swift-all-optional"),
        )
        .arg(
            clap::Arg::with_name("swift-all-var")
                .help("If all fields in swift should be vars")
                .long("swift-all-var"),
        )
        .arg(
            clap::Arg::with_name("swift-all-class")
                .help("If all structs in swift should be classes")
                .long("swift-all-class"),
        )
        .arg(
            clap::Arg::with_name("kotlin-all-optional")
                .help("If all values for kotlin should be optionals")
                .long("kotlin-all-optional"),
        )
        .arg(
            clap::Arg::with_name("typescript-all-optional")
                .help("If all values for typescript should be optionals")
                .long("typescript-all-optional"),
        )
        .arg(
            clap::Arg::with_name("ignore-fields-with-args")
                .help("Don't break on fields with args, just ignore them")
                .long("ignore-fields-with-args"),
        )
        .arg(
            clap::Arg::with_name("lang")
                .help("Language to convert to")
                .required(true)
                .possible_values(&["pass", "kotlin", "rust", "swift", "typescript"]),
        )
        .arg(
            clap::Arg::with_name("source")
                .help("Graphql source file to use")
                .required(true),
        )
        .get_matches();

    let source = m.value_of("source").unwrap();
    let lang = m.value_of("lang").unwrap();
    let types: Option<Vec<&str>> = m.values_of("types").map(Iterator::collect);
    let gql = read_file(&source).expect("Failed to read file");

    let ast = parse::<Pass>(&gql).expect("Parse failed");
    let types = types.unwrap_or_else(|| ast.tree.iter().map(Tree::name).collect());

    let swift_all_optional = m.occurrences_of("swift-all-optional") > 0;
    if swift_all_optional {
        std::env::set_var("SWIFT_ALL_OPTIONAL", "true");
    }
    let swift_all_var = m.occurrences_of("swift-all-var") > 0;
    if swift_all_var {
        std::env::set_var("SWIFT_ALL_VAR", "true");
    }
    let swift_all_class = m.occurrences_of("swift-all-class") > 0;
    if swift_all_class {
        std::env::set_var("SWIFT_ALL_CLASS", "true");
    }

    let kotlin_all_optional = m.occurrences_of("kotlin-all-optional") > 0;
    if kotlin_all_optional {
        std::env::set_var("KOTLIN_ALL_OPTIONAL", "true");
    }

    let ts_all_optional = m.occurrences_of("typescript-all-optional") > 0;
    if ts_all_optional {
        std::env::set_var("TYPESCRIPT_ALL_OPTIONAL", "true");
    }

    let ignore_fields_with_args = m.occurrences_of("ignore-fields-with-args") > 0;
    if ignore_fields_with_args {
        std::env::set_var("IGNORE_FIELDS_WITH_ARGS", "true");
    }

    let out = ToOut {
        cmd: &cmd,
        lang,
        ast,
        types,
    };

    print!("{}", out);
}

struct ToOut<'a> {
    cmd: &'a str,
    lang: &'a str,
    ast: Ast<'a, Pass>,
    types: Vec<&'a str>,
}

impl<'a> Display for ToOut<'a> {
    fn fmt(&self, f: &mut Formatter) -> Result {
        let ast = self.ast.clone();
        match self.lang {
            "pass" => {
                let flt = filter_ast(&ast, &self.types);
                writeln!(f, "# Generated using:")?;
                writeln!(f, "# {}\n", &self.cmd)?;
                writeln!(f, "{}", flt)
            }
            "kotlin" => {
                let ast: Ast<Kotlin> = unsafe { std::mem::transmute(ast) };
                let flt = filter_ast(&ast, &self.types);
                writeln!(f, "// Generated using:")?;
                writeln!(f, "// {}\n", &self.cmd)?;
                writeln!(f, "{}", flt)
            }
            "rust" => {
                let ast: Ast<Rust> = unsafe { std::mem::transmute(ast) };
                let flt = filter_ast(&ast, &self.types);
                writeln!(f, "// Generated using:")?;
                writeln!(f, "// {}\n", &self.cmd)?;
                writeln!(f, "{}", flt)
            }
            "swift" => {
                let ast: Ast<Swift> = unsafe { std::mem::transmute(ast) };
                let flt = filter_ast(&ast, &self.types);
                writeln!(f, "// Generated using:")?;
                writeln!(f, "// {}\n", &self.cmd)?;
                writeln!(f, "{}", flt)
            }
            "typescript" => {
                let ast: Ast<TypeScript> = unsafe { std::mem::transmute(ast) };
                let flt = filter_ast(&ast, &self.types);
                writeln!(f, "// Generated using:")?;
                writeln!(f, "// {}\n", &self.cmd)?;
                writeln!(f, "{}", flt)
            }
            _ => panic!("Unknown output"),
        }
    }
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
