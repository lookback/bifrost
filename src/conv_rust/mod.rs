use crate::indent::indent;
use crate::parser::Enum;
use crate::parser::Field;
use crate::parser::Type;
use crate::parser::TypeExpr;
use crate::parser::TypeKind;
use crate::parser::Union;
use crate::parser::{Ast, Tree};
use std::fmt::Display;
use std::fmt::{Formatter, Result};

// These are files rather than static str block str block to allow testing and working on this module as if
// it was a regular Rust file. The contain support for
// serializing and deserializing DateTime<Utc> and Option<DateTime<Utc>> as unix timestamps.
const DATETIME_FMT: &'static str = include_str!("datetime_fmt.rs");
const DATETIME_FMT_NULLABLE: &'static str = include_str!("datetime_fmt_nullable.rs");

#[derive(Clone)]
pub struct Rust {}

fn translate_typ(typ: &str) -> &str {
    match typ {
        "Int" => "i32",
        "Float" => "f64",
        "String" => "String",
        "Boolean" => "bool",
        "ID" => "ID",
        "Date" => "DateTime<Utc>",
        "BigInt" => "i64",
        _ => typ,
    }
}

impl<'a> Display for Ast<'a, Rust> {
    fn fmt(&self, f: &mut Formatter) -> Result {
        let rust_all_optional = std::env::var("RUST_ALL_OPTIONAL")
            .map(|s| s == "true")
            .unwrap_or(false);

        writeln!(f, "#![allow(non_snake_case)]")?;
        writeln!(f, "#![allow(non_camel_case_types)]\n")?;
        writeln!(f, "#![cfg(not(doctest))]\n")?;
        writeln!(f, "use serde::{{Deserialize, Serialize}};\n")?;

        let has_non_null_dates =
            self.has_type(|t| t.typ == "Date" && !t.null) && !rust_all_optional;
        let has_null_dates = self.has_type(|t| t.typ == "Date" && t.null) || rust_all_optional;
        let has_dates = has_non_null_dates || has_null_dates;

        if has_dates {
            writeln!(f, "use chrono::{{DateTime, Utc}};\n")?;
        }

        if has_non_null_dates {
            writeln!(
                f,
                "mod datetime_fmt {{
{}
}}",
                indent(DATETIME_FMT.trim_end(), 1)
            )?;
        }

        if has_null_dates {
            writeln!(
                f,
                "mod datetime_fmt_nullable {{
{}
}}",
                indent(DATETIME_FMT_NULLABLE.trim_end(), 1)
            )?;
        }

        if self.has_type(|t| t.typ == "ID") {
            writeln!(f, "pub type ID = String;\n")?;
        }
        for (idx, t) in self.tree.iter().enumerate() {
            if idx > 0 {
                writeln!(f)?;
            }
            write!(f, "{}", t)?;
            if let Some(t) = t.as_type() {
                for i_name in &t.interfaces {
                    if let Some(i_t) = self.get_tree(i_name).and_then(Tree::as_type) {
                        write_impl_trait(f, i_t, t)?;
                    }
                }
            }
        }
        Ok(())
    }
}

fn write_impl_trait<'a>(
    f: &mut Formatter,
    interface: &Type<'a, Rust>,
    typ: &Type<'a, Rust>,
) -> Result {
    writeln!(f, "\nimpl {} for {} {{", interface.name, typ.name)?;
    for field in &interface.fields {
        writeln!(f, "    fn {}(&self) -> {} {{", field.name, field.expr)?;
        writeln!(f, "        self.{}", field.name)?;
        writeln!(f, "    }}")?;
    }
    writeln!(f, "}}")?;
    Ok(())
}

fn write_doc(f: &mut Formatter, indent: &str, doc: Option<&str>) -> Result {
    if let Some(doc) = doc {
        let count = doc.split('\n').count();
        for (idx, line) in doc.split('\n').enumerate() {
            let line = line.trim();
            // keep interior empty lines, but not first or last
            if line.is_empty() && (idx == 0 || idx == count - 1) {
                continue;
            }
            writeln!(f, "{}/// {}", indent, line)?;
        }
    }
    Ok(())
}

impl<'a> Display for Tree<'a, Rust> {
    fn fmt(&self, f: &mut Formatter) -> Result {
        match self {
            Tree::Dr(_) => (),
            Tree::Sc(_) => (),
            Tree::Ty(t) => write!(f, "{}", t)?,
            Tree::En(e) => write!(f, "{}", e)?,
            Tree::Un(u) => write!(f, "{}", u)?,
        }
        Ok(())
    }
}

impl<'a> Display for Type<'a, Rust> {
    fn fmt(&self, f: &mut Formatter) -> Result {
        write_doc(f, "", self.doc)?;
        match self.kind {
            TypeKind::Type | TypeKind::Input => {
                writeln!(
                    f,
                    "#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]"
                )?;
                writeln!(f, "pub struct {} {{", self.name)?;
                for field in &self.fields {
                    writeln!(f, "{}", field)?;
                }
            }
            TypeKind::Interface => {
                writeln!(f, "pub trait {} {{", self.name)?;
                for field in &self.fields {
                    fmt_field(field, f, true)?;
                }
            }
        }
        writeln!(f, "}}")?;
        Ok(())
    }
}

impl<'a> Display for Field<'a, Rust> {
    fn fmt(&self, f: &mut Formatter) -> Result {
        fmt_field(self, f, false)
    }
}

fn fmt_field<'a>(field: &Field<'a, Rust>, f: &mut Formatter, as_accessor: bool) -> Result {
    let rust_all_optional = std::env::var("RUST_ALL_OPTIONAL")
        .map(|s| s == "true")
        .unwrap_or(false);

    let has_args = !field.args.is_empty();
    let ignore_fields_with_args = std::env::var("IGNORE_FIELDS_WITH_ARGS")
        .map(|s| s == "true")
        .unwrap_or(false);
    if has_args && ignore_fields_with_args {
        return Ok(());
    }
    write_doc(f, "    ", field.doc)?;
    if as_accessor {
        writeln!(f, "    fn {}(&self) -> {};", field.name, field.expr)?;
    } else {
        let expr = &field.expr;
        if expr.arr.is_arr() && expr.arr.is_null() || !expr.arr.is_arr() && expr.null {
            writeln!(f, "    #[serde(skip_serializing_if = \"Option::is_none\")]")?;
        }

        if expr.typ == "Date" {
            if expr.null || rust_all_optional {
                writeln!(f, r#"    #[serde(with = "datetime_fmt_nullable")]"#)?;
            } else {
                writeln!(f, r#"    #[serde(with = "datetime_fmt")]"#)?;
            }
        }

        write!(f, "    pub {}: {},", escape_keyword(field.name), field.expr)?;
    }
    Ok(())
}

impl<'a> Display for TypeExpr<'a, Rust> {
    fn fmt(&self, f: &mut Formatter) -> Result {
        let rust_all_optional = std::env::var("RUST_ALL_OPTIONAL")
            .map(|s| s == "true")
            .unwrap_or(false);

        if self.arr.is_arr() {
            if self.arr.is_null() || rust_all_optional {
                write!(f, "Option<")?;
            }
            write!(f, "Vec<")?;
        }
        if self.null || rust_all_optional {
            write!(f, "Option<")?;
        }
        write!(f, "{}", translate_typ(self.typ))?;
        if self.null || rust_all_optional {
            write!(f, ">")?;
        }
        if self.arr.is_arr() {
            if self.arr.is_null() || rust_all_optional {
                write!(f, ">")?;
            }
            write!(f, ">")?;
        }
        Ok(())
    }
}

impl<'a> Display for Enum<'a, Rust> {
    fn fmt(&self, f: &mut Formatter) -> Result {
        write_doc(f, "", self.doc)?;
        writeln!(
            f,
            "#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize)]"
        )?;
        writeln!(f, "pub enum {} {{", self.name)?;
        for v in &self.values {
            write_doc(f, "    ", v.doc)?;
            writeln!(f, "    {},", v.value)?;
        }
        writeln!(f, "}}")?;

        writeln!(f, "impl {} {{", self.name)?;
        writeln!(f, "    pub fn as_str(&self) -> &'static str {{",)?;
        writeln!(f, "        match self {{",)?;
        for v in &self.values {
            writeln!(
                f,
                "            {}::{} => \"{}\",",
                self.name, v.value, v.value
            )?;
        }
        writeln!(f, "        }}")?;
        writeln!(f, "    }}")?;
        writeln!(f, "}}")?;

        Ok(())
    }
}

impl<'a> Display for Union<'a, Rust> {
    fn fmt(&self, f: &mut Formatter) -> Result {
        writeln!(
            f,
            "#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]"
        )?;
        writeln!(f, "#[serde(tag = \"tag\", content = \"val\")]")?;
        writeln!(f, "pub enum {} {{", self.name)?;
        for name in &self.names {
            writeln!(f, "    {}({}),", name, name)?;
        }
        writeln!(f, "}}")
    }
}

fn escape_keyword<'a>(identifier: &'a str) -> &'a str {
    match identifier {
        "as" => "r#as",
        "break" => "r#break",
        "const" => "r#const",
        "continue" => "r#continue",
        "crate" => "r#crate",
        "else" => "r#else",
        "enum" => "r#enum",
        "extern" => "r#extern",
        "false" => "r#false",
        "fn" => "r#fn",
        "for" => "r#for",
        "if" => "r#if",
        "impl" => "r#impl",
        "in" => "r#in",
        "let" => "r#let",
        "loop" => "r#loop",
        "match" => "r#match",
        "mod" => "r#mod",
        "move" => "r#move",
        "mut" => "r#mut",
        "pub" => "r#pub",
        "ref" => "r#ref",
        "return" => "r#return",
        "self" => "r#self",
        "static" => "r#static",
        "struct" => "r#struct",
        "super" => "r#super",
        "trait" => "r#trait",
        "true" => "r#true",
        "type" => "r#type",
        "unsafe" => "r#unsafe",
        "use" => "r#use",
        "where" => "r#where",
        "while" => "r#while",
        _ => identifier,
    }
}

#[cfg(test)]
mod datetime_fmt;

#[cfg(test)]
mod test {
    use super::datetime_fmt;

    use chrono::{DateTime, Utc};
    use serde::{Deserialize, Serialize};
    use serde_json;

    #[derive(Debug, Serialize, Deserialize, PartialEq, Eq)]
    struct Foo {
        #[serde(with = "datetime_fmt")]
        date: DateTime<Utc>,
    }

    // Serialization + symmetry gives us sufficient confidence
    #[test]
    fn test_datetime_fmt_symmetry() {
        let date =
            DateTime::<Utc>::from_timestamp_micros(1710758790944000).expect("Valid datetime");
        let foo = Foo { date };

        let serialized = serde_json::to_string(&foo).expect("Serialize");

        let deserialized: Foo = serde_json::from_str(&serialized).expect("Deserialize");

        assert_eq!(foo, deserialized);
    }

    #[test]
    fn test_datetime_fmt_serialize() {
        let foo = Foo {
            date: DateTime::<Utc>::from_timestamp_micros(1539026461281000).expect("Valid datetime"),
        };

        let serialized = serde_json::to_string(&foo).expect("Serialize");
        let expected = r#"{"date":1539026461.281}"#;

        assert_eq!(expected, serialized);
    }
}
