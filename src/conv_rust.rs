use crate::parser::Enum;
use crate::parser::Field;
use crate::parser::Type;
use crate::parser::TypeExpr;
use crate::parser::Union;
use crate::parser::{Ast, Tree};
use std::fmt::Display;
use std::fmt::{Formatter, Result};

#[derive(Clone)]
pub struct Rust {}


fn translate_typ(typ: &str) -> &str {
    match typ {
        "Int" => "i32",
        "Float" => "f64",
        "String" => "String",
        "Boolean" => "bool",
        "ID" => "ID",
        "Date" => "Date<Utc>",
        _ => typ,
    }
}

impl<'a> Display for Ast<'a, Rust> {
    fn fmt(&self, f: &mut Formatter) -> Result {
        writeln!(f, "#![allow(non_snake_case)]\n ")?;
        if self.has_type(|t| t.typ == "Date") {
            writeln!(f, "use chrono::{{Date, Utc}};\n")?;
        }
        if self.has_type(|t| t.typ == "ID") {
            writeln!(f, "pub type ID = String;\n")?;
        }
        for (idx, t) in self.tree.iter().enumerate() {
            if idx > 0 {
                write!(f, "\n")?;
            }
            write!(f, "{}", t)?;
        }
        Ok(())
    }
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
        writeln!(f, "#[derive(Debug, Clone, Copy, Serialize, Deserialize)]")?;
        writeln!(f, "pub struct {} {{", self.name)?;
        for field in &self.fields {
            writeln!(f, "{}", field)?;
        }
        writeln!(f, "}}")?;
        Ok(())
    }
}

impl<'a> Display for Field<'a, Rust> {
    fn fmt(&self, f: &mut Formatter) -> Result {
        write_doc(f, "", self.doc)?;
        let expr = &self.expr;
        if expr.arr.is_arr() && expr.arr.is_null() || !expr.arr.is_arr() && expr.null {
            write!(f, "  #[serde(skip_serializing_if = \"Option::is_none\")]\n")?;
        }
        write!(f, "  pub {}: {},", self.name, self.expr)?;
        if !self.args.is_empty() {
            panic!("Can't generate field with args");
        }
        Ok(())
    }
}

impl<'a> Display for TypeExpr<'a, Rust> {
    fn fmt(&self, f: &mut Formatter) -> Result {
        if self.arr.is_arr() {
            if self.arr.is_null() {
                write!(f, "Option<")?;
            }
            write!(f, "Vec<")?;
        }
        if self.null {
            write!(f, "Option<")?;
        }
        write!(f, "{}", translate_typ(self.typ))?;
        if self.null {
            write!(f, ">")?;
        }
        if self.arr.is_arr() {
            if self.arr.is_null() {
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
        writeln!(f, "#[derive(Debug, Clone, Copy, Serialize, Deserialize)]")?;
        writeln!(f, "pub enum {} {{", self.name)?;
        for v in &self.values {
            write_doc(f, "", v.doc)?;
            writeln!(f, "  {},", v.value)?;
        }
        writeln!(f, "}}")?;
        Ok(())
    }
}

impl<'a> Display for Union<'a, Rust> {
    fn fmt(&self, _: &mut Formatter) -> Result {
        panic!("Union?!");
    }
}
