use crate::parser::Enum;
use crate::parser::Field;
use crate::parser::Type;
use crate::parser::TypeExpr;
use crate::parser::Union;
use crate::parser::{Ast, Tree};
use std::fmt::Display;
use std::fmt::{Formatter, Result};

#[derive(Clone)]
pub struct Swift {}


fn translate_typ(typ: &str) -> &str {
    match typ {
        "Int" => "Int",
        "Float" => "Float",
        "String" => "String",
        "Boolean" => "Bool",
        "ID" => "ID",
        "Date" => "Date",
        _ => typ,
    }
}

impl<'a> Display for Ast<'a, Swift> {
    fn fmt(&self, f: &mut Formatter) -> Result {
        if self.has_type(|t| t.typ == "ID") {
            writeln!(f, "typealias ID = String;\n")?;
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

impl<'a> Display for Tree<'a, Swift> {
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

impl<'a> Display for Type<'a, Swift> {
    fn fmt(&self, f: &mut Formatter) -> Result {
        write_doc(f, "", self.doc)?;
        writeln!(f, "struct {}: Codable {{", self.name)?;
        for field in &self.fields {
            writeln!(f, "{}", field)?;
        }
        writeln!(f, "}}")?;
        Ok(())
    }
}

impl<'a> Display for Field<'a, Swift> {
    fn fmt(&self, f: &mut Formatter) -> Result {
        let has_args = !self.args.is_empty();
        let ignore_fields_with_args = std::env::var("IGNORE_FIELDS_WITH_ARGS")
            .map(|s| s == "true").unwrap_or(false);
        if has_args {
            if ignore_fields_with_args {
                return Ok(());
            }
            panic!("Can't generate field with args");
        }
        write_doc(f, "  ", self.doc)?;
        write!(f, "  let {}: {}", self.name, self.expr)?;
        Ok(())
    }
}

impl<'a> Display for TypeExpr<'a, Swift> {
    fn fmt(&self, f: &mut Formatter) -> Result {
        let swift_all_optional = std::env::var("SWIFT_ALL_OPTIONAL")
            .map(|s| s == "true").unwrap_or(false);
        if self.arr.is_arr() {
            write!(f, "[")?;
        }
        write!(f, "{}", translate_typ(self.typ))?;
        if swift_all_optional || !swift_all_optional && self.null {
            write!(f, "?")?;
        }
        if self.arr.is_arr() {
            write!(f, "]")?;
            if swift_all_optional || !swift_all_optional && self.arr.is_null() {
                write!(f, "?")?;
            }
        }
        Ok(())
    }
}

impl<'a> Display for Enum<'a, Swift> {
    fn fmt(&self, f: &mut Formatter) -> Result {
        write_doc(f, "", self.doc)?;
        writeln!(f, "enum {}: String, Codable, CaseIterable {{", self.name)?;
        for v in &self.values {
            write_doc(f, "  ", v.doc)?;
            writeln!(f, "  case {}", v.value)?;
        }
        writeln!(f, "}}")?;
        Ok(())
    }
}

impl<'a> Display for Union<'a, Swift> {
    fn fmt(&self, _: &mut Formatter) -> Result {
        panic!("Union?!");
    }
}
