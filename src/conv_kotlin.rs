use crate::parser::Enum;
use crate::parser::Type;
use crate::parser::TypeExpr;
use crate::parser::TypeKind;
use crate::parser::Union;
use crate::parser::{Ast, Tree};
use std::fmt::Display;
use std::fmt::{Formatter, Result};

#[derive(Clone)]
pub struct Kotlin {}

fn translate_typ(typ: &str) -> &str {
    match typ {
        "Int" => "Int",
        "Float" => "Float",
        "String" => "String",
        "Boolean" => "Boolean",
        "ID" => "ID",
        "Date" => "Date",
        _ => typ,
    }
}

impl<'a> Display for Ast<'a, Kotlin> {
    fn fmt(&self, f: &mut Formatter) -> Result {
        if self.has_type(|t| t.typ == "Date") {
            writeln!(f, "import java.util.Date\n")?;
        }
        if self.has_type(|t| t.typ == "ID") {
            writeln!(f, "typealias ID = String\n")?;
        }
        for (idx, t) in self.tree.iter().enumerate() {
            if idx > 0 {
                writeln!(f)?;
            }
            write!(f, "{}", t)?;
        }
        Ok(())
    }
}

fn write_doc(f: &mut Formatter, indent: &str, doc: Option<&str>) -> Result {
    if let Some(doc) = doc {
        let count = doc.split('\n').count();
        writeln!(f, "{}/**", indent)?;
        for (idx, line) in doc.split('\n').enumerate() {
            let line = line.trim();
            // keep interior empty lines, but not first or last
            if line.is_empty() && (idx == 0 || idx == count - 1) {
                continue;
            }
            writeln!(f, "{} * {}", indent, line)?;
        }
        writeln!(f, "{} */", indent)?;
    }
    Ok(())
}

impl<'a> Display for Tree<'a, Kotlin> {
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

impl<'a> Display for Type<'a, Kotlin> {
    fn fmt(&self, f: &mut Formatter) -> Result {
        write_doc(f, "", self.doc)?;
        match self.kind {
            TypeKind::Type | TypeKind::Input => {
                writeln!(f, "data class {}(", self.name)?;
                for (idx, field) in self.fields.iter().enumerate() {
                    write_doc(f, "    ", field.doc)?;
                    // TODO: add "override" when field is in interfaces
                    // let override = self.is_field_in_interfaces(ast: Ast<'a, T>, field);
                    write!(f, "    val {}: {}", field.name, field.expr)?;
                    let is_last = idx == self.fields.len() - 1;
                    writeln!(f, "{}", if is_last { "" } else { ",\n" })?;
                }
                write!(f, ")")?;
                if !self.interfaces.is_empty() {
                    write!(f, " {}", self.interfaces.join(", "))?;
                }
            }
            TypeKind::Interface => {
                writeln!(f, "interface {} {{", self.name)?;
                for field in &self.fields {
                    write_doc(f, "    ", field.doc)?;
                    writeln!(f, "    var {}: {} {{ get set }}", field.name, field.expr)?;
                }
            }
        }
        Ok(())
    }
}

impl<'a> Display for TypeExpr<'a, Kotlin> {
    fn fmt(&self, f: &mut Formatter) -> Result {
        let kotlin_all_optional = std::env::var("KOTLIN_ALL_OPTIONAL")
            .map(|s| s == "true")
            .unwrap_or(false);
        if self.arr.is_arr() {
            write!(f, "List<")?;
        }
        write!(f, "{}", translate_typ(self.typ))?;
        if self.null || kotlin_all_optional {
            write!(f, "?")?;
        }
        if self.arr.is_arr() {
            write!(f, ">")?;
            if self.arr.is_null() || kotlin_all_optional {
                write!(f, "?")?;
            }
        }
        Ok(())
    }
}

impl<'a> Display for Enum<'a, Kotlin> {
    fn fmt(&self, f: &mut Formatter) -> Result {
        write_doc(f, "", self.doc)?;
        writeln!(f, "enum class {} {{", self.name)?;
        for (idx, v) in self.values.iter().enumerate() {
            write_doc(f, "    ", v.doc)?;
            let is_last = idx == self.values.len() - 1;
            write!(f, "    {}", v.value)?;
            writeln!(f, "{}", if is_last { "" } else { ",\n" })?;
        }
        writeln!(f, "}}")?;
        Ok(())
    }
}

impl<'a> Display for Union<'a, Kotlin> {
    fn fmt(&self, f: &mut Formatter) -> Result {
        writeln!(f, "sealed class {} {{", self.name)?;
        let mut lcaseds = vec![];
        for name in &self.names {
            let mut lcased = name.to_string();
            if let Some(r) = lcased.get_mut(0..1) {
                r.make_ascii_lowercase();
            }
            writeln!(
                f,
                "    class {}{}(val {}: {}) : {}",
                self.name, name, lcased, name, self.name
            )?;
            lcaseds.push((name, lcased));
        }
        Ok(())
    }
}
