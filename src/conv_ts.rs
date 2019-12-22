use crate::parser::Ast;
use crate::parser::Directive;
use crate::parser::Enum;
use crate::parser::EnumValue;
use crate::parser::Field;
use crate::parser::FieldArg;
use crate::parser::Scalar;

use crate::parser::Tree;
use crate::parser::Type;
use crate::parser::TypeExpr;
use crate::parser::TypeKind;
use crate::parser::TypedTarget;
use crate::parser::Union;
use std::fmt::Display;
use std::fmt::{Formatter, Result};

#[derive(Clone)]
pub struct TypeScript {}

fn translate_typ(typ: &str) -> &str {
    match typ {
        "Int" => "number",
        "Float" => "number",
        "String" => "string",
        "Boolean" => "boolean",
        "ID" => "ID",
        "Date" => "Date",
        "BigInt" => "number",
        _ => typ,
    }
}

impl<'a> Display for Ast<'a, TypeScript> {
    fn fmt(&self, f: &mut Formatter) -> Result {
        if self.has_type(|t| t.typ == "ID") {
            writeln!(f, "type ID = string;\n")?;
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

impl<'a> Display for Tree<'a, TypeScript> {
    fn fmt(&self, f: &mut Formatter) -> Result {
        match self {
            Tree::Dr(d) => write!(f, "{}", d)?,
            Tree::Sc(s) => write!(f, "{}", s)?,
            Tree::Ty(t) => write!(f, "{}", t)?,
            Tree::En(e) => write!(f, "{}", e)?,
            Tree::Un(u) => write!(f, "{}", u)?,
        }
        Ok(())
    }
}

fn write_doc(f: &mut Formatter, indent: &str, doc: &str) -> Result {
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
    Ok(())
}

impl<'a> Display for Directive<'a, TypeScript> {
    fn fmt(&self, _: &mut Formatter) -> Result {
        Ok(())
    }
}

impl Display for TypedTarget<TypeScript> {
    fn fmt(&self, _: &mut Formatter) -> Result {
        Ok(())
    }
}

impl<'a> Display for Scalar<'a, TypeScript> {
    fn fmt(&self, _: &mut Formatter) -> Result {
        Ok(())
    }
}

impl<'a> Display for Type<'a, TypeScript> {
    fn fmt(&self, f: &mut Formatter) -> Result {
        if let Some(doc) = self.doc {
            write_doc(f, "", doc)?;
        }
        write!(
            f,
            "{} ",
            match self.kind {
                TypeKind::Type => "export interface",
                TypeKind::Input => "export interface",
                TypeKind::Interface => "export interface",
            }
        )?;
        write!(f, "{}", self.name)?;
        if self.kind == TypeKind::Type {
            // " extends <interface1>, <interface2>
            if !self.interfaces.is_empty() {
                let istr = self.interfaces.join(", ");
                write!(f, " extends {}", istr)?;
            }
        }
        writeln!(f, " {{")?;
        for field in &self.fields {
            writeln!(f, "{}", field)?;
        }
        writeln!(f, "}}")?;
        Ok(())
    }
}

impl<'a> Display for Field<'a, TypeScript> {
    fn fmt(&self, f: &mut Formatter) -> Result {
        let ts_all_optional = std::env::var("TYPESCRIPT_ALL_OPTIONAL")
            .map(|s| s == "true")
            .unwrap_or(false);
        if let Some(doc) = self.doc {
            write_doc(f, "  ", doc)?;
        }
        write!(f, "  {}", self.name)?;
        let is_prop_nullable =
            !self.expr.arr.is_arr() && self.expr.null
            || self.expr.arr.is_arr() && self.expr.arr.is_null();
        if is_prop_nullable || ts_all_optional {
            write!(f, "?")?;
        }
        // ignore args
        write!(f, ": {};", self.expr)?;
        Ok(())
    }
}

impl<'a> Display for TypeExpr<'a, TypeScript> {
    fn fmt(&self, f: &mut Formatter) -> Result {
        if self.arr.is_arr() {
            write!(f, "Array<")?;
        }
        write!(f, "{}", translate_typ(self.typ))?;
        if self.arr.is_arr() {
            if self.null {
                write!(f, " | undefined")?;
            }
            write!(f, ">")?;
        }
        if let Some(default_value) = self.default_value {
            write!(f, " = {}", default_value)?;
        }
        Ok(())
    }
}

impl<'a> Display for FieldArg<'a, TypeScript> {
    fn fmt(&self, _: &mut Formatter) -> Result {
        Ok(())
    }
}

impl<'a> Display for Enum<'a, TypeScript> {
    fn fmt(&self, f: &mut Formatter) -> Result {
        if let Some(doc) = self.doc {
            write_doc(f, "", doc)?;
        }
        writeln!(f, "export enum {} {{", self.name)?;
        for ev in &self.values {
            writeln!(f, "{}", ev)?;
        }
        writeln!(f, "}}")?;
        Ok(())
    }
}

impl<'a> Display for EnumValue<'a, TypeScript> {
    fn fmt(&self, f: &mut Formatter) -> Result {
        if let Some(doc) = self.doc {
            write_doc(f, "  ", doc)?;
        }
        write!(f, "  {} = '{}',", self.value, self.value)?;
        Ok(())
    }
}

impl<'a> Display for Union<'a, TypeScript> {
    fn fmt(&self, f: &mut Formatter) -> Result {
        let types = self.names.join(" | ");
        writeln!(f, "export type {} = {};", self.name, types)
    }
}
