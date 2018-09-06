use parser::Enum;
use parser::Field;
use parser::Type;
use parser::Union;
use parser::{Ast, Tree};
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
        for t in &self.tree {
            writeln!(f, "{}", t)?;
        }
        Ok(())
    }
}

impl<'a> Display for Tree<'a, Rust> {
    fn fmt(&self, f: &mut Formatter) -> Result {
        match self {
            Tree::Ty(t) => writeln!(f, "{}", t)?,
            Tree::En(e) => writeln!(f, "{}", e)?,
            Tree::Un(u) => writeln!(f, "{}", u)?,
        }
        Ok(())
    }
}

impl<'a> Display for Type<'a, Rust> {
    fn fmt(&self, f: &mut Formatter) -> Result {
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
        let expr = &self.expr;
        if expr.arr.is_arr() && expr.arr.is_null() || !expr.arr.is_arr() && expr.null {
            write!(f, "  #[serde(skip_serializing_if = \"Option::is_none\")]\n")?;
        }
        write!(f, "  pub {}: ", self.name)?;
        if expr.arr.is_arr() {
            if expr.arr.is_null() {
                write!(f, "Option<")?;
            }
            write!(f, "Vec<")?;
        }
        if expr.null {
            write!(f, "Option<")?;
        }
        write!(f, "{}", translate_typ(expr.typ))?;
        if expr.null {
            write!(f, ">")?;
        }
        if expr.arr.is_arr() {
            if expr.arr.is_null() {
                write!(f, ">")?;
            }
            write!(f, ">")?;
        }
        Ok(())
    }
}

impl<'a> Display for Enum<'a, Rust> {
    fn fmt(&self, f: &mut Formatter) -> Result {
        writeln!(f, "#[derive(Debug, Clone, Copy, Serialize, Deserialize)]")?;
        writeln!(f, "pub enum {} {{", self.name)?;
        for v in &self.values {
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
