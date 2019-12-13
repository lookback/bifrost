use crate::parser::Ast;
use crate::parser::Directive;
use crate::parser::Enum;
use crate::parser::EnumValue;
use crate::parser::Field;
use crate::parser::FieldArg;
use crate::parser::Scalar;

use crate::parser::Target;
use crate::parser::Tree;
use crate::parser::Type;
use crate::parser::TypeExpr;
use crate::parser::TypeKind;
use crate::parser::TypedTarget;
use crate::parser::Union;
use std::fmt::Display;
use std::fmt::{Formatter, Result};

#[derive(Clone)]
pub struct Pass {}

impl<'a> Display for Ast<'a, Pass> {
    fn fmt(&self, f: &mut Formatter) -> Result {
        for (idx, t) in self.tree.iter().enumerate() {
            if idx > 0 {
                writeln!(f)?;
            }
            write!(f, "{}", t)?;
        }
        Ok(())
    }
}

impl<'a> Display for Tree<'a, Pass> {
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
    let need_triple = doc.chars().any(|c| c == '"' || c == '\n');
    if need_triple {
        writeln!(f, "{}\"\"\"{}\"\"\"", indent, doc)?;
    } else {
        writeln!(f, "{}\"{}\"", indent, doc)?;
    }
    Ok(())
}

impl<'a> Display for Directive<'a, Pass> {
    fn fmt(&self, f: &mut Formatter) -> Result {
        if let Some(doc) = self.doc {
            write_doc(f, "", doc)?;
        }
        write!(f, "directive @{} ", self.name)?;
        if !self.fields.is_empty() {
            writeln!(f, "(")?;
            for field in &self.fields {
                writeln!(f, "{}", field)?;
            }
            write!(f, ") ")?;
        }
        write!(f, "on ")?;
        for (idx, target) in self.targets.iter().enumerate() {
            if idx > 0 {
                write!(f, " | ")?;
            }
            write!(f, "{}", target)?;
        }
        writeln!(f)
    }
}

impl Display for TypedTarget<Pass> {
    fn fmt(&self, f: &mut Formatter) -> Result {
        write!(
            f,
            "{}",
            match self.0 {
                Target::Object => "OBJECT",
                Target::FieldDefinition => "FIELD_DEFINITION",
                Target::InputFieldDefinition => "INPUT_FIELD_DEFINITION",
                Target::Unknown => "",
            }
        )
    }
}

impl<'a> Display for Scalar<'a, Pass> {
    fn fmt(&self, f: &mut Formatter) -> Result {
        if let Some(doc) = self.doc {
            write_doc(f, "", doc)?;
        }
        writeln!(f, "scalar {}", self.name)
    }
}

impl<'a> Display for Type<'a, Pass> {
    fn fmt(&self, f: &mut Formatter) -> Result {
        if let Some(doc) = self.doc {
            write_doc(f, "", doc)?;
        }
        write!(
            f,
            "{} ",
            match self.kind {
                TypeKind::Type => "type",
                TypeKind::Input => "input",
                TypeKind::Interface => "interface",
            }
        )?;
        write!(f, "{}", self.name)?;
        if self.kind == TypeKind::Type {
            // " implements <interface1> & <interface2>
            if !self.interfaces.is_empty() {
                let istr = self.interfaces.join(" & ");
                write!(f, " implements {}", istr)?;
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

impl<'a> Display for Field<'a, Pass> {
    fn fmt(&self, f: &mut Formatter) -> Result {
        if let Some(doc) = self.doc {
            write_doc(f, "  ", doc)?;
        }
        write!(f, "  {}", self.name)?;
        if !self.args.is_empty() {
            write!(f, "(")?;
            for (idx, ar) in self.args.iter().enumerate() {
                if idx > 0 {
                    write!(f, ", ")?;
                }
                write!(f, "{}", ar)?;
            }
            write!(f, ")")?;
        }
        write!(f, ": {}", self.expr)?;
        Ok(())
    }
}

impl<'a> Display for TypeExpr<'a, Pass> {
    fn fmt(&self, f: &mut Formatter) -> Result {
        if self.arr.is_arr() {
            write!(f, "[")?;
        }
        write!(f, "{}", self.typ)?;
        if !self.null {
            write!(f, "!")?;
        }
        if self.arr.is_arr() {
            write!(f, "]")?;
            if !self.arr.is_null() {
                write!(f, "!")?;
            }
        }
        if let Some(default_value) = self.default_value {
            write!(f, " = {}", default_value)?;
        }
        Ok(())
    }
}

impl<'a> Display for FieldArg<'a, Pass> {
    fn fmt(&self, f: &mut Formatter) -> Result {
        if let Some(doc) = self.doc {
            write_doc(f, "\n    ", doc)?;
            write!(f, "    ")?;
        }
        write!(f, "{}: {}", self.name, self.expr)?;
        if let Some(def) = self.def {
            write!(f, " = {}", def)?;
        }
        Ok(())
    }
}

impl<'a> Display for Enum<'a, Pass> {
    fn fmt(&self, f: &mut Formatter) -> Result {
        if let Some(doc) = self.doc {
            write_doc(f, "", doc)?;
        }
        writeln!(f, "enum {} {{", self.name)?;
        for ev in &self.values {
            writeln!(f, "{}", ev)?;
        }
        writeln!(f, "}}")?;
        Ok(())
    }
}

impl<'a> Display for EnumValue<'a, Pass> {
    fn fmt(&self, f: &mut Formatter) -> Result {
        if let Some(doc) = self.doc {
            write_doc(f, "  ", doc)?;
        }
        write!(f, "  {},", self.value)?;
        Ok(())
    }
}

impl<'a> Display for Union<'a, Pass> {
    fn fmt(&self, f: &mut Formatter) -> Result {
        let types = self.names.join(" | ");
        writeln!(f, "union {} = {}", self.name, types)
    }
}
