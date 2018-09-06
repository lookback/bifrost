use parser::Enum;
use parser::EnumValue;
use parser::Field;
use parser::FieldArg;
use parser::Type;
use parser::TypeExpr;
use parser::Union;
use parser::{Ast, Tree};
use std::fmt::Display;
use std::fmt::{Formatter, Result};

#[derive(Clone)]
pub struct Pass {}

impl<'a> Display for Ast<'a, Pass> {
    fn fmt(&self, f: &mut Formatter) -> Result {
        for (idx, t) in self.tree.iter().enumerate() {
            if idx > 0 {
                write!(f, "\n")?;
            }
            write!(f, "{}", t)?;
        }
        Ok(())
    }
}

impl<'a> Display for Tree<'a, Pass> {
    fn fmt(&self, f: &mut Formatter) -> Result {
        match self {
            Tree::Ty(t) => write!(f, "{}", t)?,
            Tree::En(e) => write!(f, "{}", e)?,
            Tree::Un(u) => write!(f, "{}", u)?,
        }
        Ok(())
    }
}

fn write_doc(f: &mut Formatter, indent: &str, doc: &str) -> Result {
    let need_triple = doc.chars().find(|c| *c == '"' || *c == '\n').is_some();
    if need_triple {
        writeln!(f, "{}\"\"\"{}\"\"\"", indent, doc)?;
    } else {
        writeln!(f, "{}\"{}\"", indent, doc)?;
    }
    Ok(())
}

impl<'a> Display for Type<'a, Pass> {
    fn fmt(&self, f: &mut Formatter) -> Result {
        if let Some(doc) = self.doc {
            write_doc(f, "", doc)?;
        }
        writeln!(f, "type {} {{", self.name)?;
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
        Ok(())
    }
}

impl<'a> Display for FieldArg<'a, Pass> {
    fn fmt(&self, f: &mut Formatter) -> Result {
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
    fn fmt(&self, _: &mut Formatter) -> Result {
        panic!("Union?!");
    }
}
