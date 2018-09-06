use std::fmt::Display;
use std::fmt::Formatter;

impl<'a> Display for Ast<'a> {
    fn fmt(&self, w: &mut Formatter) -> Result<(), ::std::fmt::Error> {
        for tr in &self.tree {
            match &tr {
                Tree::Ty(t) => {
                    if let Some(doc) = &t.doc {
                        write!(w, "{}\n", doc.apply(self.source))?;
                    }
                    write!(w, "type {} {{\n", t.name.apply(self.source))?;
                    for f in &t.fields {
                        if let Some(doc) = &f.doc {
                            write!(w, "  {}\n", doc.apply(self.source))?;
                        }
                        write!(w, "  {}", f.name.apply(self.source))?;
                        if !f.args.is_empty() {
                            write!(w, "(")?;
                            for (idx, a) in f.args.iter().enumerate() {
                                if idx > 0 {
                                    write!(w, ", ")?;
                                }
                                write!(w, "{}: ", a.name.apply(self.source),)?;
                                if a.expr.arr.is_arr() {
                                    write!(w, "[")?;
                                }
                                write!(w, "{}", a.expr.typ.apply(self.source))?;
                                if !a.expr.null {
                                    write!(w, "!")?;
                                }
                                if a.expr.arr.is_arr() {
                                    write!(w, "]")?;
                                    if !a.expr.arr.is_null() {
                                        write!(w, "!")?;
                                    }
                                }
                                if let Some(def) = &a.def {
                                    write!(w, " = {}", def.apply(self.source))?;
                                }
                            }
                            write!(w, ")")?;
                        }
                        write!(w, ": ")?;
                        if f.expr.arr.is_arr() {
                            write!(w, "[")?;
                        }
                        write!(w, "{}", f.expr.typ.apply(self.source))?;
                        if !f.expr.null {
                            write!(w, "!")?;
                        }
                        if f.expr.arr.is_arr() {
                            write!(w, "]")?;
                            if !f.expr.arr.is_null() {
                                write!(w, "!")?;
                            }
                        }
                        write!(w, "\n")?;
                    }
                    write!(w, "}}\n")?;
                }
                Tree::En(e) => {
                    if let Some(doc) = &e.doc {
                        write!(w, "{}\n", doc.apply(self.source))?;
                    }
                    write!(w, "enum {} {{\n", e.name.apply(self.source))?;
                    for v in &e.values {
                        write!(w, "  {}\n", v.value.apply(self.source))?;
                    }
                    write!(w, "}}\n")?;
                }
                Tree::Un(u) => {
                    if let Some(doc) = &u.doc {
                        write!(w, "{}\n", doc.apply(self.source))?;
                    }
                    write!(w, "union {} ", u.name.apply(self.source))?;
                    for (idx, n) in u.names.iter().enumerate() {
                        if idx == 0 {
                            write!(w, "= {}", n.apply(self.source))?;
                        } else {
                            write!(w, "| {}", n.apply(self.source))?;
                        }
                    }
                }
            }
        }
        Ok(())
    }
}

