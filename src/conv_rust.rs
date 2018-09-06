use parser::{Ast, Tree};
use std::fmt::Display;
use std::fmt::{Formatter, Result};
use Output;

struct Rust<'a> {
    kinds: Vec<Kind<'a>>,
}

enum Kind<'a> {
    St(RustStruct<'a>),
    En(RustEnum<'a>),
    Un(RustUnion<'a>),
}

struct RustStruct<'a> {
    name: &'a str,
    fields: Vec<RustField<'a>>,
}

#[derive(Debug)]
struct RustField<'a> {
    name: &'a str,  // foo
    typ: &'a str,   // Foo
    null: bool,     // Foo!
    arr: bool,      // [Foo]
    arr_null: bool, // [Foo!]!
}

struct RustEnum<'a> {
    name: &'a str,
    values: Vec<&'a str>,
}

struct RustUnion<'a> {
    _name: &'a str,
}

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

pub fn convert_rust(ast: Ast) -> Vec<Output> {
    let mut has_id = false;
    let mut has_date = false;
    let rust = Rust {
        kinds: ast
            .tree
            .iter()
            .map(|tr| match tr {
                Tree::Ty(t) => Kind::St(RustStruct {
                    name: tr.name(ast.source),
                    fields: t
                        .fields
                        .iter()
                        .map(|f| {
                            let raw_typ = f.expr.typ.apply(ast.source);
                            if raw_typ == "ID" {
                                has_id = true;
                            }
                            if raw_typ == "Date" {
                                has_date = true;
                            }
                            let typ = translate_typ(raw_typ);
                            RustField {
                                name: f.name.apply(ast.source),
                                typ,
                                null: f.expr.null,
                                arr: f.expr.arr.is_arr(),
                                arr_null: f.expr.arr.is_null(),
                            }
                        })
                        .collect(),
                }),
                Tree::En(e) => Kind::En(RustEnum {
                    name: tr.name(ast.source),
                    values: e
                        .values
                        .iter()
                        .map(|ev| ev.value.apply(ast.source))
                        .collect(),
                }),
                Tree::Un(_) => Kind::Un(RustUnion {
                    _name: tr.name(ast.source),
                }),
            })
            .collect(),
    };
    let contents = format!(
        "{}\n{}\n{}",
        if has_date {
            "use chrono::{Date, Utc};\n"
        } else {
            ""
        },
        if has_id {
            "pub type ID = String;\n"
        } else {
            ""
        },
        rust
    );
    vec![Output {
        file_name: "schema.rs".to_string(),
        contents,
    }]
}

impl<'a> Display for Rust<'a> {
    fn fmt(&self, f: &mut Formatter) -> Result {
        for k in &self.kinds {
            writeln!(f, "{}", k)?;
        }
        Ok(())
    }
}

impl<'a> Display for Kind<'a> {
    fn fmt(&self, f: &mut Formatter) -> Result {
        match self {
            Kind::St(t) => writeln!(f, "{}", t)?,
            Kind::En(e) => writeln!(f, "{}", e)?,
            Kind::Un(u) => writeln!(f, "{}", u)?,
        }
        Ok(())
    }
}

impl<'a> Display for RustStruct<'a> {
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

impl<'a> Display for RustField<'a> {
    fn fmt(&self, f: &mut Formatter) -> Result {
        if self.arr && self.arr_null || !self.arr && self.null {
            write!(f, "  #[serde(skip_serializing_if = \"Option::is_none\")]\n")?;
        }
        write!(f, "  pub {}: ", self.name)?;
        if self.arr {
            if self.arr_null {
                write!(f, "Option<")?;
            }
            write!(f, "Vec<")?;
        }
        if self.null {
            write!(f, "Option<")?;
        }
        write!(f, "{}", self.typ)?;
        if self.null {
            write!(f, ">")?;
        }
        if self.arr {
            if self.arr_null {
                write!(f, ">")?;
            }
            write!(f, ">")?;
        }
        Ok(())
    }
}

impl<'a> Display for RustEnum<'a> {
    fn fmt(&self, f: &mut Formatter) -> Result {
        writeln!(f, "#[derive(Debug, Clone, Copy, Serialize, Deserialize)]")?;
        writeln!(f, "pub enum {} {{", self.name)?;
        for v in &self.values {
            writeln!(f, "  {},", v)?;
        }
        writeln!(f, "}}")?;
        Ok(())
    }
}

impl<'a> Display for RustUnion<'a> {
    fn fmt(&self, _: &mut Formatter) -> Result {
        panic!("Union?!");
    }
}
