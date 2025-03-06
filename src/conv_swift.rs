use crate::parser::Enum;
use crate::parser::Field;
use crate::parser::Type;
use crate::parser::TypeExpr;
use crate::parser::TypeKind;
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
        "BigInt" => "Int64",
        _ => typ,
    }
}

impl<'a> Display for Ast<'a, Swift> {
    fn fmt(&self, f: &mut Formatter) -> Result {
        writeln!(f, "import Foundation")?;
        if self.has_type(|t| t.typ == "ID") {
            writeln!(f, "typealias ID = String;\n")?;
        }
        for (idx, t) in self.tree.iter().enumerate() {
            if idx > 0 {
                writeln!(f)?;
            }
            write!(f, "{}", t)?;
        }
        let has_union = self.tree.iter().any(|t| t.as_union().is_some());
        if has_union {
            writeln!(f, "\nprivate enum UnionAssociated: CodingKey {{")?;
            writeln!(f, "    case tag")?;
            writeln!(f, "    case val")?;
            writeln!(f, "}}")?;
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
        let swift_all_var = std::env::var("SWIFT_ALL_VAR")
            .map(|s| s == "true")
            .unwrap_or(false);
        let swift_all_class = std::env::var("SWIFT_ALL_CLASS")
            .map(|s| s == "true")
            .unwrap_or(false);
        match self.kind {
            TypeKind::Type | TypeKind::Input => {
                if swift_all_class {
                    write!(f, "class {}: Codable", self.name)?;
                } else {
                    write!(f, "struct {}: Codable, Equatable, Hashable", self.name)?;
                }
                for interface in &self.interfaces {
                    write!(f, ", {}", interface)?;
                }
                writeln!(f, " {{")?;
                if swift_all_class {
                    write_init(f, self)?;
                }
                let use_var = !self.interfaces.is_empty() || swift_all_var;
                for field in &self.fields {
                    write_field(f, field, use_var)?;
                }
            }
            TypeKind::Interface => {
                writeln!(f, "protocol {} {{", self.name)?;
                for field in &self.fields {
                    write_doc(f, "    ", field.doc)?;
                    writeln!(f, "    var {}: {} {{ get set }}", field.name, field.expr)?;
                }
            }
        }
        writeln!(f, "}}")?;
        Ok(())
    }
}

fn write_init<'a>(f: &mut Formatter, t: &Type<'a, Swift>) -> Result {
    writeln!(f, "    init(")?;
    let count = t.fields.len();
    for (idx, field) in t.fields.iter().enumerate() {
        write!(f, "        {}: {}", field.name, field.expr)?;
        if idx < count - 1 {
            writeln!(f, ",")?;
        } else {
            writeln!(f)?;
        }
    }
    writeln!(f, "    ) {{")?;
    for field in &t.fields {
        writeln!(f, "        self.{} = {}", field.name, field.name)?;
    }
    writeln!(f, "    }}")?;
    Ok(())
}

fn write_field<'a>(f: &mut Formatter, field: &Field<'a, Swift>, use_var: bool) -> Result {
    write_doc(f, "    ", field.doc)?;
    if use_var {
        writeln!(f, "    var {}: {}", field.name, field.expr)?;
    } else {
        writeln!(f, "    let {}: {}", field.name, field.expr)?;
    }
    Ok(())
}

impl<'a> Display for TypeExpr<'a, Swift> {
    fn fmt(&self, f: &mut Formatter) -> Result {
        let swift_all_optional = std::env::var("SWIFT_ALL_OPTIONAL")
            .map(|s| s == "true")
            .unwrap_or(false);
        if self.arr.is_arr() {
            write!(f, "[")?;
        }
        write!(f, "{}", translate_typ(self.typ))?;
        if self.null || swift_all_optional {
            write!(f, "?")?;
        }
        if self.arr.is_arr() {
            write!(f, "]")?;
            if self.arr.is_null() || swift_all_optional {
                write!(f, "?")?;
            }
        }
        Ok(())
    }
}

impl<'a> Display for Enum<'a, Swift> {
    fn fmt(&self, f: &mut Formatter) -> Result {
        write_doc(f, "", self.doc)?;
        writeln!(
            f,
            "enum {}: String, Codable, CaseIterable, Equatable, Hashable {{",
            self.name
        )?;
        for v in &self.values {
            write_doc(f, "    ", v.doc)?;
            writeln!(f, "    case {}", v.value)?;
        }
        writeln!(f, "}}")?;
        Ok(())
    }
}

impl<'a> Display for Union<'a, Swift> {
    fn fmt(&self, f: &mut Formatter) -> Result {
        let swift_all_class = std::env::var("SWIFT_ALL_CLASS")
            .map(|s| s == "true")
            .unwrap_or(false);
        if swift_all_class {
            writeln!(f, "enum {} {{", self.name)?;
        } else {
            writeln!(f, "enum {}: Equatable, Hashable {{", self.name)?;
        }
        let mut lcaseds = vec![];
        for name in &self.names {
            let mut lcased = name.to_string();
            if let Some(r) = lcased.get_mut(0..1) {
                r.make_ascii_lowercase();
            }
            writeln!(f, "    case {}({})", lcased, name)?;
            lcaseds.push((name, lcased));
        }
        writeln!(f, "}}")?;
        // bullshit to make an enum Codable
        writeln!(f, "\nextension {}: Codable {{", self.name)?;
        writeln!(f, "    init(from decoder: Decoder) throws {{")?;
        writeln!(
            f,
            "        let c = try decoder.container(keyedBy: UnionAssociated.self)"
        )?;
        writeln!(
            f,
            "        let key = try c.decode(String.self, forKey: .tag)"
        )?;
        writeln!(f, "        switch key {{")?;
        for lcased in &lcaseds {
            writeln!(f, "        case \"{}\":", lcased.1)?;
            writeln!(
                f,
                "            self = .{}(try c.decode({}.self, forKey: .val))",
                lcased.1, lcased.0
            )?;
        }
        writeln!(f, "        default: throw DecodingError.dataCorruptedError(forKey: .tag, in: c, debugDescription: \"Failed to decode kind in {}\")", self.name)?;
        writeln!(f, "        }}")?;
        writeln!(f, "    }}")?;
        writeln!(f, "    func encode(to encoder: Encoder) throws {{")?;
        writeln!(
            f,
            "        var c = encoder.container(keyedBy: UnionAssociated.self)"
        )?;
        writeln!(f, "        switch self {{")?;
        for lcased in &lcaseds {
            writeln!(f, "            case .{}(let v):", lcased.1)?;
            writeln!(
                f,
                "                try c.encode(\"{}\", forKey: .tag)",
                lcased.1
            )?;
            writeln!(f, "                try c.encode(v, forKey: .val)")?;
        }
        writeln!(f, "        }}")?;
        writeln!(f, "    }}")?;
        writeln!(f, "}}")?;
        // extension Event: Codable {
        //     init(from decoder: Decoder) throws {
        //     }
        //     func encode(to encoder: Encoder) throws {
        //         var container = encoder.container(keyedBy: UnionAssociated.self)
        //         switch self {
        //             case .createGroup(let v):
        //                 try container.encode(0, forKey: .key)
        //                 try container.encode(v, forKey: .associated)
        //         }
        //     }
        // }
        Ok(())
    }
}
