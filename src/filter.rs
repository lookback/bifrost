use crate::parser::{Ast, Tree};

pub fn filter_ast<'a, T>(ast: &'a Ast<'a, T>, types: &[&str]) -> Ast<'a, T>
where
    T: Clone,
{
    let mut found: Vec<_> = types.iter().filter_map(|n| ast.get_tree(n)).collect();
    for t in types {
        resolve(&ast, &mut found, *t);
    }

    let tree: Vec<_> = found.into_iter().cloned().collect();

    // resulting filtered ast
    Ast::new(tree)
}

fn resolve<'a, T>(ast: &'a Ast<'a, T>, found: &mut Vec<&'a Tree<'a, T>>, cur: &str) {
    let mut add_if_not_found = |n: &str| {
        if let Some(tree) = ast.get_tree(n) {
            let is_new = found.iter().find(|t2| tree.name() == t2.name()).is_none();
            if is_new {
                found.push(tree);
            }
            resolve(ast, found, n);
        }
    };
    match ast.get_tree(cur) {
        None => (),
        Some(tree) => match tree {
            Tree::Ty(t) => {
                for if_name in &t.interfaces {
                    add_if_not_found(if_name);
                }
                for field in &t.fields {
                    add_if_not_found(field.expr.typ);
                }
            }
            Tree::Dr(_) => (),
            Tree::Sc(_) => (),
            Tree::En(_) => (),
            Tree::Un(t) => {
                for typ_name in &t.names {
                    add_if_not_found(typ_name);
                }
            },
        },
    }
}
