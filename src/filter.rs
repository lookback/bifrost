use crate::parser::{Ast, Tree};

pub fn filter_ast<'a, T>(ast: &'a Ast<'a, T>, types: &Vec<&str>) -> Ast<'a, T>
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
    match ast.get_tree(cur) {
        None => (),
        Some(tree) => match tree {
            Tree::Ty(t) => {
                let new: Vec<_> = t
                    .fields
                    .iter()
                    .filter_map(|f| ast.get_tree(f.expr.typ))
                    .filter(|t1| found.iter().find(|t2| t1.name() == t2.name()).is_none())
                    .collect();
                for n in new {
                    if found.iter().find(|t| t.name() == n.name()).is_none() {
                        found.push(n);
                    }
                    resolve(ast, found, n.name());
                }
            }
            Tree::Dr(_) => (),
            Tree::Sc(_) => (),
            Tree::En(_) => (),
            Tree::Un(_) => (),
        },
    }
}
