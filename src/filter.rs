use parser::{Ast, Tree};
use std::collections::HashMap;

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
struct PrioTree<'a> {
    tree: &'a Tree,
    prio: usize,
}

pub fn filter_ast<'a>(ast: Ast<'a>, types: Vec<&str>) -> Ast<'a> {
    //

    // filter the ast to only keep those types types vec.
    let filt: Vec<_> = ast
        .tree
        .iter()
        .filter(|t| types.is_empty() || types.contains(&t.name(ast.source)))
        .map(|tree| PrioTree { tree, prio: 0 })
        .collect();

    let mut resolved: HashMap<&str, PrioTree> = HashMap::new();
    for pt in &filt {
        let name = pt.tree.name(ast.source);
        resolved.insert(name, pt.clone());
        let mut done = vec![];
        resolve(&ast, &mut resolved, &mut done, pt);
    }

    let mut ordered: Vec<_> = resolved.into_iter().map(|(_, pt)| pt).collect();

    ordered.sort_unstable_by(|a, b| b.prio.cmp(&a.prio));

    let ret: Vec<Tree> = ordered.into_iter().map(|pt| pt.tree).cloned().collect();

    // resulting filtered ast
    Ast {
        source: &ast.source,
        tree: ret,
    }
}

fn resolve<'a>(
    all: &'a Ast<'a>,
    resolved: &mut HashMap<&'a str, PrioTree<'a>>,
    done: &mut Vec<&'a str>,
    cur: &PrioTree<'a>,
) {
    let name = cur.tree.name(all.source);
    if done.contains(&name) {
        return;
    }
    done.push(name);
    let higher_prio = {
        let exist = resolved.entry(name).or_insert_with(|| cur.clone());
        cur.prio > exist.prio
    };
    if higher_prio {
        resolved.insert(name, cur.clone());
    }
    match cur.tree {
        Tree::Ty(t) => for f in t.fields.iter().filter(|f| !all.is_scalar(&f.expr.typ)) {
            let fname = f.expr.typ.apply(all.source);
            let ftree = all
                .find(fname)
                .expect(&format!("Missing field type: {}", fname));
            let next = PrioTree {
                tree: ftree,
                prio: cur.prio + 1,
            };
            let mut fdone = done.clone();
            resolve(all, resolved, &mut fdone, &next);
        },
        Tree::Un(u) => {
            for uchunk in &u.names {
                let uname = uchunk.apply(all.source);
                let ftree = all
                    .find(uname)
                    .expect(&format!("Missing union type: {}", uname));
                let next = PrioTree {
                    tree: ftree,
                    prio: cur.prio + 1,
                };
                let mut udone = done.clone();
                resolve(all, resolved, &mut udone, &next);
            }
        }
        Tree::En(_) => {}
    }
}
