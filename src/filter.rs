use parser::Ast;

pub fn filter_ast<'a, T>(ast: &Ast<'a, T>, types: &Vec<&str>) -> Ast<'a, T>
where
    T: Clone,
{
    //

    // filter the ast to only keep those types types vec.
    let filt: Vec<_> = ast
        .tree
        .iter()
        .filter(|t| types.is_empty() || types.contains(&t.name()))
        // .map(|tree| PrioTree { tree, prio: 0 })
        .cloned()
        .collect();

    // let mut resolved: HashMap<&str, PrioTree> = HashMap::new();
    // for pt in &filt {
    //     let name = pt.tree.name();
    //     resolved.insert(name, pt.clone());
    //     let mut done = vec![];
    //     resolve(&ast, &mut resolved, &mut done, pt);
    // }

    // let mut ordered: Vec<_> = resolved.into_iter().map(|(_, pt)| pt).collect();

    // ordered.sort_unstable_by(|a, b| b.prio.cmp(&a.prio));

    // let ret: Vec<Tree> = ordered.into_iter().map(|pt| pt.tree).cloned().collect();

    // resulting filtered ast
    Ast::new(filt)
}
