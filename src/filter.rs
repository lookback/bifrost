use parser::Ast;

pub fn filter_ast<'a, T>(ast: &Ast<'a, T>, types: &Vec<&str>) -> Ast<'a, T>
where
    T: Clone,
{
    // filter the ast to only keep those types types vec.
    let mut filt: Vec<_> = ast
        .tree
        .iter()
        .filter_map(|t| {
            types
                .iter()
                .enumerate()
                .find(|(_, n)| **n == t.name())
                .map(|(idx, _)| (idx, t))
        })
        .collect();

    filt.sort_unstable_by(|(pa, _), (pb, _)| pa.cmp(pb));

    let tree: Vec<_> = filt.into_iter().map(|(_, t)| t).cloned().collect();

    // resulting filtered ast
    Ast::new(tree)
}
