use std::fmt;

/// Indent a string(with new lines) to a specific indentation level.
///
/// NB: Indentation level here isn't number of space i.e. level = 1 means for space indentation.
pub fn indent(s: &str, level: usize) -> impl fmt::Display + '_ {
    Indent::new(s, level)
}

pub(crate) struct Indent<'s> {
    s: &'s str,
    level: usize,
}

impl<'s> Indent<'s> {
    fn new(s: &'s str, level: usize) -> Self {
        Self { s, level }
    }
}

impl<'s> fmt::Display for Indent<'s> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        for l in self.s.lines() {
            // This is not perfect and will break for non-ASCII
            let width = if l.is_empty() {
                0
            } else {
                l.len() + self.level * 4
            };
            writeln!(f, "{:>width$}", l, width = width)?;
        }

        Ok(())
    }
}
