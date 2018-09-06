use token::{tokenize, Chunk, Token, TokenIter, SYMBOL};

include!("display.rs");

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct Ast<'a> {
    pub source: &'a str,
    pub tree: Vec<Tree>,
}

impl<'a> Ast<'a> {
    pub fn find(&self, name: &str) -> Option<&Tree> {
        self.tree.iter().find(|t| t.name(self.source) == name)
    }

    pub fn is_scalar(&self, chunk: &Chunk) -> bool {
        let name = chunk.apply(self.source);
        match name {
            "Int" | "Float" | "String" | "Boolean" | "ID" | "Date" => true,
            _ => false,
        }
    }

    #[cfg(test)]
    pub fn to_string(&self) -> String {
        format!("{}", self)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub enum Tree {
    Ty(Type),
    En(Enum),
    Un(Union),
}

impl Tree {
    pub fn name<'a>(&self, source: &'a str) -> &'a str {
        match self {
            Tree::Ty(t) => t.name.apply(source),
            Tree::En(e) => e.name.apply(source),
            Tree::Un(u) => u.name.apply(source),
        }
    }
}

// type Starship {
//   id: ID!
//   name: String!
//   length(unit: LengthUnit = METER): Float
// }
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct Type {
    pub doc: Option<Chunk>,
    pub name: Chunk,
    pub fields: Vec<Field>,
}
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct Field {
    pub doc: Option<Chunk>,
    pub name: Chunk,
    pub expr: TypeExpr,
    pub args: Vec<FieldArg>,
}
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct FieldArg {
    pub name: Chunk,
    pub expr: TypeExpr,
    pub def: Option<Chunk>,
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct TypeExpr {
    pub typ: Chunk,
    pub null: bool,
    pub arr: Arr,
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub enum Arr {
    No,
    Yes { null: bool },
}

impl Arr {
    pub fn is_arr(&self) -> bool {
        match self {
            Arr::Yes { .. } => true,
            _ => false,
        }
    }
    pub fn is_null(&self) -> bool {
        match self {
            Arr::Yes { null } => *null,
            _ => false,
        }
    }
}

// enum Episode {
//   NEWHOPE
//   EMPIRE
//   JEDI
// }
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct Enum {
    pub doc: Option<Chunk>,
    pub name: Chunk,
    pub values: Vec<EnumValue>,
}
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct EnumValue {
    pub doc: Option<Chunk>,
    pub value: Chunk,
}

// union SearchResult = Human | Droid | Starship
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct Union {
    pub doc: Option<Chunk>,
    pub name: Chunk,
    pub names: Vec<Chunk>,
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct SyntaxError {
    pub cause: String,
    pub context: String,
}

pub type ParseResult<T> = Result<T, SyntaxError>;

fn syntax_error(cause: &str, context: &Chunk, source: &str) -> SyntaxError {
    let cc = Chunk {
        index: context.index,
        len: source.len() - context.index,
        token: Token::Unknown,
    };
    let x = cc.apply(source);
    SyntaxError {
        cause: cause.to_string(),
        context: (&x[0..10.min(x.len())]).to_string(),
    }
}

fn err_syntax_error<T>(cause: &str, context: &Chunk, source: &str) -> ParseResult<T> {
    Err(syntax_error(cause, context, source))
}

fn unexpected_end(cause: &str) -> SyntaxError {
    SyntaxError {
        cause: cause.to_string(),
        context: "".to_string(),
    }
}

fn expect_name(source: &str, tok: &mut TokenIter) -> ParseResult<Chunk> {
    tok.consume()
        .ok_or_else(|| unexpected_end("Expected name, but found end of input"))
        .and_then(|chunk| {
            if chunk.token == Token::Name {
                Ok(chunk)
            } else {
                err_syntax_error("Expected name", &chunk, source)
            }
        })
}

fn expect_symbol(source: &str, tok: &mut TokenIter, symbol: SYMBOL) -> ParseResult<Chunk> {
    tok.consume()
        .ok_or_else(|| unexpected_end("Expected symbol, but found end of input"))
        .and_then(|chunk| {
            let is_expected_symbol = if let Token::Symbol(s) = &chunk.token {
                Ok(*s == symbol)
            } else {
                err_syntax_error("Expected symbol", &chunk, source)
            }?;
            if is_expected_symbol {
                Ok(chunk)
            } else {
                err_syntax_error("Wrong symbol", &chunk, source)
            }
        })
}

pub fn parse(source: &str) -> ParseResult<Ast> {
    let mut tok = tokenize(source);
    let mut tree = vec![];
    loop {
        let doc = parse_doc(source, &mut tok)?;
        tok.skip_white();
        match tok.consume() {
            None => break,
            Some(chunk) => {
                let tr = match chunk.token {
                    Token::Name => match chunk.apply(source) {
                        "type" => parse_type(source, &mut tok, doc),
                        "enum" => parse_enum(source, &mut tok, doc),
                        "union" => parse_union(source, &mut tok, doc),
                        _ => err_syntax_error("Unknown keyword", &chunk, source),
                    },
                    _ => err_syntax_error("Unexpected input", &chunk, source),
                }?;
                tree.push(tr);
            }
        }
    }
    Ok(Ast { source, tree })
}

fn parse_doc(source: &str, tok: &mut TokenIter) -> ParseResult<Option<Chunk>> {
    tok.skip_white();
    if tok.peek_is_symbol(SYMBOL::DQuote) {
        let start = tok.consume().unwrap();
        tok.find(|t| t.is_symbol(SYMBOL::DQuote))
            .map(|c| Some(start.extend(&c)))
            .ok_or_else(|| syntax_error("Unbalanced doc quotes", &start, source))
    } else if tok.peek_is_symbol(SYMBOL::TDQuote) {
        let start = tok.consume().unwrap();
        tok.find(|t| t.is_symbol(SYMBOL::TDQuote))
            .map(|c| Some(start.extend(&c)))
            .ok_or_else(|| syntax_error("Unbalanced doc triple-quotes", &start, source))
    } else {
        Ok(None)
    }
}

fn parse_type(source: &str, tok: &mut TokenIter, doc: Option<Chunk>) -> ParseResult<Tree> {
    // keyword is "type" and tok is positioned after that
    tok.skip_white();
    let name = expect_name(source, tok)?;
    tok.skip_white();
    expect_symbol(source, tok, SYMBOL::OpCurl)?;
    let mut fields: Vec<Field> = vec![];
    loop {
        tok.skip_white();
        if tok.peek_is_symbol(SYMBOL::ClCurl) {
            tok.consume();
            break;
        }
        let doc = parse_doc(source, tok)?;
        tok.skip_white();
        fields.push(parse_field(source, tok, doc)?);
    }
    Ok(Tree::Ty(Type { doc, name, fields }))
}

fn parse_field(source: &str, tok: &mut TokenIter, doc: Option<Chunk>) -> ParseResult<Field> {
    let name = expect_name(source, tok)?;
    tok.skip_white();
    let mut args = vec![];
    if tok.peek_is_symbol(SYMBOL::OpParen) {
        tok.consume();
        loop {
            tok.skip_white();
            if tok.peek_is_symbol(SYMBOL::ClParen) {
                tok.consume();
                break;
            }
            tok.skip_white();
            if tok.peek_is_symbol(SYMBOL::Comma) {
                tok.consume();
                tok.skip_white();
            }
            args.push(parse_field_arg(source, tok)?);
        }
    }
    tok.skip_white();
    expect_symbol(source, tok, SYMBOL::Colon)?;
    let expr = parse_type_expr(source, tok)?;
    Ok(Field {
        doc,
        name,
        expr,
        args,
    })
}

fn parse_type_expr(source: &str, tok: &mut TokenIter) -> ParseResult<TypeExpr> {
    tok.skip_white();
    let is_arr = tok.peek_is_symbol(SYMBOL::OpSquar);
    if is_arr {
        tok.consume();
        tok.skip_white();
    }
    let typ = expect_name(source, tok)?;
    let null = !tok.peek_is_symbol(SYMBOL::Exclam);
    if !null {
        tok.consume();
    }
    let arr_null = if is_arr {
        tok.skip_white();
        expect_symbol(source, tok, SYMBOL::ClSquar)?;
        let arr_null = !tok.peek_is_symbol(SYMBOL::Exclam);
        if !arr_null {
            tok.consume();
        }
        arr_null
    } else {
        true
    };
    let arr = if is_arr {
        Arr::Yes { null: arr_null }
    } else {
        Arr::No
    };
    Ok(TypeExpr { typ, null, arr })
}

fn parse_field_arg(source: &str, tok: &mut TokenIter) -> ParseResult<FieldArg> {
    let name = expect_name(source, tok)?;
    tok.skip_white();
    expect_symbol(source, tok, SYMBOL::Colon)?;
    let expr = parse_type_expr(source, tok)?;
    tok.skip_white();
    let def = if tok.peek_is_symbol(SYMBOL::Equals) {
        tok.consume();
        tok.skip_white();
        Some(expect_name(source, tok)?)
    } else {
        None
    };
    Ok(FieldArg { name, expr, def })
}

fn parse_enum(source: &str, tok: &mut TokenIter, doc: Option<Chunk>) -> ParseResult<Tree> {
    // keyword is "enum" and tok is positioned after that
    tok.skip_white();
    let name = expect_name(source, tok)?;
    tok.skip_white();
    expect_symbol(source, tok, SYMBOL::OpCurl)?;
    let mut values = vec![];
    loop {
        tok.skip_white();
        if tok.peek_is_symbol(SYMBOL::ClCurl) {
            tok.consume();
            break;
        }
        let doc = parse_doc(source, tok)?;
        tok.skip_white();
        let value = expect_name(source, tok)?;
        values.push(EnumValue { doc, value });
    }
    Ok(Tree::En(Enum { doc, name, values }))
}

fn parse_union(source: &str, tok: &mut TokenIter, doc: Option<Chunk>) -> ParseResult<Tree> {
    // keyword is "union" and tok is positioned after that
    tok.skip_white();
    let name = expect_name(source, tok)?;
    tok.skip_white();
    expect_symbol(source, tok, SYMBOL::Equals)?;
    let mut names = vec![];
    loop {
        tok.skip_white();
        names.push(expect_name(source, tok)?);
        tok.skip_white();
        if !tok.peek_is_symbol(SYMBOL::Pipe) {
            break;
        }
    }
    Ok(Tree::Un(Union { doc, name, names }))
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn parse_type_no_doc() -> ParseResult<()> {
        let r = parse(
            r#"
            type Participant {
              _id: ID
            }"#,
        )?;
        assert_eq!(
            r.to_string(),
            "type Participant {\
             \n  _id: ID\
             \n}\n"
        );
        Ok(())
    }

    #[test]
    fn parse_type_some_doc() -> ParseResult<()> {
        let r = parse(
            r#"
            "Some doc"
            type Participant {
              _id: ID!
            }"#,
        )?;
        assert_eq!(
            r.to_string(),
            "\"Some doc\"\
             \ntype Participant {\
             \n  _id: ID!\
             \n}\n"
        );
        Ok(())
    }

    #[test]
    fn parse_type_full_doc() -> ParseResult<()> {
        let r = parse(
            r#"
            "Some doc"
            type Participant {
            """Even "more" doc"""
              _id: ID
            }"#,
        )?;
        assert_eq!(
            r.to_string(),
            "\"Some doc\"\
             \ntype Participant {\
             \n  \"\"\"Even \"more\" doc\"\"\"\
             \n  _id: ID\
             \n}\n"
        );
        Ok(())
    }

    #[test]
    fn parse_type_field_args() -> ParseResult<()> {
        let r = parse(
            r#"
            type Query {
              recording(_id: ID!): Recording
            }"#,
        )?;
        assert_eq!(
            r.to_string(),
            "type Query {\
             \n  recording(_id: ID!): Recording\
             \n}\n"
        );
        Ok(())
    }

    #[test]
    fn parse_type_multi_field_args() -> ParseResult<()> {
        let r = parse(
            r#"
            type Mutation {
              deleteUser(userId: ID!, dryRun: Boolean) : DeleteUserResponse
            }"#,
        )?;
        assert_eq!(
            r.to_string(),
            "type Mutation {\
             \n  deleteUser(userId: ID!, dryRun: Boolean): DeleteUserResponse\
             \n}\n"
        );
        Ok(())
    }

    #[test]
    fn parse_type_array_field_nullable() -> ParseResult<()> {
        let r = parse(
            r#"
            type User {
              projects: [ Project! ]
            }"#,
        )?;
        assert_eq!(
            r.to_string(),
            "type User {\
             \n  projects: [Project!]\
             \n}\n"
        );
        Ok(())
    }

    #[test]
    fn parse_type_array_field() -> ParseResult<()> {
        let r = parse(
            r#"
            type User {
              projects: [ Project! ]!
            }"#,
        )?;
        assert_eq!(
            r.to_string(),
            "type User {\
             \n  projects: [Project!]!\
             \n}\n"
        );
        Ok(())
    }

    #[test]
    fn parse_type_array_field_args() -> ParseResult<()> {
        let r = parse(
            r#"
            type Query {
              usersByEmail(email: [ String! ]): [ User ]
            }"#,
        )?;
        assert_eq!(
            r.to_string(),
            "type Query {\
             \n  usersByEmail(email: [String!]): [User]\
             \n}\n"
        );
        Ok(())
    }

}
