use std::ops::Range;

type Span = (String, Range<usize>); // [start, end)
#[derive(Debug)]
struct Error {
    span: Span,
    message: String,
}
type Result<T> = std::result::Result<T, Error>;

impl std::fmt::Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        let (filename, range) = &self.span;
        let (start, end) = (range.start, range.end);
        let file = std::fs::read_to_string(filename).unwrap();

        let mut line_start = start;
        while line_start > 0 && file.chars().nth(line_start - 1) != Some('\n') {
            line_start -= 1;
        }

        let mut line_end = end;
        while line_end < file.len() && file.chars().nth(line_end) != Some('\n') {
            line_end += 1;
        }

        let line = file[line_start..line_end].to_string();
        let line_number = file[0..line_start].matches('\n').count() + 1;
        let line_column = start - line_start + 1;

        write!(
            f,
            r"{}:{}:{}: error: {}",
            filename, line_number, line_column, self.message
        )?;

        if line.len() > 0 {
            write!(f, "\n{}\n", line)?;
            for _ in 0..line_column - 1 {
                write!(f, " ")?;
            }
            write!(f, "^")?;
            for _ in line_column..end - line_start {
                write!(f, "~")?;
            }
        }

        Ok(())
    }
}

#[derive(Debug, PartialEq)]
enum TokenKind {
    // Literals
    Identifier,
    Number,
    String,

    // Keywords
    And,
    Const,
    Construct,
    Deconstruct,
    Object,
    Or,
    Null,
    If,
    Else,

    // Punctuation
    OpenParen,
    CloseParen,
    OpenBrace,
    CloseBrace,
    OpenBracket,
    CloseBracket,
    Comma,
    Dot,
    Colon,
    Semicolon,
    Pipe,

    // Operators
    Plus,
    Minus,
    Star,
    Slash,
    Percent,
    Equal,
    EqualEqual,
    Bang,
    BangEqual,
    Less,
    LessEqual,
    Greater,
    GreaterEqual,

    // Misc
    EndOfFile,
}

#[derive(Debug)]
struct Token {
    kind: TokenKind,
    value: String,
    span: Span,
}

impl std::fmt::Display for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(
            f,
            r"{:?}('{}') @ {}:{}",
            self.kind, self.value, self.span.1.start, self.span.1.end
        )
    }
}

fn tokenize(filename: &String, contents: &String) -> Result<Vec<Token>> {
    let mut tokens = vec![];
    let (mut start, mut end): (usize, usize) = (0, 0);

    let mut chars = contents.chars().peekable();
    while let Some(c) = chars.next() {
        match c {
            // Whitespace
            ' ' | '\r' | '\t' => {
                start += 1;
                end += 1;
            }
            '\n' => {
                start += 1;
                end += 1;
            }

            // Punctuation
            '(' => {
                tokens.push(Token {
                    kind: TokenKind::OpenParen,
                    value: "(".to_string(),
                    span: (filename.clone(), start..end + 1),
                });
                start = end + 1;
                end = start;
            }
            ')' => {
                tokens.push(Token {
                    kind: TokenKind::CloseParen,
                    value: ")".to_string(),
                    span: (filename.clone(), start..end + 1),
                });
                start = end + 1;
                end = start;
            }
            '{' => {
                tokens.push(Token {
                    kind: TokenKind::OpenBrace,
                    value: "{".to_string(),
                    span: (filename.clone(), start..end + 1),
                });
                start = end + 1;
                end = start;
            }
            '}' => {
                tokens.push(Token {
                    kind: TokenKind::CloseBrace,
                    value: "}".to_string(),
                    span: (filename.clone(), start..end + 1),
                });
                start = end + 1;
                end = start;
            }
            '[' => {
                tokens.push(Token {
                    kind: TokenKind::OpenBracket,
                    value: "[".to_string(),
                    span: (filename.clone(), start..end + 1),
                });
                start = end + 1;
                end = start;
            }
            ']' => {
                tokens.push(Token {
                    kind: TokenKind::CloseBracket,
                    value: "]".to_string(),
                    span: (filename.clone(), start..end + 1),
                });
                start = end + 1;
                end = start;
            }
            ',' => {
                tokens.push(Token {
                    kind: TokenKind::Comma,
                    value: ",".to_string(),
                    span: (filename.clone(), start..end + 1),
                });
                start = end + 1;
                end = start;
            }
            '.' => {
                tokens.push(Token {
                    kind: TokenKind::Dot,
                    value: ".".to_string(),
                    span: (filename.clone(), start..end + 1),
                });
                start = end + 1;
                end = start;
            }
            ':' => {
                tokens.push(Token {
                    kind: TokenKind::Colon,
                    value: ":".to_string(),
                    span: (filename.clone(), start..end + 1),
                });
                start = end + 1;
                end = start;
            }
            ';' => {
                tokens.push(Token {
                    kind: TokenKind::Semicolon,
                    value: ";".to_string(),
                    span: (filename.clone(), start..end + 1),
                });
                start = end + 1;
                end = start;
            }
            '|' => {
                tokens.push(Token {
                    kind: TokenKind::Pipe,
                    value: "|".to_string(),
                    span: (filename.clone(), start..end + 1),
                });
                start = end + 1;
                end = start;
            }

            // Operators
            '+' => {
                tokens.push(Token {
                    kind: TokenKind::Plus,
                    value: "+".to_string(),
                    span: (filename.clone(), start..end + 1),
                });
                start = end + 1;
                end = start;
            }
            '-' => {
                tokens.push(Token {
                    kind: TokenKind::Minus,
                    value: "-".to_string(),
                    span: (filename.clone(), start..end + 1),
                });
                start = end + 1;
                end = start;
            }
            '*' => {
                tokens.push(Token {
                    kind: TokenKind::Star,
                    value: "*".to_string(),
                    span: (filename.clone(), start..end + 1),
                });
                start = end + 1;
                end = start;
            }
            '/' => {
                tokens.push(Token {
                    kind: TokenKind::Slash,
                    value: "/".to_string(),
                    span: (filename.clone(), start..end + 1),
                });
                start = end + 1;
                end = start;
            }
            '%' => {
                tokens.push(Token {
                    kind: TokenKind::Percent,
                    value: "%".to_string(),
                    span: (filename.clone(), start..end + 1),
                });
                start = end + 1;
                end = start;
            }
            '=' => {
                if let Some('=') = chars.peek() {
                    chars.next();
                    end += 1;
                    tokens.push(Token {
                        kind: TokenKind::EqualEqual,
                        value: "==".to_string(),
                        span: (filename.clone(), start..end + 1),
                    });
                } else {
                    tokens.push(Token {
                        kind: TokenKind::Equal,
                        value: "=".to_string(),
                        span: (filename.clone(), start..end + 1),
                    });
                }
                start = end + 1;
                end = start;
            }
            '!' => {
                if let Some('=') = chars.peek() {
                    chars.next();
                    end += 1;
                    tokens.push(Token {
                        kind: TokenKind::BangEqual,
                        value: "!=".to_string(),
                        span: (filename.clone(), start..end + 1),
                    });
                } else {
                    tokens.push(Token {
                        kind: TokenKind::Bang,
                        value: "!".to_string(),
                        span: (filename.clone(), start..end + 1),
                    });
                }
                start = end + 1;
                end = start;
            }
            '<' => {
                if let Some('=') = chars.peek() {
                    chars.next();
                    end += 1;
                    tokens.push(Token {
                        kind: TokenKind::LessEqual,
                        value: "<=".to_string(),
                        span: (filename.clone(), start..end + 1),
                    });
                } else {
                    tokens.push(Token {
                        kind: TokenKind::Less,
                        value: "<".to_string(),
                        span: (filename.clone(), start..end + 1),
                    });
                }
                start = end + 1;
                end = start;
            }
            '>' => {
                if let Some('=') = chars.peek() {
                    chars.next();
                    end += 1;
                    tokens.push(Token {
                        kind: TokenKind::GreaterEqual,
                        value: ">=".to_string(),
                        span: (filename.clone(), start..end + 1),
                    });
                } else {
                    tokens.push(Token {
                        kind: TokenKind::Greater,
                        value: ">".to_string(),
                        span: (filename.clone(), start..end + 1),
                    });
                }
                start = end + 1;
                end = start;
            }

            // Literals
            '0'..='9' => {
                while let Some('0'..='9') = chars.peek() {
                    chars.next();
                    end += 1;
                }

                if let Some('.') = chars.peek() {
                    chars.next();
                    end += 1;
                    while let Some('0'..='9') = chars.peek() {
                        chars.next();
                        end += 1;
                    }
                }

                tokens.push(Token {
                    kind: TokenKind::Number,
                    value: contents[start..end + 1].to_string(),
                    span: (filename.clone(), start..end + 1),
                });
                start = end + 1;
                end = start;
            }
            'a'..='z' | 'A'..='Z' | '_' => {
                while let Some('a'..='z' | 'A'..='Z' | '_' | '0'..='9') = chars.peek() {
                    chars.next();
                    end += 1;
                }

                let value = contents[start..end + 1].to_string();
                let kind = match value.as_str() {
                    "and" => TokenKind::And,
                    "const" => TokenKind::Const,
                    "construct" => TokenKind::Construct,
                    "deconstruct" => TokenKind::Deconstruct,
                    "object" => TokenKind::Object,
                    "or" => TokenKind::Or,
                    "null" => TokenKind::Null,
                    "if" => TokenKind::If,
                    "else" => TokenKind::Else,
                    _ => TokenKind::Identifier,
                };

                tokens.push(Token {
                    kind,
                    value,
                    span: (filename.clone(), start..end + 1),
                });
                start = end + 1;
                end = start;
            }
            '"' => {
                while let Some('"') = chars.peek() {
                    chars.next();
                    end += 1;
                }

                tokens.push(Token {
                    kind: TokenKind::String,
                    value: contents[start..end + 1].to_string(),
                    span: (filename.clone(), start..end + 1),
                });
                start = end + 1;
                end = start;
            }

            // Comments
            '#' => {
                while let Some('\n') = chars.peek() {
                    chars.next();
                    end += 1;
                }
                start = end + 1;
                end = start;
            }

            _ => {
                return Err(Error {
                    span: (filename.clone(), start..end + 1),
                    message: format!("unexpected character '{c}'"),
                })
            }
        };
    }

    tokens.push(Token {
        kind: TokenKind::EndOfFile,
        value: "".to_string(),
        span: (filename.clone(), start..end),
    });

    Ok(tokens)
}

#[derive(Debug)]
struct Variable {
    name: String,
    value: Option<Expression>,
}

#[repr(C)]
#[derive(Debug)]
enum MethodType {
    Normal,
    Constructor,
    Deconstructor,
}

#[derive(Debug)]
struct Method {
    method_type: MethodType,
    name: String,
    parameters: Vec<Variable>,
    body: Block,
}

#[derive(Debug)]
struct Block {
    statements: Vec<Statement>,
}

#[derive(Debug)]
enum Statement {
    Const(Vec<Variable>), // allows for multiple variables to be declared in one statement
    Object(String, Vec<Variable>, Vec<Method>),
    If(Expression, Block, Option<Block>),
    Expression(Expression),
}

#[derive(Debug)]
enum Expression {
    Identifier(String),
    Number(f64),
    String(String),
    Null,
    Binary(Box<Expression>, BinaryOperator, Box<Expression>),
    Unary(UnaryOperator, Box<Expression>),
    Call(Box<Expression>, Vec<Expression>),
    Get(Box<Expression>, String),
    Set(Box<Expression>, String, Box<Expression>),
    Construct(Box<Expression>, Vec<Expression>),
    Deconstruct(Box<Expression>, Vec<Expression>),
    Lambda(Vec<Variable>, Vec<Statement>),
}

#[derive(Debug)]
enum BinaryOperator {
    Plus,
    Minus,
    Star,
    Slash,
    Percent,
    EqualEqual,
    BangEqual,
    Less,
    LessEqual,
    Greater,
    GreaterEqual,
    And,
    Or,
}

#[derive(Debug)]
enum UnaryOperator {
    Plus,
    Minus,
    Bang,
}

#[derive(Debug)]
struct Program {
    statements: Vec<Statement>,
}

#[derive(Debug)]
struct Parser {
    tokens: Vec<Token>,
    current: usize,
}

impl Parser {
    pub fn new(tokens: Vec<Token>) -> Self {
        Self { tokens, current: 0 }
    }

    pub fn parse(&mut self) -> Result<Program> {
        let mut statements = vec![];
        while !self.is_at_end() {
            statements.push(self.declaration()?);
        }
        Ok(Program { statements })
    }

    fn declaration(&mut self) -> Result<Statement> {
        if self.matches(&[TokenKind::Const]) {
            self.const_declaration()
        } else if self.matches(&[TokenKind::Object]) {
            self.object_declaration()
        } else if self.matches(&[TokenKind::If]) {
            self.if_declaration()
        } else {
            self.expression_statement()
        }
    }

    fn const_declaration(&mut self) -> Result<Statement> {
        let mut variables = vec![];
        if self.matches(&[TokenKind::OpenParen]) {
            while !self.check(&TokenKind::CloseParen) {
                self.consume(TokenKind::Identifier, "expected identifier")?;
                let name = self.previous().value.clone();
                let value = if self.matches(&[TokenKind::Equal]) {
                    Some(self.expression()?)
                } else {
                    None
                };
                if !self.matches(&[TokenKind::Comma]) {
                    break;
                }
                variables.push(Variable { name, value });
            }
            self.consume(
                TokenKind::CloseParen,
                "expected ')' after variable declaration",
            )?;
        } else {
            self.consume(TokenKind::Identifier, "expected identifier")?;
            let name = self.previous().value.clone();
            let value = if self.matches(&[TokenKind::Equal]) {
                Some(self.expression()?)
            } else {
                None
            };
            variables.push(Variable { name, value });
        }
        self.consume(
            TokenKind::Semicolon,
            "expected ';' after variable declaration",
        )?;
        Ok(Statement::Const(variables))
    }

    fn object_declaration(&mut self) -> Result<Statement> {
        self.consume(TokenKind::Identifier, "expected identifier")?;
        let name = self.previous().value.clone();
        let mut variables = vec![];
        let mut methods = vec![];
        self.consume(TokenKind::OpenBrace, "expected '{' before object body")?;
        while !self.check(&TokenKind::CloseBrace) {
            if self.matches(&[TokenKind::Const]) {
                self.consume(TokenKind::OpenParen, "expected '(' after 'const'")?;
                while !self.check(&TokenKind::CloseParen) {
                    self.consume(TokenKind::Identifier, "expected identifier")?;
                    let name = self.previous().value.clone();
                    let value = if self.matches(&[TokenKind::Equal]) {
                        Some(self.expression()?)
                    } else {
                        None
                    };
                    if !self.matches(&[TokenKind::Comma]) {
                        break;
                    }
                    variables.push(Variable { name, value });
                }
                self.consume(
                    TokenKind::CloseParen,
                    "expected ')' after variable declaration",
                )?;
                self.consume(
                    TokenKind::Semicolon,
                    "expected ';' after variable declaration",
                )?;
            } else if self.matches(&[TokenKind::Construct]) {
                let mut parameters = vec![];
                if self.check(&TokenKind::OpenParen) {
                    self.consume(TokenKind::OpenParen, "expected '(' after 'construct'")?;
                    while !self.check(&TokenKind::CloseParen) {
                        self.consume(TokenKind::Identifier, "expected identifier")?;
                        let name = self.previous().value.clone();
                        parameters.push(Variable { name, value: None });
                        if !self.matches(&[TokenKind::Comma]) {
                            break;
                        }
                    }
                    self.consume(TokenKind::CloseParen, "expected ')' after parameter list")?;
                }
                let body = self.block()?;
                methods.push(Method {
                    method_type: MethodType::Constructor,
                    name: "construct".to_string(),
                    parameters,
                    body,
                });
            } else if self.matches(&[TokenKind::Deconstruct]) {
                self.consume(TokenKind::OpenParen, "expected '(' after 'deconstruct'")?;
                let body = self.block()?;
                methods.push(Method {
                    method_type: MethodType::Deconstructor,
                    name: "deconstruct".to_string(),
                    parameters: vec![],
                    body,
                });
            } else {
                self.consume(TokenKind::Identifier, "expected identifier")?;
                let name = self.previous().value.clone();
                self.consume(TokenKind::OpenParen, "expected '(' after method name")?;
                let mut parameters = vec![];
                while !self.check(&TokenKind::CloseParen) {
                    self.consume(TokenKind::Identifier, "expected identifier")?;
                    let name = self.previous().value.clone();
                    parameters.push(Variable { name, value: None });
                    if !self.matches(&[TokenKind::Comma]) {
                        break;
                    }
                }
                self.consume(TokenKind::CloseParen, "expected ')' after parameter list")?;
                let body = self.block()?;
                methods.push(Method {
                    method_type: MethodType::Normal,
                    name,
                    parameters,
                    body,
                });
            }
        }
        self.consume(TokenKind::CloseBrace, "expected '}' after object body")?;
        Ok(Statement::Object(name, variables, methods))
    }

    fn if_declaration(&mut self) -> Result<Statement> {
        let condition = self.expression()?;
        let then_branch = self.block()?;
        let else_branch = if self.matches(&[TokenKind::Else]) {
            Some(self.block()?)
        } else {
            None
        };
        Ok(Statement::If(condition, then_branch, else_branch))
    }

    fn expression_statement(&mut self) -> Result<Statement> {
        let expression = self.expression()?;
        self.consume(TokenKind::Semicolon, "expected ';' after expression")?;
        Ok(Statement::Expression(expression))
    }

    fn block(&mut self) -> Result<Block> {
        let mut statements = vec![];
        self.consume(TokenKind::OpenBrace, "expected '{' before block")?;
        while !self.check(&TokenKind::CloseBrace) {
            statements.push(self.declaration()?);
        }
        self.consume(TokenKind::CloseBrace, "expected '}' after block")?;
        Ok(Block { statements })
    }

    fn expression(&mut self) -> Result<Expression> {
        self.assignment()
    }

    fn assignment(&mut self) -> Result<Expression> {
        let expression = self.or()?;
        if self.matches(&[TokenKind::Equal]) {
            let span = self.previous().span.clone();
            let value = self.assignment()?;
            if let Expression::Identifier(name) = expression {
                return Ok(Expression::Set(
                    Box::new(Expression::Identifier(name.clone())),
                    name,
                    Box::new(value),
                ));
            }
            return Err(Error {
                span,
                message: "invalid assignment target".to_string(),
            });
        }
        Ok(expression)
    }

    fn or(&mut self) -> Result<Expression> {
        let mut expression = self.and()?;
        while self.matches(&[TokenKind::Or]) {
            let operator = BinaryOperator::Or;
            let right = self.and()?;
            expression = Expression::Binary(Box::new(expression), operator, Box::new(right));
        }
        Ok(expression)
    }

    fn and(&mut self) -> Result<Expression> {
        let mut expression = self.equality()?;
        while self.matches(&[TokenKind::And]) {
            let operator = BinaryOperator::And;
            let right = self.equality()?;
            expression = Expression::Binary(Box::new(expression), operator, Box::new(right));
        }
        Ok(expression)
    }

    fn equality(&mut self) -> Result<Expression> {
        let mut expression = self.comparison()?;
        while self.matches(&[TokenKind::EqualEqual, TokenKind::BangEqual]) {
            let operator = match self.previous().kind {
                TokenKind::EqualEqual => BinaryOperator::EqualEqual,
                TokenKind::BangEqual => BinaryOperator::BangEqual,
                _ => unreachable!(),
            };
            let right = self.comparison()?;
            expression = Expression::Binary(Box::new(expression), operator, Box::new(right));
        }
        Ok(expression)
    }

    fn comparison(&mut self) -> Result<Expression> {
        let mut expression = self.term()?;
        while self.matches(&[
            TokenKind::Less,
            TokenKind::LessEqual,
            TokenKind::Greater,
            TokenKind::GreaterEqual,
        ]) {
            let operator = match self.previous().kind {
                TokenKind::Less => BinaryOperator::Less,
                TokenKind::LessEqual => BinaryOperator::LessEqual,
                TokenKind::Greater => BinaryOperator::Greater,
                TokenKind::GreaterEqual => BinaryOperator::GreaterEqual,
                _ => unreachable!(),
            };
            let right = self.term()?;
            expression = Expression::Binary(Box::new(expression), operator, Box::new(right));
        }
        Ok(expression)
    }

    fn term(&mut self) -> Result<Expression> {
        let mut expression = self.factor()?;
        while self.matches(&[TokenKind::Plus, TokenKind::Minus]) {
            let operator = match self.previous().kind {
                TokenKind::Plus => BinaryOperator::Plus,
                TokenKind::Minus => BinaryOperator::Minus,
                _ => unreachable!(),
            };
            let right = self.factor()?;
            expression = Expression::Binary(Box::new(expression), operator, Box::new(right));
        }
        Ok(expression)
    }

    fn factor(&mut self) -> Result<Expression> {
        let mut expression = self.unary()?;
        while self.matches(&[TokenKind::Star, TokenKind::Slash, TokenKind::Percent]) {
            let operator = match self.previous().kind {
                TokenKind::Star => BinaryOperator::Star,
                TokenKind::Slash => BinaryOperator::Slash,
                TokenKind::Percent => BinaryOperator::Percent,
                _ => unreachable!(),
            };
            let right = self.unary()?;
            expression = Expression::Binary(Box::new(expression), operator, Box::new(right));
        }
        Ok(expression)
    }

    fn unary(&mut self) -> Result<Expression> {
        if self.matches(&[TokenKind::Plus, TokenKind::Minus]) {
            let operator = match self.previous().kind {
                TokenKind::Plus => UnaryOperator::Plus,
                TokenKind::Minus => UnaryOperator::Minus,
                _ => unreachable!(),
            };
            let right = self.unary()?;
            return Ok(Expression::Unary(operator, Box::new(right)));
        }
        self.call()
    }

    fn call(&mut self) -> Result<Expression> {
        let mut expression = self.primary()?;
        loop {
            if self.matches(&[TokenKind::OpenParen]) {
                expression = self.finish_call(expression)?;
            } else if self.matches(&[TokenKind::Dot]) {
                self.consume(TokenKind::Identifier, "expected property name")?;
                let name = self.previous().value.clone();
                expression = Expression::Get(Box::new(expression), name);
            } else {
                break;
            }
        }
        Ok(expression)
    }

    fn finish_call(&mut self, callee: Expression) -> Result<Expression> {
        let mut arguments = vec![];
        if !self.check(&TokenKind::CloseParen) {
            loop {
                arguments.push(self.expression()?);
                if !self.matches(&[TokenKind::Comma]) {
                    break;
                }
            }
        }
        self.consume(TokenKind::CloseParen, "expected ')' after arguments")?;
        Ok(Expression::Call(Box::new(callee), arguments))
    }

    fn primary(&mut self) -> Result<Expression> {
        if self.matches(&[TokenKind::Identifier]) {
            return Ok(Expression::Identifier(self.previous().value.clone()));
        }
        if self.matches(&[TokenKind::Number]) {
            return Ok(Expression::Number(
                self.previous().value.parse::<f64>().unwrap(),
            ));
        }
        if self.matches(&[TokenKind::String]) {
            return Ok(Expression::String(self.previous().value.clone()));
        }
        if self.matches(&[TokenKind::Null]) {
            return Ok(Expression::Null);
        }
        if self.matches(&[TokenKind::OpenParen]) {
            let expression = self.expression()?;
            self.consume(TokenKind::CloseParen, "expected ')' after expression")?;
            return Ok(Expression::Unary(UnaryOperator::Bang, Box::new(expression)));
        }
        if self.matches(&[TokenKind::OpenBrace]) {
            let mut variables = vec![];
            self.consume(TokenKind::Pipe, "expected '|' after '{'")?;
            while !self.check(&TokenKind::Pipe) {
                self.consume(TokenKind::Identifier, "expected identifier")?;
                let name = self.previous().value.clone();
                variables.push(Variable { name, value: None });
                if !self.matches(&[TokenKind::Comma]) {
                    break;
                }
            }
            self.consume(TokenKind::Pipe, "expected '|' after variable list")?;
            let mut body = vec![];
            if !self.check(&TokenKind::CloseBrace) {
                loop {
                    body.push(self.declaration()?);
                    if !self.matches(&[TokenKind::Semicolon]) {
                        break;
                    }
                }
            }
            return Ok(Expression::Lambda(variables, body));
        }
        if self.matches(&[TokenKind::Construct]) {
            let callee = self.expression()?;
            if !self.matches(&[TokenKind::OpenParen]) {
                return Ok(Expression::Construct(Box::new(callee), vec![]));
            }
            self.consume(TokenKind::OpenParen, "expected '(' after callee")?;
            let mut arguments = vec![];
            if !self.check(&TokenKind::CloseParen) {
                loop {
                    arguments.push(self.expression()?);
                    if !self.matches(&[TokenKind::Comma]) {
                        break;
                    }
                }
            }
            self.consume(TokenKind::CloseParen, "expected ')' after arguments")?;
            return Ok(Expression::Construct(Box::new(callee), arguments));
        }
        if self.matches(&[TokenKind::Deconstruct]) {
            let callee = self.expression()?;
            if !self.matches(&[TokenKind::OpenParen]) {
                return Ok(Expression::Deconstruct(Box::new(callee), vec![]));
            }
            self.consume(TokenKind::OpenParen, "expected '(' after callee")?;
            let mut arguments = vec![];
            if !self.check(&TokenKind::CloseParen) {
                loop {
                    arguments.push(self.expression()?);
                    if !self.matches(&[TokenKind::Comma]) {
                        break;
                    }
                }
            }
            self.consume(TokenKind::CloseParen, "expected ')' after arguments")?;
            return Ok(Expression::Deconstruct(Box::new(callee), arguments));
        }
        Err(Error {
            span: self.peek().span.clone(),
            message: "expected expression".to_string(),
        })
    }

    fn matches(&mut self, kinds: &[TokenKind]) -> bool {
        for kind in kinds {
            if self.check(kind) {
                self.advance();
                return true;
            }
        }
        false
    }

    fn check(&self, kind: &TokenKind) -> bool {
        if self.is_at_end() {
            return false;
        }
        self.peek().kind == *kind
    }

    fn advance(&mut self) -> &Token {
        if !self.is_at_end() {
            self.current += 1;
        }
        self.previous()
    }

    fn is_at_end(&self) -> bool {
        self.peek().kind == TokenKind::EndOfFile
    }

    fn peek(&self) -> &Token {
        &self.tokens[self.current]
    }

    fn previous(&self) -> &Token {
        &self.tokens[self.current - 1]
    }

    fn consume(&mut self, kind: TokenKind, message: &str) -> Result<&Token> {
        if self.check(&kind) {
            Ok(self.advance())
        } else {
            Err(Error {
                span: self.peek().span.clone(),
                message: message.to_string(),
            })
        }
    }
}

fn main() {
    let args: Vec<String> = std::env::args().skip(1).collect::<_>();
    let filename = &args[0];
    let contents = std::fs::read_to_string(filename);
    let contents = match contents {
        Ok(contents) => contents,
        Err(e) => {
            println!("{}", e);
            return;
        }
    };
    let tokens = tokenize(&filename, &contents);
    let tokens = match tokens {
        Ok(tokens) => tokens,
        Err(e) => {
            println!("{}", e);
            return;
        }
    };

    for token in &tokens {
        println!(
            "{} '{}'",
            token,
            contents[token.span.1.start..token.span.1.end].to_string()
        );
    }

    let mut parser = Parser::new(tokens);
    let program = parser.parse();
    let program = match program {
        Ok(program) => program,
        Err(e) => {
            println!("{}", e);
            return;
        }
    };

    println!("{:#?}", program);
}
