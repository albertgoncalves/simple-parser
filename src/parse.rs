use crate::ast::{Expr, Ident, Op, Position, Scope, Stmt};
use std::collections::VecDeque;
use std::str::{from_utf8, from_utf8_unchecked};

type Parsed<T> = Result<Position<T>, usize>;

pub struct Tokenizer<'a>(&'a [u8], pub usize);

impl<'a> From<&'a [u8]> for Tokenizer<'a> {
    fn from(bytes: &'a [u8]) -> Self {
        Tokenizer(bytes, 0)
    }
}

fn is_keyword(string: &str) -> bool {
    [
        "null", "true", "false", "var", "function", "if", "else", "switch", "case", "for", "while",
        "break", "continue", "return",
    ]
    .contains(&string)
}

impl<'a> Tokenizer<'a> {
    pub fn pop(&mut self) -> Result<Position<&'a str>, usize> {
        let mut i = self.1;
        for j in self.1..self.0.len() {
            match self.0[j] as char {
                ' ' | '\n' => {
                    if i != j {
                        self.1 = j + 1;
                        return Ok((from_utf8(&self.0[i..j]).map_err(|_| i)?, i));
                    }
                    i = j + 1;
                }
                '(' | ')' | '{' | '}' | '=' | '.' | ';' => {
                    return Ok(if i == j {
                        self.1 = j + 1;
                        (unsafe { from_utf8_unchecked(&self.0[j..=j]) }, j)
                    } else {
                        self.1 = j;
                        (from_utf8(&self.0[i..j]).map_err(|_| i)?, i)
                    })
                }
                _ => (),
            }
        }
        Err(self.0.len())
    }

    fn peek(&mut self) -> Result<(Position<&'a str>, usize), usize> {
        let offset = self.1;
        let token = self.pop()?;
        Ok((token, std::mem::replace(&mut self.1, offset)))
    }

    fn r#match(&mut self, string: &'a str) -> Result<usize, usize> {
        let token = self.pop()?;
        if token.0 == string {
            return Ok(token.1);
        }
        Err(token.1)
    }

    fn parse_multiple<T>(
        &mut self,
        parse: impl Fn(&mut Self) -> Parsed<T>,
        open: &'a str,
        close: &'a str,
        delim: &'a str,
    ) -> Parsed<Vec<Position<T>>> {
        let offset = self.r#match(open)?;
        let mut args = vec![];
        let (token, next_offset) = self.peek()?;
        if token.0 == close {
            self.1 = next_offset;
        } else {
            args.push(parse(self)?);
            loop {
                let (token, next_offset) = self.peek()?;
                if token.0 == close {
                    self.1 = next_offset;
                    break;
                }
                let _ = self.r#match(delim)?;
                args.push(parse(self)?);
            }
        }
        Ok((args, offset))
    }

    fn parse_ident(&mut self) -> Parsed<Ident<'a>> {
        let token = self.pop()?;
        let error = Err(token.1);
        if is_keyword(token.0) {
            return error;
        }
        if (2 <= token.0.len()) && (&token.0[..2] == "__") {
            return error;
        }
        let bytes: &'a [u8] = token.0.as_bytes();
        if !((bytes[0] == b'_') || bytes[0].is_ascii_alphabetic()) {
            return error;
        }
        Ok((Ident::User(token.0, None), token.1))
    }

    fn parse_expr(&mut self, k: usize) -> Parsed<Expr<'a>> {
        let (token, next_offset) = self.peek()?;
        let offset = token.1;
        let (mut expr, offset) = match token.0 {
            "function" => {
                self.1 = next_offset;
                let args = self
                    .parse_multiple(Tokenizer::parse_ident, "(", ")", ",")?
                    .0;
                (Expr::Func(args, self.parse_scope()?), offset)
            }
            "[" => {
                let exprs = self
                    .parse_multiple(|tokenizer| tokenizer.parse_expr(0), "[", "]", ",")?
                    .0;
                (Expr::Array(exprs), offset)
            }
            "(" => {
                self.1 = next_offset;
                let expr = self.parse_expr(0)?;
                let _ = self.r#match(")")?;
                expr
            }
            string if string.as_bytes()[0].is_ascii_digit() => {
                self.1 = next_offset;
                (
                    Expr::Int(token.0.parse::<i64>().map_err(|_| token.1)?),
                    offset,
                )
            }
            _ => (Expr::Ident(self.parse_ident()?), offset),
        };
        loop {
            let (token, next_offset) = self.peek()?;
            match token.0 {
                "(" => {
                    if 34 < k {
                        break;
                    }
                    let func = expr;
                    let args = self
                        .parse_multiple(|tokenizer| tokenizer.parse_expr(0), "(", ")", ",")?
                        .0;
                    expr = Expr::Call((Box::new(func), offset), args);
                }
                "[" => {
                    if 34 < k {
                        break;
                    }
                    let array = expr;
                    let index = self.parse_expr(0)?;
                    let _ = self.r#match("]")?;
                    expr = Expr::Access(Box::new(((array, offset), index)));
                }
                string => {
                    let (op, l, r) = match string {
                        "." => (Op::Access, 34, 35),
                        "*" => (Op::Mul, 24, 25),
                        "/" => (Op::Div, 24, 25),
                        "+" => (Op::Add, 22, 23),
                        "-" => (Op::Sub, 22, 23),
                        "<=" => (Op::LessEqual, 18, 19),
                        _ => break,
                    };
                    if l < k {
                        break;
                    }
                    self.1 = next_offset;
                    let left = expr;
                    let right = self.parse_expr(r)?;
                    expr = Expr::BinOp((op, token.1), Box::new(((left, offset), right)));
                }
            }
        }
        Ok((expr, offset))
    }

    pub fn parse_stmt(&mut self) -> Parsed<Stmt<'a>> {
        let (token, next_offset) = self.peek()?;
        let offset = token.1;

        if token.0 == "var" {
            self.1 = next_offset;
            let ident = self.parse_ident()?;
            let _ = self.r#match("=")?;
            let expr = self.parse_expr(0)?;
            let _ = self.r#match(";")?;
            return Ok((Stmt::Let(ident, expr), offset));
        }

        if token.0 == "return" {
            self.1 = next_offset;
            let (token, next_offset) = self.peek()?;

            if token.0 == ";" {
                self.1 = next_offset;
                return Ok((Stmt::Return(None), offset));
            }

            let expr = self.parse_expr(0)?;
            let _ = self.r#match(";")?;
            return Ok((Stmt::Return(Some(expr)), offset));
        }

        let expr = self.parse_expr(0)?;
        let offset = expr.1;
        let (token, next_offset) = self.peek()?;

        if token.0 == "=" {
            self.1 = next_offset;
            let target = expr;
            let value = self.parse_expr(0)?;
            let _ = self.r#match(";")?;
            return Ok((Stmt::Set(target, value), offset));
        }

        let _ = self.r#match(";")?;
        Ok((Stmt::Void(expr), offset))
    }

    fn parse_scope(&mut self) -> Parsed<Scope<'a>> {
        let offset = self.r#match("{")?;
        let mut stmts = VecDeque::new();
        loop {
            let (token, next_offset) = self.peek()?;
            if token.0 == "}" {
                self.1 = next_offset;
                break;
            }
            stmts.push_back(self.parse_stmt()?);
        }
        Ok((Scope(stmts), offset))
    }
}
