use std::collections::VecDeque;
use std::fmt;

pub type Position<T> = (T, usize);

#[derive(Debug)]
pub enum Op {
    Access,
    Add,
    Sub,
    Mul,
    Div,
    LessEqual,
}

#[derive(Debug)]
pub enum Ident<'a> {
    User(&'a str, Option<usize>),
    Anonymous(usize),
}

#[derive(Debug)]
pub enum Expr<'a> {
    Int(i64),
    Ident(Position<Ident<'a>>),
    Array(Vec<Position<Expr<'a>>>),
    Access(Box<(Position<Expr<'a>>, Position<Expr<'a>>)>),
    BinOp(Position<Op>, Box<(Position<Expr<'a>>, Position<Expr<'a>>)>),
    Call(Position<Box<Expr<'a>>>, Vec<Position<Expr<'a>>>),
    Func(Vec<Position<Ident<'a>>>, Position<Scope<'a>>),
}

#[derive(Debug)]
pub enum Stmt<'a> {
    Void(Position<Expr<'a>>),
    Let(Position<Ident<'a>>, Position<Expr<'a>>),
    Set(Position<Expr<'a>>, Position<Expr<'a>>),
    Return(Option<Position<Expr<'a>>>),
}

#[derive(Debug)]
pub struct Scope<'a>(pub VecDeque<Position<Stmt<'a>>>);

fn write_delim<A, B>(f: &mut fmt::Formatter, items: &[Position<A>], delim: B) -> fmt::Result
where
    A: fmt::Display,
    B: fmt::Display,
{
    let mut items = items.iter();
    if let Some(item) = items.next() {
        write!(f, "{}", item.0)?;
        for item in items {
            write!(f, "{delim}{}", item.0)?;
        }
    }
    fmt::Result::Ok(())
}

trait Display {
    fn display(&self, f: &mut fmt::Formatter, pad: usize) -> fmt::Result;
}

impl Display for Expr<'_> {
    fn display(&self, f: &mut fmt::Formatter, pad: usize) -> fmt::Result {
        match self {
            Self::Int(int) => write!(f, "{int}"),
            Self::Ident(ident) => write!(f, "{}", ident.0),
            Self::Array(exprs) => {
                write!(f, "[")?;
                let mut exprs = exprs.iter();
                if let Some(expr) = exprs.next() {
                    expr.0.display(f, pad)?;
                    for expr in exprs {
                        write!(f, ", ")?;
                        expr.0.display(f, pad)?;
                    }
                }
                write!(f, "]")
            }
            Self::Access(access) => {
                access.0 .0.display(f, pad)?;
                write!(f, "[")?;
                access.1 .0.display(f, pad)?;
                write!(f, "]")
            }
            Self::BinOp(op, exprs) => {
                let left = &exprs.0;
                let right = &exprs.1;
                write!(f, "(")?;
                left.0.display(f, pad)?;
                (match op.0 {
                    Op::Access => write!(f, "."),
                    _ => write!(f, " {} ", op.0),
                })?;
                right.0.display(f, pad)?;
                write!(f, ")")
            }
            Self::Call(func, args) => {
                if matches!(*func.0, Expr::Func(..)) {
                    write!(f, "(")?;
                    func.0.display(f, pad)?;
                    write!(f, ")(")?;
                } else {
                    func.0.display(f, pad)?;
                    write!(f, "(")?;
                }
                let mut args = args.iter();
                if let Some(arg) = args.next() {
                    arg.0.display(f, pad)?;
                    for arg in args {
                        write!(f, ", ")?;
                        arg.0.display(f, pad)?;
                    }
                }
                write!(f, ")")
            }
            Self::Func(args, scope) => {
                write!(f, "function(")?;
                write_delim(f, &args[..], ", ")?;
                write!(f, ") ")?;
                scope.0.display(f, pad)
            }
        }
    }
}

impl Display for Stmt<'_> {
    fn display(&self, f: &mut fmt::Formatter, pad: usize) -> fmt::Result {
        match self {
            Self::Void(expr) => {
                expr.0.display(f, pad)?;
                write!(f, ";")
            }
            Self::Let(ident, expr) => {
                write!(f, "var {} = ", ident.0)?;
                expr.0.display(f, pad)?;
                write!(f, ";")
            }
            Self::Set(target, value) => {
                target.0.display(f, pad)?;
                write!(f, " = ")?;
                value.0.display(f, pad)?;
                write!(f, ";")
            }
            Self::Return(Some(expr)) => {
                write!(f, "return ")?;
                expr.0.display(f, pad)?;
                write!(f, ";")
            }
            Self::Return(None) => write!(f, "return;"),
        }
    }
}

impl Display for Scope<'_> {
    fn display(&self, f: &mut fmt::Formatter, pad: usize) -> fmt::Result {
        writeln!(f, "{{")?;
        {
            let pad = pad + 4;
            for stmt in &self.0 {
                write!(f, "{:pad$}", "")?;
                stmt.0.display(f, pad)?;
                writeln!(f)?;
            }
        }
        write!(f, "{:pad$}}}", "")
    }
}

impl fmt::Display for Op {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::Access => write!(f, "."),
            Self::Add => write!(f, "+"),
            Self::Sub => write!(f, "-"),
            Self::Mul => write!(f, "*"),
            Self::Div => write!(f, "/"),
            Self::LessEqual => write!(f, "<="),
        }
    }
}

impl fmt::Display for Ident<'_> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::User(string, Some(k)) => write!(f, "{string}${k}"),
            Self::User(string, None) => write!(f, "{string}"),
            Self::Anonymous(k) => write!(f, "__{k}__"),
        }
    }
}

impl fmt::Display for Expr<'_> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        self.display(f, 0)
    }
}

impl fmt::Display for Stmt<'_> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        self.display(f, 0)
    }
}

impl fmt::Display for Scope<'_> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        self.display(f, 0)
    }
}
