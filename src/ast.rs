use crate::lex::{Annot, Loc};

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum AstKind {
    Num(u64),
    Variable(usize), // usizeはBPからのオフセット
    UniOp {
        op: UniOp,
        e: Box<Ast>,
    },
    BinOp {
        op: BinOp,
        l: Box<Ast>,
        r: Box<Ast>,
    },
    Stmt(Box<Ast>),
    Assign {
        l: Box<Ast>,
        r: Box<Ast>,
    },
    Return(Box<Ast>),
    If {
        cond: Box<Ast>,
        expr: Box<Ast>,
        els: Option<Box<Ast>>,
    },
    While {
        cond: Box<Ast>,
        stmt: Box<Ast>,
    },
    Block(Vec<Ast>),
    For {
        declare: Option<Box<Ast>>,
        cond: Option<Box<Ast>>,
        update: Option<Box<Ast>>,
        stmt: Box<Ast>,
    },
    Fun {
        name: String,
        args: Vec<Ast>,
    },
    FunDeclare {
        name: String,
        args: Vec<Ast>,
        body: Vec<Ast>,
    },
}
pub type Ast = Annot<AstKind>;
impl Ast {
    pub fn num(n: u64, loc: Loc) -> Self {
        Self::new(AstKind::Num(n), loc)
    }
    pub fn variable(offset: usize, loc: Loc) -> Self {
        Self::new(AstKind::Variable(offset), loc)
    }
    pub fn binop(op: BinOp, l: Ast, r: Ast, loc: Loc) -> Self {
        Self::new(
            AstKind::BinOp {
                op,
                l: Box::new(l),
                r: Box::new(r),
            },
            loc,
        )
    }
    pub fn uniop(op: UniOp, e: Ast, loc: Loc) -> Self {
        Self::new(AstKind::UniOp { op, e: Box::new(e) }, loc)
    }
    pub fn stmt(expr: Ast, loc: Loc) -> Self {
        Self::new(AstKind::Stmt(Box::new(expr)), loc)
    }
    pub fn assign(l: Ast, r: Ast, loc: Loc) -> Self {
        Self::new(
            AstKind::Assign {
                l: Box::new(l),
                r: Box::new(r),
            },
            loc,
        )
    }
    pub fn make_return(expr: Ast, loc: Loc) -> Self {
        Self::new(AstKind::Return(Box::new(expr)), loc)
    }
    pub fn make_if(cond: Ast, stmt: Ast, loc: Loc) -> Self {
        Self::new(
            AstKind::If {
                cond: Box::new(cond),
                expr: Box::new(stmt),
                els: None,
            },
            loc,
        )
    }
    pub fn make_if_else(cond: Ast, stmt: Ast, els: Ast, loc: Loc) -> Self {
        Self::new(
            AstKind::If {
                cond: Box::new(cond),
                expr: Box::new(stmt),
                els: Some(Box::new(els)),
            },
            loc,
        )
    }
    pub fn make_block(v: Vec<Ast>, loc: Loc) -> Self {
        Self::new(AstKind::Block(v), loc)
    }
    pub fn make_while(cond: Ast, stmt: Ast, loc: Loc) -> Self {
        Self::new(
            AstKind::While {
                cond: Box::new(cond),
                stmt: Box::new(stmt),
            },
            loc,
        )
    }
    pub fn make_for(
        declare: Option<Ast>,
        cond: Option<Ast>,
        update: Option<Ast>,
        stmt: Ast,
        loc: Loc,
    ) -> Self {
        Self::new(
            AstKind::For {
                declare: declare.map(|ast| Box::new(ast)),
                cond: cond.map(|ast| Box::new(ast)),
                update: update.map(|ast| Box::new(ast)),
                stmt: Box::new(stmt),
            },
            loc,
        )
    }
    pub fn make_function(name: String, args: Vec<Ast>, loc: Loc) -> Self {
        Self::new(AstKind::Fun { name, args }, loc)
    }
    pub fn dec_fun(name: String, args: Vec<Ast>, body: Vec<Ast>, loc: Loc) -> Self {
        Self::new(AstKind::FunDeclare { name, args, body }, loc)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum UniOpKind {
    Plus,
    Minus,
    Reference,
    Dereference,
}
pub type UniOp = Annot<UniOpKind>;
impl UniOp {
    pub fn plus(loc: Loc) -> Self {
        Self::new(UniOpKind::Plus, loc)
    }
    pub fn minus(loc: Loc) -> Self {
        Self::new(UniOpKind::Minus, loc)
    }
    pub fn reference(loc: Loc) -> Self {
        Self::new(UniOpKind::Reference, loc)
    }
    pub fn dereference(loc: Loc) -> Self {
        Self::new(UniOpKind::Dereference, loc)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum BinOpKind {
    Add,
    Sub,
    Mult,
    Div,
    Less,
    LessEqual,
    Greater,
    GreaterEqual,
    Equal,
    NotEqual,
}
pub type BinOp = Annot<BinOpKind>;
impl BinOp {
    pub fn add(loc: Loc) -> Self {
        Self::new(BinOpKind::Add, loc)
    }
    pub fn sub(loc: Loc) -> Self {
        Self::new(BinOpKind::Sub, loc)
    }
    pub fn mult(loc: Loc) -> Self {
        Self::new(BinOpKind::Mult, loc)
    }
    pub fn div(loc: Loc) -> Self {
        Self::new(BinOpKind::Div, loc)
    }
    pub fn less(loc: Loc) -> Self {
        Self::new(BinOpKind::Less, loc)
    }
    pub fn less_equal(loc: Loc) -> Self {
        Self::new(BinOpKind::LessEqual, loc)
    }
    pub fn greater(loc: Loc) -> Self {
        Self::new(BinOpKind::Greater, loc)
    }
    pub fn greater_equal(loc: Loc) -> Self {
        Self::new(BinOpKind::GreaterEqual, loc)
    }
    pub fn equal(loc: Loc) -> Self {
        Self::new(BinOpKind::Equal, loc)
    }
    pub fn not_equal(loc: Loc) -> Self {
        Self::new(BinOpKind::NotEqual, loc)
    }
}
