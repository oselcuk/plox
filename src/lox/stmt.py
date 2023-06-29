from dataclasses import dataclass
from typing import Protocol, TypeVar

from lox import expr
from lox import scanner


T_co = TypeVar("T_co", covariant=True)


class StmtVisitor(Protocol[T_co]):
    def visit_expression(self, stmt: "Expression") -> T_co:
        ...

    def visit_print(self, stmt: "Print") -> T_co:
        ...

    def visit_var(self, stmt: "Var") -> T_co:
        ...

    def visit_block(self, stmt: "Block") -> T_co:
        ...

    def visit_if(self, stmt: "If") -> T_co:
        ...

    def visit_while(self, stmt: "While") -> T_co:
        ...

    def visit_break(self, stmt: "Break") -> T_co:
        ...

    def visit_function(self, stmt: "Function") -> T_co:
        ...

    def visit_return(self, stmt: "Return") -> T_co:
        ...


@dataclass(frozen=True, eq=False)
class Stmt:
    def accept(self, visitor: StmtVisitor[T_co]) -> T_co:
        visit_method = f"visit_{self.__class__.__name__.lower()}"
        if (visit := getattr(visitor, visit_method, False)) and callable(visit):
            return visit(self)
        raise NotImplementedError(f"{visit_method} not implemented on {visitor}")


@dataclass(frozen=True, eq=False)
class Expression(Stmt):
    expr: expr.Expr


@dataclass(frozen=True, eq=False)
class Print(Stmt):
    expr: expr.Expr


@dataclass(frozen=True, eq=False)
class Var(Stmt):
    name: scanner.Token
    initializer: expr.Expr | None


@dataclass(frozen=True, eq=False)
class Block(Stmt):
    statements: list[Stmt]


@dataclass(frozen=True, eq=False)
class If(Stmt):
    conditional: expr.Expr
    then_branch: Stmt
    else_branch: Stmt | None


@dataclass(frozen=True, eq=False)
class While(Stmt):
    conditional: expr.Expr
    body: Stmt


@dataclass(frozen=True, eq=False)
class Break(Stmt):
    keyword: scanner.Token


@dataclass(frozen=True, eq=False)
class Function(Stmt):
    name: scanner.Token
    params: list[scanner.Token]
    body: list[Stmt]


@dataclass(frozen=True, eq=False)
class Return(Stmt):
    keyword: scanner.Token
    value: expr.Expr


@dataclass(frozen=True, eq=False)
class Class(Stmt):
    name: scanner.Token
    methods: list[Function]
