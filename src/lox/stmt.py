from dataclasses import dataclass
from typing import Protocol, TypeVar

from lox.expr import Expr
from lox.scanner import Token


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


@dataclass(frozen=True)
class Stmt:
    def accept(self, visitor: StmtVisitor[T_co]) -> T_co:
        visit_method = f"visit_{self.__class__.__name__.lower()}"
        if (visit := getattr(visitor, visit_method, False)) and callable(visit):
            return visit(self)
        raise NotImplementedError(f"{visit_method} not implemented on {visitor}")


@dataclass(frozen=True)
class Expression(Stmt):
    expr: Expr


@dataclass(frozen=True)
class Print(Stmt):
    expr: Expr


@dataclass(frozen=True)
class Var(Stmt):
    name: Token
    initializer: Expr | None


@dataclass(frozen=True)
class Block(Stmt):
    statements: list[Stmt]


@dataclass(frozen=True)
class If(Stmt):
    conditional: Expr
    then_branch: Stmt
    else_branch: Stmt | None


@dataclass(frozen=True)
class While(Stmt):
    conditional: Expr
    body: Stmt
