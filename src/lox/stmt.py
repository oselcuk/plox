from dataclasses import dataclass
from typing import Protocol, TypeVar

from lox.expr import Expr


T_co = TypeVar("T_co", covariant=True)


class StmtVisitor(Protocol[T_co]):
    def visit_expression(self, expr: "Expression") -> T_co:
        ...

    def visit_print(self, expr: "Print") -> T_co:
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
