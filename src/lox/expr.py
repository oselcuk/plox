from dataclasses import dataclass
from typing import Protocol, TypeVar

from lox.scanner import Token
from lox.value import LoxValue


T_co = TypeVar("T_co", covariant=True)


class ExprVisitor(Protocol[T_co]):
    def visit_binary(self, expr: "Binary") -> T_co:
        ...

    def visit_grouping(self, expr: "Grouping") -> T_co:
        ...

    def visit_literal(self, expr: "Literal") -> T_co:
        ...

    def visit_unary(self, expr: "Unary") -> T_co:
        ...

    def visit_variable(self, expr: "Variable") -> T_co:
        ...

    def visit_assign(self, expr: "Assign") -> T_co:
        ...


@dataclass(frozen=True)
class Expr:
    def accept(self, visitor: ExprVisitor[T_co]) -> T_co:
        visit_method = f"visit_{self.__class__.__name__.lower()}"
        if (visit := getattr(visitor, visit_method, False)) and callable(visit):
            return visit(self)
        raise NotImplementedError(f"{visit_method} not implemented on {visitor}")


@dataclass(frozen=True)
class Binary(Expr):
    left: Expr
    operator: Token
    right: Expr


@dataclass(frozen=True)
class Grouping(Expr):
    expr: Expr


@dataclass(frozen=True)
class Literal(Expr):
    value: LoxValue


@dataclass(frozen=True)
class Unary(Expr):
    operator: Token
    right: Expr


@dataclass(frozen=True)
class Variable(Expr):
    name: Token


@dataclass(frozen=True)
class Assign(Expr):
    name: Token
    value: Expr
