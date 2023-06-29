from dataclasses import dataclass
from typing import Protocol, TypeVar

from lox import scanner
from lox import value


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

    def visit_logical(self, expr: "Logical") -> T_co:
        ...

    def visit_call(self, expr: "Call") -> T_co:
        ...

    def visit_get(self, expr: "Get") -> T_co:
        ...

    def visit_set(self, expr: "Set") -> T_co:
        ...

    def visit_this(self, expr: "This") -> T_co:
        ...


@dataclass(frozen=True, eq=False)
class Expr:
    def accept(self, visitor: ExprVisitor[T_co]) -> T_co:
        visit_method = f"visit_{self.__class__.__name__.lower()}"
        if (visit := getattr(visitor, visit_method, False)) and callable(visit):
            return visit(self)
        raise NotImplementedError(f"{visit_method} not implemented on {visitor}")


@dataclass(frozen=True, eq=False)
class Binary(Expr):
    left: Expr
    operator: scanner.Token
    right: Expr


@dataclass(frozen=True, eq=False)
class Grouping(Expr):
    expr: Expr


@dataclass(frozen=True, eq=False)
class Literal(Expr):
    value: value.LoxValue


@dataclass(frozen=True, eq=False)
class Unary(Expr):
    operator: scanner.Token
    right: Expr


@dataclass(frozen=True, eq=False)
class Variable(Expr):
    name: scanner.Token


@dataclass(frozen=True, eq=False)
class Assign(Expr):
    name: scanner.Token
    value: Expr


@dataclass(frozen=True, eq=False)
class Logical(Expr):
    left: Expr
    operator: scanner.Token
    right: Expr


@dataclass(frozen=True, eq=False)
class Call(Expr):
    callee: Expr
    paren: scanner.Token
    arguments: list[Expr]


@dataclass(frozen=True, eq=False)
class Get(Expr):
    object: Expr
    name: scanner.Token


@dataclass(frozen=True, eq=False)
class Set(Expr):
    object: Expr
    name: scanner.Token
    value: Expr


@dataclass(frozen=True, eq=False)
class This(Expr):
    keyword: scanner.Token
