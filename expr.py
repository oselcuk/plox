from abc import ABC, abstractmethod
from dataclasses import dataclass
from typing import Protocol, TypeVar
from scanner import Token


T_co = TypeVar("T_co", covariant=True)


class Visitor(Protocol[T_co]):
    def visit_binary(self, expr: "Binary") -> T_co:
        ...

    def visit_grouping(self, expr: "Grouping") -> T_co:
        ...

    def visit_literal(self, expr: "Literal") -> T_co:
        ...

    def visit_unary(self, expr: "Unary") -> T_co:
        ...


LoxType = None | bool | float | str


class Expr(ABC):
    @abstractmethod
    def accept(self, visitor: Visitor[T_co]) -> T_co:
        raise NotImplementedError


@dataclass(frozen=True)
class Binary(Expr):
    left: Expr
    operator: Token
    right: Expr

    def accept(self, visitor: Visitor[T_co]) -> T_co:
        return visitor.visit_binary(self)


@dataclass(frozen=True)
class Grouping(Expr):
    expr: Expr

    def accept(self, visitor: Visitor[T_co]) -> T_co:
        return visitor.visit_grouping(self)


@dataclass(frozen=True)
class Literal(Expr):
    value: LoxType

    def accept(self, visitor: Visitor[T_co]) -> T_co:
        return visitor.visit_literal(self)


@dataclass(frozen=True)
class Unary(Expr):
    operator: Token
    right: Expr

    def accept(self, visitor: Visitor[T_co]) -> T_co:
        return visitor.visit_unary(self)
