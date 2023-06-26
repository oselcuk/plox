from typing import TYPE_CHECKING

from lox.exceptions import LoxError
from lox.expr import (
    Assign,
    Binary,
    Expr,
    Grouping,
    Literal,
    Unary,
    ExprVisitor,
    Variable,
)
from lox.scanner import Token, TokenType as TT
from lox.stmt import Block, Expression, If, Print, Stmt, StmtVisitor, Var
from lox.value import LoxObject, LoxValue


class LoxRuntimeError(LoxError):
    pass


class LoxArithmeticError(LoxRuntimeError):
    expr: Expr

    def __init__(self, expr: Expr, message: str):
        self.expr = expr
        super().__init__(message)


class LoxTypeError(LoxRuntimeError):
    token: Token

    def __init__(self, token: Token, message: str):
        self.token = token
        super().__init__(f"[line {token.line}]: {message}")


class Environment:
    env: dict[str, LoxValue]
    parent: "Environment | None"

    def __init__(self, parent: "Environment | None" = None) -> None:
        self.env = {}
        self.parent = parent

    def define(self, name: str, value: LoxValue):
        if name in self.env:
            raise LoxRuntimeError(f"Cannot redeclare variable {name}.")
        self.env[name] = value

    def set(self, name: str, value: LoxValue):
        if name in self.env:
            self.env[name] = value
        elif self.parent is not None:
            self.parent.set(name, value)
        else:
            raise LoxRuntimeError(f"Undefined variable {name}.")

    def get(self, name: str) -> LoxValue:
        if name in self.env:
            return self.env[name]
        if self.parent is not None:
            return self.parent.get(name)
        else:
            raise LoxRuntimeError(f"Undefined variable {name}.")


class Interpreter(ExprVisitor[LoxValue], StmtVisitor[None]):
    env: Environment

    def __init__(self) -> None:
        self.env = Environment()

    def interpret(self, statements: list[Stmt]):
        for stmt in statements:
            stmt.accept(self)
        # return LoxObject(self.evaluate(expr))
        # try:
        #     return self.evaluate(expr)
        # except LoxRuntimeError as error:
        #     return error

    def visit_expression(self, stmt: Expression) -> None:
        self.evaluate_expr(stmt.expr)

    def visit_print(self, stmt: Print) -> None:
        val = self.evaluate_expr(stmt.expr)
        print(LoxObject(val))

    def visit_var(self, stmt: Var) -> None:
        name = stmt.name.lexeme
        val: LoxValue = None
        if stmt.initializer:
            val = self.evaluate_expr(stmt.initializer)
        self.env.define(name, val)

    def visit_block(self, stmt: Block) -> None:
        try:
            self.env = Environment(self.env)
            for statement in stmt.statements:
                statement.accept(self)
        finally:
            assert self.env.parent is not None
            self.env = self.env.parent

    def visit_if(self, stmt: If) -> None:
        if LoxObject(stmt.conditional.accept(self)).is_truthy():
            stmt.then_branch.accept(self)
        elif stmt.else_branch:
            stmt.else_branch.accept(self)

    def evaluate_expr(self, expr: Expr) -> LoxValue:
        try:
            return expr.accept(self)
        except ArithmeticError as error:
            # pylint: disable=raise-missing-from
            raise LoxArithmeticError(expr, f"Error evaluating expression: {error}")

    def visit_binary(self, expr: Binary) -> LoxValue:
        # FUTURE: Deal with short circuiting here before evaluation
        left = self.evaluate_expr(expr.left)
        right = self.evaluate_expr(expr.right)
        # Deal with equality, which has no type constraints.
        match expr.operator.typ:
            case TT.EQUAL_EQUAL:
                return left == right
            case TT.BANG_EQUAL:
                return left != right
        type_check_message = "Operands must be two numbers."
        # Deal with string addition, a special case.
        # We deal with the correct case of adding two strings, then assert
        # that both arguments are numbers if they're not both strings.
        if expr.operator.typ == TT.PLUS:
            if isinstance(left, str) and isinstance(right, str):
                return left + right
            type_check_message = "Operands must be two numbers or two strings"
        self.check_types(
            expr.operator,
            float,
            type_check_message,
            left,
            right,
        )
        if TYPE_CHECKING:
            assert isinstance(left, float) and isinstance(right, float)
        match expr.operator.typ:
            case TT.PLUS:
                return left + right
            case TT.MINUS:
                return left - right
            case TT.SLASH:
                return left / right
            case TT.STAR:
                return left * right
            case TT.GREATER:
                return left > right
            case TT.GREATER_EQUAL:
                return left >= right
            case TT.LESS:
                return left < right
            case TT.LESS_EQUAL:
                return left <= right
        raise LoxTypeError(expr.operator, "Unexpected binary operation.")

    def visit_grouping(self, expr: Grouping) -> LoxValue:
        return self.evaluate_expr(expr.expr)

    def visit_literal(self, expr: Literal) -> LoxValue:
        return expr.value

    def visit_unary(self, expr: Unary) -> LoxValue:
        val = self.evaluate_expr(expr.right)
        match expr.operator.typ:
            case TT.MINUS:
                self.check_types(expr.operator, float, "Operand must be a number.", val)
                if TYPE_CHECKING:
                    assert isinstance(val, float)
                return -val
            case TT.BANG:
                return not LoxObject(val).is_truthy()
        raise LoxTypeError(expr.operator, "Unexpected unary operation.")

    def visit_variable(self, expr: Variable) -> LoxValue:
        return self.env.get(expr.name.lexeme)

    def visit_assign(self, expr: Assign) -> LoxValue:
        val = expr.value.accept(self)
        self.env.set(expr.name.lexeme, val)
        return val

    def check_types(self, token: Token, typ: type, message: str, *values: LoxValue):
        if not all(isinstance(val, typ) for val in values):
            raise LoxTypeError(token, message)
