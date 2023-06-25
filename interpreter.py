from dataclasses import dataclass
from typing import TYPE_CHECKING
from exceptions import LoxError
from expr import Binary, Expr, Grouping, Literal, LoxType, Unary, Visitor
from scanner import Token, TokenType as TT


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


@dataclass(frozen=True)
class LoxObject:
    val: LoxType

    def __str__(self) -> str:
        if self.val is None:
            return "nil"
        if isinstance(self.val, bool):
            return str(self.val).lower()
        if isinstance(self.val, float):
            return f"{self.val:g}"
        return self.val


class Interpreter(Visitor[LoxType]):
    def interpret(self, expr: Expr) -> LoxObject:
        return LoxObject(self.evaluate(expr))
        # try:
        #     return self.evaluate(expr)
        # except LoxRuntimeError as error:
        #     return error

    def evaluate(self, expr: Expr) -> LoxType:
        try:
            return expr.accept(self)
        except ArithmeticError as error:
            # pylint: disable=raise-missing-from
            raise LoxArithmeticError(expr, f"Error evaluating expression: {error}")

    def visit_binary(self, expr: Binary) -> LoxType:
        # FUTURE: Deal with short circuiting here before evaluation
        left = self.evaluate(expr.left)
        right = self.evaluate(expr.right)
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

    def visit_grouping(self, expr: Grouping) -> LoxType:
        return self.evaluate(expr.expr)

    def visit_literal(self, expr: Literal) -> LoxType:
        return expr.value

    def visit_unary(self, expr: Unary) -> LoxType:
        val = self.evaluate(expr.right)
        match expr.operator.typ:
            case TT.MINUS:
                self.check_types(expr.operator, float, "Operand must be a number.", val)
                if TYPE_CHECKING:
                    assert isinstance(val, float)
                return -val
            case TT.BANG:
                return not self.is_truthy(val)
        raise LoxTypeError(expr.operator, "Unexpected unary operation.")

    def is_truthy(self, val: LoxType) -> bool:
        if val is None or val is False:
            return False
        return True

    def check_types(self, token: Token, typ: type, message: str, *values: LoxType):
        if not all(isinstance(val, typ) for val in values):
            raise LoxTypeError(token, message)
