from dataclasses import dataclass
import time
from typing import TYPE_CHECKING, Callable

from lox.exceptions import LoxError
from lox.expr import (
    Assign,
    Binary,
    Call,
    Expr,
    Grouping,
    Literal,
    Logical,
    Unary,
    ExprVisitor,
    Variable,
)
from lox.scanner import Token, TokenType as TT
from lox.stmt import (
    Block,
    Expression,
    Break,
    Function,
    If,
    Print,
    Stmt,
    StmtVisitor,
    Var,
    While,
)
from lox.value import LoxCallable, LoxObject, LoxValue


class LoxRuntimeError(LoxError):
    pass


class LoxArithmeticError(LoxRuntimeError):
    expr: Expr

    def __init__(self, expr: Expr, message: str):
        self.expr = expr
        super().__init__(message)


class BreakLoop(Exception):
    pass


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


@dataclass
class NativeFunction(LoxCallable):
    arity: int
    name: str
    func: Callable[..., LoxValue]

    def call(self, interpreter: "Interpreter", *args: LoxValue) -> LoxValue:
        return self.func(interpreter, *args)

    def __str__(self) -> str:
        return f"<native fn {self.name}>"


class UserFunction(LoxCallable):
    arity: int
    declaration: Function

    def __init__(self, declaration: Function) -> None:
        self.arity = len(declaration.params)
        self.declaration = declaration

    def call(self, interpreter: "Interpreter", *args: LoxValue) -> LoxValue:
        env = Environment(interpreter.globals)
        for param, arg in zip(self.declaration.params, args):
            env.define(param.lexeme, arg)
        interpreter.execute_block(self.declaration.body, env)
        return None

    def __str__(self) -> str:
        return f"<fn {self.declaration.name.lexeme}>"


class Interpreter(ExprVisitor[LoxValue], StmtVisitor[None]):
    globals: Environment
    env: Environment

    def __init__(self) -> None:
        self.globals = Environment()
        self.env = self.globals
        self.globals.define(
            "clock",
            NativeFunction(0, "clock", lambda _: time.time()),
        )

    def interpret(self, statements: list[Stmt]):
        for stmt in statements:
            stmt.accept(self)
        # return LoxObject(self.evaluate(expr))
        # try:
        #     return self.evaluate(expr)
        # except LoxRuntimeError as error:
        #     return error

    def execute_block(self, statements: list[Stmt], env: Environment):
        env_restore = self.env
        try:
            self.env = env
            self.interpret(statements)
        finally:
            self.env = env_restore

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

    def visit_while(self, stmt: While) -> None:
        # FUTURE: continue is harder to implement since we need to
        # differentiate between the body and the advancement part of the
        # header, and run the latter after a continue. We currently have no
        # mechanism to do that, so that's left as an exercise for the future.
        try:
            while LoxObject(stmt.conditional.accept(self)).is_truthy():
                stmt.body.accept(self)
        except BreakLoop:
            pass

    def visit_break(self, stmt: Break) -> None:
        raise BreakLoop()

    def visit_function(self, stmt: Function) -> None:
        self.env.define(stmt.name.lexeme, UserFunction(stmt))

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

    def visit_logical(self, expr: Logical) -> LoxValue:
        val = LoxObject(expr.left.accept(self))
        # Short circuit:
        if (val.is_truthy(), expr.operator.typ) in ((True, TT.OR), (False, TT.AND)):
            return val.val
        return expr.right.accept(self)

    def visit_call(self, expr: Call) -> LoxValue:
        callee = self.evaluate_expr(expr.callee)
        arguments: list[LoxValue] = [self.evaluate_expr(arg) for arg in expr.arguments]
        if not isinstance(callee, LoxCallable):
            raise LoxRuntimeError("Can only call functions and classes.")
        func: LoxCallable = callee
        if len(arguments) != func.arity:
            raise LoxRuntimeError(
                f"Expected {func.arity} arguments, got {len(arguments)}."
            )
        return func.call(self, *arguments)

    def check_types(self, token: Token, typ: type, message: str, *values: LoxValue):
        if not all(isinstance(val, typ) for val in values):
            raise LoxTypeError(token, message)
