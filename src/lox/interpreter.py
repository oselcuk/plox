from dataclasses import dataclass
import time
from typing import TYPE_CHECKING, Callable

import lox.exceptions
import lox.expr
import lox.lox
import lox.scanner
import lox.stmt
import lox.value


class LoxRuntimeError(lox.exceptions.LoxError):
    token: lox.scanner.Token

    def __init__(self, token: lox.scanner.Token, message: str) -> None:
        self.token = token
        super().__init__(f"{token}: {message}")


class LoxArithmeticError(LoxRuntimeError):
    pass


class LoxTypeError(LoxRuntimeError):
    pass


class BreakLoop(Exception):
    pass


@dataclass
class ReturnException(Exception):
    val: lox.value.LoxValue


class Environment:
    env: dict[str, lox.value.LoxValue]
    parent: "Environment | None"

    def __init__(self, parent: "Environment | None" = None) -> None:
        self.env = {}
        self.parent = parent

    def define(self, name: lox.scanner.Token, value: lox.value.LoxValue):
        if name.lexeme in self.env:
            raise LoxRuntimeError(name, "Cannot redeclare variable.")
        self.env[name.lexeme] = value

    def ancestor(self, steps: int) -> "Environment | None":
        env: Environment = self
        for _ in range(steps):
            if env.parent is None:
                return None
            env = env.parent
        return env

    def set(self, name: lox.scanner.Token, value: lox.value.LoxValue):
        if name.lexeme in self.env:
            self.env[name.lexeme] = value
        elif self.parent is not None:
            self.parent.set(name, value)
        else:
            raise LoxRuntimeError(name, "Undefined variable.")

    def set_at(self, name: lox.scanner.Token, value: lox.value.LoxValue, steps: int):
        env = self.ancestor(steps)
        if env is None or name.lexeme not in env.env:
            raise LoxRuntimeError(name, "Failed variable resolution.")
        env.env[name.lexeme] = value

    def get(self, name: lox.scanner.Token) -> lox.value.LoxValue:
        if name.lexeme in self.env:
            return self.env[name.lexeme]
        if self.parent is not None:
            return self.parent.get(name)
        else:
            raise LoxRuntimeError(name, "Undefined variable.")

    def get_at(self, name: lox.scanner.Token, steps: int) -> lox.value.LoxValue:
        env = self.ancestor(steps)
        if env is None or name.lexeme not in env.env:
            raise LoxRuntimeError(name, "Failed variable resolution.")
        return env.env[name.lexeme]

    def __str__(self) -> str:
        stacks: list[str] = []
        env: Environment | None = self
        for i in range(100):
            if env is None:
                break
            stacks.append(f"Frame {i:2}: {set(env.env.keys())}")
            env = env.parent
        return "\n".join(stacks)


@dataclass
class NativeFunction(lox.value.LoxCallable):
    arity: int
    name: str
    func: Callable[..., lox.value.LoxValue]

    def call(
        self, intr: "Interpreter", args: list[lox.value.LoxValue]
    ) -> lox.value.LoxValue:
        return self.func(intr, *args)

    def __str__(self) -> str:
        return f"<native fn {self.name}>"


class UserFunction(lox.value.LoxCallable):
    arity: int
    declaration: lox.stmt.Function
    closure: Environment
    is_initializer: bool

    def __init__(
        self,
        declaration: lox.stmt.Function,
        closure: Environment,
        is_initializer: bool = False,
    ) -> None:
        self.arity = len(declaration.params)
        self.declaration = declaration
        self.closure = closure
        self.is_initializer = is_initializer

    def call(
        self, intr: "Interpreter", args: list[lox.value.LoxValue]
    ) -> lox.value.LoxValue:
        env = Environment(self.closure)
        for param, arg in zip(self.declaration.params, args):
            env.define(param, arg)
        try:
            intr.execute_block(self.declaration.body, env)
        except ReturnException as ret:
            if self.is_initializer:
                return self.closure.env["this"]
            return ret.val
        if self.is_initializer:
            return self.closure.env["this"]
        return None

    def bind(self, instance: lox.value.LoxInstance) -> "UserFunction":
        env = Environment(self.closure)
        env.env["this"] = instance
        return UserFunction(self.declaration, env, self.is_initializer)

    def __str__(self) -> str:
        return f"<fn {self.declaration.name.lexeme}>"


class Interpreter(lox.expr.ExprVisitor[lox.value.LoxValue], lox.stmt.StmtVisitor[None]):
    lox_instance: "lox.lox.Lox"
    globals: Environment
    env: Environment
    locals: dict[lox.expr.Expr, int]

    def __init__(self, lox_instance: "lox.lox.Lox") -> None:
        self.lox_instance = lox_instance
        self.globals = Environment()
        self.env = self.globals
        self.locals = {}
        self.globals.define(
            lox.scanner.Token(lox.scanner.TokenType.IDENTIFIER, "clock", None, 0),
            NativeFunction(0, "clock", lambda _: time.time()),
        )

    def resolve(self, expr: lox.expr.Expr, steps: int):
        self.locals[expr] = steps

    def interpret(self, statements: list[lox.stmt.Stmt]):
        try:
            for stmt in statements:
                stmt.accept(self)
        except LoxRuntimeError as error:
            self.lox_instance.runtime_error(error)

    def execute_block(self, statements: list[lox.stmt.Stmt], env: Environment):
        env_restore = self.env
        try:
            self.env = env
            self.interpret(statements)
        finally:
            self.env = env_restore

    def visit_expression(self, stmt: lox.stmt.Expression) -> None:
        self.evaluate_expr(stmt.expr)

    def visit_print(self, stmt: lox.stmt.Print) -> None:
        val = self.evaluate_expr(stmt.expr)
        print(lox.value.LoxObject(val))

    def visit_var(self, stmt: lox.stmt.Var) -> None:
        val: lox.value.LoxValue = None
        if stmt.initializer:
            val = self.evaluate_expr(stmt.initializer)
        self.env.define(stmt.name, val)

    def visit_block(self, stmt: lox.stmt.Block) -> None:
        try:
            self.env = Environment(self.env)
            for statement in stmt.statements:
                statement.accept(self)
        finally:
            assert self.env.parent is not None
            self.env = self.env.parent

    def visit_if(self, stmt: lox.stmt.If) -> None:
        if lox.value.LoxObject(stmt.conditional.accept(self)).is_truthy():
            stmt.then_branch.accept(self)
        elif stmt.else_branch:
            stmt.else_branch.accept(self)

    def visit_while(self, stmt: lox.stmt.While) -> None:
        # FUTURE: continue is harder to implement since we need to
        # differentiate between the body and the advancement part of the
        # header, and run the latter after a continue. We currently have no
        # mechanism to do that, so that's left as an exercise for the future.
        try:
            while lox.value.LoxObject(stmt.conditional.accept(self)).is_truthy():
                stmt.body.accept(self)
        except BreakLoop:
            pass

    def visit_break(self, stmt: lox.stmt.Break) -> None:
        raise BreakLoop()

    def visit_function(self, stmt: lox.stmt.Function) -> None:
        self.env.define(stmt.name, UserFunction(stmt, self.env))

    def visit_class(self, stmt: lox.stmt.Class) -> None:
        self.env.define(stmt.name, None)
        closure = self.env
        superclass: lox.value.LoxClass | None = None
        if stmt.superclass:
            val = stmt.superclass.accept(self)
            if not isinstance(val, lox.value.LoxClass):
                raise LoxRuntimeError(
                    stmt.superclass.name, "Superclass must refer to a class."
                )
            superclass = val
            closure = Environment(closure)
            closure.env["super"] = superclass
        methods: dict[str, UserFunction] = {}
        for method in stmt.methods:
            methods[method.name.lexeme] = UserFunction(
                method, closure, method.name.lexeme == "init"
            )
        klass = lox.value.LoxClass(stmt.name.lexeme, superclass, methods)
        self.env.set(stmt.name, klass)

    def visit_return(self, stmt: lox.stmt.Return) -> None:
        raise ReturnException(stmt.value.accept(self) if stmt.value else None)

    def evaluate_expr(self, expr: lox.expr.Expr) -> lox.value.LoxValue:
        return expr.accept(self)

    def visit_binary(self, expr: lox.expr.Binary) -> lox.value.LoxValue:
        left = self.evaluate_expr(expr.left)
        right = self.evaluate_expr(expr.right)
        # Deal with equality, which has no type constraints.
        match expr.operator.typ:
            case lox.scanner.TokenType.EQUAL_EQUAL:
                return left == right
            case lox.scanner.TokenType.BANG_EQUAL:
                return left != right
        type_check_message = "Operands must be two numbers."
        # Deal with string addition, a special case.
        # We deal with the correct case of adding two strings, then assert
        # that both arguments are numbers if they're not both strings.
        if expr.operator.typ == lox.scanner.TokenType.PLUS:
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
        try:
            match expr.operator.typ:
                case lox.scanner.TokenType.PLUS:
                    return left + right
                case lox.scanner.TokenType.MINUS:
                    return left - right
                case lox.scanner.TokenType.SLASH:
                    return left / right
                case lox.scanner.TokenType.STAR:
                    return left * right
                case lox.scanner.TokenType.GREATER:
                    return left > right
                case lox.scanner.TokenType.GREATER_EQUAL:
                    return left >= right
                case lox.scanner.TokenType.LESS:
                    return left < right
                case lox.scanner.TokenType.LESS_EQUAL:
                    return left <= right
        except ArithmeticError as error:
            # pylint: disable=raise-missing-from
            raise LoxArithmeticError(expr.operator, str(error))
        raise LoxTypeError(expr.operator, "Unexpected binary operation.")

    def visit_grouping(self, expr: lox.expr.Grouping) -> lox.value.LoxValue:
        return self.evaluate_expr(expr.expr)

    def visit_literal(self, expr: lox.expr.Literal) -> lox.value.LoxValue:
        return expr.value

    def visit_unary(self, expr: lox.expr.Unary) -> lox.value.LoxValue:
        val = self.evaluate_expr(expr.right)
        match expr.operator.typ:
            case lox.scanner.TokenType.MINUS:
                self.check_types(expr.operator, float, "Operand must be a number.", val)
                if TYPE_CHECKING:
                    assert isinstance(val, float)
                return -val
            case lox.scanner.TokenType.BANG:
                return not lox.value.LoxObject(val).is_truthy()
        raise LoxTypeError(expr.operator, "Unexpected unary operation.")

    def visit_variable(self, expr: lox.expr.Variable) -> lox.value.LoxValue:
        return self.lookup_variable(expr.name, expr)

    def visit_assign(self, expr: lox.expr.Assign) -> lox.value.LoxValue:
        val = expr.value.accept(self)
        steps = self.locals.get(expr, None)
        if steps is None:
            self.globals.set(expr.name, val)
        else:
            self.env.set_at(expr.name, val, steps)
        return val

    def lookup_variable(
        self, name: lox.scanner.Token, expr: lox.expr.Expr
    ) -> lox.value.LoxValue:
        steps = self.locals.get(expr, None)
        if steps is None:
            return self.globals.get(name)
        else:
            return self.env.get_at(name, steps)

    def visit_logical(self, expr: lox.expr.Logical) -> lox.value.LoxValue:
        val = lox.value.LoxObject(expr.left.accept(self))
        # Short circuit:
        if (val.is_truthy(), expr.operator.typ) in (
            (True, lox.scanner.TokenType.OR),
            (False, lox.scanner.TokenType.AND),
        ):
            return val.val
        return expr.right.accept(self)

    def visit_call(self, expr: lox.expr.Call) -> lox.value.LoxValue:
        callee = self.evaluate_expr(expr.callee)
        arguments: list[lox.value.LoxValue] = [
            self.evaluate_expr(arg) for arg in expr.arguments
        ]
        if not isinstance(callee, lox.value.LoxCallable):
            raise LoxRuntimeError(expr.paren, "Can only call functions and classes.")
        func: lox.value.LoxCallable = callee
        if len(arguments) != func.arity:
            raise LoxRuntimeError(
                expr.paren, f"Expected {func.arity} arguments, got {len(arguments)}."
            )
        return func.call(self, arguments)

    def visit_get(self, expr: lox.expr.Get) -> lox.value.LoxValue:
        obj = expr.object.accept(self)
        if not isinstance(obj, lox.value.LoxInstance):
            raise LoxRuntimeError(expr.name, "Only instances have properties.")
        return obj.get(expr.name)

    def visit_set(self, expr: lox.expr.Set) -> lox.value.LoxValue:
        obj = expr.object.accept(self)
        if not isinstance(obj, lox.value.LoxInstance):
            raise LoxRuntimeError(expr.name, "Only instances have properties.")
        value = expr.value.accept(self)
        obj.set(expr.name, value)
        return value

    def visit_super(self, expr: lox.expr.Super) -> lox.value.LoxValue:
        steps = self.locals[expr]
        superclass = self.env.get_at(expr.keyword, steps)
        if not isinstance(superclass, lox.value.LoxClass):
            raise LoxRuntimeError(expr.keyword, "Superclass should be a class.")
        this_token = lox.scanner.Token(lox.scanner.TokenType.THIS, "this", None, 0)
        this = self.env.get_at(this_token, steps - 1)
        if not isinstance(this, lox.value.LoxInstance):
            raise LoxRuntimeError(expr.keyword, "Didn't find the expected 'this'.")
        method = superclass.get_method(expr.method.lexeme)
        if not method:
            raise LoxRuntimeError(
                expr.method,
                f"Method {expr.method.lexeme} not found on {superclass.name}.",
            )
        return method.bind(this)

    def visit_this(self, expr: lox.expr.This) -> lox.value.LoxValue:
        return self.lookup_variable(expr.keyword, expr)

    def check_types(
        self,
        token: lox.scanner.Token,
        typ: type,
        message: str,
        *values: lox.value.LoxValue,
    ):
        if not all(isinstance(val, typ) for val in values):
            raise LoxTypeError(token, message)
