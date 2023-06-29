from lox.expr import (
    Assign,
    Binary,
    Call,
    Expr,
    ExprVisitor,
    Grouping,
    Literal,
    Logical,
    Unary,
    Variable,
)
from lox.interpreter import Interpreter
from lox.scanner import Token
from lox.stmt import (
    Block,
    Break,
    Expression,
    Function,
    If,
    Print,
    Return,
    Stmt,
    StmtVisitor,
    Var,
    While,
)


class Resolver(ExprVisitor[None], StmtVisitor[None]):
    interpreter: Interpreter
    scopes: list[dict[str, bool]]

    def __init__(self, interpreter: Interpreter):
        self.interpreter = interpreter
        self.scopes = []

    def visit_block(self, stmt: Block) -> None:
        self.begin_scope()
        self.resolve_statements(stmt.statements)
        self.end_scope()

    def resolve_statements(self, statements: list[Stmt]) -> None:
        for statement in statements:
            self.resolve(statement)

    def visit_var(self, stmt: Var) -> None:
        self.declare(stmt.name)
        self.resolve(stmt.initializer)
        self.define(stmt.name)

    def visit_variable(self, expr: Variable) -> None:
        if self.scopes and not self.scopes[-1].get(expr.name.lexeme, True):
            self.error(expr.name, "Can't read local variable in its own initializer.")
        self.resolve_local(expr, expr.name)

    def resolve_local(self, expr: Expr, name: Token):
        for steps, scope in enumerate(reversed(self.scopes)):
            if name.lexeme in scope:
                print(f"Resolved {expr} at {steps}")
                self.interpreter.resolve(expr, steps)
                break

    def visit_assign(self, expr: Assign) -> None:
        self.resolve(expr.value)
        self.resolve_local(expr, expr.name)

    def visit_function(self, stmt: Function) -> None:
        self.declare(stmt.name)
        self.define(stmt.name)

        self.resolve_function(stmt)

    def resolve_function(self, func: Function):
        self.begin_scope()
        for param in func.params:
            self.declare(param)
            self.define(param)
        self.resolve_statements(func.body)
        self.end_scope()

    def declare(self, name: Token):
        if self.scopes:
            self.scopes[-1][name.lexeme] = False

    def define(self, name: Token):
        if self.scopes:
            self.scopes[-1][name.lexeme] = True

    def resolve(self, node: Stmt | Expr | None):
        if node is not None:
            node.accept(self)

    def begin_scope(self):
        self.scopes.append({})

    def end_scope(self):
        self.scopes.pop()

    def visit_expression(self, stmt: Expression) -> None:
        self.resolve(stmt.expr)

    def visit_if(self, stmt: If) -> None:
        self.resolve(stmt.conditional)
        self.resolve(stmt.then_branch)
        self.resolve(stmt.else_branch)

    def visit_print(self, stmt: Print) -> None:
        self.resolve(stmt.expr)

    def visit_return(self, stmt: Return) -> None:
        self.resolve(stmt.value)

    def visit_while(self, stmt: While) -> None:
        self.resolve(stmt.conditional)
        self.resolve(stmt.body)

    def visit_binary(self, expr: Binary) -> None:
        self.resolve(expr.left)
        self.resolve(expr.right)

    def visit_call(self, expr: Call) -> None:
        self.resolve(expr.callee)
        for arg in expr.arguments:
            self.resolve(arg)

    def visit_grouping(self, expr: Grouping) -> None:
        self.resolve(expr.expr)

    def visit_literal(self, expr: Literal) -> None:
        pass

    def visit_break(self, stmt: Break) -> None:
        pass

    def visit_logical(self, expr: Logical) -> None:
        self.resolve(expr.left)
        self.resolve(expr.right)

    def visit_unary(self, expr: Unary) -> None:
        self.resolve(expr.right)

    def error(self, token: Token, message: str):
        self.interpreter.lox_instance.error(token, message)
