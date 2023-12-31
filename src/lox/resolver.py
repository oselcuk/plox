from enum import auto, Enum
from lox.expr import (
    Assign,
    Binary,
    Call,
    Expr,
    ExprVisitor,
    Get,
    Grouping,
    Literal,
    Logical,
    Set,
    Super,
    This,
    Unary,
    Variable,
)
from lox.interpreter import Interpreter
from lox.scanner import Token, TokenType
from lox.stmt import (
    Block,
    Break,
    Class,
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


class FunctionType(Enum):
    NONE = auto()
    FUNCTION = auto()
    INITIALIZER = auto()
    METHOD = auto()


class ClassType(Enum):
    NONE = auto()
    CLASS = auto()
    SUBCLASS = auto()


class VariableState(Enum):
    NONE = auto()
    DECLARED = auto()
    DEFINED = auto()
    USED = auto()


class Resolver(ExprVisitor[None], StmtVisitor[None]):
    interpreter: Interpreter
    scopes: list[dict[str, VariableState]]
    current_function: FunctionType = FunctionType.NONE
    current_class: ClassType = ClassType.NONE
    loop_depth: int = 0

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
        if stmt.initializer is not None:
            self.resolve(stmt.initializer)
            self.define(stmt.name)

    def visit_variable(self, expr: Variable) -> None:
        self.resolve_local(expr, expr.name, True)

    def resolve_local(self, expr: Expr, name: Token, read: bool):
        """
        Resolve local variable. `read` should be True if the variable is being read,
        False otherwise.
        """
        for steps, scope in enumerate(reversed(self.scopes)):
            if (state := scope.get(name.lexeme, None)) is not None:
                match (read, state):
                    case (True, VariableState.DECLARED):
                        self.error(name, "Variable used before assignment.")
                    case (True, VariableState.DEFINED):
                        scope[name.lexeme] = VariableState.USED
                    case (False, VariableState.DECLARED):
                        scope[name.lexeme] = VariableState.DEFINED

                self.interpreter.resolve(expr, steps)
                break

    def visit_assign(self, expr: Assign) -> None:
        self.resolve(expr.value)
        self.resolve_local(expr, expr.name, False)

    def visit_function(self, stmt: Function) -> None:
        self.declare(stmt.name)
        self.define(stmt.name)

        self.resolve_function(stmt, FunctionType.FUNCTION)

    def visit_class(self, stmt: Class) -> None:
        enclosing_class = self.current_class
        self.current_class = ClassType.CLASS
        self.declare(stmt.name)
        self.define(stmt.name)
        if stmt.superclass:
            self.current_class = ClassType.SUBCLASS
            if stmt.superclass.name.lexeme == stmt.name.lexeme:
                self.error(stmt.superclass.name, "A class can't inherit from itself.")
            self.resolve(stmt.superclass)
            self.begin_scope()
            self.scopes[-1]["super"] = VariableState.USED
        self.begin_scope()
        self.scopes[-1]["this"] = VariableState.USED
        for method in stmt.methods:
            declaration = FunctionType.METHOD
            if method.name.lexeme == "init":
                declaration = FunctionType.INITIALIZER
            self.resolve_function(method, declaration)
        self.end_scope()
        if stmt.superclass:
            self.end_scope()
        self.current_class = enclosing_class

    def resolve_function(self, func: Function, typ: FunctionType):
        restore = self.current_function
        self.current_function = typ
        loop_depth = self.loop_depth
        self.loop_depth = 0
        self.begin_scope()
        for param in func.params:
            self.declare(param)
            self.define(param)
        self.resolve_statements(func.body)
        self.end_scope()
        self.loop_depth = loop_depth
        self.current_function = restore

    def declare(self, name: Token):
        if self.scopes:
            if name.lexeme in self.scopes[-1]:
                self.error(name, "Variable already declared in this scope.")
            self.scopes[-1][name.lexeme] = VariableState.DECLARED

    def define(self, name: Token):
        if self.scopes:
            self.scopes[-1][name.lexeme] = VariableState.DEFINED

    def resolve(self, node: Stmt | Expr | None):
        if node is not None:
            node.accept(self)

    def begin_scope(self):
        self.scopes.append({})

    def end_scope(self):
        for variable, state in self.scopes.pop().items():
            if state in (VariableState.DECLARED, VariableState.DEFINED):
                # FUTURE: Store identifier token and frame number for variables.
                token = Token(TokenType.IDENTIFIER, variable, None, 0)
                self.error(token, f"Unused variable {variable}.")

    def visit_expression(self, stmt: Expression) -> None:
        self.resolve(stmt.expr)

    def visit_if(self, stmt: If) -> None:
        self.resolve(stmt.conditional)
        self.resolve(stmt.then_branch)
        self.resolve(stmt.else_branch)

    def visit_print(self, stmt: Print) -> None:
        self.resolve(stmt.expr)

    def visit_return(self, stmt: Return) -> None:
        if self.current_function == FunctionType.NONE:
            self.error(stmt.keyword, "Can't return from top-level code.")
        if stmt.value is not None:
            if self.current_function == FunctionType.INITIALIZER and stmt.value:
                self.error(stmt.keyword, "Can't return from init method.")
            self.resolve(stmt.value)

    def visit_while(self, stmt: While) -> None:
        self.resolve(stmt.conditional)
        self.loop_depth += 1
        self.resolve(stmt.body)
        self.loop_depth -= 1

    def visit_binary(self, expr: Binary) -> None:
        self.resolve(expr.left)
        self.resolve(expr.right)

    def visit_call(self, expr: Call) -> None:
        self.resolve(expr.callee)
        for arg in expr.arguments:
            self.resolve(arg)

    def visit_get(self, expr: Get) -> None:
        self.resolve(expr.object)

    def visit_set(self, expr: Set) -> None:
        self.resolve(expr.value)
        self.resolve(expr.object)

    def visit_super(self, expr: Super) -> None:
        if self.current_class == ClassType.NONE:
            self.error(expr.keyword, "Can't use 'super' outside of a class.")
        elif self.current_class == ClassType.CLASS:
            self.error(expr.keyword, "Can't use 'super' in a class with no superclass.")
        self.resolve_local(expr, expr.keyword, True)

    def visit_this(self, expr: This) -> None:
        if self.current_class == ClassType.NONE:
            self.error(expr.keyword, "Can't use 'this' outside of a class.")
        self.resolve_local(expr, expr.keyword, True)

    def visit_grouping(self, expr: Grouping) -> None:
        self.resolve(expr.expr)

    def visit_literal(self, expr: Literal) -> None:
        pass

    def visit_break(self, stmt: Break) -> None:
        if self.loop_depth == 0:
            self.error(stmt.keyword, "Can't break outside of loop.")

    def visit_logical(self, expr: Logical) -> None:
        self.resolve(expr.left)
        self.resolve(expr.right)

    def visit_unary(self, expr: Unary) -> None:
        self.resolve(expr.right)

    def error(self, token: Token, message: str):
        self.interpreter.lox_instance.error(token, message)
