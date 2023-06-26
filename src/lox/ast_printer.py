from typing import Sequence
from lox.expr import (
    Binary,
    Expr,
    Grouping,
    Literal,
    Unary,
    ExprVisitor,
    Variable,
    Assign,
)
from lox.stmt import Block, Expression, Print, Stmt, StmtVisitor, Var


class AstPrinter(ExprVisitor[str], StmtVisitor[str]):
    def print(self, ast: Sequence[Expr | Stmt]) -> str:
        return "\n".join(node.accept(self) for node in ast)

    def parenthesize(self, name: str, *exprs: Expr) -> str:
        args_list = " ".join(expr.accept(self) for expr in exprs)
        return f"( {name} {args_list} )"

    def visit_binary(self, expr: Binary) -> str:
        return self.parenthesize(expr.operator.lexeme, expr.left, expr.right)

    def visit_grouping(self, expr: Grouping) -> str:
        return self.parenthesize("group", expr.expr)

    def visit_literal(self, expr: Literal) -> str:
        if expr.value is None:
            return "nil"
        return str(expr.value)

    def visit_unary(self, expr: Unary) -> str:
        return self.parenthesize(expr.operator.lexeme, expr.right)

    def visit_variable(self, expr: Variable) -> str:
        return f"( variable {expr.name} )"

    def visit_assign(self, expr: Assign) -> str:
        return f"( assign {expr.name.lexeme} {expr.value.accept(self)})"

    def visit_expression(self, stmt: Expression) -> str:
        return f"( {stmt.expr.accept(self)} ; )"

    def visit_print(self, stmt: Print) -> str:
        return self.parenthesize("print", stmt.expr)

    def visit_var(self, stmt: Var) -> str:
        val = "{uninitialized}"
        if stmt.initializer:
            val = stmt.initializer.accept(self)
        return f"( var {stmt.name.lexeme} {val} )"

    def visit_block(self, stmt: Block) -> str:
        val = " ; ".join(s.accept(self) for s in stmt.statements)
        return f"{{ {val} }}"
