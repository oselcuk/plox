from typing import Sequence
from lox.expr import Binary, Expr, Grouping, Literal, Unary, ExprVisitor
from lox.stmt import Expression, Print, Stmt, StmtVisitor


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

    def visit_expression(self, expr: Expression) -> str:
        return f"({expr.expr.accept(self)} ; )"

    def visit_print(self, expr: Print) -> str:
        return self.parenthesize("print", expr.expr)
