from typing import Callable, Optional

from lox.exceptions import LoxError
from lox.expr import Binary, Expr, Grouping, Literal, Unary
from lox.scanner import Token
from lox.scanner import TokenType as TT
from lox.stmt import Expression, Print, Stmt

ErrorReporterType = Callable[[int, str, str], None]


class LoxParseError(LoxError):
    def __init__(self, tok: Token, message: str):
        self.tok = tok
        super().__init__(f"[line {tok.line}]: {message}")


class Parser:
    tokens: list[Token]
    current: int = 0

    def __init__(self, tokens: list[Token]) -> None:
        self.tokens = tokens

    def parse(self) -> list[Stmt]:
        statements: list[Stmt] = []
        while not self.is_at_end():
            statements.append(self.statement())
        return statements

    def statement(self) -> Stmt:
        if self.match(TT.PRINT):
            return self.print_statement()
        return self.expression_statement()

    def print_statement(self) -> Stmt:
        expr = self.expression()
        self.consume(TT.SEMICOLON, "Expect semicolon after value.")
        return Print(expr)

    def expression_statement(self) -> Stmt:
        expr = self.expression()
        self.consume(TT.SEMICOLON, "Expect semicolon after statement.")
        return Expression(expr)

    def expression(self) -> Expr:
        return self.equality()

    def equality(self) -> Expr:
        return self.parse_binary_operation(
            self.comparison, TT.BANG_EQUAL, TT.EQUAL_EQUAL
        )

    def comparison(self) -> Expr:
        return self.parse_binary_operation(
            self.term, TT.GREATER, TT.GREATER_EQUAL, TT.LESS, TT.LESS_EQUAL
        )

    def term(self) -> Expr:
        return self.parse_binary_operation(self.factor, TT.PLUS, TT.MINUS)

    def factor(self) -> Expr:
        return self.parse_binary_operation(self.unary, TT.STAR, TT.SLASH)

    def unary(self) -> Expr:
        if tok := self.match(TT.BANG, TT.MINUS):
            return Unary(tok, self.unary())
        return self.primary()

    def primary(self) -> Expr:
        if self.match(TT.FALSE):
            return Literal(False)
        if self.match(TT.TRUE):
            return Literal(True)
        if self.match(TT.NIL):
            return Literal(None)

        if tok := self.match(TT.NUMBER, TT.STRING):
            return Literal(tok.literal)

        if self.match(TT.LEFT_PAREN):
            expr = self.expression()
            self.consume(TT.RIGHT_PAREN, "Expect ')' after expression.")
            return Grouping(expr)

        raise self.error(self.peek(), "Expect expression.")

    def parse_binary_operation(self, next_expr: Callable[[], Expr], *types: TT) -> Expr:
        expr = next_expr()
        while tok := self.match(*types):
            expr = Binary(expr, tok, next_expr())
        return expr

    def synchronize(self):
        self.current += 1
        while not self.is_at_end():
            if self.tokens[self.current - 1].typ == TT.SEMICOLON:
                return
            if self.peek().typ in (
                TT.CLASS,
                TT.FUN,
                TT.VAR,
                TT.FOR,
                TT.IF,
                TT.WHILE,
                TT.PRINT,
                TT.RETURN,
            ):
                return
            self.current += 1

    def consume(self, typ: TT, message: str) -> Token:
        if tok := self.match(typ):
            return tok
        raise self.error(self.peek(), message)

    def error(self, tok: Token, message: str) -> LoxParseError:
        # if tok.typ == TT.EOF:
        #     self.error_reporter(tok.line, " at end", message)
        # else:
        #     self.error_reporter(tok.line, f"at '{tok.lexeme}'", message)
        return LoxParseError(tok, message)

    def is_at_end(self) -> bool:
        return self.peek().typ == TT.EOF

    def peek(self) -> Token:
        return self.tokens[self.current]

    def match(self, *types: TT) -> Optional[Token]:
        """
        Tries to match the next token to the given types. Consumes and returns
        the token on match, returns None if there is no match.
        """
        if (tok := self.peek()).typ in types:
            self.current += 1
            return tok
        return None
