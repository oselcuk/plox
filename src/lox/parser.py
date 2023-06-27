from typing import Callable, Optional

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
    Variable,
)
from lox.scanner import Token
from lox.scanner import TokenType as TT
from lox.stmt import Block, Expression, Break, If, Print, Stmt, Var, While

ErrorReporterType = Callable[[int, str, str], None]


class LoxParseError(LoxError):
    def __init__(self, tok: Token, message: str):
        self.tok = tok
        super().__init__(f"[line {tok.line}]: {message}")


class Parser:
    tokens: list[Token]
    current: int = 0
    # To know when to allow flow control words like "break".
    loop_depth: int = 0

    def __init__(self, tokens: list[Token]) -> None:
        self.tokens = tokens

    def parse(self) -> list[Stmt]:
        return self.program()

    def program(self) -> list[Stmt]:
        statements: list[Stmt] = []
        while not self.match(TT.EOF):
            if stmt := self.declaration():
                statements.append(stmt)
        return statements

    def declaration(self) -> Optional[Stmt]:
        try:
            if self.match(TT.VAR):
                return self.var_declaration()
            return self.statement()
        except LoxParseError as error:
            print(error)
            self.synchronize()
            return None

    def var_declaration(self) -> Stmt:
        name = self.consume(TT.IDENTIFIER, "Expect identifier after 'var'.")
        initializer: Optional[Expr] = None
        if self.match(TT.EQUAL):
            initializer = self.expression()
        self.consume(TT.SEMICOLON, "Expect semicolon after var declaration.")
        return Var(name, initializer)

    def statement(self) -> Stmt:
        if self.match(TT.PRINT):
            return self.print_statement()
        if self.match(TT.LEFT_BRACE):
            return Block(self.block_statement())
        if self.match(TT.IF):
            return self.if_statement()
        if self.match(TT.WHILE):
            return self.while_()
        if self.match(TT.FOR):
            return self.for_()
        if token := self.match(TT.BREAK):
            return self.break_()
        return self.expression_statement()

    def print_statement(self) -> Stmt:
        expr = self.expression()
        self.consume(TT.SEMICOLON, "Expect semicolon after value.")
        return Print(expr)

    def block_statement(self) -> list[Stmt]:
        statements: list[Stmt] = []
        while self.peek().typ != TT.RIGHT_BRACE and not self.is_at_end():
            if stmt := self.declaration():
                statements.append(stmt)
        self.consume(TT.RIGHT_BRACE, "Expect closing brace after block.")
        return statements

    def parse_condition(self) -> Expr:
        self.consume(TT.LEFT_PAREN, "Expect parenthses around condition.")
        cond = self.expression()
        self.consume(TT.RIGHT_PAREN, "Expect parenthses around condition.")
        return cond

    def if_statement(self) -> Stmt:
        cond = self.parse_condition()
        then = self.statement()
        else_: Stmt | None = None
        if self.match(TT.ELSE):
            else_ = self.statement()
        return If(cond, then, else_)

    def while_(self) -> Stmt:
        cond = self.parse_condition()
        try:
            self.loop_depth += 1
            body = self.statement()
        finally:
            self.loop_depth -= 1
        return While(cond, body)

    def for_(self) -> Stmt:
        self.consume(TT.LEFT_PAREN, "Expect parenthses around for header.")
        statements: list[Stmt] = []
        if self.match(TT.VAR):
            statements.append(self.var_declaration())
        elif not self.match(TT.SEMICOLON):
            statements.append(self.expression_statement())
        cond: Expr = Literal(True)
        if not self.match(TT.SEMICOLON):
            cond = self.expression()
        self.consume(TT.SEMICOLON, "Expect semicolon after for condition.")
        advancement: Expr | None = None
        if not self.match(TT.SEMICOLON):
            advancement = self.expression()
        self.consume(TT.RIGHT_PAREN, "Expect parenthses around for header.")
        try:
            self.loop_depth += 1
            body = self.statement()
        finally:
            self.loop_depth -= 1
        if advancement is not None:
            body = Block([body, Expression(advancement)])
        statements.append(While(cond, body))
        return Block(statements)

    def break_(self) -> Stmt:
        if self.loop_depth == 0:
            self.error(
                self.tokens[self.current], "Break is not allowed outside of a loop."
            )
            return self.statement()
        self.consume(TT.SEMICOLON, "Expect semicolon after break.")
        return Break()

    def expression_statement(self) -> Stmt:
        expr = self.expression()
        self.consume(TT.SEMICOLON, "Expect semicolon after statement.")
        return Expression(expr)

    def expression(self) -> Expr:
        return self.assignment()

    def assignment(self) -> Expr:
        expr = self.or_()
        if equals := self.match(TT.EQUAL):
            value = self.assignment()
            if isinstance(expr, Variable):
                return Assign(expr.name, value)
            self.error(equals, "Invalid assignment target.")
        return expr

    def or_(self) -> Expr:
        expr = self.and_()
        while operator := self.match(TT.OR):
            expr = Logical(expr, operator, self.and_())
        return expr

    def and_(self) -> Expr:
        expr = self.equality()
        while operator := self.match(TT.AND):
            expr = Logical(expr, operator, self.equality())
        return expr

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
        return self.call()

    def call(self) -> Expr:
        expr = self.primary()
        while (paren := self.match(TT.LEFT_PAREN)) is not None:
            arguments = self.arguments()
            expr = Call(expr, paren, arguments)
        return expr

    def arguments(self) -> list[Expr]:
        if self.match(TT.RIGHT_PAREN):
            return []
        args: list[Expr] = [self.expression()]
        while not self.match(TT.RIGHT_PAREN):
            self.consume(TT.COMMA, "Expect comma between args.")
            if len(args) >= 255:
                self.error(self.peek(), "Can't have more than 255 arguments.")
            args.append(self.expression())
        return args

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
        if tok := self.match(TT.IDENTIFIER):
            return Variable(tok)
        raise self.error(self.peek(), "Expect expression.")

    def parse_binary_operation(self, next_expr: Callable[[], Expr], *types: TT) -> Expr:
        expr = next_expr()
        while tok := self.match(*types):
            expr = Binary(expr, tok, next_expr())
        return expr

    def synchronize(self):
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
        error = LoxParseError(tok, message)
        print(error)
        return error

    def is_at_end(self) -> bool:
        return self.peek().typ == TT.EOF

    def peek(self) -> Token:
        return self.tokens[self.current]

    def peek_next(self) -> Token | None:
        if self.is_at_end():
            return None
        return self.tokens[self.current + 1]

    def match(self, *types: TT) -> Optional[Token]:
        """
        Tries to match the next token to the given types. Consumes and returns
        the token on match, returns None if there is no match.
        """
        if (tok := self.peek()).typ in types:
            self.current += 1
            return tok
        return None
