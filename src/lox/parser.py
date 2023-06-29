from typing import Callable, Optional

import lox.expr
import lox.lox
import lox.scanner
import lox.stmt


TT = lox.scanner.TokenType


class LoxParseError(Exception):
    pass


class Parser:
    tokens: list[lox.scanner.Token]
    current: int = 0
    lox_instance: "lox.lox.Lox"

    def __init__(
        self, lox_instance: "lox.lox.Lox", tokens: list[lox.scanner.Token]
    ) -> None:
        self.lox_instance = lox_instance
        self.tokens = tokens

    def parse(self) -> list[lox.stmt.Stmt]:
        return self.program()

    def program(self) -> list[lox.stmt.Stmt]:
        statements: list[lox.stmt.Stmt] = []
        while not self.match(TT.EOF):
            if stmt := self.declaration():
                statements.append(stmt)
        return statements

    def declaration(self) -> Optional[lox.stmt.Stmt]:
        try:
            if self.match(TT.VAR):
                return self.var_declaration()
            if self.match(TT.FUN):
                return self.fun_declaration("function")
            if self.match(TT.CLASS):
                return self.class_declaration()
            return self.statement()
        except LoxParseError:
            self.synchronize()
            return None

    def var_declaration(self) -> lox.stmt.Stmt:
        name = self.consume(TT.IDENTIFIER, "Expect identifier after 'var'.")
        initializer: Optional[lox.expr.Expr] = None
        if self.match(TT.EQUAL):
            initializer = self.expression()
        self.consume(TT.SEMICOLON, "Expect semicolon after var declaration.")
        return lox.stmt.Var(name, initializer)

    def fun_declaration(self, kind: str) -> lox.stmt.Function:
        name = self.consume(TT.IDENTIFIER, f"Expect {kind} name.")
        self.consume(
            TT.LEFT_PAREN,
            f"Expect parenthses around {kind} parameters.",
        )
        params: list[lox.scanner.Token] = []
        if not self.match(TT.RIGHT_PAREN):
            params.append(self.consume(TT.IDENTIFIER, "Expect parameters."))
            while not self.match(TT.RIGHT_PAREN):
                if len(params) >= 255:
                    self.error(
                        self.peek(), f"{kind}s cannot have more than 255 parameters."
                    )
                self.consume(TT.COMMA, "Expect commas between parameters.")
                params.append(self.consume(TT.IDENTIFIER, "Expect parameters."))
        self.consume(TT.LEFT_BRACE, f"Expect braces around {kind} body.")
        return lox.stmt.Function(name, params, self.block_statement())

    def class_declaration(self) -> lox.stmt.Stmt:
        name = self.consume(TT.IDENTIFIER, "Expect class name.")
        superclass: lox.expr.Variable | None = None
        if self.match(TT.LESS):
            superclass = lox.expr.Variable(
                self.consume(TT.IDENTIFIER, "Expect class name after '<'.")
            )
        self.consume(TT.LEFT_BRACE, "Expect brace before class body.")
        methods: list[lox.stmt.Function] = []
        while not self.check(TT.RIGHT_BRACE) and not self.is_at_end():
            method = self.fun_declaration("method")
            # if method.name.lexeme == "init":
            methods.append(method)
        self.consume(TT.RIGHT_BRACE, "Expect brace after class body.")
        return lox.stmt.Class(name, superclass, methods)

    def statement(self) -> lox.stmt.Stmt:
        if self.match(TT.PRINT):
            return self.print_statement()
        if self.match(TT.LEFT_BRACE):
            return lox.stmt.Block(self.block_statement())
        if self.match(TT.IF):
            return self.if_statement()
        if self.match(TT.WHILE):
            return self.while_()
        if self.match(TT.FOR):
            return self.for_()
        if token := self.match(TT.BREAK):
            return self.break_(token)
        if token := self.match(TT.RETURN):
            return self.return_(token)
        return self.expression_statement()

    def print_statement(self) -> lox.stmt.Stmt:
        expr = self.expression()
        self.consume(TT.SEMICOLON, "Expect semicolon after value.")
        return lox.stmt.Print(expr)

    def block_statement(self) -> list[lox.stmt.Stmt]:
        statements: list[lox.stmt.Stmt] = []
        while self.peek().typ != TT.RIGHT_BRACE and not self.is_at_end():
            if stmt := self.declaration():
                statements.append(stmt)
        self.consume(TT.RIGHT_BRACE, "Expect closing brace after block.")
        return statements

    def parse_condition(self) -> lox.expr.Expr:
        self.consume(TT.LEFT_PAREN, "Expect parenthses around condition.")
        cond = self.expression()
        self.consume(TT.RIGHT_PAREN, "Expect parenthses around condition.")
        return cond

    def if_statement(self) -> lox.stmt.Stmt:
        cond = self.parse_condition()
        then = self.statement()
        else_: lox.stmt.Stmt | None = None
        if self.match(TT.ELSE):
            else_ = self.statement()
        return lox.stmt.If(cond, then, else_)

    def while_(self) -> lox.stmt.Stmt:
        cond = self.parse_condition()
        body = self.statement()
        return lox.stmt.While(cond, body)

    def for_(self) -> lox.stmt.Stmt:
        self.consume(TT.LEFT_PAREN, "Expect parenthses around for header.")
        statements: list[lox.stmt.Stmt] = []
        if self.match(TT.VAR):
            statements.append(self.var_declaration())
        elif not self.match(TT.SEMICOLON):
            statements.append(self.expression_statement())
        cond: lox.expr.Expr = lox.expr.Literal(True)
        if not self.match(TT.SEMICOLON):
            cond = self.expression()
        self.consume(TT.SEMICOLON, "Expect semicolon after for condition.")
        advancement: lox.expr.Expr | None = None
        if not self.match(TT.SEMICOLON):
            advancement = self.expression()
        self.consume(TT.RIGHT_PAREN, "Expect parenthses around for header.")
        body = self.statement()
        if advancement is not None:
            body = lox.stmt.Block([body, lox.stmt.Expression(advancement)])
        statements.append(lox.stmt.While(cond, body))
        return lox.stmt.Block(statements)

    def break_(self, token: lox.scanner.Token) -> lox.stmt.Stmt:
        self.consume(TT.SEMICOLON, "Expect semicolon after break.")
        return lox.stmt.Break(token)

    def return_(self, token: lox.scanner.Token) -> lox.stmt.Stmt:
        value: lox.expr.Expr | None = None
        if not self.match(TT.SEMICOLON):
            value = self.expression()
            self.consume(TT.SEMICOLON, "Expect semicolon after Return.")
        return lox.stmt.Return(token, value)

    def expression_statement(self) -> lox.stmt.Stmt:
        expr = self.expression()
        self.consume(TT.SEMICOLON, "Expect semicolon after statement.")
        return lox.stmt.Expression(expr)

    def expression(self) -> lox.expr.Expr:
        return self.assignment()

    def assignment(self) -> lox.expr.Expr:
        expr = self.or_()
        if equals := self.match(TT.EQUAL):
            value = self.assignment()
            if isinstance(expr, lox.expr.Variable):
                return lox.expr.Assign(expr.name, value)
            if isinstance(expr, lox.expr.Get):
                return lox.expr.Set(expr.object, expr.name, value)
            self.error(equals, "Invalid assignment target.")
        return expr

    def or_(self) -> lox.expr.Expr:
        expr = self.and_()
        while operator := self.match(TT.OR):
            expr = lox.expr.Logical(expr, operator, self.and_())
        return expr

    def and_(self) -> lox.expr.Expr:
        expr = self.equality()
        while operator := self.match(TT.AND):
            expr = lox.expr.Logical(expr, operator, self.equality())
        return expr

    def equality(self) -> lox.expr.Expr:
        return self.parse_binary_operation(
            self.comparison,
            TT.BANG_EQUAL,
            TT.EQUAL_EQUAL,
        )

    def comparison(self) -> lox.expr.Expr:
        return self.parse_binary_operation(
            self.term,
            TT.GREATER,
            TT.GREATER_EQUAL,
            TT.LESS,
            TT.LESS_EQUAL,
        )

    def term(self) -> lox.expr.Expr:
        return self.parse_binary_operation(self.factor, TT.PLUS, TT.MINUS)

    def factor(self) -> lox.expr.Expr:
        return self.parse_binary_operation(self.unary, TT.STAR, TT.SLASH)

    def unary(self) -> lox.expr.Expr:
        if tok := self.match(TT.BANG, TT.MINUS):
            return lox.expr.Unary(tok, self.unary())
        return self.call()

    def call(self) -> lox.expr.Expr:
        expr = self.primary()
        while not self.is_at_end():
            if paren := self.match(TT.LEFT_PAREN):
                arguments = self.arguments()
                expr = lox.expr.Call(expr, paren, arguments)
            elif self.match(TT.DOT):
                name = self.consume(TT.IDENTIFIER, "Expect identifier after dot.")
                expr = lox.expr.Get(expr, name)
            else:
                break
        return expr

    def arguments(self) -> list[lox.expr.Expr]:
        if self.match(TT.RIGHT_PAREN):
            return []
        args: list[lox.expr.Expr] = [self.expression()]
        while not self.match(TT.RIGHT_PAREN):
            self.consume(TT.COMMA, "Expect comma between args.")
            if len(args) >= 255:
                self.error(self.peek(), "Can't have more than 255 arguments.")
            args.append(self.expression())
        return args

    def primary(self) -> lox.expr.Expr:
        if self.match(TT.FALSE):
            return lox.expr.Literal(False)
        if self.match(TT.TRUE):
            return lox.expr.Literal(True)
        if self.match(TT.NIL):
            return lox.expr.Literal(None)
        if tok := self.match(TT.NUMBER, TT.STRING):
            return lox.expr.Literal(tok.literal)
        if self.match(TT.LEFT_PAREN):
            expr = self.expression()
            self.consume(TT.RIGHT_PAREN, "Expect ')' after expression.")
            return lox.expr.Grouping(expr)
        if tok := self.match(TT.IDENTIFIER):
            return lox.expr.Variable(tok)
        if tok := self.match(TT.THIS):
            return lox.expr.This(tok)
        if tok := self.match(TT.SUPER):
            self.consume(TT.DOT, "Expect '.' after 'super'.")
            method = self.consume(TT.IDENTIFIER, "Expect superclass method name.")
            return lox.expr.Super(tok, method)
        raise self.error(self.peek(), "Expect expression.")

    def parse_binary_operation(
        self, next_expr: Callable[[], lox.expr.Expr], *types: TT
    ) -> lox.expr.Expr:
        expr = next_expr()
        while tok := self.match(*types):
            expr = lox.expr.Binary(expr, tok, next_expr())
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

    def consume(self, typ: TT, message: str) -> lox.scanner.Token:
        if tok := self.match(typ):
            return tok
        raise self.error(self.peek(), message)

    def error(self, token: lox.scanner.Token, message: str) -> LoxParseError:
        self.lox_instance.error(token, message)
        return LoxParseError()

    def is_at_end(self) -> bool:
        return self.peek().typ == TT.EOF

    def peek(self) -> lox.scanner.Token:
        return self.tokens[self.current]

    def peek_next(self) -> lox.scanner.Token | None:
        if self.is_at_end():
            return None
        return self.tokens[self.current + 1]

    def check(self, *types: TT) -> Optional[lox.scanner.Token]:
        """
        Tries to match the next token to the given types. Consumes and returns
        the token on match, returns None if there is no match.
        """
        if (tok := self.peek()).typ in types:
            return tok
        return None

    def match(self, *types: TT) -> Optional[lox.scanner.Token]:
        """
        Tries to match the next token to the given types. Consumes and returns
        the token on match, returns None if there is no match.
        """
        if tok := self.check(*types):
            self.current += 1
            return tok
        return None
