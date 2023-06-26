# pylint: disable=missing-function-docstring,missing-class-docstring,missing-module-docstring

import ast
from dataclasses import dataclass
from enum import Enum
from typing import Any, Optional

from lox.value import LoxValue

# fmt: off
TokenType = Enum("TokenType", [
    # Single-character tokens.
    "LEFT_PAREN", "RIGHT_PAREN", "LEFT_BRACE", "RIGHT_BRACE",
    "COMMA", "DOT", "MINUS", "PLUS", "SEMICOLON", "SLASH", "STAR",

    # One or two character tokens.
    "BANG", "BANG_EQUAL",
    "EQUAL", "EQUAL_EQUAL",
    "GREATER", "GREATER_EQUAL",
    "LESS", "LESS_EQUAL",

    # Literals.
    "IDENTIFIER", "STRING", "NUMBER",

    # Keywords.
    "AND", "CLASS", "ELSE", "FALSE", "FUN", "FOR", "IF", "NIL", "OR",
    "PRINT", "RETURN", "SUPER", "THIS", "TRUE", "VAR", "WHILE",

    "EOF"
])

keywords = {
    "and":    TokenType.AND,
    "class":  TokenType.CLASS,
    "else":   TokenType.ELSE,
    "false":  TokenType.FALSE,
    "for":    TokenType.FOR,
    "fun":    TokenType.FUN,
    "if":     TokenType.IF,
    "nil":    TokenType.NIL,
    "or":     TokenType.OR,
    "print":  TokenType.PRINT,
    "return": TokenType.RETURN,
    "super":  TokenType.SUPER,
    "this":   TokenType.THIS,
    "true":   TokenType.TRUE,
    "var":    TokenType.VAR,
    "while":  TokenType.WHILE,
}
# fmt: on


@dataclass(frozen=True)
class Token:
    typ: TokenType
    lexeme: str
    literal: LoxValue
    line: int

    def __str__(self):
        return f"{self.typ} {self.lexeme} {self.literal}"


class Scanner:
    source: str
    tokens: list[Token]

    errors: list[str]
    start: int = 0
    current: int = 0
    line: int = 1

    def __init__(self, source: str):
        self.source = source
        self.tokens = []
        self.errors = []

    def is_at_end(self) -> bool:
        return self.current >= len(self.source)

    def scan_tokens(self) -> tuple[list[Token], list[str]]:
        while not self.is_at_end():
            self.start = self.current
            self.scan_token()

        self.tokens.append(Token(TokenType.EOF, "", None, self.line))
        return (self.tokens, self.errors)

    def peek(self) -> Optional[str]:
        """
        Returns the next character, or None if we're at EOF
        """
        if self.is_at_end():
            return None
        return self.source[self.current]

    def peek_next(self) -> Optional[str]:
        """
        Returns the character after next, or None it's EOF
        """
        if self.current + 1 >= len(self.source):
            return None
        return self.source[self.current + 1]

    def advance(self) -> str:
        self.current += 1
        return self.source[self.current - 1]

    def add_token(self, typ: TokenType, literal: Any = None):
        text = self.source[self.start : self.current]
        self.tokens.append(Token(typ, text, literal, self.line))

    def match(self, expected: str) -> bool:
        if self.peek() != expected:
            return False
        self.current += 1
        return True

    def advance_until_match(self, expected: str) -> bool:
        """
        Advances current pointer until expected character is matched (but
        not consumed) or EOF. Returns True if character is found before EOF.
        """
        while not self.is_at_end():
            p = self.peek()
            if p == expected:
                return True
            if p == "\n":
                self.line += 1
            self.advance()
        return False

    def block_comment(self):
        starting_line = self.line
        while self.advance_until_match("*"):
            self.match("*")
            if self.match("/"):
                return
        self.report_error(
            f"Unterminated block comment starting on line {starting_line}."
        )

    def string(self):
        if not self.advance_until_match('"'):
            self.report_error("Unterminated string.")
            return
        # Consume closing ".
        self.advance()

        # Trim the quotes.
        value = self.source[self.start + 1 : self.current - 1]

        # Cheating a bit by having python evaluate escape sequences.
        # Note the triple quotes, needed for multiline strings, which lox supports.
        value = ast.literal_eval(f'"""{value}"""')

        self.add_token(TokenType.STRING, value)

    def number(self):
        while (p := self.peek()) and p.isdigit():
            self.advance()
        if self.peek() == "." and (p := self.peek_next()) and p.isdigit():
            self.advance()
            while (p := self.peek()) and p.isdigit():
                self.advance()
        value = float(self.source[self.start : self.current])
        self.add_token(TokenType.NUMBER, value)

    def identifier(self):
        while (p := self.peek()) and p.isalnum() or p == "_":
            self.advance()

        text = self.source[self.start : self.current]
        typ = keywords.get(text, TokenType.IDENTIFIER)
        self.add_token(typ)

    def scan_token(self):
        c = self.advance()
        match c:
            case "(":
                self.add_token(TokenType.LEFT_PAREN)
            case ")":
                self.add_token(TokenType.RIGHT_PAREN)
            case "{":
                self.add_token(TokenType.LEFT_BRACE)
            case "}":
                self.add_token(TokenType.RIGHT_BRACE)
            case ",":
                self.add_token(TokenType.COMMA)
            case ".":
                self.add_token(TokenType.DOT)
            case "-":
                self.add_token(TokenType.MINUS)
            case "+":
                self.add_token(TokenType.PLUS)
            case ";":
                self.add_token(TokenType.SEMICOLON)
            case "*":
                self.add_token(TokenType.STAR)
            case "!":
                self.add_token(
                    TokenType.BANG_EQUAL if self.match("=") else TokenType.BANG
                )
            case "=":
                self.add_token(
                    TokenType.EQUAL_EQUAL if self.match("=") else TokenType.EQUAL
                )
            case "<":
                self.add_token(
                    TokenType.LESS_EQUAL if self.match("=") else TokenType.LESS
                )
            case ">":
                self.add_token(
                    TokenType.GREATER_EQUAL if self.match("=") else TokenType.GREATER
                )
            case "/":
                # Skip over line comments
                if self.match("/"):
                    self.advance_until_match("\n")
                elif self.match("*"):
                    self.block_comment()
                else:
                    self.add_token(TokenType.SLASH)
            # Discard whitespace
            case "\n":
                self.line += 1
            case c if c.isspace():
                pass
            case '"':
                self.string()
            case c if c.isdigit():
                self.number()
            case c if c.isalpha() or c == "_":
                self.identifier()
            case _:
                self.report_error(f"Unexpected character {c}")

    def report_error(self, message: str):
        self.errors.append(f"[line {self.line}]: {message}")
