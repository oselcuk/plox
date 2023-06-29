import sys
import traceback

# from lox import ast_printer
from lox import interpreter
from lox import parser
from lox import resolver
from lox import scanner
from lox import stmt


class Lox:
    intr: interpreter.Interpreter
    had_error: bool = False
    had_runtime_error: bool = False

    def __init__(self):
        self.intr = interpreter.Interpreter(self)

    def run_file(self, path: str):
        source: str
        with open(path, encoding="utf-8") as source_file:
            source = source_file.read()
        self.run(source)
        if self.had_error:
            sys.exit(65)
        if self.had_runtime_error:
            sys.exit(70)

    def run_prompt(self):
        while True:
            try:
                line = input(">>> ")
                self.run(line)
                self.had_error = False
            except EOFError:
                break

    def run(self, source: str):
        scan = scanner.Scanner(self, source)
        tokens = scan.scan_tokens()

        # self.debug("Parsed tokens:")
        # for token in tokens:
        #     self.debug(token)
        # self.debug()

        parse = parser.Parser(self, tokens)
        statements: list[stmt.Stmt] = parse.parse()

        # self.debug("Parsed expression:")
        # self.debug(ast_printer.AstPrinter().print(statements))
        # self.debug()

        if self.had_error:
            return

        resolver.Resolver(self.intr).resolve_statements(statements)
        # self.debug("Resolver:")
        # for k, v in self.intr.locals.items():
        #     self.debug(f"{k}: {v}")
        if self.had_error:
            return
        self.intr.interpret(statements)

    def error_line(self, line: int, message: str):
        self.report(line, "", message)

    def error(self, token: scanner.Token, message: str):
        if token.typ == scanner.TokenType.EOF:
            self.report(token.line, "at end", message)
        else:
            self.report(token.line, f"at '{token.lexeme}'", message)

    def report(self, line: int, where: str, message: str):
        if where:
            where = " " + where
        print(f"[line {line}] Error{where}: {message}", file=sys.stderr)
        self.had_error = True

    def runtime_error(self, error: interpreter.LoxRuntimeError):
        print(f"{error}\n[line {error.token.line}]", file=sys.stderr)
        for line in traceback.format_exception(error):
            self.debug(line)
        self.had_runtime_error = True

    def debug(self, *args, **kwargs):
        print(*args, **kwargs, file=sys.stderr)


def main():
    if len(sys.argv) > 2:
        print("Usage: plox [script]")
        sys.exit(64)
    if len(sys.argv) == 2:
        Lox().run_file(sys.argv[1])
    else:
        Lox().run_prompt()


if __name__ == "__main__":
    main()
