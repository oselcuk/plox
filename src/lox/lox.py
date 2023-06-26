import sys

from lox.ast_printer import AstPrinter
from lox.exceptions import LoxError
from lox.interpreter import Interpreter
from lox.parser import Parser
from lox.scanner import Scanner
from lox.stmt import Stmt


def main():
    if len(sys.argv) > 2:
        print("Usage: plox [script]")
        sys.exit(64)
    if len(sys.argv) == 2:
        run_file(sys.argv[1])
    else:
        run_prompt()


def run_file(path: str):
    source: str
    with open(path, encoding="utf-8") as source_file:
        source = source_file.read()
    try:
        run(Interpreter(), source)
    except LoxError as error:
        print(error)
        sys.exit(65)


def run_prompt():
    interpreter = Interpreter()
    while True:
        try:
            run(interpreter, input("> "))
        except LoxError as error:
            print(error)
        except EOFError:
            break


def run(interpreter: Interpreter, source: str):
    scanner = Scanner(source)
    tokens, errors = scanner.scan_tokens()
    # FUTURE: Should this raise instead? Does it make sense to continue
    # to parsing if scanning failed?
    if errors:
        print("Errors encountered during scanning:", file=sys.stderr)
        for error in errors:
            print(error)

    print("Parsed tokens:", file=sys.stderr)
    for token in tokens:
        print(token, file=sys.stderr)
    print(file=sys.stderr)

    parser = Parser(tokens)
    expr: list[Stmt] = parser.parse()

    print("Parsed expression:", file=sys.stderr)
    print(AstPrinter().print(expr), file=sys.stderr)
    print(file=sys.stderr)

    interpreter.interpret(expr)


if __name__ == "__main__":
    main()
