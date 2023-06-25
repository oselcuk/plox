import sys

from ast_printer import AstPrinter
from exceptions import LoxError
from interpreter import Interpreter

# pylint: disable=deprecated-module
from parser import Parser

from scanner import Scanner


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
        run(source)
    except LoxError as error:
        print(error)
        sys.exit(65)


def run_prompt():
    while True:
        try:
            run(input("> "))
        except LoxError as error:
            print(error)
        except EOFError:
            break


def run(source: str):
    scanner = Scanner(source)
    tokens, errors = scanner.scan_tokens()
    # FUTURE: Should this raise instead? Does it make sense to continue
    # to parsing if scanning failed?
    if errors:
        print("Errors encountered during scanning:")
        for error in errors:
            print(error)

    print("Parsed tokens:")
    for token in tokens:
        print(token)
    print()

    parser = Parser(tokens)
    expr = parser.parse()

    print("Parsed expression:")
    print(AstPrinter().print(expr))
    print()

    res = Interpreter().interpret(expr)
    print("Expression evaluates to:")
    print(res)
    print()


if __name__ == "__main__":
    main()
