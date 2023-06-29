from dataclasses import dataclass
from typing import Protocol, runtime_checkable

from lox import interpreter
from lox import scanner


@runtime_checkable
class LoxCallable(Protocol):
    arity: int

    def call(
        self, intr: "interpreter.Interpreter", args: "list[LoxValue]"
    ) -> "LoxValue":
        pass


class LoxInstance:
    klass: "LoxClass"
    fields: "dict[str, LoxValue]"

    def __init__(self, klass: "LoxClass") -> None:
        self.klass = klass
        self.fields = {}

    def get(self, name: "scanner.Token") -> "LoxValue":
        if name.lexeme in self.fields:
            return self.fields[name.lexeme]
        if method := self.klass.get_method(name.lexeme):
            return method.bind(self)
        raise interpreter.LoxRuntimeError(name, f"Undefined property {name.lexeme}.")

    def set(self, name: "scanner.Token", value: "LoxValue") -> None:
        self.fields[name.lexeme] = value

    def __str__(self) -> str:
        return f"<lox instance {self.klass.name}>"


class LoxClass(LoxCallable):
    arity: int = 0
    name: str
    methods: dict[str, "interpreter.UserFunction"]

    def __init__(
        self, name: str, methods: "dict[str, interpreter.UserFunction]"
    ) -> None:
        self.name = name
        self.methods = methods
        if initializer := self.get_method("init"):
            self.arity = initializer.arity

    def call(
        self, intr: "interpreter.Interpreter", args: "list[LoxValue]"
    ) -> "LoxValue":
        instance = LoxInstance(self)
        if initializer := self.get_method("init"):
            initializer.bind(instance).call(intr, args)
        return instance

    def get_method(self, name: str) -> "interpreter.UserFunction | None":
        return self.methods.get(name)

    def __str__(self) -> str:
        return f"<lox class {self.name}>"


LoxValue = None | bool | float | str | LoxCallable | LoxClass | LoxInstance


@dataclass(frozen=True, eq=False)
class LoxObject:
    val: LoxValue

    def __str__(self) -> str:
        if self.val is None:
            return "nil"
        if isinstance(self.val, bool):
            return str(self.val).lower()
        if isinstance(self.val, float):
            return f"{self.val:f}".rstrip("0").rstrip(".")
        return str(self.val)

    def is_truthy(self) -> bool:
        if self.val is None or self.val is False:
            return False
        return True
