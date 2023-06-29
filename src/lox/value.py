from dataclasses import dataclass
from typing import Protocol, runtime_checkable

from lox import interpreter


@runtime_checkable
class LoxCallable(Protocol):
    arity: int

    def call(
        self, intr: "interpreter.Interpreter", args: "list[LoxValue]"
    ) -> "LoxValue":
        pass


@dataclass(frozen=True, eq=False)
class LoxInstance:
    klass: "LoxClass"

    def __str__(self) -> str:
        return f"<lox instance {self.klass.name}>"


class LoxClass(LoxCallable):
    arity: int = 0
    name: str

    def __init__(self, name: str) -> None:
        self.name = name

    def call(
        self, intr: "interpreter.Interpreter", args: "list[LoxValue]"
    ) -> "LoxValue":
        return LoxInstance(self)

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
