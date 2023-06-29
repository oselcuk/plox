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


LoxValue = None | bool | float | str | LoxCallable


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
