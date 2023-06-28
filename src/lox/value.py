from dataclasses import dataclass
from typing import Any, Protocol, runtime_checkable


@runtime_checkable
class LoxCallable(Protocol):
    arity: int

    def call(self, interpreter: Any, args: "list[LoxValue]") -> "LoxValue":
        pass


LoxValue = None | bool | float | str | LoxCallable


@dataclass(frozen=True)
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
