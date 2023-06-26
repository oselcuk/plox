from dataclasses import dataclass


LoxValue = None | bool | float | str


@dataclass(frozen=True)
class LoxObject:
    val: LoxValue

    def __str__(self) -> str:
        if self.val is None:
            return "nil"
        if isinstance(self.val, bool):
            return str(self.val).lower()
        if isinstance(self.val, float):
            return f"{self.val:g}"
        return self.val

    def is_truthy(self) -> bool:
        if self.val is None or self.val is False:
            return False
        return True
