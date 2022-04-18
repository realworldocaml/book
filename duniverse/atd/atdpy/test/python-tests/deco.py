"""Test custom decorators.
"""

from typing import Any


def deco1(cls: Any) -> Any:
    print(f"Decorating class {cls.__name__} with deco1")
    return cls


def deco2(n: int) -> Any:
    def decorator(cls: Any) -> Any:
        print(f"Decorating class {cls.__name__} with deco2({n})")
        return cls
    return decorator
