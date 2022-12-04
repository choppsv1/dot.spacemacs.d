import ast
import sys
import typing as t

import astunparse


def test(region: str = "foo", bar: t.List[t.Any] = [1, 2],
         baz: int = 0) -> None:
    """

    Args:
        region (str): (default: 'foo')
        bar (t.List[t.Any]): (default: [1, 2])
        baz (int): (default: 0)
    Returns:
        None: nothing
    """

    def __init__(self) -> None:
        """Initialize object

        Returns:
            None: nothing
        """
        ...
    ...

TAB = "    "


def main() -> None:
    raw_string = ""
    indent = ""
    for line in sys.stdin.readlines():
        if (pos := line.find("def ")) != -1 :
            indent = line[0:pos]
        raw_string += line.strip()

    indent = f"{indent}{TAB}"
    try:
        ast_data = ast.parse(raw_string + f"\n{indent}...")
    except:
        return ""

    result = ""

    for fun in ast_data.body:
        if isinstance(fun, ast.FunctionDef):
            if fun.name == "__init__":
                result += f"{indent}\"\"\"Initialize object\n\n"
            else:
                result += f"{indent}\"\"\"\n\n"
            args = [arg for arg in fun.args.args if arg.arg != "self"]
            if len(fun.args.defaults) != 0:
                for i, default in enumerate(reversed(fun.args.defaults)):
                    args[-(i + 1)].__local_default = astunparse.unparse(default).strip()
            if len(args) != 0:
                result += f"{indent}Args:\n"
                for arg in args:
                    type_decr = "t.Any"
                    if arg.annotation is not None:
                        type_decr = astunparse.unparse(arg.annotation).strip()
                    result += f"{indent}{TAB}{arg.arg.strip()} ({type_decr}):"
                    if hasattr(arg, "__local_default"):
                        result += f" (default: {arg.__local_default})"
                    result += "\n"
            if fun.args.vararg:
                result += f"{indent}{TAB}*args (t.List[t.Any]):\n"
            if fun.args.kwarg:
                result += f"{indent}{TAB}**kwargs (t.Dict[t.Any, t.Any]):\n"
            result += f"{indent}Returns:\n"
            if ((fun.returns is None or getattr(fun.returns, "value", None) is None)
                and not getattr(fun.returns, "id", None)):
                result += f"{indent}{TAB}None: nothing\n"
            else:
                result += f"{indent}{TAB}{astunparse.unparse(fun.returns).strip()}:\n"
            result += f"{indent}\"\"\""
        if isinstance(fun, ast.ClassDef):
            result += f"{indent}\"\"\"{fun.name} is a class for\n"
            result += f"{indent}\"\"\""

    print(result)


if __name__ == "__main__":
    main()