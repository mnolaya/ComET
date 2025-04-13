import pathlib

from attrs import define, field

@define
class AbaqusConverter:
    filepath: pathlib.Path
    lines: str = field(init=False)

    def __attrs_post_init__(self) -> None:
        with open(self.filepath, 'r') as f:
            return f.readlines()