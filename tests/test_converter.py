import pathlib

from comet import mesh_converters

TEST_MESH = pathlib.Path('ex_2fib.inp')

def main() -> None:
    converter = mesh_converters.AbaqusConverter(TEST_MESH)
    converter.read_nodes()
    converter.read_connectivity()
    converter.read_sets()
    converter.read_materials()
    converter.to_vtk(pathlib.Path('ex_2fib.vtu'))

    # print(converter.material_definitions)

if __name__ == "__main__":
    main()