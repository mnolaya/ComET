# Collection of classes for converting between various mesh formats (Abaqus, Hypermesh) to VTU for use with ComET.
import pathlib
import re

import numpy as np
from attrs import define, field

@define
class AbaqusConverter:
    filepath: pathlib.Path
    lines: np.ndarray[str] = field(init=False, repr='...')
    nodes: np.ndarray[float] = field(init=False)
    connectivity: dict[np.ndarray[int]] = field(init=False)
    mesh_sets: dict[str, dict[str, np.ndarray[int] | list[int]]] = field(init=False)
    material_definitions: dict[str, dict] = field(init=False)

    def __attrs_post_init__(self) -> None:
        with open(self.filepath, 'r') as f:
            self.lines = np.array(f.readlines())

    def read_nodes(self) -> None:
        idx_list = []
        reading = False
        pattern = re.compile(r'\*node', re.IGNORECASE)
        for i, line in enumerate(self.lines):
            _match = pattern.search(line)
            if _match is None: continue
            idx_list.append(i)
            if reading: 
                break
            else:
                pattern = re.compile(r'\*\S+', re.IGNORECASE)
                reading = True
        node_lines = self.lines[idx_list[0]+1:idx_list[1]]
        coords = []
        for line in node_lines:
            coords.append(np.array([float(n) for n in line.split(',')[1:]]))
        self.nodes = np.array(coords)

    def read_connectivity(self) -> None:
        patterns = [r'\*element, type=(\S+)', r'\*\S+']
        idx_dict = {}
        reading = False
        elem_type = None
        pattern = re.compile(patterns[0], re.IGNORECASE)
        for i, line in enumerate(self.lines):
            _match = pattern.search(line)
            if _match is None: continue
            if elem_type is None: elem_type = _match.group(1)
            if reading:
                idx_dict[elem_type].append(i)
                reading = False
                pattern = re.compile(patterns[0], re.IGNORECASE)
            else:
                idx_dict.update({elem_type: [i]})
                pattern = re.compile(patterns[1], re.IGNORECASE)
                reading = True
        elem_lines = {k: self.lines[idx[0]+1:idx[1]] for k, idx in idx_dict.items()}
        connectivity = {}
        for elem_type, lines in elem_lines.items():
            connectivity.update({elem_type: []})
            for line in lines:
                connectivity[elem_type].append(np.array([int(n) - 1 for n in line.split(',')[1:]]))
            connectivity[elem_type] = np.array(connectivity[elem_type])
        self.connectivity = connectivity

    def chunk_file_lines_by_pattern(self, pstart: re.Pattern, pend: re.Pattern) -> list[list[str]]:
        patterns = [pstart, pend]
        reading = False
        line_chunks = []
        start = 0
        while True:
            if start == len(self.lines): break
            lines = self.lines[start:]
            chunk = []
            reading = False
            for line in lines:
                start += 1
                if line[0:2] == "**": continue    # Skip commented lines
                matches = [p.search(line) for p in patterns]  # Check for matches
                # Break if currently reading lines and found a second match
                if matches[1] is not None and reading: break
                # Set reading to true if not currently reading and found first match
                if not reading and matches[0] is not None: reading = True
                # Append line to chunk if currently reading
                if reading: chunk.append(line)
            # If a chunk was created, append to list of chunks and set start of next chunk where last ended
            if chunk: 
                line_chunks.append(chunk)
                start -= 1 
        return line_chunks

    def read_sets(self) -> None:
        # Retrieve chunks of file lines that contain element and node sets
        patterns = [
            re.compile(r'\*(Nset|Elset), (nset|elset)=([^,]*)', re.IGNORECASE),
            re.compile(r'\*\S+', re.IGNORECASE)
        ]
        line_chunks = self.chunk_file_lines_by_pattern(*patterns)

        # Assign sets to class as dictionary, labeled by mesh location and set name.
        set_types = {'elset': 'element', 'nset': 'node'}
        mesh_sets = {'node': {}, 'element': {}}
        for chunk in line_chunks:
            m = patterns[0].search(chunk[0])  # First line of chunks should contain set information
            set_type = m.group(2).strip()
            set_name = m.group(3).strip()
            if 'generate' in chunk[0].lower():
                label_range = [int(n) for n in chunk[1].split(',')]
                label_range[0] -= 1
                labels = np.arange(*label_range)
            else:
                labels = []
                for line in chunk[1:]:
                    line = line.strip()
                    labels.append(np.array([int(n) - 1 for n in line.split(',') if n]))
            mesh_sets[set_types[set_type]].update({set_name: labels})
        self.mesh_sets = mesh_sets

    def read_materials(self) -> None:
        # Retrieve chunks of file lines that contain material defintions
        patterns = [
            re.compile(r'\*Solid Section, elset=([^,]*)(, orientation=(\S+),|\W) material=(\S+)', re.IGNORECASE),
            re.compile(r'\*\S+', re.IGNORECASE)
        ]
        line_chunks = self.chunk_file_lines_by_pattern(*patterns)

        # Assign sets to class as dictionary, labeled by mesh location and set name.
        material_definitions = {}
        for chunk in line_chunks:
            m = patterns[0].search(chunk[0])  # First line of chunks should contain set information
            material_name = m.group(4)
            orientation = None
            if 'orientation' in chunk[0]: orientation = m.group(3)
            set_name = m.group(1)
            material_definitions.update({material_name: {'set_name': set_name, 'orientation': orientation}})

        self.material_definitions = material_definitions

    def _map_materials_to_element_id(self) -> dict[str, np.ndarray]:
        # Get the element ids associated with the set associated with each defined material
        material_element_ids = {}
        for material, details in self.material_definitions.items():
            for element in self.mesh_sets['element'][details['set_name']]:
                material_element_ids.update({element: material})
        return material_element_ids
    
    def to_vtk(self, output_path: pathlib.Path, version: str = '0.1') -> None:
        # Get mesh details and node coords
        nnodes = self.nodes.shape[0]
        ndim = self.nodes.shape[1]
        nelems = np.max([len(c) for c in self.connectivity.values()])
        node_coords = [' '.join(coords.tolist()) for coords in self.nodes.astype(str)]

        # Initialize data to be stored as DataArrays in .vtu
        assembly_connectivity = []
        offsets = []
        elem_types = []
        material_ids = []

        # Create DataArrays for element type (per VTK standards), element connectivity matrix, and offset array (per VTK standards).
        i = 1
        for elem_type, connectivity in self.connectivity.items():
            for elements in connectivity.astype(str):
                assembly_connectivity.append(' '.join(elements.tolist()))
                offsets.append(str(elements.shape[0]*i))
                if elem_type in ['C3D8RT', 'C3D8R', 'C3D8', 'C3D8T']:
                    elem_types.append('12')
                i += 1

        # Create DataArrays for material ID.
        material_element_ids = self._map_materials_to_element_id()
        material_ids_ = {m: str(i + 1) for i, m in enumerate(self.material_definitions.keys())}
        for i in range(nelems):
            material = material_element_ids[i]
            material_ids.append(material_ids_[material])

        file_lines = [
            rf'<VTKFile type="UnstructuredGrid" version="{version}" byte_order="LittleEndian">',
            r'<UnstructuredGrid>',
            rf'<Piece NumberOfPoints="{nnodes}" NumberOfCells="{nelems}">',
            rf'<Points>',
            rf'<DataArray NumberOfComponents="{ndim}" type="Float32" format="ascii">',
        ] + node_coords + [
            rf'</DataArray>',
            rf'</Points>',
            rf'<Cells>',
            rf'<DataArray Name="connectivity" type="Int32" format="ascii">',
        ] + assembly_connectivity + [
            rf'</DataArray>',
            rf'<DataArray Name="offsets" type="Int32" format="ascii">',
        ] + offsets + [
            rf'</DataArray>',
            rf'<DataArray Name="types" type="UInt8" format="ascii">',
        ] + elem_types + [
            rf'</DataArray>',
            rf'</Cells>',
            rf'<CellData>',
            rf'<DataArray Name="material_id" type="Int32" format="ascii">',
        ] + material_ids + [
            rf'</DataArray>',
            rf'</CellData>',
            r'</Piece>',
            r'</UnstructuredGrid>',
            r'</VTKFile>',
        ]
        with open(output_path, 'w+') as f:
            f.write('\n'.join(file_lines))
            print(f'sucessfully converted Abaqus .inp file to vtk: {output_path}')