import numpy as np
import pyvista as pv

def main():
    rng = np.random.default_rng(seed=0)
    points = rng.random((100, 3))
    mesh = pv.PolyData(points)
    # mesh.plot(point_size=10, style='points')


if __name__ == "__main__":
    main()