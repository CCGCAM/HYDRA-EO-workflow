
# Minimal example: load raster, compute NDVI, and save a thumbnail plot.
import rasterio
import numpy as np
import matplotlib.pyplot as plt
import sys, os

def compute_ndvi(nir, red):
    nir = nir.astype("float32")
    red = red.astype("float32")
    ndvi = (nir - red) / (nir + red + 1e-6)
    return np.clip(ndvi, -1, 1)

def main(path):
    with rasterio.open(path) as src:
        # Example band order: 1=Blue, 2=Green, 3=Red, 4=NIR
        red = src.read(3)
        nir = src.read(4)
        ndvi = compute_ndvi(nir, red)
    plt.imshow(ndvi)
    plt.title("NDVI (demo)")
    out = os.path.join(os.path.dirname(path), "ndvi_demo.png")
    plt.savefig(out, dpi=150, bbox_inches="tight")
    print(f"Saved {out}")

if __name__ == "__main__":
    if len(sys.argv) < 2:
        print("Usage: python example_pipeline.py /path/to/stack.tif")
        sys.exit(1)
    main(sys.argv[1])
