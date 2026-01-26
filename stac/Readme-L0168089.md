# HYDRA-EO STAC

This folder contains the STAC metadata for HYDRA-EO.

- `catalog.json` is the root catalog.
- `collections/` contains one Collection per sensor × site.
- `items/` contains Items (acquisitions) linked to each Collection.

Large EO assets (COG GeoTIFF, masks, field tables) should be stored in scalable object storage and referenced via `assets.href`.
