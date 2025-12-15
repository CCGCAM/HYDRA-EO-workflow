
# HYDRA-EO Deployment Guide  
*Core Platform: ESA Agriculture Virtual Lab (AVL) + EarthCODE*

---

## 1. Prerequisites
- ESA contract login (for AVL + EarthCODE access).  
- GitLab repository (development).  
- Docker / Conda / R `renv` environment files prepared.  
- Small FAIR-compliant datasets (≤1 GB per bundle) for demonstration.  

---

## 2. Prepare Local Repository
1. **Structure**  
   Ensure repo follows:
   ```
   scripts/        # R + Python
   notebooks/      # Jupyter / RMarkdown workflows
   data/processed/ # small samples only
   docs/           # ATBD, validation, dataset description
   environment.yml # conda env
   scripts/R/renv_packages.R
   ```

2. **Containerisation**  
   - Option A: **Conda environment**
     ```bash
     conda env create -f environment.yml
     conda activate hydra-eo
     ```
   - Option B: **Dockerfile** (minimal example)
     ```dockerfile
     FROM continuumio/miniconda3:23.10.0-1
     COPY environment.yml .
     RUN conda env create -f environment.yml
     SHELL ["conda", "run", "-n", "hydra-eo", "/bin/bash", "-c"]
     WORKDIR /workspace
     CMD ["jupyter-lab", "--ip=0.0.0.0", "--no-browser"]
     ```

3. **Check entrypoints**
   - `notebooks/demo_ndvi.ipynb` → simple NDVI cube.  
   - `scripts/python/example_pipeline.py` → NDVI raster script.  
   - `scripts/R/demo_scope.R` → trait inversion workflow.  

---

## 3. Deployment in ESA Agriculture Virtual Lab (AVL)
1. **Request project space** from ESA contract officer.  
2. **Upload repo**  
   - Mirror GitLab or upload ZIP into AVL workspace.  
3. **Launch JupyterLab** in AVL.  
4. **Install environment**  
   - Run `conda env create -f environment.yml` (or use Docker image if supported).  
   - For R, run `source("scripts/R/renv_init.R")`.  
5. **Test workflows**  
   - Run Sentinel-2 cube processing.  
   - Run PRISMA resampling example.  
6. **Register as AVL App**  
   - Tag notebook as “Application” so users can execute with their own inputs.  

---

## 4. Publication in ESA EarthCODE
1. **Prepare deposit package**  
   - Code snapshot (`v0.1.0` ZIP from GitLab).  
   - Dataset sample (100×100 m cube, ≤50 MB).  
   - Documents (ATBD, Validation Report, Dataset Description).  

2. **Metadata template**  
   - Title: “HYDRA-EO Experimental Dataset v1”  
   - Authors: Camino et al.  
   - Keywords: `crop stress`, `hyperspectral`, `thermal`, `RTM`, `PROSAIL`, `SCOPE`  
   - Temporal coverage: YYYY-YYYY  
   - Spatial coverage: site coordinates / bounding box  
   - License: CC-BY 4.0 (unless NDA restricted)  

3. **Upload to EarthCODE** via ESA Open Science Catalogue portal.  
4. **Request DOI assignment** for dataset + code bundle.  
5. **Cross-link AVL and EarthCODE**:  
   - In AVL, point workflow inputs to EarthCODE dataset.  
   - In EarthCODE, reference AVL workflow for reproducibility.  

---

## 5. Integration Workflow
- **GitLab** = development + version control.  
- **AVL** = execution + collaboration.  
- **EarthCODE** = publication + long-term archive.  

---

## 6. Validation Checklist (before submission)
- [ ] Repo cleaned (`.gitignore`, no raw data).  
- [ ] Notebooks run top-to-bottom.  
- [ ] Metadata completed (`CITATION.cff`, `DATASETS.md`).  
- [ ] Sample dataset anonymised & small.  
- [ ] AVL workflow tested on Sentinel-2 cube.  
- [ ] EarthCODE upload validated with DOI assigned.  

---

## 7. Next Steps
- Iteration 1 (M3): Deploy NDVI + trait inversion demo in AVL.  
- Iteration 2 (M5): Upload Experimental Dataset v1 to EarthCODE.  
- Iteration 3 (M6): Publish validated monitoring prototype + ATBD with DOI.  

---

**In summary**  
- **AVL** = run workflows.  
- **EarthCODE** = publish datasets and codes.  
- **GitLab** = develop and maintain.  
