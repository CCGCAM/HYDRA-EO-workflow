
# Synthetic Modeling Module  
*HYDRA-EO Core Platform: coupling SCOPEinR + ToolsRTM*

---

## 1. Purpose
This module provides a **synthetic data generation and modeling capability** within the HYDRA-EO monitoring system, based entirely on the **SCOPEinR** and **ToolsRTM** packages developed in the project’s GitLab repositories.  

It enables:
- Controlled simulation of **plant traits, fluxes, and spectral signals** under abiotic/biotic stress.  
- Synthetic datasets for **algorithm training, testing, and validation**.  
- Comparison between **modeled stress responses** and **EO retrievals** from Sentinel-2, PRISMA, EnMAP, FLEX, etc.  

---

## 2. Repository Components

| Repository | Function | Integration in Monitoring System |
|------------|----------|----------------------------------|
| **ToolsRTM** (`gitlab.com/caminoccg/toolsrtm`) | 1D canopy RTMs (PROSAIL, custom LUTs, spectral resampling, sensor FWHM) | Generates structural synthetic spectra & band-degraded datasets |
| **SCOPEinR** (`gitlab.com/caminoccg/scopeinr`) | Photosynthesis/energy balance RTM (leaf → canopy, includes SIF, thermal fluxes) | Generates functional synthetic traits & SIF/thermal observables |

---

## 3. Workflow

1. **Trait Scenarios**  
   Define input trait ranges (Cab, LAI, Cw, Cm, Vcmax, soil/atmosphere conditions).  

2. **Run Structural RTMs (ToolsRTM)**  
   - PROSAIL LUTs for reflectance.  
   - Band resampling to Sentinel-2, PRISMA, EnMAP, CHIME.  
   - Outputs: canopy reflectance cubes, spectral indices.  

3. **Run Functional RTMs (SCOPEinR)**  
   - Photosynthesis and energy balance simulations.  
   - Outputs: GPP, SIF (760/687 nm), canopy temperature, transpiration.  

4. **Combine Outputs**  
   - Structural + functional signals merged.  
   - Annotated with stressor metadata (e.g. drought, heat, infection scenario).  

5. **Publish**  
   - Small samples (100×100 px synthetic cubes) → `data/processed/`.  
   - Full LUTs (large) → ESA EarthCODE with metadata.  
   - Workflows (scripts, notebooks) → ESA AVL for execution.  

---

## 4. Implementation in AVL
- **Upload repos** (`scopeinr`, `toolsrtm`) to AVL workspace.  
- Provide Jupyter/RMarkdown notebooks with pre-set scenarios (e.g. “drought stress”, “disease infection”).  
- Register workflows as AVL Apps (so ESA partners can run simulations interactively).  

---

## 5. Publication in EarthCODE
- Package **synthetic datasets** (LUTs, canopy flux simulations).  
- Provide metadata (input trait ranges, sensor configurations, version of ToolsRTM/SCOPEinR).  
- Upload with DOI for reproducibility.  

---

## 6. Example Applications
- **Cross-validation**: Compare Sentinel-2 NDVI decline with simulated Cab+LAI changes.  
- **FLEX preparatory science**: Generate SIF time series under heat stress scenarios.  
- **Sensor intercomparison**: Simulate identical trait scenarios resampled to PRISMA vs CHIME.  
- **Uncertainty analysis**: Quantify sensitivity of stressor detection to band choice, spectral resolution, and RTM assumptions.  

---

## 7. Advantages
- 100% open, GitLab-based, maintained by HYDRA-EO team.  
- Directly links **structural (ToolsRTM)** and **functional (SCOPEinR)** models.  
- Scalable from **leaf → canopy → pixel**.  
- Ready for integration in **ESA AVL (execution)** and **EarthCODE (archival)**.  
