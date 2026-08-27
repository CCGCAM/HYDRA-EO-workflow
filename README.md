# HYDRA‑EO

<p align="center">

![HYDRA‑EO logo](assets/Hydra-eo-logo.png)

</p>

Hybrid Machine Learning for **Multi‑Stressor Crop Disease and Pest Detection** using **hyperspectral + thermal** sensing, **radiative transfer models (RTMs)**, and **multi‑scale Earth Observation (EO)**

(UAV → airborne → satellite).

This repository hosts the open materials of the ESA HYDRA‑EO concept: code, data schemas, docs, and the scientific roadmap.

<p align="center">
  <a href="https://hydra-eo.eu/"><img src="https://img.shields.io/badge/HYDRA--EO-Project_website-003247" alt="HYDRA-EO website"></a>
  <a href="https://ccgcam.github.io/HYDRA-EO-workflow/"><img src="https://img.shields.io/badge/Quarto-Workflow_docs-39729E?logo=quarto&logoColor=white" alt="HYDRA-EO workflow documentation"></a>
  <a href="https://ccgcam.github.io/RTM-Suite/"><img src="https://img.shields.io/badge/RTM--Suite-R_+_Python-2BB3A3" alt="RTM-Suite"></a>
  <a href="https://github.com/CCGCAM/RTM-Suite"><img src="https://img.shields.io/badge/GitHub-RTM--Suite-181717?logo=github&logoColor=white" alt="RTM-Suite on GitHub"></a>
</p>

## Documentation and related platforms

| Resource | Purpose |
|:--|:--|
| [HYDRA-EO project website](https://hydra-eo.eu/) | Project overview, objectives, consortium, news and public outputs |
| [HYDRA-EO workflow documentation](https://ccgcam.github.io/HYDRA-EO-workflow/) | Quarto documentation for pipelines, tools, documents and timeline |
| [RTM-Suite](https://ccgcam.github.io/RTM-Suite/) | R and Python packages, reference manuals, tutorials, applications and examples |
| [RTM-Suite source](https://github.com/CCGCAM/RTM-Suite) | Coordinated development and cross-language verification of the modelling ecosystem |

## Project Objectives

HYDRA-EO is designed to advance crop stress monitoring through a hybrid framework that integrates radiative transfer modeling, machine learning, and multi-sensor EO data. The core objectives are:

1.  **Detect and attribute multiple stressors** (biotic and abiotic) affecting crops, including diseases, pests, drought, and heat stress.
2.  **Integrate multi-scale EO observations** from UAVs, airborne campaigns, and satellites (Sentinel-2, PRISMA, EnMAP, FLEX, CHIME) into a harmonized monitoring system.
3.  **Develop and validate hybrid algorithms** that couple RTMs (PROSAIL, SCOPE) with machine learning for retrieving vegetation traits and stress indicators.
4.  **Deploy open workflows and datasets** within ESA’s **Agriculture Virtual Lab (AVL)** and **EarthCODE**, ensuring reproducibility, open science, and community adoption.
5.  **Support ESA mission preparatory activities** by generating synthetic datasets, sensor intercomparison studies, and science roadmaps relevant for future EO missions.

## Key Outputs

HYDRA-EO delivers open and reproducible resources that can be directly reused by the EO and crop science community:

- **Synthetic Datasets**: PROSAIL and SCOPE-based look-up tables (LUTs), trait inversion experiments, and multi-sensor resampled reflectance libraries (Sentinel-2, PRISMA, EnMAP, CHIME, FLEX, ECOSTRESS).
- **Shiny Applications**: Interactive tools for trait–reflectance exploration, NDVI/SIF analysis, and classification workflows, accessible through the `apps/` folder.
- **Notebooks & Tutorials**: R Markdown and Python notebooks for hyperspectral/thermal data processing, radiative transfer simulations, and hybrid ML workflows (`notebooks/`).
- **Reusable Scripts**: Core R and Python functions for data preprocessing, sensor harmonization, and ML model training (`scripts/`).
- **Scientific Roadmap**: Documentation of methodological advances, validation strategies, and ESA mission preparatory contributions (`docs/` + `routemap/`).
- **Open Science Integration**: Workflows packaged for ESA’s **Agriculture Virtual Lab (AVL)** and datasets published on **EarthCODE** with FAIR metadata and DOI assignment.

------------------------------------------------------------------------

## Repository structure

```         
HYDRA‑EO/
├─ assets/                # logos, images and scientific figures
├─ apps/                  # Shiny apps
├─ quarto/                # editable Quarto website sources
├─ docs-quarto/           # rendered Quarto website for GitHub Pages
├─ scripts/               # reusable code (R / Python)
│  ├─ R/
│  └─ python/
├─ notebooks/             # exploration, tutorials, reports
├─ stac/                  # stac collections (e.g., aerial campaigns)
├─ data/                  # example data, inventories and data-policy notes
│  ├─ raw/                # raw acquisitions (not tracked)
│  ├─ interim/            # intermediate products
│  └─ processed/          # final products / examples
│  └─ matchup/            # spatio-temporal matchups between datasets
├─ docs/                  # methods, specs, templates
└─ routemap/              # milestones, deliverables, KPIs
```

> **Note:** Large files are not tracked. See `.gitignore` and `data/README.md` for the data policy.

## Environments & CI

**Python (conda)**

``` bash
conda env create -f environment.yml
conda activate hydra-eo
pip install -r scripts/python/requirements.txt
```

**R (renv)**

``` r
source("scripts/R/renv_init.R")  # installs renv, snapshots packages
```

**GitLab templates & CI** - Issue templates: `.gitlab/issue_templates/{Bug.md, Feature.md}` - MR template: `.gitlab/merge_request_templates/Standard.md` - CI: `.gitlab-ci.yml` with basic Python/R lint stages and docs placeholder.

## Review & Templates

- **Pull request review**: see `PULL_REVIEW.md` for scientific content checklist (RTM–ML, datasets, reproducibility).\
- **GitLab issue templates**: Bug, Feature, Dataset under `.gitlab/issue_templates/`.\
- **Merge request template**: `.gitlab/merge_request_templates/Standard.md`.

## Radiative transfer modelling and RTM-Suite

HYDRA-EO uses [**RTM-Suite**](https://ccgcam.github.io/RTM-Suite/) as its
open modelling ecosystem. RTM-Suite keeps the R and Python implementations,
interactive applications, documentation and verified tutorials together. This
provides a consistent physical basis for simulations and trait-retrieval
workflows across both languages.

| Tool | Language | Main role | Source | Documentation |
|:--|:--:|:--|:--:|:--:|
| **ToolsRTM** | ![R](https://img.shields.io/badge/R-276DC3?logo=r&logoColor=white) | Leaf, canopy and soil RTMs; LUTs; sensor convolution; ML/DL inversion | [![GitLab](https://img.shields.io/badge/GitLab-source-FC6D26?logo=gitlab&logoColor=white)](https://gitlab.com/caminoccg/toolsrtm) | [Reference](https://ccgcam.github.io/RTM-Suite/toolsrtm/) |
| **SCOPEinR** | ![R](https://img.shields.io/badge/R-276DC3?logo=r&logoColor=white) | Reflectance, photosynthesis, SIF, temperature and energy balance | [![GitLab](https://img.shields.io/badge/GitLab-source-FC6D26?logo=gitlab&logoColor=white)](https://gitlab.com/caminoccg/scopeinr) | [Reference](https://ccgcam.github.io/RTM-Suite/scopeinr/) |
| **toolsrtm** | ![Python](https://img.shields.io/badge/Python-3776AB?logo=python&logoColor=white) | Python implementation of the ToolsRTM core, STAC retrieval and inversion | [![GitHub](https://img.shields.io/badge/GitHub-source-181717?logo=github&logoColor=white)](https://github.com/CCGCAM/ToolsRTMinPython) | [Documentation hub](https://ccgcam.github.io/RTM-Suite/#docs) |
| **scopeinpython** | ![Python](https://img.shields.io/badge/Python-3776AB?logo=python&logoColor=white) | Python workflows for SCOPE simulations and batch processing | [![GitHub](https://img.shields.io/badge/GitHub-RTM--Suite-181717?logo=github&logoColor=white)](https://github.com/CCGCAM/RTM-Suite) | [Documentation hub](https://ccgcam.github.io/RTM-Suite/#docs) |
| **ToolsRTM.app** | ![Shiny](https://img.shields.io/badge/R_Shiny-apps-1F77B4?logo=rstudio&logoColor=white) | Interactive RTM simulation, LUT, inversion and STAC applications | [![GitHub](https://img.shields.io/badge/GitHub-apps-181717?logo=github&logoColor=white)](https://github.com/CCGCAM/RTM-Suite/tree/main/ToolsRTM.app) | [Applications](https://ccgcam.github.io/RTM-Suite/#apps) |

------------------------------------------------------------------------

Together, these tools allow HYDRA-EO to generate **synthetic datasets** that
couple **structural signals** (reflectance and canopy architecture) with
**functional signals** (photosynthesis, SIF and temperature). These datasets
support algorithm validation, stress detection and multi-sensor integration
within the ESA monitoring framework.

### Documentation and tutorials

RTM-Suite provides maintained entry points for all documentation:

- [Reference manuals](https://ccgcam.github.io/RTM-Suite/#docs)
- [R tutorials](https://ccgcam.github.io/RTM-Suite/tutorials-overview.html)
- [Python tutorials](https://ccgcam.github.io/RTM-Suite/tutorials-python.html)
- [R and Python comparison](https://ccgcam.github.io/RTM-Suite/comparison.html)
- [Interactive applications](https://ccgcam.github.io/RTM-Suite/#apps)
- [Copy-ready examples](https://ccgcam.github.io/RTM-Suite/#examples)

### Citation

If you use **ToolsRTM** packages, please cite the following references:

Camino et al., (2024). RT-Simulator: An Online Platform to Simulate Canopy Reflectance from Biochemical and Structural Plant Properties Using Radiative Transfer Models, *IGARSS 2024 - 2024 IEEE International Geoscience and Remote Sensing Symposium*, Athens, Greece, 2024, pp. 2811-2814, [doi: 10.1109/IGARSS53475.2024.10642442](https://ieeexplore.ieee.org/document/10642442).

Arano et al., (2024). Enhancing Chlorophyll Content Estimation With Sentinel-2 Imagery: A Fusion of Deep Learning and Biophysical Models, *IGARSS 2024 - 2024 IEEE International Geoscience and Remote Sensing Symposium*, Athens, Greece, 2024, pp. 4486-4489, [doi: 10.1109/IGARSS53475.2024.10641613](https://ieeexplore.ieee.org/document/10641613).

Camino et al., (in prep). Integrating physiological plant traits with Sentinel-2 imagery for monitoring gross primary production and detecting forest disturbances.

### License

[![](https://img.shields.io/badge/License-MIT-yellow.svg)](LICENSE)

This repository is licensed under the MIT License. ToolsRTM, SCOPEinR and the
other RTM-Suite components are distributed through their respective
repositories; consult each component's license before reuse or redistribution.
