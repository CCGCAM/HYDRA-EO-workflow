# Run this script once after cloning the repository.
# ================================================

# ---------------------------
# 📦 Step 1: Define Required CRAN Packages
# ---------------------------

required_packages <- c(
  # --- Core ---
  "rmarkdown","knitr","shiny",

  # --- Geospatial / Raster ---
  "terra","sf","stars","rgdal",
  "leaflet","leafem","leaflet.extras2","gdalcubes",
  "exactextractr",

  # --- Visualization ---
  "plotly","ggplot2","DT","htmltools",
  "bslib","thematic","webshot2",
  "viridisLite","RColorBrewer",

  # --- Shiny UI Enhancements ---
  "shinyWidgets","shinydashboard","shinybusy",

  # --- Data wrangling ---
  "dplyr","tidyr","readr","purrr","stringr",

  # --- Machine Learning ---
  "caret","randomForest","ranger","e1071",
  "xgboost","glmnet","kernlab","tidymodels",

  # --- Reproducibility / Dev tools ---
  "renv","testthat","roxygen2"
)
# ------------------------------------------------------------------------------
# 🔍 Step 2: Install Missing CRAN Packages
# ------------------------------------------------------------------------------

installed <- required_packages %in% rownames(installed.packages())
if (any(!installed)) {
  install.packages(required_packages[!installed])
} else {
  cat("✅ All required CRAN packages are already installed.\n")
}

# ------------------------------------------------------------------------------
# 🌱 Step 3: Install HYDRA-EO GitLab Packages (ToolsRTM + SCOPEinR)
# ------------------------------------------------------------------------------

if (!requireNamespace("remotes", quietly = TRUE)) install.packages("remotes")

# ToolsRTM
if (!requireNamespace("ToolsRTM", quietly = TRUE)) {
  remotes::install_gitlab("caminoccg/toolsrtm", upgrade = "never")
}
library(ToolsRTM)
cat("\n✅ ToolsRTM is ready: ", as.character(packageVersion("ToolsRTM")), "\n", sep = "")

# SCOPEinR
if (!requireNamespace("SCOPEinR", quietly = TRUE)) {
  remotes::install_gitlab("caminoccg/scopeinr", upgrade = "never")
}
library(SCOPEinR)
cat("\n✅ SCOPEinR is ready: ", as.character(packageVersion("SCOPEinR")), "\n", sep = "")

# ------------------------------------------------------------------------------
# 🧪 Step 4: Install TinyTeX for PDF Output (if missing)
# ------------------------------------------------------------------------------

if (!requireNamespace("tinytex", quietly = TRUE)) {
  install.packages("tinytex")
  require('tinytex')
}

if (!tinytex::is_tinytex()) {
  message("Installing TinyTeX (LaTeX engine) for PDF report generation...")
  tinytex::install_tinytex()
} else {
  cat("✅ TinyTeX is already installed.\n")
}

# ------------------------------------------------------------------------------
# 📂 Step 5: Load All Required Packages
# ------------------------------------------------------------------------------

invisible(lapply(c(required_packages,"tinytex"), library, character.only = TRUE))

# ================================================
# ✅ Setup Complete!
# HYDRA-EO environment is ready.
# ================================================
