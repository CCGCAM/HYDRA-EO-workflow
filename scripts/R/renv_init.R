
# Initialize renv environment for HYDRA-EO
if (!requireNamespace("renv", quietly = TRUE)) install.packages("renv")
renv::init(bare = TRUE)
source("scripts/R/renv_packages.R")
renv::snapshot(prompt = FALSE)
