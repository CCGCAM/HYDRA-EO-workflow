# ------------------------------------------------------------
# HYDRA-EO — Generate/Update STAC Collections from CSV
# and update the root catalog.json (static STAC)
#
# What it does:
#   - Reads a collections CSV (one row = one Collection)
#   - Writes: stac/collections/<collection_id>/collection.json
#   - Ensures catalog.json has a child link for each collection
#
# Assumptions:
#   - Static STAC layout:
#       stac/catalog.json
#       stac/collections/<collection_id>/collection.json
#       stac/items/<collection_id>/*.json
# ------------------------------------------------------------

library(readr)
library(jsonlite)
library(fs)

`%||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0 || (is.character(x) && nchar(x) == 0)) y else x
}

read_json <- function(path) fromJSON(path, simplifyVector = FALSE)

write_json_pretty <- function(x, path) {
  dir_create(path_dir(path))
  writeLines(toJSON(x, auto_unbox = TRUE, pretty = TRUE, null = "null"), path)
}

# ---- Config ----
STAC_ROOT <- "stac"
CATALOG_PATH <- file.path(STAC_ROOT, "catalog.json")
COLLECTIONS_DIR <- file.path(STAC_ROOT, "collections")
CSV_PATH <- file.path("data", "stac_collections.csv")

if (!file.exists(CATALOG_PATH)) stop("Missing: ", CATALOG_PATH)
if (!file.exists(CSV_PATH)) stop("Missing: ", CSV_PATH)

catalog <- read_json(CATALOG_PATH)
df <- read_csv(CSV_PATH, show_col_types = FALSE)

# Parse bbox "xmin ymin xmax ymax"
parse_bbox <- function(bbox_str) {
  v <- as.numeric(strsplit(gsub('"', "", bbox_str), "\\s+")[[1]])
  if (length(v) != 4 || any(is.na(v))) stop("Invalid bbox: ", bbox_str)
  list(xmin=v[1], ymin=v[2], xmax=v[3], ymax=v[4])
}

# Split semicolon list
split_semicolon <- function(x) {
  if (is.na(x) || nchar(x) == 0) return(NULL)
  trimws(strsplit(x, ";", fixed = TRUE)[[1]])
}

# ---- 1) Write collections ----
for (i in seq_len(nrow(df))) {
  row <- df[i, ]
  cid <- row$collection_id

  bb <- parse_bbox(row$bbox)

  collection <- list(
    stac_version = "1.0.0",
    type = "Collection",
    id = cid,
    title = row$title %||% cid,
    description = row$description %||% "",
    license = row$license %||% "CC-BY-4.0",
    keywords = list("HYDRA-EO", "crop-stress", "disease", "STAC"),
    extent = list(
      spatial = list(bbox = list(c(bb$xmin, bb$ymin, bb$xmax, bb$ymax))),
      temporal = list(interval = list(c(row$interval_start, row$interval_end)))
    ),

    # Summaries declare what fields/values may appear in Items
    summaries = list(
      platform = split_semicolon(row$platform),
      instruments = split_semicolon(row$instruments),
      "hydra:site_id" = list(row$site_id %||% NA_character_),
      "hydra:sensor_family" = split_semicolon(row$sensor_family),
      "hydra:crop" = list(row$crop %||% NA_character_),
      "hydra:has_field_data" = list(TRUE, FALSE),

      # Disease-related fields will exist at item-level; here we just declare the concept
      "hydra:disease_name" = list("unknown"),
      "hydra:assessment_method" = list("visual", "lab", "model")
    ),

    stac_extensions = list(
      "https://stac-extensions.github.io/eo/v1.1.0/schema.json",
      "https://stac-extensions.github.io/projection/v1.1.0/schema.json",
      "https://stac-extensions.github.io/raster/v1.1.0/schema.json",
      "https://stac-extensions.github.io/view/v1.0.0/schema.json"
    ),

    links = list(
      list(rel = "self", href = "./collection.json", type = "application/json"),
      list(rel = "parent", href = "../../catalog.json", type = "application/json"),
      list(rel = "root", href = "../../catalog.json", type = "application/json")
    )
  )

  out_path <- file.path(COLLECTIONS_DIR, cid, "collection.json")
  write_json_pretty(collection, out_path)
}

cat("Collections written/updated.\n")

# ---- 2) Update catalog.json child links ----
# Keep existing self/root links, then ensure one child link per collection.
keep_links <- Filter(function(l) l$rel %in% c("self", "root"), catalog$links)

# Build desired child links from the CSV
desired_children <- lapply(df$collection_id, function(cid) {
  list(
    rel = "child",
    href = paste0("./collections/", cid, "/collection.json"),
    type = "application/json",
    title = df$title[df$collection_id == cid][[1]] %||% cid
  )
})

catalog$links <- c(keep_links, desired_children)
write_json_pretty(catalog, CATALOG_PATH)

cat("Catalog.json updated with child links.\n")
