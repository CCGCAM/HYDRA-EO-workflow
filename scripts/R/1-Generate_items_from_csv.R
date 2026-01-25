# ------------------------------------------------------------
# HYDRA-EO — Generate STAC Items from a CSV inventory (static STAC)
#
# What it does:
#   - Reads a CSV inventory describing acquisitions (one row = one Item).
#   - For each row, builds a valid STAC Item JSON:
#       * geometry + bbox
#       * properties (including HYDRA disease + field + meteo metadata)
#       * assets (COG GeoTIFF, field tables, protocols, etc.)
#       * links to the parent Collection and root Catalog
#   - Writes items to: stac/items/<collection_id>/<item_id>.json
#
# Requirements:
#   - stac/catalog.json must exist
#   - stac/collections/<collection_id>/collection.json must exist
# ------------------------------------------------------------

library(readr)
library(jsonlite)
library(fs)

# ---- Utility: return left value unless NULL or empty string ----
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
ITEMS_DIR <- file.path(STAC_ROOT, "items")

CSV_PATH <- file.path("data", "stac_inventory.csv")

# ---- Basic checks ----
if (!file.exists(CATALOG_PATH)) stop("Missing: ", CATALOG_PATH)
if (!file.exists(CSV_PATH)) stop("Missing: ", CSV_PATH)

inv <- read_csv(CSV_PATH, show_col_types = FALSE)

# Helper to parse bbox "xmin ymin xmax ymax"
parse_bbox <- function(bbox_str) {
  v <- as.numeric(strsplit(bbox_str, "\\s+")[[1]])
  if (length(v) != 4 || any(is.na(v))) stop("Invalid bbox: ", bbox_str)
  list(xmin=v[1], ymin=v[2], xmax=v[3], ymax=v[4])
}

# Create a simple polygon geometry from bbox (GeoJSON)
bbox_to_polygon_geom <- function(bb) {
  list(
    type = "Polygon",
    coordinates = list(list(
      c(bb$xmin, bb$ymin),
      c(bb$xmax, bb$ymin),
      c(bb$xmax, bb$ymax),
      c(bb$xmin, bb$ymax),
      c(bb$xmin, bb$ymin)
    ))
  )
}

# Convert a semicolon-separated list into a JSON array
split_semicolon <- function(x) {
  if (is.na(x) || nchar(x) == 0) return(NULL)
  trimws(strsplit(x, ";", fixed = TRUE)[[1]])
}

# Create an asset entry if href is provided
make_asset <- function(href, type, roles = c("data")) {
  if (is.na(href) || nchar(href) == 0) return(NULL)
  list(
    href = href,
    type = type,
    roles = roles
  )
}

# ---- Main loop: build items ----
n_written <- 0L

for (i in seq_len(nrow(inv))) {
  row <- inv[i, ]

  collection_id <- row$collection_id
  item_id <- row$item_id

  # Ensure the collection exists (static STAC convention)
  collection_path <- file.path(COLLECTIONS_DIR, collection_id, "collection.json")
  if (!file.exists(collection_path)) {
    stop("Collection JSON not found for collection_id='", collection_id, "': ", collection_path)
  }

  bb <- parse_bbox(row$bbox)
  geom <- bbox_to_polygon_geom(bb)

  # ---- Properties (STAC + HYDRA custom namespace) ----
  props <- list(
    datetime = row$datetime,

    # Standard-ish EO fields
    "eo:cloud_cover" = as.numeric(row$cloud_cover %||% NA),

    # HYDRA: site & experimental context
    "hydra:site_id" = row$site_id %||% NA_character_,
    "hydra:crop" = row$crop %||% NA_character_,
    "hydra:plot_id" = row$plot_id %||% NA_character_,
    "hydra:treatment" = row$treatment %||% NA_character_,

    # HYDRA: disease labels
    "hydra:disease_name" = row$disease_name %||% NA_character_,
    "hydra:disease_stage" = row$disease_stage %||% NA_character_,
    "hydra:incidence_pct" = as.numeric(row$incidence_pct %||% NA),
    "hydra:severity_score" = as.numeric(row$severity_score %||% NA),
    "hydra:severity_scale" = row$severity_scale %||% NA_character_,
    "hydra:assessment_method" = row$assessment_method %||% NA_character_,
    "hydra:label_source" = row$label_source %||% NA_character_,
    "hydra:label_confidence" = as.numeric(row$label_confidence %||% NA),

    # HYDRA: meteo / conditions
    "hydra:air_temperature_c" = as.numeric(row$air_temperature_c %||% NA),
    "hydra:relative_humidity_pct" = as.numeric(row$relative_humidity_pct %||% NA),
    "hydra:wind_speed_ms" = as.numeric(row$wind_speed_ms %||% NA),
    "hydra:precip_24h_mm" = as.numeric(row$precip_24h_mm %||% NA),
    "hydra:vpd_kpa" = as.numeric(row$vpd_kpa %||% NA),

    # HYDRA: field data traceability
    "hydra:has_field_data" = as.logical(row$has_field_data %||% NA),
    "hydra:field_protocol" = row$field_protocol %||% NA_character_,
    "hydra:field_measurements" = split_semicolon(row$field_measurements),
    "hydra:field_qc_status" = row$field_qc_status %||% NA_character_,
    "hydra:field_dataset_id" = row$field_dataset_id %||% NA_character_
  )

  # Remove NULL entries (important for clean JSON)
  props <- props[!vapply(props, is.null, logical(1))]

  # ---- Assets (only create entries when href is non-empty) ----
  assets <- list(
    reflectance = make_asset(
      row$asset_reflectance_href,
      "image/tiff; application=geotiff; profile=cloud-optimized",
      roles = c("data")
    ),
    sif = make_asset(
      row$asset_sif_href,
      "image/tiff; application=geotiff; profile=cloud-optimized",
      roles = c("data")
    ),
    lst = make_asset(
      row$asset_lst_href,
      "image/tiff; application=geotiff; profile=cloud-optimized",
      roles = c("data")
    ),
    mask = make_asset(
      row$asset_mask_href,
      "image/tiff; application=geotiff; profile=cloud-optimized",
      roles = c("metadata")
    ),
    field_table = make_asset(
      row$asset_field_table_href,
      "text/csv",
      roles = c("metadata")
    ),
    field_protocol = make_asset(
      row$asset_protocol_href,
      "application/pdf",
      roles = c("metadata")
    )
  )

  # Drop NULL assets
  assets <- assets[!vapply(assets, is.null, logical(1))]

  # ---- STAC Item ----
  item <- list(
    stac_version = "1.0.0",
    type = "Feature",
    id = item_id,

    # Extensions (keep consistent with your Collection)
    stac_extensions = list(
      "https://stac-extensions.github.io/eo/v1.1.0/schema.json",
      "https://stac-extensions.github.io/projection/v1.1.0/schema.json",
      "https://stac-extensions.github.io/raster/v1.1.0/schema.json",
      "https://stac-extensions.github.io/view/v1.0.0/schema.json"
    ),

    bbox = c(bb$xmin, bb$ymin, bb$xmax, bb$ymax),
    geometry = geom,
    properties = props,
    assets = assets,

    links = list(
      list(rel = "self", href = paste0("./", item_id, ".json"), type = "application/json"),
      list(rel = "collection", href = paste0("../../collections/", collection_id, "/collection.json"), type = "application/json"),
      list(rel = "parent", href = paste0("../../collections/", collection_id, "/collection.json"), type = "application/json"),
      list(rel = "root", href = "../../catalog.json", type = "application/json")
    )
  )

  # ---- Write to disk ----
  out_path <- file.path(ITEMS_DIR, collection_id, paste0(item_id, ".json"))
  write_json_pretty(item, out_path)
  n_written <- n_written + 1L
}

cat("Wrote", n_written, "STAC Item JSON files into", ITEMS_DIR, "\n")
