# ------------------------------------------------------------
# HYDRA-EO — STAC static catalog demo (Collections -> Items)
# Purpose:
#   Demonstrate how we organize and browse a *static* STAC catalog
#   (catalog.json + collection.json + item.json files stored in GitHub)
#   using R. This script lists Collections and then lists Items per Collection.
#
# Notes:
#   - 'rstac' is mainly designed for STAC APIs (endpoints like /collections, /search).
#   - A static STAC (files + links) can be browsed by reading JSON and following links.
# ------------------------------------------------------------

library(jsonlite)
# library(rstac)  # Optional: useful for STAC API, not required for static STAC

# ---- Config ----
STAC_ROOT <- "stac"
CATALOG_PATH <- file.path(STAC_ROOT, "catalog.json")
COLLECTIONS_DIR <- file.path(STAC_ROOT, "collections")
ITEMS_DIR <- file.path(STAC_ROOT, "items")

# ------------------------------------------------------------
# Utility: %||% operator (use left value unless NULL)
# ------------------------------------------------------------
`%||%` <- function(x, y) if (is.null(x)) y else x



# ---- Helper to read JSON as a list (preserve nested fields) ----
read_json <- function(path) {
  fromJSON(path, simplifyVector = FALSE)
}

# ---- 1) Read the static STAC catalog.json ----
catalog <- read_json(CATALOG_PATH)

cat("\n--- HYDRA-EO STAC static catalog ---\n")
cat("Catalog ID:   ", catalog$id, "\n")
cat("Catalog Title:", catalog$title, "\n\n")

# ---- 2) Get 'child' links from the catalog (these point to Collections) ----
child_links <- Filter(function(l) identical(l$rel, "child"), catalog$links)

if (length(child_links) == 0) {
  stop("No 'child' links found in catalog.json. Add collection links with rel='child'.")
}

# ---- 3) Load each Collection JSON and print a summary ----
collections <- lapply(child_links, function(lk) {
  # lk$href is relative to the catalog.json location.
  # In our repo structure, catalog.json sits inside 'stac/',
  # so we resolve it as file.path(STAC_ROOT, lk$href).
  collection_path <- file.path(STAC_ROOT, lk$href)

  if (!file.exists(collection_path)) {
    stop("Collection file not found: ", collection_path,
         "\nCheck catalog.json child href paths.")
  }

  col <- read_json(collection_path)

  list(
    id = col$id,
    title = col$title %||% NA_character_,
    description = col$description %||% NA_character_,
    path = collection_path
  )
})

# Print collections
cat("--- Collections discovered ---\n")
for (i in seq_along(collections)) {
  cat(sprintf("[%d] %s — %s\n", i, collections[[i]]$id, collections[[i]]$title))
}
cat("\n")

# ---- 4) For each Collection, list Items stored in stac/items/<collection_id>/ ----
# Assumption (HYDRA-EO convention):
#   Items for collection <collection_id> are stored in:
#     stac/items/<collection_id>/*.json
#
# This is a practical convention for static catalogs (not required by STAC spec),
# but it makes the repository easy to browse and maintain.

load_items_for_collection <- function(collection_id) {
  item_dir <- file.path(ITEMS_DIR, collection_id)

  if (!dir.exists(item_dir)) {
    # Not an error: the collection may exist but no items have been created yet
    return(list(items = list(), item_files = character(0), item_dir = item_dir))
  }

  item_files <- list.files(item_dir, pattern = "\\.json$", full.names = TRUE)
  items <- lapply(item_files, read_json)

  list(items = items, item_files = item_files, item_dir = item_dir)
}

# Build a "stack" summary (one row per item) to visualize the organization
stack_rows <- list()

for (col in collections) {
  res <- load_items_for_collection(col$id)

  cat("--- Items for collection:", col$id, "---\n")
  cat("Item folder:", res$item_dir, "\n")

  if (length(res$items) == 0) {
    cat("No items found.\n\n")
    next
  }

  cat("Number of items:", length(res$items), "\n")

  # Print first item quick check
  first <- res$items[[1]]
  cat("First item id:       ", first$id, "\n")
  cat("First item datetime: ", first$properties$datetime, "\n")
  cat("First item assets:   ", paste(names(first$assets), collapse = ", "), "\n\n")

  # Add item rows to stack summary
  for (it in res$items) {
    # Safe extraction of optional HYDRA fields
    disease_name <- it$properties[["hydra:disease_name"]] %||% NA_character_
    has_field <- it$properties[["hydra:has_field_data"]] %||% NA
    cloud <- it$properties[["eo:cloud_cover"]] %||% NA

    stack_rows[[length(stack_rows) + 1]] <- data.frame(
      collection_id = col$id,
      item_id = it$id,
      datetime = it$properties$datetime %||% NA_character_,
      cloud_cover = cloud,
      disease_name = disease_name,
      has_field_data = has_field,
      n_assets = length(names(it$assets)),
      stringsAsFactors = FALSE
    )
  }
}

# ---- 5) Create a data.frame "stack view" ----
if (length(stack_rows) > 0) {
  stack_df <- do.call(rbind, stack_rows)

  cat("\n--- STACK VIEW (Collections x Items) ---\n")
  print(stack_df)

} else {
  cat("\nNo items found across collections. Create at least one item JSON to see the stack.\n")
}



