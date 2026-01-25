# ------------------------------------------------------------
# HYDRA-EO — Static STAC Validator (extended)
#
# Validates a static STAC catalog stored as JSON files in a repo:
#   stac/catalog.json
#   stac/collections/<collection_id>/collection.json
#   stac/items/<collection_id>/*.json
#
# Core checks:
#   1) catalog.json child links exist on disk
#   2) collections have valid extent (spatial bbox + temporal interval)
#   3) items:
#       - required fields exist (id, bbox, geometry, properties.datetime, assets)
#       - link to a valid collection.json (rel="collection")
#       - bbox inside collection bbox
#       - datetime inside collection temporal interval
#       - assets have non-empty href
#
# Extra checks (added):
#   A) item$id matches file name (<item_id>.json)
#   B) item is stored under stac/items/<collection_id>/ (folder = collection_id)
#   C) item collection folder exists in catalog child links (no orphan items)
#   D) item 'collection' link target matches the same collection_id as the folder
#
# Output:
#   - prints summary to console
#   - writes a JSON report to stac/validation_report.json (optional)
# ------------------------------------------------------------

library(jsonlite)
library(fs)

# ---- Config ----
STAC_ROOT <- "stac"
CATALOG_PATH <- file.path(STAC_ROOT, "catalog.json")
COLLECTIONS_DIR <- file.path(STAC_ROOT, "collections")
ITEMS_DIR <- file.path(STAC_ROOT, "items")
WRITE_REPORT_JSON <- TRUE
REPORT_PATH <- file.path(STAC_ROOT, "validation_report.json")

# ---- Helpers ----
read_json <- function(path) fromJSON(path, simplifyVector = FALSE)

`%||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0) y else x
}

is_nonempty_string <- function(x) {
  is.character(x) && length(x) == 1 && !is.na(x) && nchar(trimws(x)) > 0
}

as_iso_time <- function(x) {
  if (!is_nonempty_string(x)) return(as.POSIXct(NA))
  out <- suppressWarnings(as.POSIXct(x, format = "%Y-%m-%dT%H:%M:%OSZ", tz = "UTC"))
  if (is.na(out)) out <- suppressWarnings(as.POSIXct(x, tz = "UTC"))
  out
}

bbox_inside <- function(item_bbox, col_bbox) {
  if (length(item_bbox) != 4 || length(col_bbox) != 4) return(FALSE)
  all(!is.na(item_bbox)) && all(!is.na(col_bbox)) &&
    item_bbox[1] >= col_bbox[1] &&
    item_bbox[2] >= col_bbox[2] &&
    item_bbox[3] <= col_bbox[3] &&
    item_bbox[4] <= col_bbox[4]
}

interval_contains <- function(item_time, start_time, end_time) {
  if (is.na(item_time) || is.na(start_time) || is.na(end_time)) return(FALSE)
  item_time >= start_time && item_time <= end_time
}

add_issue <- function(report, level = c("error", "warning"), code, message, context = list()) {
  level <- match.arg(level)
  report$issues[[length(report$issues) + 1]] <- list(
    level = level,
    code = code,
    message = message,
    context = context
  )
  report
}

# ---- Start report ----
report <- list(
  catalog = CATALOG_PATH,
  timestamp_utc = format(Sys.time(), tz = "UTC", usetz = TRUE),
  counts = list(collections = 0L, items = 0L),
  issues = list()
)

if (!file.exists(CATALOG_PATH)) stop("Missing catalog: ", CATALOG_PATH)

catalog <- read_json(CATALOG_PATH)

# ---- 1) Validate catalog child links (collections) ----
child_links <- Filter(function(l) identical(l$rel, "child"), catalog$links %||% list())

if (length(child_links) == 0) {
  report <- add_issue(
    report, "error", "CATALOG_NO_CHILDREN",
    "catalog.json has no child links (rel='child').",
    list(path = CATALOG_PATH)
  )
}

# Resolve and load collections -> index by collection_id
collections_index <- list()  # collection_id -> list(path, bbox, start_time, end_time)

for (lk in child_links) {
  href <- lk$href %||% NA_character_
  if (!is_nonempty_string(href)) {
    report <- add_issue(report, "error", "CATALOG_CHILD_MISSING_HREF",
                        "A catalog child link is missing 'href'.", list(link = lk))
    next
  }

  col_path <- file.path(STAC_ROOT, href)

  if (!file.exists(col_path)) {
    report <- add_issue(report, "error", "CATALOG_CHILD_NOT_FOUND",
                        "Collection link target does not exist on disk.",
                        list(href = href, resolved_path = col_path))
    next
  }

  col <- read_json(col_path)

  if (!is_nonempty_string(col$id)) {
    report <- add_issue(report, "error", "COLLECTION_MISSING_ID",
                        "Collection is missing a valid 'id'.", list(path = col_path))
    next
  }

  # extent.spatial.bbox (first bbox)
  col_bbox <- NULL
  try(col_bbox <- col$extent$spatial$bbox[[1]], silent = TRUE)

  if (is.null(col_bbox) || length(col_bbox) != 4) {
    report <- add_issue(report, "error", "COLLECTION_INVALID_BBOX",
                        "Collection extent.spatial.bbox is missing or invalid (expected length 4).",
                        list(collection_id = col$id, path = col_path))
    next
  }
  col_bbox <- as.numeric(col_bbox)

  # extent.temporal.interval (first interval)
  t0 <- col$extent$temporal$interval[[1]][[1]] %||% NA_character_
  t1 <- col$extent$temporal$interval[[1]][[2]] %||% NA_character_
  start_time <- as_iso_time(t0)
  end_time <- as_iso_time(t1)

  if (is.na(start_time) || is.na(end_time)) {
    report <- add_issue(report, "warning", "COLLECTION_INVALID_INTERVAL",
                        "Collection temporal interval is missing or not parseable. Item time checks may be skipped.",
                        list(collection_id = col$id, start = t0, end = t1, path = col_path))
  }

  collections_index[[col$id]] <- list(
    path = col_path,
    bbox = col_bbox,
    start_time = start_time,
    end_time = end_time
  )
}

report$counts$collections <- length(collections_index)

# Quick set of collection IDs referenced by the catalog
catalog_collection_ids <- names(collections_index)

# ---- 2) Validate items for each collection folder ----
# We scan *all* item folders under stac/items/, including those not in catalog (orphan detection).
all_item_files <- list.files(ITEMS_DIR, pattern = "\\.json$", recursive = TRUE, full.names = TRUE)
report$counts$items <- length(all_item_files)

if (length(all_item_files) == 0) {
  report <- add_issue(report, "warning", "NO_ITEMS_FOUND",
                      "No item JSON files found under stac/items/.", list(items_dir = ITEMS_DIR))
}

# helper: extract collection_id from item file path "stac/items/<cid>/<file>.json"
extract_folder_collection_id <- function(item_path) {
  # Normalize separators
  p <- path_norm(item_path)
  # Split and find the segment after "stac/items"
  parts <- strsplit(p, .Platform$file.sep, fixed = TRUE)[[1]]
  # Find index of "items"
  idx <- which(parts == "items")
  if (length(idx) == 0 || idx[1] == length(parts)) return(NA_character_)
  parts[idx[1] + 1]
}

for (f in all_item_files) {
  it <- read_json(f)

  folder_cid <- extract_folder_collection_id(f)
  if (!is_nonempty_string(folder_cid)) {
    report <- add_issue(report, "error", "ITEM_PATH_PARSE_FAILED",
                        "Could not determine collection_id from item path.",
                        list(path = f))
    next
  }

  # ---- Extra check B: folder convention ----
  # (This is the convention you chose, and it helps keep things organized.)
  # We validate that folder_cid matches an existing collection folder path.
  expected_dir <- file.path(ITEMS_DIR, folder_cid)
  if (!dir.exists(expected_dir)) {
    report <- add_issue(report, "error", "ITEM_FOLDER_MISSING",
                        "Item is in a collection folder that does not exist.",
                        list(path = f, expected_dir = expected_dir, folder_collection_id = folder_cid))
  }

  # ---- Extra check C: orphan items (folder collection_id not in catalog) ----
  if (!(folder_cid %in% catalog_collection_ids)) {
    report <- add_issue(report, "warning", "ORPHAN_ITEM_FOLDER_NOT_IN_CATALOG",
                        "Item belongs to a collection folder that is not referenced by catalog.json (child links).",
                        list(path = f, folder_collection_id = folder_cid))
  }

  # Required: id
  if (!is_nonempty_string(it$id)) {
    report <- add_issue(report, "error", "ITEM_MISSING_ID",
                        "Item is missing a valid 'id'.",
                        list(path = f, folder_collection_id = folder_cid))
    next
  }

  # ---- Extra check A: item id vs file name ----
  file_id <- path_ext_remove(path_file(f))
  if (!identical(it$id, file_id)) {
    report <- add_issue(report, "warning", "ITEM_ID_FILENAME_MISMATCH",
                        "Item id does not match the JSON filename.",
                        list(path = f, item_id = it$id, filename_id = file_id, folder_collection_id = folder_cid))
  }

  # Required: bbox
  item_bbox <- it$bbox
  if (is.null(item_bbox) || length(item_bbox) != 4) {
    report <- add_issue(report, "error", "ITEM_INVALID_BBOX",
                        "Item bbox missing or invalid (expected length 4).",
                        list(item_id = it$id, path = f, folder_collection_id = folder_cid))
  } else {
    item_bbox <- as.numeric(item_bbox)
  }

  # Required: geometry
  if (is.null(it$geometry) || is.null(it$geometry$type)) {
    report <- add_issue(report, "error", "ITEM_MISSING_GEOMETRY",
                        "Item geometry is missing or invalid.",
                        list(item_id = it$id, path = f, folder_collection_id = folder_cid))
  }

  # Required: datetime
  dt <- it$properties$datetime %||% NA_character_
  item_time <- as_iso_time(dt)
  if (is.na(item_time)) {
    report <- add_issue(report, "error", "ITEM_INVALID_DATETIME",
                        "Item properties.datetime is missing or not parseable.",
                        list(item_id = it$id, path = f, folder_collection_id = folder_cid, datetime = dt))
  }

  # Required: links include rel="collection"
  links <- it$links %||% list()
  col_links <- Filter(function(l) identical(l$rel, "collection"), links)

  if (length(col_links) == 0) {
    report <- add_issue(report, "error", "ITEM_MISSING_COLLECTION_LINK",
                        "Item has no link with rel='collection'.",
                        list(item_id = it$id, path = f, folder_collection_id = folder_cid))
  } else {
    # Validate each collection link resolves to an existing file
    for (cl in col_links) {
      href <- cl$href %||% NA_character_
      if (!is_nonempty_string(href)) {
        report <- add_issue(report, "error", "ITEM_COLLECTION_LINK_NO_HREF",
                            "Item collection link is missing href.",
                            list(item_id = it$id, path = f, folder_collection_id = folder_cid))
        next
      }

      resolved <- path_norm(file.path(path_dir(f), href))
      if (!file.exists(resolved)) {
        report <- add_issue(report, "error", "ITEM_COLLECTION_LINK_BROKEN",
                            "Item collection link does not resolve to an existing file.",
                            list(item_id = it$id, path = f, href = href, resolved_path = resolved,
                                 folder_collection_id = folder_cid))
      } else {
        # ---- Extra check D: item collection link must match folder collection_id ----
        target_col <- read_json(resolved)
        target_cid <- target_col$id %||% NA_character_

        if (is_nonempty_string(target_cid) && !identical(target_cid, folder_cid)) {
          report <- add_issue(report, "warning", "ITEM_COLLECTION_LINK_MISMATCH",
                              "Item folder collection_id differs from the collection.json id pointed by the item link.",
                              list(item_id = it$id, path = f,
                                   folder_collection_id = folder_cid,
                                   link_collection_id = target_cid,
                                   resolved_collection_path = resolved))
        }
      }
    }
  }

  # Assets check: each asset must have a non-empty href
  assets <- it$assets %||% list()
  if (length(assets) == 0) {
    report <- add_issue(report, "warning", "ITEM_NO_ASSETS",
                        "Item has no assets (allowed for prototypes, but not recommended).",
                        list(item_id = it$id, path = f, folder_collection_id = folder_cid))
  } else {
    for (aname in names(assets)) {
      ahref <- assets[[aname]]$href %||% ""
      if (!is_nonempty_string(ahref)) {
        report <- add_issue(report, "error", "ASSET_MISSING_HREF",
                            "Asset is missing a valid href.",
                            list(item_id = it$id, path = f, folder_collection_id = folder_cid, asset = aname))
      }
    }
  }

  # If we have a collection index for this folder_cid, do spatial/temporal checks
  if (folder_cid %in% names(collections_index)) {
    colinfo <- collections_index[[folder_cid]]

    if (!is.null(item_bbox) && length(item_bbox) == 4 && !bbox_inside(item_bbox, colinfo$bbox)) {
      report <- add_issue(report, "warning", "ITEM_OUTSIDE_COLLECTION_BBOX",
                          "Item bbox is outside the collection bbox.",
                          list(item_id = it$id, path = f, folder_collection_id = folder_cid,
                               item_bbox = item_bbox, collection_bbox = colinfo$bbox))
    }

    if (!is.na(item_time) && !is.na(colinfo$start_time) && !is.na(colinfo$end_time) &&
        !interval_contains(item_time, colinfo$start_time, colinfo$end_time)) {
      report <- add_issue(report, "warning", "ITEM_OUTSIDE_COLLECTION_INTERVAL",
                          "Item datetime is outside the collection temporal interval.",
                          list(item_id = it$id, path = f, folder_collection_id = folder_cid,
                               item_datetime = dt,
                               collection_start = format(colinfo$start_time, tz = "UTC", usetz = TRUE),
                               collection_end = format(colinfo$end_time, tz = "UTC", usetz = TRUE)))
    }
  }
}

# ---- Print summary ----
n_errors <- sum(vapply(report$issues, function(x) x$level == "error", logical(1)))
n_warnings <- sum(vapply(report$issues, function(x) x$level == "warning", logical(1)))

cat("\n================ STAC VALIDATION SUMMARY ================\n")
cat("Collections indexed:", report$counts$collections, "\n")
cat("Items scanned:      ", report$counts$items, "\n")
cat("Errors:             ", n_errors, "\n")
cat("Warnings:           ", n_warnings, "\n")
cat("Report:             ", if (WRITE_REPORT_JSON) REPORT_PATH else "(not written)", "\n")
cat("========================================================\n\n")

if (length(report$issues) > 0) {
  cat("Issues:\n")
  for (iss in report$issues) {
    tag <- if (iss$level == "error") "Error" else "Warning"
    cat(tag, iss$code, "-", iss$message, "\n")
    if (!is.null(iss$context$path)) cat("   path:", iss$context$path, "\n")
    if (!is.null(iss$context$folder_collection_id)) cat("   folder_collection_id:", iss$context$folder_collection_id, "\n")
    if (!is.null(iss$context$item_id)) cat("   item_id:", iss$context$item_id, "\n")
  }
  cat("\n")
} else {
  cat("No issues found.\n\n")
}

# ---- Write JSON report ----
if (WRITE_REPORT_JSON) {
  writeLines(toJSON(report, auto_unbox = TRUE, pretty = TRUE, null = "null"), REPORT_PATH)
}