# ------------------------------------------------------------
# HYDRA-EO — Static STAC Validator
#
# Validates a static STAC catalog stored as JSON files in a repo:
#   stac/catalog.json
#   stac/collections/<collection_id>/collection.json
#   stac/items/<collection_id>/*.json
#
# Checks:
#   1) catalog.json child links exist on disk
#   2) collections have valid extent (spatial bbox + temporal interval)
#   3) items:
#       - required fields exist (id, bbox, geometry, properties.datetime, assets)
#       - link to a valid collection.json (rel="collection")
#       - bbox inside collection bbox
#       - datetime inside collection temporal interval
#       - assets have non-empty href
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
  # Convert ISO8601 string to POSIXct in UTC, return NA if invalid
  if (!is_nonempty_string(x)) return(as.POSIXct(NA))
  # Ensure 'Z' is treated as UTC
  # R can parse "YYYY-mm-ddTHH:MM:SSZ" with format %Y-%m-%dT%H:%M:%OSZ
  out <- suppressWarnings(as.POSIXct(x, format = "%Y-%m-%dT%H:%M:%OSZ", tz = "UTC"))
  if (is.na(out)) {
    # Try without Z (fallback)
    out <- suppressWarnings(as.POSIXct(x, tz = "UTC"))
  }
  out
}

bbox_inside <- function(item_bbox, col_bbox) {
  # item_bbox: numeric length 4 [xmin, ymin, xmax, ymax]
  # col_bbox:  numeric length 4 [xmin, ymin, xmax, ymax]
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

if (!file.exists(CATALOG_PATH)) {
  stop("Missing catalog: ", CATALOG_PATH)
}

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

# Resolve and load collections
collections_index <- list()  # collection_id -> list(path, bbox, start, end)

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

  # Basic collection checks
  if (!is_nonempty_string(col$id)) {
    report <- add_issue(report, "error", "COLLECTION_MISSING_ID",
                        "Collection is missing a valid 'id'.", list(path = col_path))
    next
  }

  # Parse collection extent spatial bbox (take first bbox)
  col_bbox <- NA_real_
  try({
    col_bbox <- col$extent$spatial$bbox[[1]]
  }, silent = TRUE)

  if (is.null(col_bbox) || length(col_bbox) != 4) {
    report <- add_issue(report, "error", "COLLECTION_INVALID_BBOX",
                        "Collection extent.spatial.bbox is missing or invalid (expected length 4).",
                        list(collection_id = col$id, path = col_path))
    next
  }
  col_bbox <- as.numeric(col_bbox)

  # Parse collection temporal interval (take first interval)
  t0 <- col$extent$temporal$interval[[1]][[1]] %||% NA_character_
  t1 <- col$extent$temporal$interval[[1]][[2]] %||% NA_character_
  start_time <- as_iso_time(t0)
  end_time <- as_iso_time(t1)

  if (is.na(start_time) || is.na(end_time)) {
    report <- add_issue(report, "warning", "COLLECTION_INVALID_INTERVAL",
                        "Collection temporal interval is missing or not parseable. Items time checks may be skipped.",
                        list(collection_id = col$id, start = t0, end = t1, path = col_path))
  }

  # Save to index
  collections_index[[col$id]] <- list(
    path = col_path,
    bbox = col_bbox,
    start_time = start_time,
    end_time = end_time
  )
}

report$counts$collections <- length(collections_index)

# ---- 2) Validate items for each collection ----
# Convention: items live in stac/items/<collection_id>/*.json
all_item_files <- character(0)

for (cid in names(collections_index)) {
  item_dir <- file.path(ITEMS_DIR, cid)

  if (!dir.exists(item_dir)) {
    report <- add_issue(report, "warning", "ITEMS_DIR_MISSING",
                        "No items folder found for collection (this is allowed, but means no items yet).",
                        list(collection_id = cid, expected_dir = item_dir))
    next
  }

  item_files <- list.files(item_dir, pattern = "\\.json$", full.names = TRUE)
  all_item_files <- c(all_item_files, item_files)

  for (f in item_files) {
    it <- read_json(f)

    # Required: id
    if (!is_nonempty_string(it$id)) {
      report <- add_issue(report, "error", "ITEM_MISSING_ID",
                          "Item is missing a valid 'id'.", list(path = f, collection_id = cid))
      next
    }

    # Required: bbox
    item_bbox <- it$bbox
    if (is.null(item_bbox) || length(item_bbox) != 4) {
      report <- add_issue(report, "error", "ITEM_INVALID_BBOX",
                          "Item bbox missing or invalid (expected length 4).",
                          list(item_id = it$id, path = f, collection_id = cid))
    } else {
      item_bbox <- as.numeric(item_bbox)
      # Spatial check against collection bbox
      if (!bbox_inside(item_bbox, collections_index[[cid]]$bbox)) {
        report <- add_issue(report, "warning", "ITEM_OUTSIDE_COLLECTION_BBOX",
                            "Item bbox is outside the collection bbox.",
                            list(item_id = it$id, path = f, collection_id = cid,
                                 item_bbox = item_bbox, collection_bbox = collections_index[[cid]]$bbox))
      }
    }

    # Required: geometry
    if (is.null(it$geometry) || is.null(it$geometry$type)) {
      report <- add_issue(report, "error", "ITEM_MISSING_GEOMETRY",
                          "Item geometry is missing or invalid.",
                          list(item_id = it$id, path = f, collection_id = cid))
    }

    # Required: datetime
    dt <- it$properties$datetime %||% NA_character_
    item_time <- as_iso_time(dt)
    if (is.na(item_time)) {
      report <- add_issue(report, "error", "ITEM_INVALID_DATETIME",
                          "Item properties.datetime is missing or not parseable.",
                          list(item_id = it$id, path = f, collection_id = cid, datetime = dt))
    } else {
      # Temporal check only if collection interval parseable
      start_time <- collections_index[[cid]]$start_time
      end_time <- collections_index[[cid]]$end_time
      if (!is.na(start_time) && !is.na(end_time) && !interval_contains(item_time, start_time, end_time)) {
        report <- add_issue(report, "warning", "ITEM_OUTSIDE_COLLECTION_INTERVAL",
                            "Item datetime is outside the collection temporal interval.",
                            list(item_id = it$id, path = f, collection_id = cid,
                                 item_datetime = dt,
                                 collection_start = format(start_time, tz = "UTC", usetz = TRUE),
                                 collection_end = format(end_time, tz = "UTC", usetz = TRUE)))
      }
    }

    # Required: links include rel="collection"
    links <- it$links %||% list()
    has_collection_link <- any(vapply(links, function(l) identical(l$rel, "collection"), logical(1)))

    if (!has_collection_link) {
      report <- add_issue(report, "error", "ITEM_MISSING_COLLECTION_LINK",
                          "Item has no link with rel='collection'.",
                          list(item_id = it$id, path = f, collection_id = cid))
    } else {
      # If present, validate the link resolves to an existing collection.json on disk
      col_links <- Filter(function(l) identical(l$rel, "collection"), links)
      for (cl in col_links) {
        href <- cl$href %||% NA_character_
        if (!is_nonempty_string(href)) {
          report <- add_issue(report, "error", "ITEM_COLLECTION_LINK_NO_HREF",
                              "Item collection link is missing href.", list(item_id = it$id, path = f))
          next
        }
        resolved <- path_norm(file.path(path_dir(f), href))
        if (!file.exists(resolved)) {
          report <- add_issue(report, "error", "ITEM_COLLECTION_LINK_BROKEN",
                              "Item collection link does not resolve to an existing file.",
                              list(item_id = it$id, path = f, href = href, resolved_path = resolved))
        }
      }
    }

    # Assets check: each asset must have a non-empty href
    assets <- it$assets %||% list()
    if (length(assets) == 0) {
      report <- add_issue(report, "warning", "ITEM_NO_ASSETS",
                          "Item has no assets (allowed for prototypes, but not recommended).",
                          list(item_id = it$id, path = f, collection_id = cid))
    } else {
      for (aname in names(assets)) {
        ahref <- assets[[aname]]$href %||% ""
        if (!is_nonempty_string(ahref)) {
          report <- add_issue(report, "error", "ASSET_MISSING_HREF",
                              "Asset is missing a valid href.",
                              list(item_id = it$id, path = f, collection_id = cid, asset = aname))
        }
      }
    }
  }
}

report$counts$items <- length(all_item_files)

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

# Print issues grouped by level
if (length(report$issues) > 0) {
  cat("Issues:\n")
  for (iss in report$issues) {
    tag <- if (iss$level == "error") "error" else "warning"
    cat(tag, iss$code, "-", iss$message, "\n")
    # Print minimal context info
    if (!is.null(iss$context$path)) cat("   path:", iss$context$path, "\n")
    if (!is.null(iss$context$collection_id)) cat("   collection:", iss$context$collection_id, "\n")
    if (!is.null(iss$context$item_id)) cat("   item:", iss$context$item_id, "\n")
  }
  cat("\n")
} else {
  cat(" No issues found.\n\n")
}

# ---- Write JSON report ----
if (WRITE_REPORT_JSON) {
  writeLines(toJSON(report, auto_unbox = TRUE, pretty = TRUE, null = "null"), REPORT_PATH)
}