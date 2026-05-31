#!/usr/bin/env Rscript

# Import worldcities CSV and export as a GeoPackage point layer.

suppressPackageStartupMessages({
  library(readr)
  library(dplyr)
  library(sf)
})

csv_path <- "raw_data/simplemaps_worldcities_basicv1.91/worldcities.csv"
out_dir <- "data"
out_gpkg <- file.path(out_dir, "worldcities.gpkg")
layer_name <- "worldcities"

if (!file.exists(csv_path)) {
  stop(sprintf("Input file not found: %s", csv_path))
}

if (!dir.exists(out_dir)) {
  dir.create(out_dir, recursive = TRUE)
}

cities <- readr::read_csv(csv_path, show_col_types = FALSE)

required_cols <- c("lat", "lng")
missing_cols <- setdiff(required_cols, names(cities))
if (length(missing_cols) > 0) {
  stop(sprintf(
    "Missing required coordinate columns: %s",
    paste(missing_cols, collapse = ", ")
  ))
}

cities_sf <- cities %>%
  sf::st_as_sf(coords = c("lng", "lat"), crs = 4326, remove = FALSE)

if (file.exists(out_gpkg)) {
  file.remove(out_gpkg)
}

sf::st_write(cities_sf, out_gpkg, layer = layer_name, quiet = TRUE)

message(sprintf("GeoPackage written to: %s", out_gpkg))
message(sprintf("Layer name: %s", layer_name))
message(sprintf("Features: %d", nrow(cities_sf)))
