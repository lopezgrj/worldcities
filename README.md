# OldCities

Historical cities mapping project using R and QGIS.

## Project Goal

This repository combines tabular and geospatial sources to work with historical city locations (with a focus on Mesopotamia and related regions), then visualize and analyze them in QGIS and R.

## Repository Structure

- `R/`: R scripts for data cleaning, joins, analysis, and export.
- `raw_data/simplemaps_worldcities_basicv1.91/worldcities.csv`: base world cities dataset.
- `Las Ciudades del Imperio.csv`: project-specific city table.
- `Mesopotamia.kml` / `Mesopotamia.kmz`: regional geospatial layers.
- `oldcities.shp` (+ `.dbf`, `.shx`, `.prj`, `.cpg`): shapefile components.
- `history.qgz`, `julio2021.qgz`: QGIS project files.
- `OldCities.Rproj`: RStudio project file.

## Requirements

- R (recommended: latest stable release)
- RStudio (optional but recommended)
- QGIS (for map editing and project visualization)

## Quick Start (R)

1. Open `OldCities.Rproj` in RStudio.
2. Place or verify raw inputs in `raw_data/`.
3. Run scripts from `R/` in order (if numbered) or according to script comments.
4. Export results as needed (CSV, GeoPackage, or shapefile) for QGIS.

## Quick Start (QGIS)

1. Open `history.qgz` or `julio2021.qgz` in QGIS.
2. Verify layer paths if prompted (especially if files were moved).
3. Refresh symbology/labels and export maps.

## Data Notes

- Keep all shapefile sidecar files together (`.shp`, `.dbf`, `.shx`, `.prj`, `.cpg`).
- Avoid renaming geospatial files directly outside QGIS unless you update all references.
- Prefer UTF-8 CSV exports to preserve place names and diacritics.

## Suggested Next Improvements

- Add script-level documentation in `R/` (inputs, outputs, execution order).
- Add a reproducible pipeline script (for example, `R/00_run_all.R`).
- Add a small `outputs/` directory and ignore large generated artifacts in `.gitignore`.


# To Dowload data go to

https://simplemaps.com/data/world-cities