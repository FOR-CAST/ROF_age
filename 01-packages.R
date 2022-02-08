pkgDir <- Sys.getenv("PRJ_PKG_DIR")
if (!nzchar(pkgDir)) {
  pkgDir <- "packages" ## default: use subdir within project directory
}
pkgDir <- normalizePath(
  file.path(pkgDir, version$platform, paste0(version$major, ".", strsplit(version$minor, "[.]")[[1]][1])),
  winslash = "/",
  mustWork = FALSE
)

if (!dir.exists(pkgDir)) {
  dir.create(pkgDir, recursive = TRUE)
}

.libPaths(pkgDir)
message("Using libPaths:\n", paste(.libPaths(), collapse = "\n"))

if (!require("Require", quietly = TRUE)) {
  install.packages("Require")
  library(Require)
}

Require("PredictiveEcology/SpaDES.install@development")

.spatialPkgs <- c("lwgeom", "rgdal", "rgeos", "sf", "sp", "raster", "terra")

if (!all(.spatialPkgs %in% rownames(installed.packages()))) {
  installSpatialPackages(.spatialPkgs)
  #install.packages(c("raster", "terra"), repos = "https://rspatial.r-universe.dev")
  sf::sf_extSoftVersion() ## want GEOS 3.9.0, GDAL 3.2.1, PROJ 7.2.1
}

## TODO: remove unused packages
pkgs1 <- c(
  "data.table", "DHARMa", "effects", "foreign", "gamlss", "ggpubr", "lme4", "lmerTest",
  "dplyr", "readr", "tidyverse", ## TODO: remove these in favour of data.table
  "performance", "qs", "RCurl", "splines", "styler"
)
Require(pkgs1, require = FALSE) ## don't load/attach yet, just ensure these get installed

Require("PredictiveEcology/reproducible@terraInProjectInputs (>= 1.2.8.9023)", require = FALSE)
Require("PredictiveEcology/LandR@development (>= 1.0.7.9002)", require = FALSE)

## install these if needed, and load/attach:
pkgs2 <- c(
  "fasterize", "ggplot2", "googledrive", "mgcv" ## TODO: need tidyr but Rstudio is a jerk
)
Require(c(pkgs2, "raster", "sf", "terra", "reproducible"))

if (identical(Sys.info()[["user"]], "achubaty")) {
  drive_auth(email = "achubaty@for-cast.ca")
}
