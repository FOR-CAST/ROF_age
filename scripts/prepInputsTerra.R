prepInputs_Terra <- function(url, targetFile, destinationPath, studyArea = NULL, rasterToMatch = NULL, method = NULL) {
  xDL <- preProcess(
    url = url,
    targetFile = targetFile, alsoExtract = "similar",
    destinationPath = destinationPath
  )
  x <- terra::rast(xDL$targetFilePath)
  x <- if (!is.null(rasterToMatch)) {
    if (is(rasterToMatch, "Raster")) {
      rasterToMatch <- terra::rast(rasterToMatch)
    }
    postProcessTerra(from = x, to = rasterToMatch)
  } else if (!is.null(studyArea) & is.null(rasterToMatch)) {
    postProcessTerra(from = x, to = studyArea, method = method)
  } else {
    stop("Please supply studyArea or rasterMatch.")
  }

  return(x)
}

## TODO: replace this with improved version
prepInputs_Tave <- function(url, targetFile, destinationPath, studyArea = NULL, rasterToMatch = NULL) {
  fz <- file.path(destinationPath, "Normal_1981_2010S.zip")
  if (!file.exists(fz)) {
    drive_download(as_id(url), fz)
    archive::archive_extract(fz, dir = destinationPath, files = targetFile)
  }
  rm(fz)

  Tave <- terra::rast(file.path(destinationPath, targetFile)) / 10 ## originally an integer (and x10)
  crs(Tave) <- "EPSG:4326"
  Tave <- setMinMax(Tave)
  Tave <- postProcessTerra(
    from = Tave,
    to = rasterToMatch,
    destinationPath = destinationPath
  )

  return(Tave)
}

## TODO: replace this with improved version that rasterizes directly from shapefile
prepInputs_ecozones <- function(url, targetFile, destinationPath, studyArea = NULL, rasterToMatch = NULL) {
  studyArea <- as_Spatial(studyArea)
  ecozone_shp <- prepInputs(
    url = url,
    targetFile = targetFile,
    alsoExtract = "similar",
    fun = "sf::st_read",
    destinationPath = destinationPath,
    studyArea = studyArea,
    targetCRS = proj4string(studyArea)
  )

  ecozone_shp[["ZONE_NAME"]] <- as.factor(ecozone_shp[["ZONE_NAME"]])
  ecozone <- fasterize::fasterize(ecozone_shp, raster(rasterToMatch), field = "ZONE_NAME", fun = "sum")
  ecozone <- terra::rast(ecozone)
  ecozone <- terra::as.int(ecozone)
  ecozone <- terra::project(ecozone, rasterToMatch, method = "near")

  return(ecozone)
}

## TODO: replace this with prepInputs_Terra when it works
prepInputs_prevAgeLayer <- function(url, targetFile, destinationPath, studyArea = NULL, rasterToMatch = NULL) {
  prevAgeLayer <- prepInputs(
    url = url,
    targetFile = targetFile,
    alsoExtract = "similar",
    fun = "raster::raster",
    destinationPath = destinationPath,
    rasterToMatch = raster(rasterToMatch)
  )
  prevAgeLayer <- terra::rast(prevAgeLayer)
  prevAgeLayer <- terra::as.int(prevAgeLayer)
  prevAgeLayer <- terra::project(prevAgeLayer, rasterToMatch, method = "near")

  return(prevAgeLayer)
}
