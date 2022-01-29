prepInputs_Terra <- function(url, targetFile, destinationPath, studyArea = NULL, rasterToMatch = NULL) {
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
    postProcessTerra(from = x, to = studyArea)
  } else {
    stop("Please supply studyArea or rasterMatch.")
  }

  return(x)
}
