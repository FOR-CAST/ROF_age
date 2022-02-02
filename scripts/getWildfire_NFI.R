#' Download and prepare fire data from NFI
#'
#' @param dPath destination path for archive
#' @param studyArea polygon of study area to crop/mask to (projection used to reproject)
#'
#' @return a raster with values representing fire year 1985-2015
#'
#' @export
#' @importFrom reproducible preProcess postProcessTerra
#' @importFrom terra rast
#' @importFrom raster extension
getWildfire_NFI <- function(dPath, rasterToMatch) {
  ## NOTE: use the 250m NFI for now
  NFIurl <- paste0("https://opendata.nfis.org/downloads/forest_change/",
                   "CA_forest_wildfire_year_DNBR_Magnitude_1985_2015.zip")

  ## this is an enormous raster - we want the second raster in the stack, wildfire year
  fireDL <- preProcess(
    url = NFIurl,
    destinationPath = dPath,
    targetFile = raster::extension(basename(NFIurl), "tif"),
    alsoExtract = "similar"
  )
  wildfireYear <- terra::rast(fireDL$targetFilePath)
  wildfireYear2 <- wildfireYear$CA_forest_wildfire_year_DNBR_Magnitude_1985_2015_2 ## only need year

  wildfire_SA <- postProcessTerra(wildfireYear2, to = rasterToMatch)

  wildfire_SA[wildfire_SA == 0] <- NA
  wildfire_SA <- wildfire_SA + 1900
  return(wildfire_SA)
}
