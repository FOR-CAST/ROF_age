#' Download and prepare fire data from NFI
#'
#' @param rasterToMatch a template raster
#' @param dPath destination path for archive
#'
#' @return a raster with values representing fire year 1985-2015
#'
#' @export
#' @importFrom reproducible prepInputs
#' @importFrom terra rast crop project mask
#' @importFrom raster ncell nlayers getValues extent

getWildfire_NFI <- function(rasterToMatch, dPath){
  #note I wil use the 250 m NFI for now
  NFIurl <- "https://opendata.nfis.org/downloads/forest_change/CA_forest_wildfire_year_DNBR_Magnitude_1985_2015.zip"
  #this is an enormous raster - we want the second raster in the stack, wildfire year
  wildfireYear <- prepInputs(url = NFIurl,
                             destinationPath = dPath,
                             fun = raster::stack)
  wildfireYear2 <- wildfireYear$CA_forest_wildfire_year_DNBR_Magnitude_1985_2015.2 #only project year
  #we could switch below to postProcess when it is working reliably
  wildfire_SA <- projectRaster(wildfireYear2, to = rasterToMatch, method = "ngb") #Must be NGB!
  wildfire_SA <- mask(wildfire_SA, rasterToMatch)
  wildfire_SA[wildfire_SA == 0] <- NA
  wildfire_SA <- wildfire_SA + 1900
  return(wildfire_SA)

  #for raquel - Alex adjust this as needed - we can probably delete this lines once its all in the preamble
  # if (!dir.exists("outputs/fire")) dir.create("outputs/fire")
  # writeRaster(wildfire_SA, file.path("outputs", "fire/wildfire_ROF_RTML.tif"))
  # utils::zip(zipfile = "outputs/wildfire_ROF.zip", files = "outputs/fire")
  # gdriveUrl <- "https://drive.google.com/drive/u/0/folders/1DzbbVSYp0br-MIi1iI0EPMGGy4BRrjnk"
  # googledrive::drive_upload(media = "outputs/wildfire_ROF.zip",
  #                           path = gdriveUrl,
  #                           name = "wildfire_RTML_ROF")

}
