## NOTE: data available on Google Drive or from public internet sources
##   https://drive.google.com/drive/folders/1ZM8i8VZ8BcsxEdxWPE2S-AMO0JvQ9DRI

canProvs <- raster::getData("GADM", path = inputDir, country = "CAN", level = 1, type = "sf")
st_crs(canProvs) <- st_crs(canProvs) ## fix "old-style crs" warning from sf
canProvs <- st_transform(canProvs, crs = targetProj) ## TODO: use targetCRS
canProvs <- as_Spatial(canProvs)

studyArea_ROF <- Cache(
  prepInputs,
  url = "https://drive.google.com/file/d/1DzVRglqJNvZA8NZZ7XKe3-6Q5f8tlydQ/",
  targetCRS = targetProj,
  targetFile = "ROF_RA_def.shp", alsoExtract = "similar",
  fun = "sf::st_read",
  destinationPath = inputDir
)

prebuiltRasterFilenames <- list(
  LCC2015 = "CAN_LC_2015_CAL_Clip1.tif",
  ba = "CA_forest_basal_area_2015_ROF.tif",
  Tave = "Normal_1981_2010_Tave_sm_ROF.tif",
  wildfires = "wildfire_ROF.tif"
)

prebuiltRasterDirNames <- lapply(prebuiltRasterFilenames, function(f) {
  checkPath(file.path(inputDir, tools::file_path_sans_ext(f)), create = TRUE)
})

prebuiltRasterURLs <- list(
  LCC2015 = "https://drive.google.com/file/d/13bHz8XEW5sIBZ4Mn-4_hxg-iaWmDEnlO/",
  ba = "https://drive.google.com/file/d/1aKCclzcKk8Aowj0kTK6oV36lAhJhIxJM/",
  Tave = "https://drive.google.com/file/d/1HT0swKK22D59n47RbbBJAyC1qGlAGb-E/",
  wildfires = "https://drive.google.com/file/d/1WcxdP-wyHBCnBO7xYGZ0qKgmvqvOzmxp/"
)

stopifnot(identical(names(prebuiltRasterFilenames), names(prebuiltRasterURLs)))

source("scripts/prepInputsTerra.R") ## TODO: temporary until functionality in `reproducible`

if (lowMemory) {
  ## use rasters pre-cropped to ROF
  LCC2015 <- Cache(
    prepInputs_Terra,
    url = prebuiltRasterURLs$LCC2015,
    targetFile = prebuiltRasterFilenames$LCC2015,
    destinationPath = inputDir,
    studyArea = studyArea_ROF
  )

  ## TODO: reupload these prebuilt rasters after making them in !lowMemory branch below
  ba <- Cache(
    prepInputs_Terra,
    url = prebuiltRasterURLs$ba,
    targetFile = prebuiltRasterFilenames$ba,
    destinationPath = inputDir,
    rasterToMatch = LCC2015
  )

  Tave <- Cache(
    prepInputs_Terra,
    url = prebuiltRasterURLs$Tave,
    targetFile = prebuiltRasterFilenames$Tave,
    destinationPath = inputDir,
    rasterToMatch = LCC2015
  )

  wildfires <- Cache(
    prepInputs_Terra,
    url = prebuiltRasterURLs$wildfires,
    targetFile = prebuiltRasterFilenames$wildfires,
    destinationPath = inputDir,
    rasterToMatch = LCC2015
  )
} else {
  ## use national layers

  ## from https://open.canada.ca/data/en/dataset/4e615eae-b90c-420b-adee-2ca35896caf6
  f_lcc <- file.path(inputDir, "CAN_LC_2015_CAL.tif")
  LCC2015 <- Cache(
    prepInputs_Terra,
    url = paste0(
      "https://ftp.maps.canada.ca/pub/nrcan_rncan/Land-cover_Couverture-du-sol/",
      "canada-landcover_canada-couverture-du-sol/CanadaLandcover2015.zip"
    ), ## TODO: use 2010?
    targetFile = basename(f_lcc),
    destinationPath = dirname(f_lcc),
    studyArea = studyArea_ROF
  )

  ## from https://open.canada.ca/data/en/dataset/4c0d9755-9347-42f2-bb1b-f4d2ff673254
  f_ba <- file.path(inputDir, "CA_forest_basal_area_2015_NN.tif")
  ba <- Cache(
    prepInputs_Terra,
    url = "https://opendata.nfis.org/downloads/forest_change/CA_forest_basal_area_2015_NN.zip", ## 113 GB !
    # url = "https://drive.google.com/file/d/1Vyqb3Q-2T45v963RlFb6EApqyPSMKwKO/", ## backup version
    targetFile = basename(f_ba),
    destinationPath = dirname(f_ba),
    rasterToMatch = LCC2015
  )

  ## NOTE: no longer using the 1km product ## TODO: remove
  # f_tave <- file.path(inputDir, "Normal_1981_2010_Tave_sm.tif")
  # Tave <- Cache(
  #   prepInputs_Terra,
  #   url = "https://s3-us-west-2.amazonaws.com/www.cacpd.org/CMIP6/normals/Normal_1981_2010_bioclim.zip",
  #   targetFile = basename(f_tave),
  #   destinationPath = dirname(f_tave),
  #   rasterToMatch = LCC2015
  # )

  Tave <- Cache(
    prepInputs_Tave,
    url = "https://drive.google.com/file/d/1d5wtRQGjHje6aE5ghDaAp9mzuGFkmWoA/", ## 3 arcmin output from ClimateNA
    targetFile = "Normal_1981_2010S/Tave_sm.asc",
    destinationPath = inputDir,
    rasterToMatch = LCC2015
  )

  wildfires <- Cache(LandR::getWildfire_NFI, dPath = inputDir, rasterToMatch = LCC2015)
}

## TODO: skip for now - confirm whether to use raster or add to df directly
if (FALSE) {
  ## create time since fire (TSF) layer
  dataYear <- terra::rast(wildfires) ## template using `wildfires`
  dataYear[] <- 2015L ## TODO: adjust this to match the years of the relevant data

  tsf <- dataYear - wildfires
}

ecozones <- Cache(
  prepInputs_ecozones,
  url = "https://sis.agr.gc.ca/cansis/nsdb/ecostrat/zone/ecozone_shp.zip",
  targetFile = "ecozones.shp",
  destinationPath = inputDir,
  studyArea = studyArea_ROF,
  rasterToMatch = LCC2015
)

## TODO: `terra` not working here?
prevAgeLayer <- Cache(
  prepInputs_prevAgeLayer,
  url = "https://drive.google.com/file/d/1hKyVbPyM9bR09u465fusa5mU7_cz-iZz/", ## orig 250 m layer
  targetFile = "standAgeMap2011_ROF.tif",
  destinationPath = inputDir,
  rasterToMatch = LCC2015
)
# plot(prevAgeLayer)

## sanity checks
stopifnot(terra::compareGeom(LCC2015, ba, crs = TRUE, res = TRUE))
stopifnot(terra::compareGeom(LCC2015, Tave, crs = TRUE, res = TRUE))
stopifnot(terra::compareGeom(LCC2015, ecozones, crs = TRUE, res = TRUE))
stopifnot(terra::compareGeom(LCC2015, wildfires, crs = TRUE, res = TRUE))
stopifnot(terra::compareGeom(LCC2015, prevAgeLayer, crs = TRUE, res = TRUE))

## plot previous age layer with study area boundary and groundplot locations
png(file.path(figsDir, "prevAgeLayer_w_points.png"), width = 1200, height = 600)
plot(prevAgeLayer)
plot(as_Spatial(studyArea_ROF), add = TRUE)
plot(DatasetAge1_sp, col = "blue", add = TRUE)
dev.off()

gc()
