if (!exists("pkgDir")) {
  pkgDir <- file.path("packages", version$platform, paste0(version$major, ".",
                                                           strsplit(version$minor, "[.]")[[1]][1]))

  if (!dir.exists(pkgDir)) {
    dir.create(pkgDir, recursive = TRUE)
  }
  .libPaths(pkgDir)
}

if (!suppressWarnings(require("Require"))) {
  install.packages("Require")
  library(Require)
}

pkgs1 <- c(
  "data.table", "DHARMa", "effects", "foreign", "gamlss", "ggplot2", "lme4", "lmerTest", "mgcv",
  "dplyr", "readr", "tidyverse", ## TODO: remove these in favour of data.table
  "performance", "qs", "splines"
)
Require(pkgs1, Require = FALSE) ## don't load/attach yet, just ensure these get installed

pkgs2 <- c(
  "googledrive"
)
Require(pkgs2) ## install if needed, and load/attach

sptlPkgs <- c("rgdal", "sf", "terra", "raster", "rgeos") ## TODO: remove raster
if (!all(sptlPkgs %in% rownames(installed.packages()))) {
  install.packages(sptlPkgs, repos = "https://cran.rstudio.com")

  sf::sf_extSoftVersion() ## want GEOS 3.9.0, GDAL 3.2.1, PROJ 7.2.1 or higher
}
Require(sptlPkgs)

Require("PredictiveEcology/reproducible@development")

## project paths
cacheDir <- checkPath("cache", create = TRUE)
inputDir <- checkPath("inputs", create = TRUE)
outputDir <- checkPath("outputs", create = TRUE)
scratchDir <- checkPath("scratch", create = TRUE)

## project options
raster::rasterOptions(default = TRUE)
opts <- options(
  "rasterMaxMemory" = 5e+12,
  "rasterTmpDir" = scratchDir,
  "reproducible.cachePath" = cacheDir,
  "reproducible.cacheSaveFormat" = "qs",
  "reproducible.nThreads" = 2,
  "reproducible.quick" = FALSE,
  "reproducible.showSimilar" = TRUE,
  "reproducible.useCache" = TRUE,
  "reproducible.useCloud" = TRUE,
  "reproducible.useGDAL" = FALSE, ## TODO: can't use true until system call bugs are resolved
  "reproducible.useMemoise" = FALSE
)

## TODO: Alex resume here; data available on Google Drive:
## https://drive.google.com/drive/folders/1ZM8i8VZ8BcsxEdxWPE2S-AMO0JvQ9DRI?usp=sharing

## run the model
f1 <- file.path(inputDir, "FinalAllAgeDataset2.txt")
drive_download(as_id("13R7YW9RpxVQ6u-h4qGQhXDpQp76Umbva"), path = f1, overwrite = TRUE)
plot3 <- read.table(f1, header = TRUE, sep = " ", fill = TRUE, dec = ".") # the dataset of ground plots
plot3$ecozone <- as.factor(as.character(plot3$ecozone))
summary(plot3$ecozone)
plot3$LCC <- as.factor(as.character(plot3$LCC))
summary(plot3$LCC)

## NOTE: need too much RAM to run below with the parameter select=TRUE
modage2 <- bam(TSLF ~ s(total_BA) + ecozone + LCC + s(total_BA, by = ecozone) + s(total_BA, by = LCC) +
                 s(longitude, latitude, bs = "gp", k = 100, m = 2) + s(Tave_sm) + ti(total_BA, Tave_sm) +
                 s(year_BA, bs = "re"),
               #select = TRUE, ## Error: cannot allocate vector of size 123559.1 Gb
               data = plot3, method = "fREML", family = negbin(3.416129), discrete = TRUE)
summary(modage2)
gam.check(modage2)

## TODO: script dataset creation in R instead of pre-making in ArcMap (resolution 1 x 1 km)
f2 <- file.path(inputDir, "TaveMultiPoints.txt")
drive_download(as_id("1LtN98HClGK7xgoNiZJ8PKZEkXk8ruPzR"), path = f2)
newdataset <- read.table(f2, header = TRUE, sep = ",", fill = TRUE, dec = ".")
head(newdataset)
str(newdataset)
newdataset <- na.omit(newdataset[, -c(1:2)])
newdataset <- as.data.frame(newdataset)
colnames(newdataset) <- c("Tave_sm", "ecozone", "LCC", "total_BA", "PreviusStandAge", "latitude", "longitude")
newdataset$year_BA <- 2015
newdataset$ecozone <- round(newdataset$ecozone, 0)
newdataset$LCC <- round(newdataset$LCC, 0)
newdataset$ecozone <- as.factor(as.character(newdataset$ecozone))
newdataset$LCC <- as.factor(as.character(newdataset$LCC))
summary(newdataset$LCC)
summary(newdataset$ecozone)
levels(newdataset$LCC)[levels(newdataset$LCC) == "11"] <- "11_12_13"
levels(newdataset$LCC)[levels(newdataset$LCC) == "12"] <- "11_12_13"
levels(newdataset$LCC)[levels(newdataset$LCC) == "13"] <- "11_12_13"
newdataset <- subset(newdataset, !(LCC %in% c("3", "4", "7", "15", "16", "17", "18", "9"))) ## TODO: confirm it's 9 and not 19
summary(newdataset$LCC)
levels(newdataset$ecozone)[levels(newdataset$ecozone) == "10"] <- "BOREAL SHIELD"
levels(newdataset$ecozone)[levels(newdataset$ecozone) == "11"] <- "HUDSON PLAIN"
newdataset$predictstack <- exp(predict(modage2, newdataset))
head(newdataset$predictstack)
range(newdataset$predictstack)
hist((newdataset$predictstack))
cor.test(newdataset$PreviusStandAge, newdataset$predictstack)
hist((newdataset$PreviusStandAge))
#write.csv(newdataset,"Predictions.csv") ## exported the dataset and created the raster in ArcMap

f3 <- file.path(inputDir, "Predictions_PointToRaster1.tif") ## TODO: need this raster in gdrive
predAge <- raster(f3)
plot(predAge)

## This is the previous layer of Stand Age, is very different compared to the new one.
predPrevAge <- prepInputs(
  url = "https://drive.google.com/file/d/14zxLiW_XVoOeLILi9bqpdTtDzOw4JyuP/",
  targetFile = "standAgeMap_it_1_ts_2011_ProROF.tif",
  fun = "raster::raster", ## TODO: use terra
  destinationPath = inputDir
)
plot(predPrevAge)

#####

LCC <- prepInputs(
  url = "https://drive.google.com/file/d/13bHz8XEW5sIBZ4Mn-4_hxg-iaWmDEnlO/",
  targetFile = "CAN_LC_2015_CAL_Clip1.tif", alsoExtract = "similar",
  fun = "raster::raster", ## TODO: use terra
  destinationPath = inputDir
)

## NOTE: reprojecting rasters in memory requires ~110 GB RAM
ba <- Cache(
  prepInputs,
  url = "https://drive.google.com/file/d/1aKCclzcKk8Aowj0kTK6oV36lAhJhIxJM/",
  targetFile = "CA_forest_basal_area_2015_ROF.tif",
  fun = "raster::raster", ## TODO: use terra
  destinationPath = inputDir,
  rasterToMatch = LCC
)

Tave <- Cache(
  prepInputs,
  url = "https://drive.google.com/file/d/1HT0swKK22D59n47RbbBJAyC1qGlAGb-E/",
  targetFile = "Normal_1981_2010_Tave_sm_ROF.tif",
  fun = "raster::raster", ## TODO: use terra
  destinationPath = inputDir,
  rasterToMatch = LCC
)

ecozone <- Cache(
  prepInputs,
  url = "https://drive.google.com/file/d/1IwRayjkjOGFjIUDfCYyPsKmgx9MRGqKA/",
  targetFile = "ecozones_PolygonToRaster21_C1.tif", alsoExtract = "similar",
  fun = "raster::raster", ## TODO: use terra
  destinationPath = inputDir,
  rasterToMatch = LCC
)

LCCpoints <- Cache(rasterToPoints, x = LCC, progress = "text")
gc()

LCCpoints2 <- as.data.frame(LCCpoints) ## TODO: use data.table (NOTE: weird issue with S4 conversion?)
colnames(LCCpoints2) <- c("x", "y", "LCC")
str(LCCpoints2)
head(LCCpoints2)
rm(LCCpoints)
gc()

LCCpoints3 <- subset(LCCpoints2, LCC < 15)
LCCpoints3$LCC <- as.factor(LCCpoints3$LCC)
summary(LCCpoints3$LCC)
rm(LCCpoints2)
gc()

LCCpoints4 <- subset(LCCpoints3, !(LCC %in% c("0", "9")))
summary(LCCpoints4$LCC)
rm(LCCpoints3)
gc()

levels(LCCpoints4$LCC)[grepl("11|12|13", levels(LCCpoints4$LCC))] <- "11_12_13"

coordinates(LCCpoints4) <- ~ x + y ## NOTE: needs ~80GB RAM

rasStack <- stack(ba, Tave, ecozone)
rasValue <- extract(rasStack, LCCpoints4)
