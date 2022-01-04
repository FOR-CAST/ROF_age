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

pkgs <- c(
  "data.table", "DHARMa", "effects", "foreign", "gamlss", "ggplot2", "lme4", "lmerTest", "mgcv",
  "dplyr", "readr", "tidyverse", ## TODO: remove these in favour of data.table
  "performance", "splines"
)
Require(pkgs)

sptlPkgs <- c("rgdal", "sf", "terra", "raster") ## TODO: remove raster
if (!all(sptlPkgs %in% rownames(installed.packages()))) {
  install.packages(sptlPkgs, repos = "https://cran.rstudio.com")

  sf::sf_extSoftVersion() ## want GEOS 3.9.0, GDAL 3.2.1, PROJ 7.2.1
}
Require(sptlPkgs)

Require("PredictiveEcology/reproducible@development")

cacheDir <- checkPath("cache", create = TRUE)
inputDir <- checkPath("inputs", create = TRUE)
outputDir <- checkPath("outputs", create = TRUE)

## TODO: Alex resume here; data available on Google Drive:
## https://drive.google.com/drive/folders/1ZM8i8VZ8BcsxEdxWPE2S-AMO0JvQ9DRI?usp=sharing


# run the model
plot3 <- read.table(file.path(inputDir, "FinalAllAgeDataset2.txt"), header = TRUE, sep = " ", fill = TRUE, dec = ".") # the dataset of ground plots
plot3$ecozone <- as.factor(as.character(plot3$ecozone))
summary(plot3$ecozone)
plot3$LCC <- as.factor(as.character(plot3$LCC))
summary(plot3$LCC)

modage2 <- bam(TSLF ~ s(total_BA) + ecozone + LCC + s(total_BA, by = ecozone) + s(total_BA, by = LCC) +
  s(longitude, latitude, bs = "gp", k = 100, m = 2) + s(Tave_sm) + ti(total_BA, Tave_sm)
  + s(year_BA, bs = "re"), data = plot3, method = "fREML", family = negbin(3.416129), discrete = TRUE) # I don't have enough memory to run it with the parameter,select=TRUE
summary(modage2)
gam.check(modage2) #

# I have created the dataset that I will use to predict the model in ArcMap (resolution 1 x 1 km)
newdataset <- read.table(file.path(inputDir, "TaveMultiPoints.txt"), header = TRUE, sep = ",", fill = TRUE, dec = ".")
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
newdataset <- subset(newdataset, LCC != "3" & LCC != "4" & LCC != "7" & LCC != "15" & LCC != "16" & LCC != "17" & LCC != "18" & LCC != "9")
summary(newdataset$LCC)
levels(newdataset$ecozone)[levels(newdataset$ecozone) == "10"] <- "BOREAL SHIELD"
levels(newdataset$ecozone)[levels(newdataset$ecozone) == "11"] <- "HUDSON PLAIN"
newdataset$predictstack <- exp(predict(modage2, newdataset))
head(newdataset$predictstack)
range(newdataset$predictstack)
hist((newdataset$predictstack))
cor.test(newdataset$PreviusStandAge, newdataset$predictstack)
hist((newdataset$PreviusStandAge))
# write.csv(newdataset,"Predictions.csv")# I have exported the dataset and created the raster in ArcMap

predAge <- raster(file.path(inputDir, "Predictions_PointToRaster1.tif"))
plot(predAge)

predPrevAge <- raster(file.path(inputDir, "standAgeMap_it_1_ts_2011_ProROF.tif")) # This is the previous layer of Stand Age, is very different compared to the new one.
plot(predPrevAge)

#####

## I would like to create the dataset to predict in R. This is what I tried so far.
ba <- raster(file.path(inputDir, "CA_forest_basal_area_2015_ROF.tif"))
Tave <- raster(file.path(inputDir, "Normal_1981_2010_Tave_sm_ROF.tif"))
ecozone <- raster(file.path(inputDir, "ecozones_PolygonToRaster21_C1.tif"))
LCC <- raster(file.path(inputDir, "CAN_LC_2015_CAL_Clip1.tif"))

### In ArcGis I have reprojected the rasters, but I am having problems in r (I have removed all my failure code)
# the idea was to extract the values of the rasters for the layer of points of LCC

## TODO: get the ArcGIS pieces scripted in R here

rasStack <- stack(ba, Tave, ecozone)

gc()
memory.limit(1e+100)
LCCpoints <- rasterToPoints(LCC, progress = "text")
LCCpoints2 <- as.data.frame(LCCpoints)
str(LCCpoints2)
head(LCCpoints2)
colnames(LCCpoints2)[3] <- "LCC"
LCCpoints3 <- subset(LCCpoints2, LCC < 15)
rm(LCCpoints)
rm(LCCpoints2)
LCCpoints3$LCC <- as.factor(LCCpoints3$LCC)
summary(LCCpoints3$LCC)
LCCpoints4 <- subset(LCCpoints3, LCC != "0" & LCC != "9")
summary(LCCpoints4$LCC)
rm(LCCpoints3)
levels(LCCpoints4$LCC)[levels(LCCpoints4$LCC) == "11"] <- "11_12_13"
levels(LCCpoints4$LCC)[levels(LCCpoints4$LCC) == "12"] <- "11_12_13"
levels(LCCpoints4$LCC)[levels(LCCpoints4$LCC) == "13"] <- "11_12_13"

coordinates(LCCpoints4) <- ~ x + y # my computer crash here

rasValue <- extract(rasStack, LCCpoints4)
