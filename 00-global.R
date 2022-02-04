if (file.exists(".Renviron")) readRenviron(".Renviron")

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
  install.packages(c("raster", "terra"), repos = "https://rspatial.r-universe.dev")# this is the only way to install terra, but if we call it again in Pkgs 2 it instalss the previous version
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
  "fasterize", "ggplot2", "googledrive", "mgcv", ## TODO: need tidyr but Rstudio is a jerk
)
Require(c(pkgs2, "raster", "sf", "terra", "reproducible"))

Require(.spatialPkgs, require = TRUE)
Require(pkgs1, require = TRUE)
Require(pkgs2, require = TRUE)

if (identical(Sys.info()[["user"]], "achubaty")) {
  drive_auth(email = "achubaty@for-cast.ca")
}

## NOTE: many GIS etc. ops require large amounts of memory (>80 GB)
lowMemory <- if (grepl("for-cast[.]ca", Sys.info()[["nodename"]])) FALSE else TRUE
reupload <- if (grepl("for-cast[.]ca", Sys.info()[["nodename"]])) TRUE else FALSE
targetProj <- paste(
  "+proj=lcc +lat_1=49 +lat_2=77 +lat_0=0 +lon_0=-95",
  "+x_0=0 +y_0=0 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0"
)
targetCRS <- "EPSG:42304" ## equivalent crs to targetProj; see https://epsg.io/42304
targetRes <- 300 ## TODO: change this to 125 m to match simulation layers

## project paths
cacheDir <- checkPath("cache", create = TRUE)
inputDir <- checkPath("inputs", create = TRUE)
outputDir <- checkPath("outputs", create = TRUE)
scratchDir <- checkPath("scratch", create = TRUE)

figsDir <- checkPath("figures", create = TRUE)

## project options
raster::rasterOptions(default = TRUE)
opts <- options(
  "rasterMaxMemory" = 5e+50,
  "rasterTmpDir" = scratchDir,
  "reproducible.cachePath" = cacheDir,
  "reproducible.cacheSaveFormat" = "qs",
  "reproducible.nThreads" = 2,
  "reproducible.quick" = FALSE,
  "reproducible.showSimilar" = TRUE,
  "reproducible.useCache" = TRUE,
  "reproducible.useCloud" = TRUE,
  "reproducible.useGDAL" = "force", ## one of FALSE, TRUE, "force"
  "reproducible.useMemoise" = FALSE
)

# data from national study plots --------------------------------------------------------------

## NOTE: data available on Google Drive
##   https://drive.google.com/drive/folders/1ZM8i8VZ8BcsxEdxWPE2S-AMO0JvQ9DRI

## input data
f01 <- file.path(inputDir, "DatasetAgeNA.txt")
if (!file.exists(f01)) {
  drive_download(as_id("1Ig7pNz1eYk5zWTYYpeR5LLYvdGzbV8Mx"), path = f01)
}
DatasetAge0 <- read.table(f01, header = TRUE, sep = "\t", fill = TRUE, dec = ".")
colnames(DatasetAge0)
DatasetAge0$ecozone_combined <- as.factor(DatasetAge0$ecozone_combined)
DatasetAge0$ecozone_combined <- factor(DatasetAge0$ecozone_combined, levels(DatasetAge0$ecozone_combined)[c(1, 2, 4, 6, 3, 5)])
DatasetAge0$year_BA <- as.factor(DatasetAge0$year_BA)
summary(DatasetAge0$ecozone_combined)
str(DatasetAge0)
DatasetAge0 <- subset(DatasetAge0, project_ID != "BurnedNWT")
DatasetAge0$year_BA <- as.numeric(as.character(DatasetAge0$year_BA))
DatasetAge0 <- tidyr::drop_na(DatasetAge0, latitude)
DatasetAge0 <- tidyr::drop_na(DatasetAge0, TSLF)
DatasetAge0 <- subset(DatasetAge0, ecozone_combined != c("ALASKA INT"))
colnames(DatasetAge0)
DatasetAge0$Type <- "Synthesis"
dataSyn <- DatasetAge0[, c(
  "Type", "site_ID", "burn_ID", "latitude", "longitude", "ecozone", "TSLF",
  "year_BA", "PPT_sm", "Tave_sm", "DD5", "total_BA", "LCC"
)]
dataSyn$ecozone <- as.factor(dataSyn$ecozone)
summary(dataSyn$ecozone)
dataSyn <- subset(dataSyn, ecozone != c("WESTERN CORDILLERA"))
levels(droplevels(dataSyn$ecozone))
dataSyn$LCC <- as.factor((dataSyn$LCC))
summary(dataSyn$LCC)
dataSyn$LCC <- as.factor(as.character(dataSyn$LCC))
summary(dataSyn$LCC)
dataSyn <- dataSyn[dataSyn$LCC != "0", ]
droplevels(dataSyn$LCC)
dataSyn2 <- tidyr::drop_na(dataSyn, total_BA)
dataSyn2$year_BA <- as.integer(dataSyn2$year_BA)

# NFI, TREESOURCE
f04 <- file.path(inputDir, "FinalNFI&TreeSource.txt") #
if (!file.exists(f04)) {
  drive_download(as_id("1ROJeiPdI7fvdcPm9MMQMJTseLko0-TZR"), path = f04)
}
dataFF2 <- read.table(f04, header = TRUE, sep = "\t", fill = TRUE, dec = ".")
colnames(dataFF2)[6] <- "ecozone"

unique(dataFF2$Type)
dataFF2$Type <- as.factor(dataFF2$Type)
summary(dataFF2$Type)
dataFF2$ecozone <- as.factor(dataFF2$ecozone)
summary(dataFF2$ecozone)
levels(dataFF2$ecozone) <- toupper(levels(dataFF2$ecozone))
summary(dataFF2$ecozone)
dataFF3 <- dataFF2[grepl(paste(
  "ATLANTIC MARITIME", "BOREAL CORDILLERA", "BOREAL PLAIN", "BOREAL SHIELD",
  "HUDSON PLAIN", "MONTANE CORDILLERA", "PACIFIC MARITIME", "TAIGA CORDILLERA",
  "TAIGA PLAIN", "TAIGA SHIELD", "MIXEDWOOD PLAIN",
  sep = "|"
), dataFF2$ecozone), ]
summary(dataFF3$ecozone)
range(dataFF3$PPT_sm)
range(dataFF3$Tave_sm)
range(dataFF3$DD5)
range(dataFF3$TSLF)
range(dataFF3$total_BA)
dataFF3 <- subset(dataFF3, TSLF > 0 & PPT_sm >= 0 & LCC > 0)
dataFF3$LCC <- as.factor(as.character(dataFF3$LCC))
str(dataFF3)
summary(dataFF3$LCC)

dataFF3$site_ID <- as.factor((dataFF3$site_ID))
dataFF3$PPT_sm <- as.integer((dataFF3$PPT_sm))
dataFF3$DD5 <- as.integer((dataFF3$DD5))

colnames(dataSyn2)
colnames(dataFF3)

DatasetAge0 <- rbind(dataSyn2, dataFF3)
DatasetAge0 <- na.omit(DatasetAge0[, -3])
nrow(DatasetAge0)
summary(DatasetAge0$ecozone)
summary(DatasetAge0$LCC)
summary(droplevels(DatasetAge0$ecozone))
colnames(DatasetAge0)

png(file.path(figsDir, "hist_DatasetAge0_TSLF.png"))
hist(DatasetAge0$TSLF)
dev.off()

DatasetAge0 <- subset(DatasetAge0, TSLF < 600) # we avoid prehistoric wood
summary(DatasetAge0$ecozone)
levels(DatasetAge0$ecozone)[levels(DatasetAge0$ecozone) == "TAIGA SHIELD EAST"] <- "TAIGA SHIELD"
levels(DatasetAge0$ecozone)[levels(DatasetAge0$ecozone) == "TAIGA SHIELD WEST"] <- "TAIGA SHIELD"

DatasetAge1 <- DatasetAge0

png(file.path(figsDir, "hist_DatasetAge1_total_BA.png"))
hist(DatasetAge1$total_BA)
dev.off()

summary(DatasetAge1$ecozone)
DatasetAge1 <- DatasetAge1[which(!(DatasetAge1$ecozone %in% c(
  "TAIGA CORDILLERA", "PACIFIC MARITIME", "MIXEDWOOD PLAIN",
  "ATLANTIC MARITIME", "BOREAL CORDILLERA", "MONTANE CORDILLERA"
))), ]
summary(DatasetAge1$LCC)
DatasetAge1 <- subset(DatasetAge1, !(LCC %in% c("0", "3", "4", "7", "9", "15", "16", "17", "18")))

png(file.path(figsDir, "hist_DatasetAge1_TSLF.png"))
hist(DatasetAge1$TSLF)
dev.off()

levels(DatasetAge1$LCC)[grepl("11|12|13", levels(DatasetAge1$LCC))] <- "11_12_13"
DatasetAge1$Type <- as.factor(DatasetAge1$Type)
summary(DatasetAge1$Type)

DatasetAge1_sp <- DatasetAge1
coordinates(DatasetAge1_sp) <- c("longitude", "latitude")
DatasetAge1_sf <- st_as_sf(DatasetAge1_sp)
st_crs(DatasetAge1_sf) <- "epsg:4326"
DatasetAge1_sf <- st_transform(DatasetAge1_sf, targetProj) ## TODO: use targetCRS
DatasetAge1_sp <- as_Spatial(DatasetAge1_sf)
DatasetAge1_proj <- as.data.frame(DatasetAge1_sp) ## coords.x1 ~= longitude; coords.x2 ~= latitude

# get/load spatial data -----------------------------------------------------------------------

canProvs <- raster::getData("GADM", path = inputDir, country = "CAN", level = 1, type = "sf")
st_crs(canProvs) <- st_crs(canProvs) ## fix "old-style crs" warning from sf
canProvs <- st_transform(canProvs, crs = targetProj) ## TODO: use targetCRS
canProvs <- as_Spatial(canProvs)

png(file.path(figsDir, "DatasetAge1_all.png"))
plot(canProvs)
plot(DatasetAge1_sp, add = TRUE)
dev.off()

studyArea_ROF <- prepInputs(
  url = "https://drive.google.com/file/d/1DzVRglqJNvZA8NZZ7XKe3-6Q5f8tlydQ/",
  targetCRS = targetProj,
  targetFile = "ROF_RA_def.shp", alsoExtract = "similar",
  fun = "sf::st_read",
  destinationPath = inputDir,
  filename2 = "ROF_RA_def_50km_buff",
  overwrite = TRUE
)
compareCRS(targetProj, studyArea_ROF, DatasetAge1_sp)

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

source("scripts/prepInputsTerra.R")

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

  f_tave <- file.path(inputDir, "Normal_1981_2010_Tave_sm.tif")
  Tave <- Cache(
    prepInputs_Terra,
    url = "https://s3-us-west-2.amazonaws.com/www.cacpd.org/CMIP6/normals/Normal_1981_2010_bioclim.zip",
    targetFile = basename(f_tave),
    destinationPath = dirname(f_tave),
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

  ## NOTE: no longer using the 1km product
  # f_tave <- file.path(inputDir, "Normal_1981_2010_Tave_sm.tif")
  # Tave <- Cache(
  #   prepInputs_Terra,
  #   url = "https://s3-us-west-2.amazonaws.com/www.cacpd.org/CMIP6/normals/Normal_1981_2010_bioclim.zip",
  #   targetFile = basename(f_tave),
  #   destinationPath = dirname(f_tave),
  #   rasterToMatch = LCC2015
  # )

  ft <- "Normal_1981_2010S/Tave_sm.asc"
  fz <- file.path(inputDir, "Normal_1981_2010S.zip")
  if (!file.exists(ft)) {
    drive_download(as_id("1d5wtRQGjHje6aE5ghDaAp9mzuGFkmWoA"), fz) ## 3 arcmin output from ClimateNA
    archive::archive_extract(fz, dir = inputDir, files = ft)
  }

  Tave <- terra::rast(file.path(inputDir, ft)) / 10 ## originally an integer (and x10)
  crs(Tave) <- "EPSG:4326"
  Tave <- setMinMax(Tave)
  Tave <- postProcessTerra(
    from = Tave,
    to = LCC2015,
    destinationPath = inputDir
  )

  wildfires <- Cache(LandR::getWildfire_NFI, dPath = inputDir, rasterToMatch = LCC2015)
}

## create time since fire (TSF) layer
dataYear <- terra::rast(wildfires) ## template using `wildfires`
dataYear[] <- 2015L ## TODO: adjust this to match the years of the relevant data

tsf <- dataYear - wildfires

ecozone_shp <- prepInputs(
  url = "https://sis.agr.gc.ca/cansis/nsdb/ecostrat/zone/ecozone_shp.zip",
  targetFile = "ecozones.shp",
  alsoExtract = "similar",
  fun = "sf::st_read",
  destinationPath = inputDir,
  studyArea = studyArea_ROF,
  targetCRS = targetProj
)

ecozone_shp$ZONE_NAME <- as.factor(ecozone_shp$ZONE_NAME)
ecozone <- fasterize::fasterize(ecozone_shp, raster(LCC2015), field = "ZONE_NAME", fun = "sum")

## TODO: `terra` not working here?
prevAgeLayer <- Cache(
  prepInputs,
  url = "https://drive.google.com/file/d/1hKyVbPyM9bR09u465fusa5mU7_cz-iZz/", ## orig 250 m layer
  targetFile = "standAgeMap2011_ROF.tif",
  destinationPath = inputDir,
  rasterToMatch = raster(LCC2015)
)
#plot(prevAgeLayer)

stopifnot(compareCRS(LCC2015, ba, Tave, ecozone, wildfires, prevAgeLayer))

## ROF Dataset for predictions -----------------------------------------------------------------

## ROF region
png(file.path(figsDir, "prevAgeLayer_w_points.png"), width = 1200, height = 600)
plot(prevAgeLayer)
plot(as_Spatial(studyArea_ROF), add = TRUE)
plot(DatasetAge1_sp, col = "blue", add = TRUE)
dev.off()

DatasetAge1_ROF <- st_intersection(DatasetAge1_sf, studyArea_ROF)
rasValue0 <- terra::extract(prevAgeLayer, terra::vect(as_Spatial(DatasetAge1_ROF)))
#rasValue0 <- na.omit(rasValue0)

DatasetAge2 <- as.data.frame(cbind(
  as.data.frame(DatasetAge1_ROF), coordinates(as_Spatial(DatasetAge1_ROF)), rasValue0
))
id <- which(colnames(DatasetAge2) == "standAgeMap2011_ROF")
colnames(DatasetAge2)[id] <- "PrevAge"
DatasetAge3 <- na.omit(DatasetAge2)
DatasetAge3$predictAge <- exp(predict(modage2, DatasetAge3)) ## TODO: confirm this line

## ROF Target resolution:300 x 300 m
LCC_simpoints <- Cache(rasterToPoints, x = raster(LCC_sim), progress = "text")
LCC_simpointsdf <- as.data.frame(LCC_simpoints) ## TODO: use data.table (NOTE: weird issue with S4 conversion?)
colnames(LCC_simpointsdf) <- c("coords.x1", "coords.x2")
rasStack <- stack(LCC2015, ba, Tave, ecozone, wildfires, prevAgeLayer)
rasValue1 <- raster::extract(rasStack, LCC_simpoints)
DatasetAgeROF <- as.data.frame(cbind(LCC_simpoints, rasValue1))
head(DatasetAgeROF)
colnames(DatasetAgeROF) <- c("coords.x1", "coords.x2", "LCC", "total_BA", "Tave_sm", "ecozone", "wildfires", "prevAge")
head(DatasetAgeROF)

# here we include the time since last fire for de pixels with recorded information of fire history
DatasetAgeROF2 <- na.omit(DatasetAgeROF)
str(DatasetAgeROF2)
# DatasetAgeROF2$year_BA <- 2015
# DatasetAgeROF2$year_BA<-as.integer(DatasetAgeROF2$year_BA)
DatasetAgeROF2$ecozone <- as.factor(as.character(DatasetAgeROF2$ecozone))
summary(DatasetAgeROF2$ecozone)
levels(DatasetAgeROF2$ecozone)[levels(DatasetAgeROF2$ecozone) == "10"] <- "BOREAL SHIELD"
levels(DatasetAgeROF2$ecozone)[levels(DatasetAgeROF2$ecozone) == "11"] <- "HUDSON PLAIN"
DatasetAgeROF2 <- subset(DatasetAgeROF2, !(ecozone %in% c("3"))) # SOUTHERN ARTIC, IF WE DON'T INCLUDE ECOZONE AS A PREDICTOR WE DON'T NEED TO RUN THIS LINE

DatasetAgeROF2$LCC <- as.factor(as.character(DatasetAgeROF2$LCC))
summary(DatasetAgeROF2$LCC)
DatasetAgeROF2 <- subset(DatasetAgeROF2, !(LCC %in% c("0", "3", "4", "7", "9", "15", "16", "17", "18")))
summary(DatasetAgeROF2$LCC)
DatasetAgeROF2 <- subset(DatasetAgeROF2, Tave_sm > 0)
DatasetAgeROF2 <- subset(DatasetAgeROF2, total_BA > 0)
levels(DatasetAgeROF2$LCC)[grepl("11|12|13", levels(DatasetAgeROF2$LCC))] <- "11_12_13"

DatasetAgeROF2$TypeData <- "PredDataset"
DatasetAgeROF2$TSLF <- "TSLF"
colnames(DatasetAgeROF2)

DatasetAge1_proj$TypeData <- "InputDataset"
DatasetAge1_proj$wildfires <- "wildfires"
DatasetAge1_proj$prevAge <- "prevAge"
colnames(DatasetAge1_proj)

DatasetAge1_proj <- DatasetAge1_proj[, c(
  "coords.x1", "coords.x2", "LCC", "total_BA", "Tave_sm",
  "ecozone", "wildfires", "prevAge", "TypeData", "TSLF"
)]

DataInputPred <- rbind(DatasetAgeROF2, DatasetAge1_proj) # I want to scale the coordinates of both datasets together
DataInputPred$sccoords.x1 <- scale(DataInputPred$coords.x1)
DataInputPred$sccoords.x2 <- scale(DataInputPred$coords.x2)

DatasetAgeROF2 <- subset(DataInputPred[, -c(10)], TypeData == "PredDataset") ## TODO: don't index manually
DatasetAge1_proj <- subset(DataInputPred[, -c(7, 8)], TypeData == "InputDataset") ## TODO: don't index manually
str(DatasetAge1_proj)
DatasetAge1_proj$TSLF <- as.numeric(DatasetAge1_proj$TSLF)

## the model
## NOTE: need too much RAM to run below with the parameter select=TRUE

modage2 <- bam(TSLF ~ s(total_BA) +
                 s(Tave_sm) +
                 LCC +
                 ecozone +
                 ecozone * LCC +
                 ti(total_BA, Tave_sm) +
                 # s(total_BA, by = LCC) +
                 # s(total_BA, by = ecozone) +
                 # s(Tave_sm, by = LCC) +
                 # s(Tave_sm, by = ecozone)+
                 s(sccoords.x1, sccoords.x2, bs = "gp", k = 100, m = 2),
               data = DatasetAge1_proj, method = "fREML", family = nb(), drop.intercept = FALSE, discrete = TRUE
)
AIC(modage2)
summary(modage2)

png(file.path(figsDir, "modage2.png"), width = 1200, height = 600, pointsize = 12)
par(mfrow = c(2, 2), mar = c(4, 4, 2, 1))
gam.check(modage2, rep = 100)
dev.off()

DatasetAge1_proj$predictAge <- exp(predict(modage2, DatasetAge1_proj))

FigHist1 <- ggplot(DatasetAge1_proj, aes(x = TSLF)) +
  xlim(0, 300) +
  geom_histogram() +
  ylab("Observed (Years)") +
  xlab("Predicted (Years)") +
  facet_wrap(~ecozone, ncol = 1) +
  ggtitle("Plot age -Input data-") +
  theme_bw()

FigHist2 <- ggplot(DatasetAge1_proj, aes(x = predictAge)) +
  xlim(0, 300) +
  geom_histogram(fill = "brown4") +
  ylab("Observed (Years)") +
  xlab("Predicted (Years)") +
  facet_wrap(~ecozone, ncol = 1) +
  ggtitle("Plot age -Predicted values-") +
  theme_bw()

FigHist <- ggpubr::ggarrange(FigHist1, FigHist2)
ggsave(file.path(figsDir, "plot_age_pred_vs_obs_hists.png"), FigHist)

# Predicted vs Observed all ecozones included--> new Age layer
cor.test((DatasetAge1_proj$predictAge), DatasetAge1_proj$TSLF)
Fig1 <- ggplot(DatasetAge1_proj, aes(y = TSLF, x = (predictAge))) +
  geom_point() +
  ggtitle("Plot age") +
  ylab("Observed (Years)") +
  xlab("Predicted (Years)") +
  geom_smooth(method = lm, se = FALSE, size = 1) +
  xlim(0, 300) +
  ylim(0, 300) +
  facet_wrap(~ecozone) +
  theme_bw()
ggsave(file.path(figsDir, "Fig1.png"), Fig1)

## Predicted vs Observed for the ground plots within the ROF region
DatasetAge3$predictAge <- exp(predict(modage2, DatasetAge3))
cor.test(
  DatasetAge3[which(DatasetAge3$TSLF > 30 & DatasetAge3$PrevAge > 30), ]$predictAge,
  DatasetAge3[which(DatasetAge3$TSLF > 30 & DatasetAge3$PrevAge > 30), ]$TSLF
)
Fig2 <- ggplot(
  DatasetAge3[which(DatasetAge3$TSLF > 30 & DatasetAge3$predictAge > 30), ],
  aes(y = TSLF, x = (predictAge))
) +
  geom_point() +
  ggtitle("ROF region -NEW Age layer-") +
  ylab("Observed (Years)") +
  xlab("Predicted (Years)") +
  geom_smooth(method = lm, se = FALSE, size = 1) +
  xlim(30, 200) +
  ylim(30, 200) +
  facet_wrap(~ecozone) +
  # annotate("text", x = 20, y = 200, label = "r=0.46, p<0.001", hjust = 0, vjust = 0, fontface = 1, size = 4) +
  theme_bw()

cor.test(
  DatasetAge3[which(DatasetAge3$TSLF > 30 & DatasetAge3$PrevAge > 30), ]$PrevAge,
  DatasetAge3[which(DatasetAge3$TSLF > 30 & DatasetAge3$PrevAge > 30), ]$TSLF
)
Fig3 <- ggplot(
  DatasetAge3[which(DatasetAge3$TSLF > 30 & DatasetAge3$PrevAge > 30), ],
  aes(y = TSLF, x = PrevAge)
) +
  geom_point() +
  ggtitle("ROF region -Previous Age layer-") +
  ylab("Observed (Years)") +
  xlab("Predicted (Years)") +
  geom_smooth(method = lm, se = FALSE, size = 1) +
  xlim(0, 200) +
  ylim(0, 200) +
  facet_wrap(~ecozone) +
  # annotate("text", x = 20, y = 200, label = "r=0.46, p<0.001", hjust = 0, vjust = 0, fontface = 1, size = 4) +
  theme_bw()
Figs23 <- ggpubr::ggarrange(Fig2, Fig3, labels = "AUTO")
ggsave(file.path(figsDir, "Figs23.png"), Figs23)

## ROF predictions
hist(DatasetAgeROF2$wildfires)
unique(DatasetAgeROF2$wildfires)
DatasetAgeROF2$wildfires <- ifelse(DatasetAgeROF2$wildfires < 1970, NA, DatasetAgeROF2$wildfires)
DatasetAgeROF2$wildfires <- 2015 - DatasetAgeROF2$wildfires

DatasetAgeROF2$predictAge <- exp(predict(modage2, DatasetAgeROF2))
DatasetAgeROF2$predictAge <- round(DatasetAgeROF2$predictAge, 0)
hist(DatasetAgeROF2$predictAge)
head(DatasetAgeROF2$predictAge)
DatasetAgeROF2$predictAge[!is.finite(DatasetAgeROF2$predictAge)] <- NA
DatasetAgeROF3 <- subset(DatasetAgeROF2, predictAge < 300 & predictAge > 0)
range(na.omit(DatasetAgeROF3$predictAge))
hist((DatasetAgeROF3$predictAge))

## substitute predictions with time since last fire for the plots with information about wildfires
DatasetAgeROF3$predictAge <- ifelse(!is.na(DatasetAgeROF3$wildfires), DatasetAgeROF3$wildfires, DatasetAgeROF3$predictAge)
DatasetAgeROF3$predictAge <- as.numeric(DatasetAgeROF3$predictAge)
hist((DatasetAgeROF3$predictAge))

## create new age raster layer -----------------------------------------------------------------

## TODO: use terra equivalents
template_raster <- raster(raster(LCC2015)) ## use as template
ageLayerNew30 <- rasterize(
  x = DatasetAgeROF3[, 1:2], # lon-lat data
  y = template_raster,
  field = DatasetAgeROF3[, 7], # vals to fill raster with
  fun = mean
)
plot(ageLayerNew30) ## TODO: use ggplot, save to png

## previous Age layer
plot(prevAgeLayer)
rasValue2 <- terra::extract(prevAgeLayer, LCC_simpoints)
DatasetAgeROF4 <- as.data.frame(cbind(LCC_simpoints, rasValue2))
colnames(DatasetAgeROF4)[3] <- "PrevAge"
DatasetAgeROF4 <- na.omit(DatasetAgeROF4)
hist(DatasetAgeROF4$PrevAge)

## final layer for simulations:
ageLayerNew <- terra::aggregate(ageLayerNew30, fact = targetRes / 30, fun = modal, dissolve = FALSE)

gc()

## upload files to google drive ----------------------------------------------------------------
if (isTRUE(reupload)) {
  ## input rasters (write, zip, then upload)
  lapply(names(prebuiltRasterFilenames), function(f) {
    checkPath(prebuiltRasterDirNames[[f]], create = TRUE)
    terra::writeRaster(get(f, envir = .GlobalEnv),
      file.path(prebuiltRasterDirNames[[f]], prebuiltRasterFilenames[[f]]),
      overwrite = TRUE
    )
    z <- extension(prebuiltRasterDirNames[[f]], "zip")
    archive::archive_write_dir(z, prebuiltRasterDirNames[[f]])

    retry(quote(drive_put(z, as_id("1DzbbVSYp0br-MIi1iI0EPMGGy4BRrjnk"))), retries = 5, exponentialDecayBase = 2)
  })

  ## output figures + final raster
  gid_results <- as_id("11_VAG1pREuf-9OlFRJPep57yp4iPVgFw")
  filesToUpload <- c(
    list.files(figsDir, full.names = TRUE) ## TODO: add final raster output
  )

  lapply(filesToUpload, function(f) {
    if (file.exists(f)) {
      retry(quote(drive_put(f, gid_results)), retries = 5, exponentialDecayBase = 2)
    }
  })
}
