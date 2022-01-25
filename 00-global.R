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
  #install.packages(c("raster", "terra"), repos = "https://rspatial.r-universe.dev")
  sf::sf_extSoftVersion() ## want GEOS 3.9.0, GDAL 3.2.1, PROJ 7.2.1
}

pkgs1 <- c( ## TODO: remove unused packages
  "data.table", "DHARMa", "effects", "foreign", "gamlss", "ggpubr",
  "lme4", "lmerTest",
  "dplyr", "readr", "tidyverse", ## TODO: remove these in favour of data.table
  "performance", "qs", "RCurl", "splines", "styler"
)
Require(pkgs1, require = FALSE) ## don't load/attach yet, just ensure these get installed

pkgs2 <- c(
  .spatialPkgs, "fasterize", "ggplot2", "googledrive", "mgcv", "tidyr"
)
Require(pkgs2) ## install if needed, and load/attach

Require("PredictiveEcology/reproducible@development")

## NOTE: many GIS etc. ops require large amounts of memory (>80 GB)
lowMemory <- if (grepl("for-cast[.]ca", Sys.info()[["nodename"]])) FALSE else TRUE
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

figsDir <- checkPath(file.path(outputDir, "figures"), create = TRUE)

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
  "reproducible.useGDAL" = "force", ## one of FALSE, TRUE, "force"
  "reproducible.useMemoise" = FALSE
)

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
DatasetAge0 <- drop_na(DatasetAge0, latitude)
DatasetAge0 <- drop_na(DatasetAge0, TSLF)
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
dataSyn2 <- drop_na(dataSyn, total_BA)
dataSyn2$year_BA <- as.integer(dataSyn2$year_BA)

# NFI, TREESOURCE
f04 <- file.path(inputDir, "FinalNFI&TreeSource.txt") #
if (!file.exists(f04)) {
  drive_download(as_id("1ROJeiPdI7fvdcPm9MMQMJTseLko0-TZR"), path = f04) # , overwrite =FALSE
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
hist(DatasetAge0$TSLF)
DatasetAge0 <- subset(DatasetAge0, TSLF < 600) # we avoid prehistoric wood
summary(DatasetAge0$ecozone)
levels(DatasetAge0$ecozone)[levels(DatasetAge0$ecozone) == "TAIGA SHIELD EAST"] <- "TAIGA SHIELD"
levels(DatasetAge0$ecozone)[levels(DatasetAge0$ecozone) == "TAIGA SHIELD WEST"] <- "TAIGA SHIELD"

DatasetAge1 <- DatasetAge0
hist(DatasetAge1$total_BA)
summary(DatasetAge1$ecozone)
DatasetAge1 <- DatasetAge1[which(!(DatasetAge1$ecozone %in% c(
  "TAIGA CORDILLERA", "PACIFIC MARITIME", "MIXEDWOOD PLAIN",
  "ATLANTIC MARITIME", "BOREAL CORDILLERA", "MONTANE CORDILLERA"
))), ]
summary(DatasetAge1$LCC)
DatasetAge1 <- subset(DatasetAge1, !(LCC %in% c("0", "3", "4", "7", "9", "15", "16", "17", "18")))
hist(DatasetAge1$TSLF)
levels(DatasetAge1$LCC)[grepl("11|12|13", levels(DatasetAge1$LCC))] <- "11_12_13"
DatasetAge1$Type <- as.factor(DatasetAge1$Type)
summary(DatasetAge1$Type)

DatasetAge1_sp <- DatasetAge1
coordinates(DatasetAge1_sp) <- c("longitude", "latitude")
DatasetAge1_sf <- st_as_sf(DatasetAge1_sp)
st_crs(DatasetAge1_sf) <- "epsg:4326"
DatasetAge1_sf <- st_transform(DatasetAge1_sf, targetCRS)
DatasetAge1_sp <- as_Spatial(DatasetAge1_sf)
DatasetAge1_proj <- as.data.frame(DatasetAge1_sp) ## coords.x1 ~= longitude; coords.x2 ~= latitude ## TODO: confirm
rm(DatasetAge1_sf, DatasetAge1_sp)

## spatial data
studyArea_ROF <- prepInputs(
  url = "https://drive.google.com/file/d/1iOXXIkvY-YaR9BTG_SRd5R_iLstk99n0",
  targetCRS = targetProj,
  targetFile = "ROF_RA_def_50km_buff.shp", alsoExtract = "similar",
  fun = "sf::st_read",
  destinationPath = inputDir,
  filename2 = "ROF_RA_def_50km_buff",
  overwrite = TRUE
)
compareCRS(studyArea_ROF, targetProj)

if (lowMemory) {
  ## use rasters pre-cropped to ROF
  LCC <- prepInputs(
    url = "https://drive.google.com/file/d/13bHz8XEW5sIBZ4Mn-4_hxg-iaWmDEnlO/",
    targetFile = "CAN_LC_2015_CAL_Clip1.tif", alsoExtract = "similar",
    fun = "raster::raster", ## TODO: use terra
    destinationPath = inputDir
  )

  ## NOTE: reprojecting rasters in memory requires too much RAM to not use GDAL (see options above)
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
} else {
  ## use national layers

  ## from https://open.canada.ca/data/en/dataset/4e615eae-b90c-420b-adee-2ca35896caf6
  LCC <- Cache(
    prepInputs,
    url = paste0(
      "https://ftp.maps.canada.ca/pub/nrcan_rncan/Land-cover_Couverture-du-sol/",
      "canada-landcover_canada-couverture-du-sol/CanadaLandcover2015.zip"
    ), ## TODO: use 2010?
    targetFile = "CAN_LC_2015_CAL.tif", alsoExtract = "similar",
    fun = "raster::raster",
    destinationPath = inputDir,
    studyArea = studyArea_ROF,
    useStudyAreaCRS = TRUE
  )
  ## TODO: remove following workaround to fix projection of LCC:
  LCC2 <- Cache(
    terra::project,
    x = terra::rast(LCC),
    y = targetProj ## targetCRS not found in GDAL db
  )
  LCC <- raster(LCC2)
  proj4string(LCC)  ## compare with `targetProj` : OK

  ## from https://open.canada.ca/data/en/dataset/4c0d9755-9347-42f2-bb1b-f4d2ff673254
  ba <- Cache(
    prepInputs,
    # url = "https://opendata.nfis.org/downloads/forest_change/CA_forest_basal_area_2015_NN.zip", ## TODO: server problem
    url = "https://drive.google.com/file/d/1Vyqb3Q-2T45v963RlFb6EApqyPSMKwKO/",
    targetFile = "CA_forest_basal_area_2015_NN.tif", alsoExtract = "similar",
    fun = "raster::raster",
    destinationPath = inputDir,
    rasterToMatch = LCC
  )

  Tave <- Cache(
    prepInputs,
    url = "https://s3-us-west-2.amazonaws.com/www.cacpd.org/CMIP6/normals/Normal_1981_2010_bioclim.zip",
    targetFile = "Normal_1981_2010_Tave_sm.tif", alsoExtract = "similar",
    fun = "raster::raster",
    destinationPath = inputDir,
    rasterToMatch = LCC
  )

  ## TODO: use finer-resolution climate data from ClimateNA desktop app:
  # ft <- "Normal_1981_2010S/Tave_sm.asc"
  # fz <- file.path(inputDir, "Normal_1981_2010S.zip")
  # if (!file.exists(f)) {
  #   drive_download(as_id("1d5wtRQGjHje6aE5ghDaAp9mzuGFkmWoA"), fz)
  #   unzip(fz, files = file.path(inputDir, ft))
  # }
  #
  # Tave <- raster(file.path(inputDir, basename(ft)))
  # crs(Tave) <- "EPSG:4326"
  # Tave <- Cache(
  #   postProcess,
  #   x = Tave,
  #   destinationPath = inputDir,
  #   rasterToMatch = LCC
  # )

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
  ecozone <- fasterize::fasterize(ecozone_shp, ba, field = "ZONE_NAME", fun = "sum")
}

predPrevAge <- Cache(
  prepInputs,
  url = "https://drive.google.com/file/d/14zxLiW_XVoOeLILi9bqpdTtDzOw4JyuP/",
  targetFile = "standAgeMap_it_1_ts_2011_ProROF.tif",
  fun = "raster::raster",
  destinationPath = inputDir
)
## TODO: remove following workaround to fix projection of LCC:
predPrevAge2 <- Cache(
  terra::project,
  x = terra::rast(predPrevAge),
  y = targetProj ## targetCRS not found in GDAL db
)
predPrevAge <- raster(predPrevAge2)
proj4string(predPrevAge) ## compare with `targetProj` : OK

## use different resolution
LCC_sim <- terra::aggregate(LCC2, fact = targetRes / 30, fun = modal, dissolve = FALSE)
res(LCC_sim)
plot(LCC_sim)

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
                 s(coords.x1, coords.x2, bs = "gp", k = 100, m = 2),
               data = DatasetAge1_proj, method = "fREML", family = nb(), drop.intercept = FALSE, discrete = TRUE
)
AIC(modage2)
summary(modage2)

## TODO: plot to file
dev.new(width = 40, height = 20, pointsize = 12)
par(mfrow = c(2, 2), mar = c(4, 4, 2, 1))
gam.check(modage2, rep = 100)
dev.off()

## TODO: remove duplicate code below / cleanup

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

FigHist <- ggarrange(FigHist1, FigHist2)
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

## ROF region
compareCRS(predPrevAge, DatasetAge1_proj, verbose = TRUE)
rasValue0 <- raster::extract(predPrevAge, DatasetAge1_proj)
DatasetAge2 <- as.data.frame(cbind(DatasetAge1_proj, rasValue0))
colnames(DatasetAge2)[11] <- "PrevAge"
DatasetAge3 <- na.omit(DatasetAge2)
DatasetAge3$predictAge <- exp(predict(modage2, DatasetAge3))

# Predicted vs Observed for the ROF region--> new Age layer
cor.test(DatasetAge3[which(DatasetAge3$TSLF > 30 & DatasetAge3$PrevAge > 30), ]$predictAge,
         DatasetAge3[which(DatasetAge3$TSLF > 30 & DatasetAge3$PrevAge > 30), ]$TSLF)
Fig2 <- ggplot(DatasetAge3[which(DatasetAge3$TSLF > 30 & DatasetAge3$predictAge > 30), ],
               aes(y = TSLF, x = (predictAge))) +
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

cor.test(DatasetAge3[which(DatasetAge3$TSLF > 30 & DatasetAge3$PrevAge > 30), ]$PrevAge,
         DatasetAge3[which(DatasetAge3$TSLF > 30 & DatasetAge3$PrevAge > 30), ]$TSLF)
Fig3 <- ggplot(DatasetAge3[which(DatasetAge3$TSLF > 30 & DatasetAge3$PrevAge > 30), ],
               aes(y = TSLF, x = PrevAge)) +
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
Figs23 <- ggarrange(Fig2, Fig3, labels = "AUTO")
ggsave(file.path(figsDir, "Figs23.png"), Figs23)

## create new raster at targetRes
LCC_simpoints <- Cache(rasterToPoints, x = LCC_sim, progress = "text")
LCC_simpointsdf <- as.data.frame(LCC_simpoints) ## TODO: use data.table (NOTE: weird issue with S4 conversion?)
colnames(LCC_simpointsdf) <- c("coords.x1", "coords.x2", "LCC")
rasStack <- stack(LCC, ba, Tave, ecozone)
rasValue1 <- raster::extract(rasStack, LCC_simpoints[, -3])
DatasetAgeROF <- as.data.frame(cbind(LCC_simpoints[, -3], rasValue1))
head(DatasetAgeROF)
colnames(DatasetAgeROF) <- c("coords.x1", "coords.x2", "LCC", "total_BA", "Tave_sm", "ecozone")

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

DatasetAgeROF2$predictAge <- exp(predict(modage2, DatasetAgeROF2)) # ,exclude=c('longitude','latitude')
DatasetAgeROF2$predictAge <- round(DatasetAgeROF2$predictAge, 0)
head(DatasetAgeROF2$predictAge)
DatasetAgeROF2$predictAge[!is.finite(DatasetAgeROF2$predictAge)] <- NA
range(na.omit(DatasetAgeROF2$predictAge))
hist((DatasetAgeROF2$predictAge))
DatasetAgeROF3 <- subset(DatasetAgeROF2, predictAge < 200)
hist((DatasetAgeROF3$predictAge))

## TODO: substitute the predictions by the real time since last fire for the plots with information about FF
## - waiting on Ian (2022-01-24)

r_obj <- raster(extent(LCC), resolution = c(targetRes, targetRes), crs(LCC))

## new age raster layer
rastersim <- rasterize(
  x = DatasetAgeROF3[, 1:2], # lon-lat data
  y = r_obj, # raster object
  field = DatasetAgeROF3[, 7], # vals to fill raster with
  fun = mean
)
plot(rastersim) ## TODO: use ggplot, save to png

## previous Age layer
plot(predPrevAge)
rasValue2 <- raster::extract(predPrevAge, LCC_simpoints[, -3])
DatasetAgeROF4 <- as.data.frame(cbind(LCC_simpoints[, -3], rasValue2))
colnames(DatasetAgeROF4)[3] <- "PrevAge"
DatasetAgeROF4 <- na.omit(DatasetAgeROF4)
hist(DatasetAgeROF4$PrevAge)

gc()
