if (!exists("pkgDir")) {
  pkgDir <- file.path("packages", version$platform, paste0(
    version$major, ".",
    strsplit(version$minor, "[.]")[[1]][1]
  ))
  
  if (!dir.exists(pkgDir)) {
    dir.create(pkgDir, recursive = TRUE)
  }
  .libPaths(pkgDir)
}

if (!suppressWarnings(require("Require"))) {
  install.packages("Require")
  library(Require)
}

pkgs1 <- c( ## TODO: remove unused packages
  "data.table", "DHARMa", "effects", "foreign", "gamlss", "ggplot2", "ggpubr",
  "lme4", "lmerTest", "mgcv",
  "dplyr", "readr", "tidyverse", ## TODO: remove these in favour of data.table
  "performance", "qs", "RCurl", "splines", "styler"
)
Require(pkgs1, Require = FALSE) ## don't load/attach yet, just ensure these get installed

pkgs2 <- c(
  "googledrive", "tidyr"
)
Require(pkgs2) ## install if needed, and load/attach

sptlPkgs <- c("rgdal", "sf", "terra", "raster", "rgeos") ## TODO: remove raster
if (!all(sptlPkgs %in% rownames(installed.packages()))) {
  install.packages(sptlPkgs, repos = "https://cran.rstudio.com")
  
  sf::sf_extSoftVersion() ## want GEOS 3.9.0, GDAL 3.2.1, PROJ 7.2.1 or higher
}
Require(c(sptlPkgs, "fasterize"))

Require("PredictiveEcology/reproducible@development")

## NOTE: many GIS etc. ops require large amounts of memory (>80 GB)
lowMemory <- if (grepl("for-cast.ca", Sys.info()["nodename"])) FALSE else TRUE

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
  "reproducible.useGDAL" = "force", ## one of FALSE, TRUE, "force"
  "reproducible.useMemoise" = FALSE
)

## NOTE: data available on Google Drive
##   https://drive.google.com/drive/folders/1ZM8i8VZ8BcsxEdxWPE2S-AMO0JvQ9DRI

## input data

f01 <- file.path(inputDir, ".txt")#DatasetAgeNA
if (!file.exists(f01)) {
  drive_download(as_id("1Ig7pNz1eYk5zWTYYpeR5LLYvdGzbV8Mx"), path = f01, overwrite = TRUE)
}
DatasetAge0 <- read.table(f01, header = TRUE, sep = "\t", fill = TRUE, dec = ".") ## TODO: fix error: more columns than column names
colnames(DatasetAge0)
DatasetAge0$ecozone_combined <- as.factor(DatasetAge0$ecozone_combined)
DatasetAge0$ecozone_combined <- factor(DatasetAge0$ecozone_combined, levels(DatasetAge0$ecozone_combined)[c(1, 2, 4, 6, 3, 5)])
DatasetAge0$year_BA <- as.factor(DatasetAge0$year_BA)
summary(DatasetAge0$ecozone_combined)
str(DatasetAge0)
# DatasetAge0<-subset(DatasetAge0,project_ID!='BurnedNWT')
DatasetAge0$year_BA <- as.numeric(as.character(DatasetAge0$year_BA))
DatasetAge0 <- drop_na(DatasetAge0, latitude)
DatasetAge0 <- drop_na(DatasetAge0, TSLF)
DatasetAge0 <- subset(DatasetAge0, ecozone_combined != c("ALASKA INT"))
colnames(DatasetAge0)
DatasetAge0$Type <- "Synthesis"
dataSyn <- DatasetAge0[, c("Type", "site_ID", "burn_ID", "latitude", "longitude", "ecozone", "TSLF",
                     "year_BA", "PPT_sm", "Tave_sm", "DD5", "total_BA", "LCC")]
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

# BNFF, NFI, TREESOURCE
f02 <- file.path(inputDir, "ExtractFirePoints_LCC15_BA15_Ecozone_ROF_ClimaRed2.txt")
if (!file.exists(f02)) {
  drive_download(as_id("1cpgqsFEV6QUD_ZhKLsgn4mA8GwbFLVcD"), path = f02) # , overwrite =FALSE
}

dataFF <- read.table(f02, header = TRUE, sep = "\t", fill = TRUE, dec = ".")
colnames(dataFF)
colnames(dataFF)[8] <- "year_BA"
colnames(dataFF)[5] <- "LCC"
dataFF$Type <- "BNFF"
## TODO: which sive to use as threhold? Including all forest fires, the model doesn't perform well.
dataFF <- subset(dataFF, SIZE_HA > 499)# I suggest taking only large forest fires
dataFF <- dataFF[, c("Type", "site_ID", "burn_ID", "latitude", "longitude", "ecozone", "TSLF", "year_BA", "total_BA", "LCC")]
dataFF2 <- dataFF[, c(2, 4, 5)]
unique(length(dataFF2$site_ID))
colnames(dataFF2) <- c("ID1", "lat", "lon")

## TODO: add the climate from the raster (done in ArcMap)
f03 <- file.path(inputDir, "CoordFF2clima.txt") #
if (!file.exists(f03)) {
  drive_download(as_id("1cQHieqwJPej13D76rDhGiYiSND-sOzwD"), path = f03) # , overwrite =FALSE
}
dataFF2clima <- read.table(f03, header = TRUE, sep = ",", fill = TRUE, dec = ".")
colnames(dataFF2clima) <- c("FID", "site_ID", "latitude", "longitude", "PPT_sm", "Tave_sm", "DD5")
dataFF2clima <- subset(dataFF2clima, PPT_sm > 0 & site_ID != 0)
dataFF2 <- merge(dataFF, dataFF2clima[, -1], by = c("site_ID", "latitude", "longitude"))
dataFF2 <- dataFF2[, c("Type", "site_ID", "burn_ID", "latitude", "longitude", "ecozone", "TSLF",
                       "year_BA", "PPT_sm", "Tave_sm", "DD5", "total_BA", "LCC")]

f04 <- file.path(inputDir, "FinalNFI&TreeSource.txt") #
if (!file.exists(f04)) {
  drive_download(as_id("1ROJeiPdI7fvdcPm9MMQMJTseLko0-TZR"), path = f04) # , overwrite =FALSE
}
dataNFIMORE <- read.table(f04, header = TRUE, sep = "\t", fill = TRUE, dec = ".")
colnames(dataNFIMORE)[6] <- "ecozone"
dataFF2 <- rbind(dataFF2, dataNFIMORE)
unique(dataFF2$Type)
dataFF2$Type<-as.factor(dataFF2$Type)
summary(dataFF2$Type)
dataFF2$ecozone <- as.factor(dataFF2$ecozone)
summary(dataFF2$ecozone)
levels(dataFF2$ecozone) <- toupper(levels(dataFF2$ecozone))
summary(dataFF2$ecozone)
dataFF3 <- dataFF2[grepl(paste("ATLANTIC MARITIME", "BOREAL CORDILLERA", "BOREAL PLAIN", "BOREAL SHIELD",
                               "HUDSON PLAIN", "MONTANE CORDILLERA", "PACIFIC MARITIME", "TAIGA CORDILLERA",
                               "TAIGA PLAIN", "TAIGA SHIELD", "MIXEDWOOD PLAIN", sep = "|"), dataFF2$ecozone), ]
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
DatasetAge0 <- subset(DatasetAge0, TSLF < 600)
summary(DatasetAge0$ecozone)
levels(DatasetAge0$ecozone)[levels(DatasetAge0$ecozone) == "TAIGA SHIELD EAST"] <- "TAIGA SHIELD"
levels(DatasetAge0$ecozone)[levels(DatasetAge0$ecozone) == "TAIGA SHIELD WEST"] <- "TAIGA SHIELD"
DatasetAge1 <- DatasetAge0
# DatasetAge1<-subset(DatasetAge1,Type!='BNFF')# removing the datset of forest fires
# DatasetAge1<-subset(DatasetAge0,Type!='BNFF'&total_BA>0)
# DatasetAge1<-subset(DatasetAge1,total_BA>0)
hist(DatasetAge1$total_BA)
summary(DatasetAge1$ecozone)
DatasetAge1 <- DatasetAge1[which(!(DatasetAge1$ecozone %in% c("TAIGA CORDILLERA", "PACIFIC MARITIME", "MIXEDWOOD PLAIN",
                                            "ATLANTIC MARITIME", "BOREAL CORDILLERA", "MONTANE CORDILLERA"))), ]

DatasetAge1$LCC <- as.numeric(DatasetAge1$LCC)
DatasetAge1 <- DatasetAge1[which(DatasetAge1$LCC < 15), ]
DatasetAge1$LCC <- as.factor(DatasetAge1$LCC)
summary(DatasetAge1$LCC)
levels(DatasetAge1$LCC)[grepl("11|12|13", levels(DatasetAge1$LCC))] <- "11_12_13"
nrow(DatasetAge1[which(DatasetAge1$ecozone == "HUDSON PLAIN"), ])
hist(DatasetAge1$TSLF)

## this project's CRS/projection to use for all spatial data
targetCRS <- paste(
  "+proj=lcc +lat_1=49 +lat_2=77 +lat_0=0 +lon_0=-95",
  "+x_0=0 +y_0=0 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0"
)

studyArea_ROF <- prepInputs(
  url = "https://drive.google.com/file/d/1iOXXIkvY-YaR9BTG_SRd5R_iLstk99n0",
  targetCRS = targetCRS,
  targetFile = "ROF_RA_def_50km_buff.shp", alsoExtract = "similar",
  fun = "sf::st_read",
  destinationPath = inputDir,
  filename2 = "ROF_RA_def_50km_buff",
  overwrite = TRUE
)

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
  
  predPrevAge <- prepInputs(
    url = "https://drive.google.com/file/d/14zxLiW_XVoOeLILi9bqpdTtDzOw4JyuP/",
    targetFile = "standAgeMap_it_1_ts_2011_ProROF.tif",
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
    targetCRS = targetCRS
  )
  
  ## from https://open.canada.ca/data/en/dataset/4c0d9755-9347-42f2-bb1b-f4d2ff673254
  ba <- Cache(
    prepInputs,
    #url = "https://opendata.nfis.org/downloads/forest_change/CA_forest_basal_area_2015_NN.zip", ## TODO: server problem
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
    fun = "sf::st_read",
    destinationPath = inputDir,
    studyArea = studyArea_ROF,
    targetCRS = targetCRS
  )
  
  ecozone <- fasterize::fasterize(ecozones_shp, ba, field = "ZONE_NAME", fun = "sum")
}

## TODO: need ~125m pixels; for now, use lower resolution rasters (1 x 1 km)
res(LCC)
res(ba)
res(Tave)

LCC_1km <-terra::aggregate(LCC, fact = 25,fun=modal,dissolve=FALSE)  # 750 m resolution save it please
res(LCC_1km)
plot(LCC_1km)

ecozone_1km <- terra::aggregate(ecozone, fact = 25,fun=modal) # 750 m resolution save it please
res(ecozone_1km)


# wheneeve I try this process, my R session closes. I cannot continue. Please, can you run these as save the rasters?
values(ba) <- runif(ncell(ba))
ba_1km <- terra::aggregate(ba, fact = 25,fun=mean) # 750 m resolution save it please
res(ba_1km)
f4 <- file.path(inputDir, "ba_1km.tif") # not sure if this is right
writeRaster(ba_1km, f4, overwrite = FALSE)

values(Tave) <- runif(ncell(Tave))
Tave_1km <- terra::aggregate(Tave, fact = 25,fun=mean,na.rm=TRUE) # 750 m resolution save it please
res(Tave_1km)
f5 <- file.path(inputDir, "Tave_1km.tif") # not sure if this is right
writeRaster(Tave_1km, f5, overwrite = FALSE)

rasStack0 <- stack(LCC_1km,ba_1km,Tave_1km,ecozone_1km)

## the model
## NOTE: need too much RAM to run below with the parameter select=TRUE
modage2 <- bam(
  TSLF ~ s(total_BA) + ti(total_BA, Tave_sm) + s(total_BA, by = LCC) +
    s(Tave_sm, by = LCC) +
    s(longitude, latitude, bs = "gp", k = 100, m = 2) +
    s(Tave_sm) + s(ecozone, bs = "re"),
  data = DatasetAge1, method = "fREML", family = nb(), drop.intercept = FALSE, discrete = TRUE
)
# need to do some tests, but the model has improved by including the dataset of forest fires larger than 1000 ha.
AIC(modage2)
summary(modage2)

dev.new(width = 40, height = 20, pointsize = 12)
par(mfrow = c(2, 2), mar = c(4, 4, 2, 1))
gam.check(modage2, rep = 100)
dev.off()

## TODO: remove duplicate code below / cleanup

DatasetAge1$predictAge <- predict(modage2, DatasetAge1) #

FigHist <- ggplot(DatasetAge1, aes(x = TSLF)) +
  xlim(0, 300) +
  geom_histogram() +
  ylab("Observed (Years)") +
  xlab("Predicted (Years)") +
  facet_wrap(~ecozone, ncol = 1) +
  ggtitle("Plot age -Input data-") +
  theme_bw()

FigHist2 <- ggplot(DatasetAge1, aes(x = exp(predictAge))) +
  xlim(0, 300) +
  geom_histogram(fill = "brown4") +
  ylab("Observed (Years)") +
  xlab("Predicted (Years)") +
  facet_wrap(~ecozone, ncol = 1) +
  ggtitle("Plot age -Predicted values-") +
  theme_bw()

## the ring of fire is in the Boreal Shield.
ggarrange(FigHist, FigHist2)

# Predicted vs Observed all ecozones included--> new Age layer
cor.test(exp(DatasetAge1$predictAge), DatasetAge1$TSLF) #
Fig1 <- ggplot(DatasetAge1, aes(y = TSLF, x = exp(predictAge))) +
  geom_point() +
  ggtitle("Plot age") +
  ylab("Observed (Years)") +
  xlab("Predicted (Years)") +
  geom_smooth(method = lm, se = FALSE, size = 1) +
  xlim(0, 300) +
  ylim(0, 300) +
  facet_wrap(~ecozone) +
  theme_bw()
Fig1

# ROF region
DatasetAge2 <- DatasetAge1
# PROBLEM, Altough the projection of the rasters since to be in lat and long, if I plot them I see that they are in UTM, I think.
# That's why the extract is not working
plot(predPrevAge)
coordinates(DatasetAge2) <- ~ longitude + latitude
rasValue0 <- raster::extract(predPrevAge, DatasetAge2)
DatasetAge2 <- as.data.frame(cbind(DatasetAge2, rasValue0))
colnames(DatasetAge2)[12] <- "PrevAge"
DatasetAge3 <- na.omit(DatasetAge2)

# to do a fair comparison, we need to remove the plots after 2011, because the previous age layer
DatasetAge3 <- subset(DatasetAge3, year_BA < 2012)

# Predicted vs Observed for the ROF region--> new Age layer
cor.test(exp(DatasetAge3$predictAge), DatasetAge3$TSLF) # significant
Fig2 <- ggplot(DatasetAge3, aes(y = TSLF, x = exp(predictAge))) +
  geom_point() +
  ggtitle("ROF region -NEW Age layer-") +
  ylab("Observed (Years)") +
  xlab("Predicted (Years)") +
  geom_smooth(method = lm, se = FALSE, size = 1) +
  xlim(0, 200) +
  ylim(0, 200) +
  facet_wrap(~ecozone) +
  annotate("text", x = 20, y = 200, label = "r=0.46, p<0.001", hjust = 0, vjust = 0, fontface = 1, size = 4) +
  theme_bw()
Fig2

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
levels(newdataset$LCC)[grepl("11|12|13", levels(newdataset$LCC))] <- "11_12_13"
newdataset <- subset(newdataset, !(LCC %in% c("3", "4", "7", "9", "15", "16", "17", "18")))
summary(newdataset$LCC)
levels(newdataset$ecozone)[levels(newdataset$ecozone) == "10"] <- "BOREAL SHIELD"
levels(newdataset$ecozone)[levels(newdataset$ecozone) == "11"] <- "HUDSON PLAIN"
newdataset$predictstack <- exp(predict(modage2, newdataset))
head(newdataset$predictstack)
range(newdataset$predictstack)
hist((newdataset$predictstack))
cor.test(newdataset$PreviusStandAge, newdataset$predictstack)
hist((newdataset$PreviusStandAge))
# write.csv(newdataset,"Predictions.csv") ## exported the dataset and created the raster in ArcMap

predAge <- prepInputs(
  url = "https://drive.google.com/file/d/1pxApvFABso78ihpWmONlbygPv1NcI96Q/",
  targetFile = "Predictions_PointToRaster1.tif",
  fun = "raster::raster", ## TODO: use terra
  destinationPath = inputDir
)
plot(predAge)


# add the values of the previous Age layer to datasets of ground plots
colnames(DatasetAge1)
DatasetAge2 <- DatasetAge1
coordinates(DatasetAge2) <- ~ longitude + latitude
rasValue <- extract(predPrevAge, DatasetAge2) ## TODO: object 'rasValue' is overwritten below!
DatasetAge2 <- as.data.frame(cbind(DatasetAge2, rasValue))
colnames(DatasetAge2)[11] <- "PrevAge"

DatasetAge2$predictAge <- predict(modage2, DatasetAge2)

# Predicted vs Observed all ecozones included--> new Age layer
Fig1 <- ggplot(DatasetAge2, aes(y = TSLF, x = exp(predictAge))) +
  geom_point() +
  ggtitle("Plot age") +
  ylab("Observed (Years)") +
  xlab("Predicted (Years)") +
  geom_smooth(method = lm, se = FALSE, size = 1) +
  xlim(0, 300) +
  ylim(0, 300) +
  facet_wrap(~ecozone) +
  theme_bw() +
  theme(legend.position = "right")
Fig1
cor.test(exp(DatasetAge2$predictAge), DatasetAge2$TSLF) ## significant

# ROF region
DatasetAge3 <- na.omit(DatasetAge2)

# Predicted vs Observed for the ROF region--> new Age layer
Fig2 <- ggplot(DatasetAge3, aes(y = TSLF, x = exp(predictAge))) +
  geom_point() +
  ggtitle("ROF region -NEW Age layer-") +
  ylab("Observed (Years)") +
  xlab("Predicted (Years)") +
  geom_smooth(method = lm, se = FALSE, size = 1) +
  xlim(0, 200) +
  ylim(0, 200) +
  facet_wrap(~ecozone) +
  theme_bw() +
  theme(legend.position = "right")
Fig2
cor.test(exp(DatasetAge3$predictAge), DatasetAge3$TSLF) ## significant

# Predicted vs Observed for the ROF region--> old Age layer
Fig3 <- ggplot(DatasetAge3, aes(y = TSLF, x = PrevAge)) +
  geom_point() +
  ggtitle("ROF region -Previous Age layer-") +
  ylab("Observed (Years)") +
  xlab("Predicted (Years)") +
  geom_smooth(method = lm, se = FALSE, size = 1) +
  xlim(0, 200) +
  ylim(0, 200) +
  facet_wrap(~ecozone) +
  theme_bw() +
  theme(legend.position = "right")
Fig3
cor.test(exp(DatasetAge3$PrevAge), DatasetAge3$TSLF) ## non-significant

ggarrange(Fig2, Fig3, labels = "AUTO")

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
f <- file.path(outputDir, "rasValue.qs")
qs::qsave(rasValue, f)
drive_put(f, as_id("1ZM8i8VZ8BcsxEdxWPE2S-AMO0JvQ9DRI"), name = basename(f))
gc()
