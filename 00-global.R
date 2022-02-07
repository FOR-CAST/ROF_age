if (file.exists(".Renviron")) readRenviron(".Renviron")

## project setup + package installation --------------------------------------------------------

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
  "fasterize", "ggplot2", "googledrive", "mgcv" ## TODO: need tidyr but Rstudio is a jerk
)
Require(c(pkgs2, "raster", "sf", "terra", "reproducible"))

if (identical(Sys.info()[["user"]], "achubaty")) {
  drive_auth(email = "achubaty@for-cast.ca")
}

## project options + set paths ------------------------------------------------------------------

## Google Drive folder IDs
gid_inputs <- as_id("1DzbbVSYp0br-MIi1iI0EPMGGy4BRrjnk")
gid_outputs <- as_id("11_VAG1pREuf-9OlFRJPep57yp4iPVgFw")

## NOTE: many GIS ops on complete national datasets require large amounts of memory (>30 GB)
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

## data from national ground plots --------------------------------------------------------------

source("01-ground-plot-data.R")

## get/load spatial data -----------------------------------------------------------------------

source("02-input-rasters.R")

png(file.path(figsDir, "DatasetAge1_all.png"))
plot(canProvs)
plot(DatasetAge1_sp, add = TRUE)
dev.off()

stopifnot(compareCRS(targetProj, studyArea_ROF))
stopifnot(compareCRS(targetProj, DatasetAge1_sp))

## ROF Dataset for predictions -----------------------------------------------------------------

DatasetAge1_ROF <- st_intersection(DatasetAge1_sf, studyArea_ROF)
rasValue0 <- terra::extract(prevAgeLayer, terra::vect(as_Spatial(DatasetAge1_ROF)))
rasValue0 <- na.omit(rasValue0)

DatasetAge2_ROF <- as.data.frame(cbind(
  as.data.frame(DatasetAge1_ROF), coordinates(as_Spatial(DatasetAge1_ROF)), rasValue0
))
id <- which(colnames(DatasetAge2_ROF) == "standAgeMap2011_ROF")
colnames(DatasetAge2_ROF)[id] <- "PrevAge"
rm(id, rasValue0)
gc()

DatasetAge3_ROF <- na.omit(DatasetAge2_ROF)

## TODO: where is LCC_points used? reduce memory by removing intermediate objects!! # RAS: It is used in L132 and 159
LCC_points <- Cache(rasterToPoints, x = raster(LCC2015), progress = "text") ## requires ~30 GB
LCC_points <- as.data.frame(LCC_points[, -3]) ## drop LCC column
colnames(LCC_points) <- c("coords.x1", "coords.x2")

fo <- file.path(outputDir, "covariate_layers_ROF", "covariate_layers_ROF.tif")
fz <- extension(dirname(fo), "zip")
checkPath(dirname(fo), create = TRUE)

## TODO: tweak/test this and move file upload to section with al other uploads
if (!file.exists(fo)) {
  if (isTRUE(reupload)) {
    ## TODO: Cache this once reproducible better handles terra objs
    rasStack <- terra::rast(list(LCC2015, ba, Tave, ecozones, tsf, prevAgeLayer)) ## TODO: using tsf, not wildfires; update below

    terra::writeRaster(rasStack, fo)
    archive::archive_write_dir(fz, dirname(fo))
    retry(quote(drive_put(fz, gid_outputs)), retries = 5, exponentialDecayBase = 2)
  } else {
    drive_download(as_id("1VVqOFObd8nZdLtyOsfztEihweSTSlXVZ"))
    archive::archive_extract(fz, dir = outputDir) ## TODO: confirm download + extract works
  }
}

## TODO: there's got to be a faster/lower-memory way to do this: #19

##rasStack <- terra::rast(fo) ## ensure loaded from file and not in memory
##rasValue1 <- terra::extract(rasStack, LCC_points) ## TODO: 280+ GB RAM used !!
rasStack <- raster::stack(fo) ## ensure loaded from file and not in memory
rasValue1 <- Cache(raster::extract, x = rasStack, y = LCC_points) ## 100+ GB
rm(rasStack)
gc()

## TODO: note the NaN values in rasValue1 -- are these 'correct', or an error?

DatasetAge_ROF <- as.data.frame(cbind(LCC_points, rasValue1))
# head(DatasetAge_ROF)
colnames(DatasetAge_ROF) <- c("coords.x1", "coords.x2", "LCC", "total_BA", "Tave_sm", "ecozone", "timesincefire", "prevAge")
# head(DatasetAge_ROF)
rm(rasValue1)
gc()

DatasetAgeROF2 <- na.omit(DatasetAge_ROF)# leave this one at least. Thanks.

# str(DatasetAgeROF2)
DatasetAgeROF2$ecozone <- as.factor(as.character(DatasetAgeROF2$ecozone))
# summary(DatasetAgeROF2$ecozone)
levels(DatasetAgeROF2$ecozone)[levels(DatasetAgeROF2$ecozone) == "10"] <- "BOREAL SHIELD"
levels(DatasetAgeROF2$ecozone)[levels(DatasetAgeROF2$ecozone) == "11"] <- "HUDSON PLAIN"
DatasetAgeROF2 <- subset(DatasetAgeROF2, !(ecozone %in% c("3"))) # SOUTHERN ARTIC

DatasetAgeROF2$LCC <- as.factor(as.character(DatasetAgeROF2$LCC))
# summary(DatasetAgeROF2$LCC)
DatasetAgeROF2 <- subset(DatasetAgeROF2, !(LCC %in% c("0", "3", "4", "7", "9", "15", "16", "17", "18")))
# summary(DatasetAgeROF2$LCC)
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
# str(DatasetAge1_proj)
DatasetAge1_proj$TSLF <- as.numeric(DatasetAge1_proj$TSLF) ## TODO: why numeric? needed for model below? # RAS: Yes

## the model -----------------------------------------------------------------------------------

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
AIC(modage2)  ## TODO: write to file
summary(modage2) ## TODO: write to file

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
DatasetAge3_ROF$predictAge <- exp(predict(modage2, DatasetAge3_ROF))
cor.test(
  DatasetAge3_ROF[which(DatasetAge3_ROF$TSLF > 30 & DatasetAge3_ROF$PrevAge > 30), ]$predictAge,
  DatasetAge3_ROF[which(DatasetAge3_ROF$TSLF > 30 & DatasetAge3_ROF$PrevAge > 30), ]$TSLF
)
Fig2 <- ggplot(
  DatasetAge3_ROF[which(DatasetAge3_ROF$TSLF > 30 & DatasetAge3_ROF$predictAge > 30), ],
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
  DatasetAge3_ROF[which(DatasetAge3_ROF$TSLF > 30 & DatasetAge3_ROF$PrevAge > 30), ]$PrevAge,
  DatasetAge3_ROF[which(DatasetAge3_ROF$TSLF > 30 & DatasetAge3_ROF$PrevAge > 30), ]$TSLF
)
Fig3 <- ggplot(
  DatasetAge3_ROF[which(DatasetAge3_ROF$TSLF > 30 & DatasetAge3_ROF$PrevAge > 30), ],
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

# hist(DatasetAgeROF2$wildfires)
# unique(DatasetAgeROF2$wildfires)
DatasetAgeROF2$wildfires <- ifelse(DatasetAgeROF2$wildfires < 1970, NA, DatasetAgeROF2$wildfires)
DatasetAgeROF2$wildfires <- 2015 - DatasetAgeROF2$wildfires ## TODO: use `dataYear` layer?

## predictions for ROF -------------------------------------------------------------------------

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

## TODO: use terra equivalents
template_raster <- raster(raster(LCC2015)) ## use as template
ageLayerNew30 <- rasterize(
  x = DatasetAgeROF3[, 1:2], # lon-lat data
  y = template_raster,
  field = DatasetAgeROF3[, 7], # vals to fill raster with
  fun = mean
)
## TODO: use ggplot + ggsave
png(file.path(figsDir, "age_layer_new_no_wildfires.png")) ## TODO: confirm this is without wildfire overlay
plot(ageLayerNew30)
dev.off()

## previous Age layer
png(file.path(figsDir, "age_layer_orig_250m.png")) ## TODO: confirm this is without wildfire overlay
plot(prevAgeLayer)
dev.off()

## TODO: if all you want is a hist of prev age, use layer directly: `hist(prevAgeLayer)`
rasValue2 <- terra::extract(prevAgeLayer, LCC_points)
DatasetAgeROF4 <- as.data.frame(cbind(LCC_points, rasValue2))
colnames(DatasetAgeROF4)[3] <- "PrevAge" ## TODO: don't hardcode indices
DatasetAgeROF4 <- na.omit(DatasetAgeROF4)

png(file.path(figsDir, "hist_prev_age.png"))
hist(DatasetAgeROF4$PrevAge)
dev.off()

## final (new) age layer for simulations:
ageLayerNew <- terra::aggregate(ageLayerNew30, fact = targetRes / 30, fun = modal, dissolve = FALSE)

fn <- sprintf("age_layer_new_%01dm", targetRes)
fo <- file.path(outputDir, fn, extension(fn, "tif"))
fz <- extension(dirname(fo), "zip")
checkPath(dirname(fo), create = TRUE)
terra::writeRaster(ageLayerNew, fo, overwrite = TRUE)
archive::archive_write_dir(fz, dirname(fo))
retry(quote(drive_put(fz, gid_outputs)), retries = 5, exponentialDecayBase = 2)

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

    retry(quote(drive_put(z, gid_inputs)), retries = 5, exponentialDecayBase = 2)
  })

  ## output figures + rasters
  filesToUpload <- c(
    list.files(figsDir, full.names = TRUE) ## TODO: add final raster output
  )

  lapply(filesToUpload, function(f) {
    if (file.exists(f)) {
      retry(quote(drive_put(f, gid_outputs)), retries = 5, exponentialDecayBase = 2)
    }
  })
}
