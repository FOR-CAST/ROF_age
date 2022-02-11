if (file.exists(".Renviron")) readRenviron(".Renviron")

## project setup + package installation --------------------------------------------------------

source("01-packages.R")

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

source("02-ground-plot-data.R")

## get/load spatial data -----------------------------------------------------------------------

source("03-input-rasters.R")

png(file.path(figsDir, "DatasetAge1_all.png"))
plot(canProvs)
plot(DatasetAge1_sp, add = TRUE)
dev.off()

stopifnot(compareCRS(targetProj, studyArea_ROF))
stopifnot(compareCRS(targetProj, DatasetAge1_sp))

## ROF Dataset for predictions -----------------------------------------------------------------

DatasetAge1_ROF <- st_intersection(DatasetAge1_sf, studyArea_ROF)
rasValue0 <- terra::extract(prevAgeLayer, terra::vect(DatasetAge1_ROF))
colnames(rasValue0) <- c("ID", "PrevAge")
rasValue0 <- na.omit(rasValue0)

DatasetAge2_ROF <- as.data.frame(cbind(
  as.data.frame(DatasetAge1_ROF), coordinates(as_Spatial(DatasetAge1_ROF)), rasValue0
))
rm(rasValue0)

DatasetAge3_ROF <- na.omit(DatasetAge2_ROF)

LCC_points <- Cache(rasterToPoints, x = raster(LCC2015), progress = "text") ## requires ~30 GB
LCC_points <- as.data.frame(LCC_points[, -which(colnames(LCC_points) == "CAN_LC_2015_CAL")])
colnames(LCC_points) <- c("coords.x1", "coords.x2")

fo <- file.path(outputDir, "covariate_layers_ROF", "covariate_layers_ROF.tif")

if (!file.exists(fo)) {
  drive_download(as_id("1VVqOFObd8nZdLtyOsfztEihweSTSlXVZ"))
  archive::archive_extract(extension(dirname(fo), "zip"), dir = outputDir) ## TODO: confirm download + extract works
}

## TODO: faster/lower-memory way to do this? see #19
##rasValue1 <- terra::extract(rasStack, LCC_points, na.rm = TRUE) ## TODO: 280+ GB RAM used !!
rasStack <- raster::stack(fo)
layerNames <- c("LCC", "total_BA", "Tave_sm", "ecozone", "TSLF", "PrevAge")
names(rasStack) <- layerNames

fq <- file.path(outputDir, "rasValue1.rds")
if (!file.exists(fq)) {
  rasValue1 <- as.data.frame(raster::extract(x = rasStack, y = LCC_points)) ## 100+ GB; 45+ mins
  rasValue1 <- rasValue1[, -which(colnames(rasValue1) == "ID")]

  ## ensure correct datatypes
  rasValue1$LCC <- as.integer(rasValue1$LCC)
  rasValue1$ecozone <- as.integer(rasValue1$ecozone)
  rasValue1$TSLF <- as.integer(rasValue1$TSLF)
  rasValue1$PrevAge <- as.integer(rasValue1$Prev)

  saveRDS(rasValue1, fq)
} else {
  rasValue1 <- readRDS(fq)
}
rm(rasStack)
gc()

DatasetAge_ROF <- as.data.frame(cbind(LCC_points, rasValue1))
stopifnot(colnames(DatasetAge_ROF) == c("coords.x1", "coords.x2", layerNames))
rm(rasValue1)
gc()

# str(DatasetAge_ROF)
DatasetAge_ROF$ecozone <- as.factor(as.character(DatasetAge_ROF$ecozone))
# summary(DatasetAge_ROF$ecozone)
levels(DatasetAge_ROF$ecozone)[levels(DatasetAge_ROF$ecozone) == "1"] <- "BOREAL SHIELD"
levels(DatasetAge_ROF$ecozone)[levels(DatasetAge_ROF$ecozone) == "2"] <- "HUDSON PLAIN"
DatasetAge_ROF <- subset(DatasetAge_ROF, !(ecozone %in% c("3"))) # SOUTHERN ARTIC

DatasetAge_ROF$LCC <- as.factor(as.character(DatasetAge_ROF$LCC))
# summary(DatasetAge_ROF$LCC)
DatasetAge_ROF <- subset(DatasetAge_ROF, !(LCC %in% c("0", "3", "4", "7", "9", "15", "16", "17", "18")))
# summary(DatasetAge_ROF$LCC)
DatasetAge_ROF <- subset(DatasetAge_ROF, Tave_sm > 0)
DatasetAge_ROF <- subset(DatasetAge_ROF, total_BA > 0)
levels(DatasetAge_ROF$LCC)[grepl("11|12|13", levels(DatasetAge_ROF$LCC))] <- "11_12_13"

## NOTE: should be [centered] and scaled together to ensure they are comparable:
cols2use <- c("coords.x1", "coords.x2", layerNames, "source")

DatasetAge_ROF$source <- "DatasetAge_ROF"
stopifnot(colnames(DatasetAge_ROF) == cols2use)

# str(DatasetAge1_proj)
DatasetAge1_proj$source <- "DatasetAge1_proj"
DatasetAge1_proj$PrevAge <- "PrevAge"
DatasetAge1_proj <- DatasetAge1_proj[, cols2use]

# str(DatasetAge3_ROF)
DatasetAge3_ROF$source <- "DatasetAge3_ROF"
DatasetAge3_ROF$PrevAge <- as.integer(DatasetAge3_ROF$PrevAge)
DatasetAge3_ROF <- DatasetAge3_ROF[, cols2use]

tmp <- rbind(
  DatasetAge_ROF[, c("coords.x1", "coords.x2", "source")],
  DatasetAge3_ROF[, c("coords.x1", "coords.x2", "source")],
  DatasetAge1_proj[, c("coords.x1", "coords.x2", "source")]
)
tmp2 <- scale(tmp[, c("coords.x1", "coords.x2")]) ## centered using mean see ?scale
## TODO: save centre/scale attributes to use later when unscaling (#23)
scaled.center <- attributes(tmp2)$`scaled:center`
scaled.scale <- attributes(tmp2)$`scaled:scale`
tmp2 <- as.data.frame(tmp2)
tmp2 <- cbind(tmp2, tmp[, "source"])
colnames(tmp2) <- c("sccoords.x1", "sccoords.x2", "source")

DatasetAge_ROF <- cbind(DatasetAge_ROF, subset(tmp2, source == "DatasetAge_ROF"))
DatasetAge3_ROF <- cbind(DatasetAge3_ROF, subset(tmp2, source == "DatasetAge3_ROF"))
DatasetAge1_proj <- cbind(DatasetAge1_proj, subset(tmp2, source == "DatasetAge1_proj"))

rm(tmp, tmp2)

DatasetAge_ROF$TSLF <- as.integer(DatasetAge_ROF$TSLF)
DatasetAge_ROF$TSLF[DatasetAge_ROF$TSLF < 1970L] <- NA_integer_

## the model -----------------------------------------------------------------------------------

## drop unused columns from input and prediction datasets; keep only those used in modage2 + prevAge
cols2keep <- c("coords.x1", "coords.x2", "sccoords.x1", "sccoords.x2", layerNames) ## TODO: with #23
DatasetAge_ROF <- DatasetAge_ROF[, names(DatasetAge_ROF) %in% cols2keep]
DatasetAge3_ROF <- DatasetAge3_ROF[, names(DatasetAge3_ROF) %in% cols2keep]
DatasetAge1_proj <- DatasetAge1_proj[, names(DatasetAge1_proj) %in% cols2keep]

## NOTE: too much RAM to run below with the parameter select=TRUE
modage2 <- bam(log(TSLF) ~ s(total_BA) +
                 s(Tave_sm) +
                 LCC +
                 ecozone +
                 ecozone * LCC +
                 ti(total_BA, Tave_sm) +
                 s(total_BA, by = LCC) +
                 s(total_BA, by = ecozone) +
                 s(Tave_sm, by = LCC) +
                 s(Tave_sm, by = ecozone)+
                 s(sccoords.x1, sccoords.x2, bs = "gp", k = 100, m = 2),
               data = DatasetAge1_proj, method = "fREML", discrete = TRUE, #drop.intercept = FALSE,
)

sink(file.path(outputDir, "modage2.txt"))
summary(modage2)
AIC(modage2)
sink()

png(file.path(figsDir, "modage2.png"), width = 1200, height = 600, pointsize = 12)
par(mfrow = c(2, 2), mar = c(4, 4, 2, 1))
gam.check(modage2, rep = 100)
dev.off()

sink(file.path(outputDir, "modage2_gam_check.txt"))
gam.check(modage2, rep = 100)
sink()

## NOTE: not all ecozones and LCCs present in ROF area, so some warnings produced; it's OK
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

## predictions for ROF -------------------------------------------------------------------------

source("scripts/misc.R")
f <- findFactors(nrow(DatasetAge_ROF))
n <- f$pos[f$pos >= 200 & f$pos <= 300][1] ## divide into ~250 groups
g <- factor(sort(rank(row.names(DatasetAge_ROF)) %% n))
Ncores <- 10

DatasetAge_ROF_split <- split(DatasetAge_ROF, g)
DatasetAge_ROF_split2 <- parallel::mclapply(DatasetAge_ROF_split, function(x) {
  val <- round(exp(predict(modage2, x)), 0)
  val[!is.finite(val)] <- NA
  x$predictAge <- as.integer(val)
  x
}, mc.cores = Ncores) ## 450 GB
DatasetAge_ROF <- unsplit(DatasetAge_ROF_split2, g)

ff <- file.path(outputDir, "DatasetAge_ROF_predictAge.qs")
qs::qsave(DatasetAge_ROF, ff)
drive_put(ff, gid_outputs, name = basename(ff))

DatasetAgeROF3 <- subset(DatasetAge_ROF, predictAge > 0 & predictAge < 300)
# range(na.omit(DatasetAgeROF3$predictAge))

png(file.path(figsDir, "hist_DatasetAgeROF3_predictAge.png"))
par(mfrow = c(1, 2))
hist(DatasetAgeROF3$predictAge, main = "ROF predicted age (uncorrected)")
hist(DatasetAgeROF3$predictAge, main = "ROF predicted age (corrected with fire data)")
dev.off()

## substitute predictions with time since last fire for the plots with information about wildfires
DatasetAgeROF3$predictAge[!is.na(DatasetAgeROF3$TSLF)] <- DatasetAgeROF3$TSLF
DatasetAgeROF3$predictAge <- as.integer(DatasetAgeROF3$predictAge)

## TODO: use terra equivalents
#template_raster <- raster(raster(LCC2015)) ## use as template
f_age <- file.path(outputDir, "ageLayer_ROF_new_30m.tif")
template_raster <- terra::rast(LCC2015) ## use as template
ageLayerNew30 <- terra::rasterize(
  x = as.matrix(DatasetAgeROF3[, which(colnames(DatasetAgeROF3) %in% c("coords.x1", "coords.x2"))]),
  y = template_raster,
  values = DatasetAgeROF3$predictAge,
  fun = mean,
  filename = f_age
)
drive_put(f_age, gid_outputs, name = basename(f_age))

## TODO: use ggplot + ggsave
png(file.path(figsDir, "age_layer_new_no_wildfires.png")) ## TODO: confirm this is without wildfire overlay
plot(ageLayerNew30)
dev.off()

## previous Age layer
png(file.path(figsDir, "age_layer_orig_250m.png")) ## TODO: confirm this is without wildfire overlay
plot(prevAgeLayer)
dev.off()

png(file.path(figsDir, "hist_prev_age.png"))
hist(prevAgeLayer[])
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

  fo <- file.path(outputDir, "covariate_layers_ROF", "covariate_layers_ROF.tif")
  fz <- extension(dirname(fo), "zip")
  checkPath(dirname(fo), create = TRUE)

  ## TODO: Cache this once reproducible better handles terra objs
  rasStack <- c(LCC2015, ba, Tave, ecozones, tslf, prevAgeLayer)

  terra::writeRaster(rasStack, fo, overwrite = TRUE)
  archive::archive_write_dir(fz, dirname(fo))
  retry(quote(drive_put(fz, gid_outputs)), retries = 5, exponentialDecayBase = 2)

  ## output figures + rasters
  filesToUpload <- c(
    list.files(figsDir, full.names = TRUE), ## TODO: add final raster output
    file.path(outputDir, modage2.txt)
  )

  lapply(filesToUpload, function(f) {
    if (file.exists(f)) {
      retry(quote(drive_put(f, gid_outputs)), retries = 5, exponentialDecayBase = 2)
    }
  })
}
