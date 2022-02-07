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
# summary(DatasetAge0$ecozone_combined)
# str(DatasetAge0)
#DatasetAge0 <- subset(DatasetAge0, project_ID != "BurnedNWT")
DatasetAge0$year_BA <- as.numeric(as.character(DatasetAge0$year_BA))
DatasetAge0 <- tidyr::drop_na(DatasetAge0, latitude)
DatasetAge0 <- tidyr::drop_na(DatasetAge0, TSLF)
DatasetAge0 <- subset(DatasetAge0, ecozone_combined != c("ALASKA INT"))
# colnames(DatasetAge0)
DatasetAge0$Type <- "Synthesis"

dataSyn <- DatasetAge0[, c(
  "Type", "site_ID", "burn_ID", "latitude", "longitude", "ecozone", "TSLF",
  "year_BA", "PPT_sm", "Tave_sm", "DD5", "total_BA", "LCC"
)]

dataSyn$ecozone <- as.factor(dataSyn$ecozone)
# summary(dataSyn$ecozone)
dataSyn <- subset(dataSyn, ecozone != c("WESTERN CORDILLERA"))
levels(droplevels(dataSyn$ecozone))

dataSyn$LCC <- as.factor(as.character(dataSyn$LCC))
# summary(dataSyn$LCC)
dataSyn <- dataSyn[dataSyn$LCC != "0", ]
droplevels(dataSyn$LCC)

dataSyn2 <- tidyr::drop_na(dataSyn, total_BA)
dataSyn2$year_BA <- as.integer(dataSyn2$year_BA)

## NFI, TREESOURCE
f04 <- file.path(inputDir, "FinalNFI&TreeSource.txt") #
if (!file.exists(f04)) {
  drive_download(as_id("1ROJeiPdI7fvdcPm9MMQMJTseLko0-TZR"), path = f04)
}
dataFF2 <- read.table(f04, header = TRUE, sep = "\t", fill = TRUE, dec = ".")
id <- which(colnames(dataFF2) == "Ecozone")
colnames(dataFF2)[id] <- "ecozone" ## to lower case for downstream
rm(id)
# head(dataFF2)

# unique(dataFF2$Type)
dataFF2$Type <- as.factor(dataFF2$Type)
# summary(dataFF2$Type)

dataFF2$ecozone <- as.factor(dataFF2$ecozone)
# summary(dataFF2$ecozone)
levels(dataFF2$ecozone) <- toupper(levels(dataFF2$ecozone))
# summary(dataFF2$ecozone)

dataFF3 <- dataFF2[grepl(paste(
  "ATLANTIC MARITIME", "BOREAL CORDILLERA", "BOREAL PLAIN", "BOREAL SHIELD",
  "HUDSON PLAIN", "MONTANE CORDILLERA", "PACIFIC MARITIME", "TAIGA CORDILLERA",
  "TAIGA PLAIN", "TAIGA SHIELD", "MIXEDWOOD PLAIN",
  sep = "|"
), dataFF2$ecozone), ]
# summary(dataFF3$ecozone)
# range(dataFF3$PPT_sm)
# range(dataFF3$Tave_sm)
# range(dataFF3$DD5)
# range(dataFF3$TSLF)
# range(dataFF3$total_BA)

dataFF3 <- subset(dataFF3, TSLF > 0 & PPT_sm >= 0 & LCC > 0)
dataFF3$site_ID <- as.factor(dataFF3$site_ID)
dataFF3$LCC <- as.factor(as.character(dataFF3$LCC))
# str(dataFF3)
# summary(dataFF3$LCC)

stopifnot(identical(colnames(dataSyn2), colnames(dataFF3)))

DatasetAge0 <- rbind(dataSyn2, dataFF3)
id <- which(colnames(DatasetAge0) == "burn_ID")
DatasetAge0 <- na.omit(DatasetAge0[, -id])
rm(id)
# nrow(DatasetAge0)
# summary(DatasetAge0$ecozone)
# summary(DatasetAge0$LCC)
# summary(droplevels(DatasetAge0$ecozone))
# colnames(DatasetAge0)

png(file.path(figsDir, "hist_DatasetAge0_TSLF.png"))
hist(DatasetAge0$TSLF)
dev.off()

DatasetAge0 <- subset(DatasetAge0, TSLF < 600) ## we avoid prehistoric wood
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
# summary(DatasetAge1$LCC)
DatasetAge1 <- subset(DatasetAge1, !(LCC %in% c("0", "3", "4", "7", "9", "15", "16", "17", "18")))

png(file.path(figsDir, "hist_DatasetAge1_TSLF.png"))
hist(DatasetAge1$TSLF)
dev.off()

levels(DatasetAge1$LCC)[grepl("11|12|13", levels(DatasetAge1$LCC))] <- "11_12_13"
DatasetAge1$Type <- as.factor(DatasetAge1$Type)
# summary(DatasetAge1$Type)

DatasetAge1_sp <- DatasetAge1
coordinates(DatasetAge1_sp) <- c("longitude", "latitude")
DatasetAge1_sf <- st_as_sf(DatasetAge1_sp)
st_crs(DatasetAge1_sf) <- "epsg:4326"
DatasetAge1_sf <- st_transform(DatasetAge1_sf, targetProj) ## TODO: use targetCRS
DatasetAge1_sp <- as_Spatial(DatasetAge1_sf)
DatasetAge1_proj <- as.data.frame(DatasetAge1_sp) ## coords.x1 ~= longitude; coords.x2 ~= latitude

gc()
