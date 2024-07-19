################################################################################
### Libraries and Working directory
################################################################################

# load the libraries
library(terra)
library(dplyr)

################################################################################
### Combining all SSSIs
################################################################################

# load shapefiles
sssi_S <- vect("Data/SSSI/SSSI_Scotland/SSSI_SCOTLAND.shp")
sssi_E <- vect("Data/SSSI/SSSI_England/SSSI_(England).shp")
sssi_W <- vect("Data/SSSI/SSSI_Wales/NRW_SSSI.shp")

# combine shapefiles
sssi <- rbind(sssi_E, sssi_S, sssi_W)

# add a column to idicate type of PA
sssi$source <- "SSSI"

################################################################################
### Combining all NNRs
################################################################################

# load shapefiles
nnr_S <- vect("Data/NNR/NNR_Scotland/NNR_SCOTLAND.shp")
nnr_E <- vect("Data/NNR/NNR_England/NNR_(England).shp")
nnr_W <- vect("Data/NNR/NNR_Wales/NRW_NNR.shp")

# combine shapefiles
nnr <- rbind(nnr_E, nnr_S, nnr_W)

# add a column to indicate type of PA
nnr$source <- "NNR"

################################################################################
### PlantLife
################################################################################

# load PL shapefile
pl <- vect("Data/PLBoundary")

# add a column to indicate type of PA
pl$source <- "PL"

# save PL shapefile
writeVector(pl, file = "Data/Reserves/PLReserves.shp", overwrite = TRUE)

################################################################################
### Combining all reserves 
################################################################################

# rename columns in NNR shapefile
names(nnr)[names(nnr) == "nnr_name"] <- "name"
names(nnr)[names(nnr) == "nnr_area"] <- "area"
names(nnr)[names(nnr) == "area_1"] <- "area"

# rename columns in SSSI shapefile
names(sssi)[names(sssi) == "SSSI_NAME"] <- "name"
names(sssi)[names(sssi) == "name_1"] <- "name"
names(sssi)[names(sssi) == "SSSI_AREA"] <- "area"
names(sssi)[names(sssi) == "area_1"] <- "area"
names(sssi)[names(sssi) == "LATITUDE"] <- "latitude"
names(sssi)[names(sssi) == "EASTING"] <- "easting"
names(sssi)[names(sssi) == "LONGITUDE"] <- "longitude"
names(sssi)[names(sssi) == "NORTHING"] <- "northing"

# alter shapefiles so they only include columns needed
nnr2 <- nnr[, c("easting", "northing", "latitude", "longitude", "source", "name", "area")]
sssi2 <- sssi[, c("easting", "northing", "latitude", "longitude", "source", "name", "area")]

# combine 
combined <- rbind(nnr2, sssi2)

################################################################################
### Save combined reserves 
################################################################################

# save combined shapefile
writeVector(combined, file = "Data/Reserves/CombinedReserves.shp", overwrite = TRUE)
