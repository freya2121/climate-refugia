################################################################################
### This script looks at the variables that may influence the suitability of 
### receptor sites and prepares the data for stats tests -
### latitude, longitude, habitat type and reserve size
################################################################################

################################################################################
### set working directory and load packages
################################################################################

#library(raster)
library(terra)
library(ggplot2)
library(magrittr)
library(tidyterra)
library(dplyr)

################################################################################
### import data needed
################################################################################

# import all_data 
all_data <- vect("Results/AllData/AllData.shp")

################################################################################
### Elevation
################################################################################

# Load the data and convert to raster
elevation <- rast("Data/Elevation")

# project the elevation data to match the crs of all data
elevation <- project(elevation, crs(all_data))

# crop all data to match the extent of elevation
all_data <- crop(all_data, elevation)

################################################################################
### Mean elevation
################################################################################

# use extract to get elevation values for each feature in all_data
elevation_values_m <- terra::extract(elevation$Elevation, all_data, mean, na.rm = TRUE)

# add the elevation values as a column in all_data
all_data$elevat_m <- elevation_values_m$Elevation 

# remove all negative and zero values
all_data <- subset(all_data, all_data$elevat_m >= 0)
all_data <- subset(all_data, all_data$elevat_m != 0)

# log mean elevation
all_data$elevat_log_m <- log(all_data$elevat_m)

################################################################################
### Elevational range
################################################################################

# use extract to get elevation values for each feature in all_data
elevation_values_r <- terra::extract(elevation$Elevation, all_data, range, na.rm = TRUE)

# add the elevation values as a column in all_data
all_data$elevat_r <- elevation_values_r[,"Elevation.1"] - elevation_values_r[,"Elevation"]

# remove all negative and zero values
all_data <- subset(all_data, all_data$elevat_r >= 0)
all_data <- subset(all_data, all_data$elevat_r != 0)

# log elevational range
all_data$elevat_log_r <- log(all_data$elevat_r)

################################################################################
### latitude
################################################################################

# convert DMS values to decimal
dms_to_decimal <- function(lat) {
  if(is.na(lat) || !grepl("^\\d+:\\d+:\\d+N$", lat)) return(NA_real_)
  parts <- strsplit(gsub("N$", "", lat), ":")[[1]]
  sum(as.numeric(parts) * c(1, 1/60, 1/3600))
}

# Apply this conversion to all values
all_data$lat_dec <- sapply(all_data$latitude, dms_to_decimal)

################################################################################
### longitude
################################################################################

# convert from DMS values to decimal
dms_to_decimal_long <- function(long) {
  if(is.na(long) || !grepl("^\\d+:\\d+:\\d+[EW]$", long)) return(NA_real_)
  hemisphere <- substr(long, nchar(long), nchar(long))
  long <- gsub("[EW]$", "", long)
  parts <- strsplit(long, ":")[[1]]
  decimal_degrees <- sum(as.numeric(parts) * c(1, 1/60, 1/3600))
  if(hemisphere == "W") decimal_degrees <- -decimal_degrees
  return(decimal_degrees)
}

# Apply this conversion to longitude values
all_data$long_dec <- sapply(all_data$longitude, dms_to_decimal_long)

################################################################################
### reserve size
################################################################################

# convert area to km2
all_data$area_km2 <- all_data$area * 0.01

# log area
all_data$area_log <- log(all_data$area)

# remove all negative and zero values
all_data <- subset(all_data, all_data$area_log >= 0)
all_data <- subset(all_data, all_data$area_log != 0)

################################################################################
### land cover type
################################################################################

# import land cover data - UKCEH
CEH_rast <- rast("data/gblcm25m2021.tif")

# extract the first layer of CEH
CEH_rast <- CEH_rast[[1]]

# project CEH to match the crs of all data
CEH_rast <- project(CEH_rast, crs(all_data))

# use extract() to find the habitat type of each nature reserve
extracted_values <- terra::extract(CEH_rast, all_data, na.rm = TRUE) 

# make extracted values a column in all_data
all_data$CEH_hab <- extracted_values[[2]]

# make CEH_hab a factor (so blox plot works)
all_data$CEH_hab_f <- as.factor(all_data$CEH_hab)

################################################################################
### Number of habitat types
################################################################################

# add column that counts number of habitat types in each nature reserve
all_data <- all_data %>%
  group_by(name) %>%
  mutate(hab_count = NROW(unique(CEH_hab))) %>%
  ungroup()

# make hab_count a factor (so box plot works)
all_data$hab_count_f <- as.factor(all_data$hab_count)

################################################################################
### save data for glm
################################################################################

writeVector(all_data, filename = "Results/AllData/AllData_glm.shp", overwrite = TRUE)
