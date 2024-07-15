################################################################################
### This script is a repeat of 5 except plots graphs and includes line of best  
### fit based off the glmm - 
### This script is for N_act_suit
################################################################################

################################################################################
### set working directory and load packages
################################################################################
#setwd("R:/rsrch/cb751/lab/Freya") 
#setwd("/tmp/.x2go-frr512/media/disk/_Volumes_Biology_rsrch_cb751_lab/Freya")
setwd("/Volumes/Biology/rsrch/cb751/lab/Freya")

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

# import results from spatial model - this is from the next script so will need to edit order!
m_spatial <- readRDS(file = "Results/m_spatial_model_Nact.rds")

################################################################################
### altitude
################################################################################

# Load the data and convert to raster
elevation <- rast("Data/Elevation")

# project the elevation data to match the crs of all data
elevation <- project(elevation, crs(all_data))

# crop all data to match the extent of elevation
all_data <- crop(all_data, elevation)

################################################################################
### elevation range
################################################################################

# use extract to get elevation values for each feature in all_data
elevation_values_r <- terra::extract(elevation$Elevation, all_data, range, na.rm = TRUE)

# add the elevation values as a column in all_data
all_data$elevat_r <- elevation_values_r[,"Elevation.1"] - elevation_values_r[,"Elevation"]

# remove all negative and 0 values
all_data <- subset(all_data, all_data$elevat_r >= 0)
all_data <- subset(all_data, all_data$elevat_r != 0)

# log elevation values
all_data$elevat_log_r <- log(all_data$elevat_r)

# scale logged values
all_data$elevat_log_r_s <- scale(all_data$elevat_log_r)

# extract data needed
# mean
mean_elevat_r <- mean(all_data$elevat_log_r)
# sd
sd_elevat_r <- sd(all_data$elevat_log_r)

# values needed for graph
elev_seq <- seq(min(all_data$elevat_log_r_s), max(all_data$elevat_log_r_s), len = 1000)
elev_seq_unscaled <- exp((sd_elevat_r * elev_seq) + mean_elevat_r) 
median <- exp((elev_seq * 0.64) - 3.01)
lower <- exp((elev_seq * 0.56) - 3.01)
upper <- exp((elev_seq * 0.72) - 3.01)
predicted <- data.frame(elevat_r = elev_seq_unscaled, low = lower, high = upper, median = median)

# plot graph
# 0.64 0.56 0.72
pdf(file = paste0("Figures/Final_Analysis/Elevational_range_Nact1.pdf"), height = 3, width = 4)
print(ggplot(all_data, aes(x = elevat_r, y = N_act_suit)) +
        geom_point(position = position_jitter(width = 0, height = 0.5), colour = rgb(0.4, 0.4, 0.4, 0.2)) +
        geom_ribbon(data = predicted, mapping = aes(x = elevat_r, ymin = lower, ymax = upper), inherit.aes = FALSE, fill = "#7FFF00B3") +
        geom_line(aes(x=elevat_r, y = median), data = predicted, inherit.aes = FALSE, colour = "black") +
  labs(x = "Elevational range (m)", y = "Species with In Situ Refugia") +
  theme_classic() +
  ylim(c(0, 5)) +
  scale_x_log10(breaks = c(1, 10, 100, 1000), labels = c("1", "10", "100", "1000")))
dev.off()

################################################################################
### elevation mean
################################################################################

# use extract to get elevation values for each feature in all_data
elevation_values_m <- terra::extract(elevation$Elevation, all_data, mean, na.rm = TRUE)

# add the elevation values as a column in all_data
all_data$elevat_m <- elevation_values_m$Elevation 

# remove all negative and zero values
all_data <- subset(all_data, all_data$elevat_m >= 0)
all_data <- subset(all_data, all_data$elevat_m != 0)

# save log data to all_data
all_data$elevat_log_m <- log(all_data$elevat_m)

# extract data needed
# mean
mean_elevat_m <- mean(all_data$elevat_log_m)
# sd
sd_elevat_m <- sd(all_data$elevat_log_m)

# values needed for graph
elev_seq <- seq(min(all_data$elevat_log_m_s), max(all_data$elevat_log_m_s), len = 1000)
elev_seq_unscaled <- exp((sd_elevat_m * elev_seq) + mean_elevat_m) 
median <- exp((elev_seq * 0.18) - 3.01)
lower <- exp((elev_seq * 0.08) - 3.01)
upper <- exp((elev_seq * 0.28) - 3.01)
predicted <- data.frame(elevat_m = elev_seq_unscaled, low = lower, high = upper, median = median)

# plot graph and save
pdf(file = paste0("Figures/Final_Analysis/Elevational_mean_Nact1.pdf"), height = 3, width = 4)
print(ggplot(all_data, aes(x = elevat_m, y = N_act_suit)) +
        geom_point(position = position_jitter(width = 0.3, height = 0.5), colour = rgb(0.4, 0.4, 0.4, 0.2)) +
        geom_ribbon(data = predicted, mapping = aes(x = elevat_m, ymin = lower, ymax = upper), inherit.aes = FALSE, fill = "#7FFF00B5") + 
        geom_line(aes(x=elevat_m, y = median), data = predicted, inherit.aes = FALSE, colour = "black") +
        labs(x = "Elevational mean (m)", y = "Species with In Situ Refugia") +
        theme_classic() +
        ylim(c(0, 5)) +
        scale_x_log10(breaks = c(1, 10, 100, 1000), labels = c("1", "10", "100", "1000")))
dev.off()

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

# remove na values
all_data$lat_dec <- na.omit(all_data$lat_dec)

# cale latitude
all_data$lat_dec_s <- scale(all_data$lat_dec)

# extract data needed
# mean
mean_lat <- mean(all_data$lat_dec_s)
# sd
sd_lat <- sd(all_data$lat_dec_s)

# plot graph
pdf(file = paste0("Figures/Final_Analysis/Latitude_Nact.pdf"), height = 3, width = 4)
print(ggplot(all_data, aes(x = lat_dec, y = N_act_suit)) +
        geom_point(position = position_jitter(width = 0.3, height = 0.5), colour = rgb(0.4, 0.4, 0.4, 0.2)) +
        geom_ribbon(aes(ymin = (function(x) exp((sd_lat * (log(x) + mean_lat)) * -0.07 - 3.01))(lat_dec), ymax = (function(x) exp((sd_lat * (log(x) + mean_lat)) * 0.16 - 3.01))(lat_dec)), fill = "#7FFF00B5", alpha = 0.2) +
        stat_function(fun = function(x) exp((sd_lat * (log(x) + mean_lat)) * 0.05 - 3.01)), size = 0.9, colour = "blue2") +
  labs(x = expression(Latitude~(degree)), y = "Species with In Situ Refugia") +
  theme_classic()
dev.off()

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

# Apply this conversion to all values
all_data$long_dec <- sapply(all_data$longitude, dms_to_decimal_long)

# remove na values
all_data$long_dec <- na.omit(all_data$long_dec)

all_data$long_dec_s <- scale(all_data$long_dec)

# mean
mean_long <- mean(all_data$long_dec_s)
# sd
sd_long <- sd(all_data$long_dec_s)

pdf(file = paste0("Figures/Final_Analysis/Longitude_Nact.pdf"), height = 3, width = 4)
print(ggplot(all_data, aes(x = long_dec, y = N_act_suit)) +
        geom_point(position = position_jitter(width = 0.3, height = 0.5), colour = rgb(0.4, 0.4, 0.4, 0.2)) +
        stat_function(fun = function(x) exp((sd_long * ((x) + mean_long)) * 0.55 - 3.01)), size = 0.9) +
  geom_ribbon(aes(ymin = (function(x) exp((sd_long * ((x) + mean_long)) * 0.40 - 3))(long_dec), ymax = (function(x) exp((sd_long * ((x) + mean_long)) * 0.69 - 3.01))(long_dec)), fill = "#7FFF00B5", alpha = 0.2) +
  labs(x = expression(Longitude~(degree)), y = "Species with In Situ Refugia") +
  theme_classic()
dev.off()

################################################################################
### reserve size
################################################################################

# convert to km2
all_data$area_km2 <- all_data$area * 0.01

# add a column for log(area)
all_data$area_log <- log(all_data$area_km2)

# remove negative values
all_data <- subset(all_data, all_data$area_km2 > 0)

# scale data
all_data$area_log_s <- scale(all_data$area_log)

# extract data needed 
# mean
mean_area <- mean(all_data$area_log_s)
# sd
sd_area <- sd(all_data$area_log_s)

# plot graph
pdf(file = paste0("Figures/Final_Analysis/Area_Nact.pdf"), height = 3, width = 4)
print(ggplot(all_data, aes(x = area_log, y = N_act_suit)) +
        geom_point(position = position_jitter(width = 0.3, height = 0.5), colour = rgb(0.4, 0.4, 0.4, 0.2)) +
        stat_function(fun = function(x) exp((sd_area * (log(x) + mean_area)) * 0.15 - 3.01)), size = 0.9, colour = "black") +
  geom_ribbon(aes(ymin = (function(x) exp((sd_area * (log(x) + mean_area)) * 0.09 - 3.01))(area_log), ymax = (function(x) exp((sd_area * (log(x) + mean_area)) * 0.21 - 3.01))(area_log)), fill = "#7FFF00B5", alpha = 0.2) +
  labs(x = expression("Area (" * km^2 * ")"), y = "Species with In Situ Refugia") +
  theme_classic() +
  ylim(c(0, 5)) +
  scale_x_log10(breaks = c(1, 10, 100, 1000), labels = c("1", "10", "100", "1000"))
dev.off()

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
extracted_values <- terra::extract(CEH_rast, all_data, na.rm = TRUE) ## needs a summary function

# make extracted values a column in all_data
all_data$CEH_hab <- extracted_values[[2]]

# make CEH_hab a factor (so blox plot works)
all_data$CEH_hab_f <- as.factor(all_data$CEH_hab)

################################################################################
### Number of habitat types
################################################################################

# add column that counts number of habitat types in each nature reserve
# is name the best column to do this from?
all_data <- all_data %>%
  group_by(name) %>%
  mutate(hab_count = NROW(unique(CEH_hab))) %>%
  ungroup()

# make hab_count a factor (so box plot works)
all_data$hab_count_f <- as.factor(all_data$hab_count)

# plot graph and save
pdf(file = paste0("Figures/Final_Analysis/Habitat_Nact.pdf"), height = 3, width = 4)
ggplot(all_data[all_data$N_act_suit>0,], aes(hab_count_f, N_target)) +  
  geom_bar(position = "dodge", stat = "summary", fun = "mean", width = 0.7, fill = rgb(0.2, 0.2, 0.2, 0.4), colour = "black") +
  geom_errorbar(stat = "summary", fun.data = "mean_se", position = position_dodge(width = 0.7), width = 0.25) +
  labs(x = "Number of Habitat Types", y = "Species with In Situ Refugia") +
  theme_classic()
dev.off()

################################################################################
### analysis - by glm
################################################################################

writeVector(all_data, filename = "Results/AllData/AllData_Nact_glm.shp", overwrite = TRUE)
