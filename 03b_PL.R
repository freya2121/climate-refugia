################################################################################
### set working directory and load packages
################################################################################

#setwd("/tmp/.x2go-frr512/media/disk/_Volumes_Biology_rsrch_cb751_lab/Freya")
#setwd("R:/rsrch/cb751/lab/Freya/")
setwd("/Volumes/Biology/rsrch/cb751/lab/Freya")

library(terra)
library(tidyterra)
library(maps)
library(geodata)
library(ggplot2)
library(raster)
library(dplyr)

################################################################################
### load all species results
################################################################################

# identify all species results csv
results_list <- list.files('Results', pattern='results_PL.csv')

# read in combined reserves data:
combined <- vect("Data/Reserves/PLReserves.shp") 

# import species files
species <- strsplit(results_list[1], "_")[[1]][1]
results <- read.csv(file.path('Results', results_list[1]))
results <- results[,-(1)]
all_sp_data <- results

for(f in results_list[-1]){
  species <- strsplit(f, "_")[[1]][1]
  results <- read.csv(file.path('Results', f))
  results <- results[,-(1)]
  all_sp_data <- cbind(all_sp_data, results)
}

all_sp_data <- cbind(all_sp_data, combined)

################################################################################
### combine all species results
################################################################################

current_colnames <- names(all_sp_data)
suitable_columns <- grep("suitable", current_colnames)
all_sp_data$N_suit <- rowSums(all_sp_data[,suitable_columns], na.rm = TRUE)
actual_columns <- grep("act_pres", current_colnames)
all_sp_data$N_act <- rowSums(all_sp_data[,actual_columns], na.rm = TRUE)

################################################################################
### Target locations
################################################################################

# actual presence is false and suitability true - sites for reintroduction
all_sp_data$N_target <- rowSums(all_sp_data[,suitable_columns] * !all_sp_data[,actual_columns], na.rm = TRUE)

# actual presence is true and suitability true - sites for reinforcement and protection
all_sp_data$N_act_suit <- rowSums(all_sp_data[,suitable_columns] * all_sp_data[,actual_columns], na.rm = TRUE)

all_data <- vect(geom(combined), "polygons", atts = all_sp_data, crs = crs(combined))

################################################################################
### Create table
################################################################################

### create an output table by species:
species <- sapply(results_list, FUN = function(x) strsplit(x, "_")[[1]][1])
species_tab <- data.frame(species = rep(species[1], NROW(all_data)),
                          site = all_data[,"NRNAME"], source = all_data[,"SITECODE"],
                          'Suitable' = c(values(all_data[, paste0("suitable_", species[1])])),
                          'Suitable_score' = c(values(all_data[, paste0("comb_suit_", species[1])])),
                          'Current_pres' = c(values(all_data[, paste0("act_pres_", species[1])])))
names(species_tab)[4:6] <- c("Suitable", "Suitable_score", "Presence")

for(i in 2:NROW(species)){
  temp_tab <- data.frame(species = rep(species[i], NROW(all_data)),
                         site = all_data[,"NRNAME"], source = all_data[,"SITECODE"],
                         'Suitable' = c(values(all_data[, paste0("suitable_", species[i])])),
                         'Suitable_score' = c(values(all_data[, paste0("comb_suit_", species[i])])),
                         'Presence' = c(values(all_data[, paste0("act_pres_", species[i])])))
  names(temp_tab)[4:6] <- c("Suitable", "Suitable_score", "Presence")
  species_tab <- rbind(species_tab, temp_tab)
}

################################################################################
### Filter table
################################################################################

# filter table
filtered_species_tab <- species_tab %>%
  # filter based on multiple conditions
  filter(Suitable != FALSE | Presence != FALSE) %>%
  # round Suitable_score to 5 decimal places
  mutate(Suitable_score = round(Suitable_score, 6)) %>%
  # remove duplicate rows
  distinct() %>%
  # add Introduction column based on conditions
  mutate(Introduction = Suitable == TRUE & Presence == FALSE) %>%
  # arrange by species, then by Introduction (descending), and then by Suitable_score (descending)
  arrange(species, desc(Introduction), desc(Suitable_score)) %>%
  # add at risk column based on conditions
  mutate(At_Risk = Suitable == FALSE & Presence == TRUE) %>%
  rename(Species = species) %>%
  rename(Reserve = NRNAME) %>%
  dplyr::select(-SITECODE)

################################################################################
### Save table
################################################################################

# save species table
write.csv(filtered_species_tab, file = "Results/PL_table.csv")

################################################################################
### Table on reserves
################################################################################

# rearrange so focus is on reserves
filtered_reserves_tab <- filtered_species_tab %>%
  select(Reserve, Species, Suitable, Suitable_score, Presence, Introduction, At_Risk) %>%
  arrange(Species, desc(Introduction), desc(Suitable_score))

# save species table
write.csv(filtered_reserves_tab, file = "Results/PL_reserves_table.csv")