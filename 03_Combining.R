###############################################################################
### This script takes the outout from the reserves combination script (script_2_reserve.R)
### and the output from the species generation script (script_1.R) and overlays them
### to find the maximum suitability in each of the potential reserves. It's quite slow!
### this is generalized to any plant

################################################################################
### Working directory and load packages
################################################################################

#setwd("R:/rsrch/cb751/lab") 
#setwd("/tmp/.x2go-frr512/media/disk/_Volumes_Biology_rsrch_cb751_lab_Freya")
setwd("/Volumes/Biology/rsrch/cb751/lab/Freya")

library(terra)
library(maps)
library(geodata)
library(tidyr)
library(ggplot2)
library(tidyterra)
library(raster)
library(readxl)

################################################################################
### Assign species
################################################################################

## run this if only want to run one species at a time
#species_names <- c("Mertensia maritima")
#full_species <- "Mertensia maritima"
## Process Cerastium alpinum without a loop
#species <- paste0(substr(full_species, 1, 1), ".", strsplit(full_species, " ")[[1]][2])
#modified_species <- gsub(" ", ".", full_species)

## import species_list data set
species_list <- read_excel("Data/Species_list.xlsx")
## extract the species_name column from species_list data set
species_names <- species_list$species_name

### read in the combined reserves data:
combined <- vect("Data/Reserves/CombinedReserves.shp")

# start loop
for(full_species in species_names) {
  species <- paste0(substr(full_species,1,1), ".", strsplit(full_species, " ")[[1]][2])
  modified_species <- gsub(" ", ".", full_species)
  
  #load the current distributions data
  file_path <- paste0("Data/Current_distributions/", modified_species, ".tif")
  raw_rast <- rast(file_path)
  
  ################################################################################
  ### Read in data
  ################################################################################
  
  # currently only using comb_dist_lc (current and future climate suitability, good habitat)
  cur_dist_lc <- rast(file.path("Results", paste0(species, "_cur_lc.tif")))
  fut_dist_lc <- rast(file.path("Results", paste0(species, "_fut_lc.tif")))
  comb_dist_lc <- rast(file.path("Results", paste0(species, "_com_lc.tif")))
  comb_dist <- rast(file.path("Results", paste0(species, "_com.tif")))
  
  ################################################################################
  ### combine nature reserves and plant suitability
  ################################################################################
  
  # overlay them
  crs(comb_dist_lc) <- crs(combined)
  
  # find the max value within each sssi
  combined_suit <- terra::extract(comb_dist_lc, combined, fun = "max") 
  
  # combine them
  combined <- cbind(combined, combined_suit)
  
  #rename column
  names(combined)[9] <- "comb_suit"
  
  ################################################################################
  ### Choose a threshold. This will be the 10% threshold of the minimum non-zero score within a 10km presence cell
  ################################################################################
  
  raw_rast <- crop(raw_rast, comb_dist)
  
  # Set the CRS of comb_dist to match raw_rast (EPSG:27700)
  crs(comb_dist) <- crs(raw_rast)
  
  thresh_comb <- raw_rast * comb_dist
  
  thresh_comb <- values(thresh_comb)[values(thresh_comb)>0]
  thresh_comb <- na.omit(thresh_comb)
  thresh_comb <-  quantile(thresh_comb, 0.1)
  
  ################################################################################
  ### Plot suitable nature reserves
  ################################################################################
  
  ## to do a quick plot of reserves with potential we can now filter by the 'band1' column of combined:
  
  suitable_reserves <- combined[combined$comb_suit>thresh_comb,]  ## this filters for anywhere above 0
  
  ################################################################################
  ### Compute reserves with existing populations:
  ################################################################################
  
  crs(raw_rast) <- crs(combined)
  reserves_with_sp <- extract(raw_rast, combined, fun = function(x){any(x==1)}) ## find presence value within each reserve
  
  ## combine them:
  reserves_with_sp <- cbind(combined, reserves_with_sp)
  head(reserves_with_sp)
  names(reserves_with_sp)[10:11] <- c( "reserveID", "ActualPresence")
  reserves_with_sp$suitable <- reserves_with_sp$comb_suit > thresh_comb
  
  ################################################################################
  ### Save vector
  ################################################################################
  
  ## to save this efficiently, we need three column for each species in the order of the combined data, so we can simply cbind them back in later
  for_saving <- data.frame(comb_suit = reserves_with_sp$comb_suit, 
                           act_pres = reserves_with_sp$ActualPresence,
                           suitable = reserves_with_sp$suitable)
  
  reserves_with_sp <- reserves_with_sp[reserves_with_sp$suitable | reserves_with_sp$ActualPresence > 0,]
  
  ################################################################################
  ### Add species name to the end of each column
  ################################################################################
  
  # change col names to end with the species names
  current_col_names <- names(for_saving)
  new_col_names <- paste0(current_col_names, "_", species)
  colnames(for_saving) <- new_col_names
  
  # save data
  write.csv(for_saving, file = file.path('Results', paste0(species, "_results.csv")))
  
  ################################################################################
  ### Plot with map of the UK
  ################################################################################
  
  # load map for the UK
  uk <- gadm(country = "GBR", level = 1, resolution = 2,
             path = "data") 
  
  # plot distribution map and save
  pdf(file = paste0("Figures/", species, "_3.pdf"), height = 12, width = 8)
  print(ggplot() +
          geom_spatraster(data = raw_rast, fill = NA) +
          geom_spatvector(data = uk, fill = "white") +
          geom_spatvector(data = reserves_with_sp[!reserves_with_sp$suitable & (reserves_with_sp$ActualPresence==1)], lwd = 1, fill = "firebrick3", fg = "firebrick3") + 
          geom_spatvector(data = reserves_with_sp[reserves_with_sp$suitable & (reserves_with_sp$ActualPresence==1)], lwd = 1, fill = "chartreuse3", fg = "chartreuse3") + 
          geom_spatvector(data = reserves_with_sp[reserves_with_sp$suitable & (reserves_with_sp$ActualPresence==0)], lwd = 1, fill = "blue2", fg = "blue2") +
          theme(plot.background = element_rect(fill = "white"), 
                panel.background = element_rect(fill = "white")))
  dev.off()
  
}