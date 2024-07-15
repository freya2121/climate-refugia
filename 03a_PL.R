###############################################################################
################################################################################## 
###This script takes the outout from the reserves combination script (script_2_reserve.R)
### and the output from the species generation script (script_1.R) and overlays them
### to find the maximum suitability in each of the potential reserves. It's quite slow!
### this is generalized to any plant

################################################################################
### Working directory and load packages
################################################################################

#setwd("R:/rsrch/cb751/lab") 
#setwd("/tmp/.x2go-frr512/media/disk/_Volumes_Biology_rsrch_cb751_lab")
setwd("/Volumes/Biology/rsrch/cb751/lab")

# load packages
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

## use if you only want to run one species at once
## make a vector of species names then this script can be run in a loop:
#species_names <- c("Persicaria vivipara")
## Directly assign Cerastium alpinum to the variable full_species
#full_species <- "Persicaria vivipara"
## Process Cerastium alpinum without a loop
#species <- paste0(substr(full_species, 1, 1), ".", strsplit(full_species, " ")[[1]][2])
#modified_species <- gsub(" ", ".", full_species)

# import species_list data set
species_list <- read_excel("Freya/Data/Species_list.xlsx")
# extract the species_name column from species_list data set
species_names <- species_list$species_name

# read in the combined reserves data:
combined <- vect("Freya/Data/Reserves/PLReserves.shp")

# start loop
for(full_species in species_names) {
 species <- paste0(substr(full_species,1,1), ".", strsplit(full_species, " ")[[1]][2])
 modified_species <- gsub(" ", ".", full_species)

# import current distributions data
file_path <- paste0("Freya/Data/Current_distributions/", modified_species, ".tif")
raw_rast <- rast(file_path)

################################################################################
### Read in data
################################################################################

# currently only using comb_dist_lc (current and future climate suitability, good habitat)
cur_dist <- rast(file.path("Freya/Results", paste0(species, "_cur.tif")))
fut_dist <- rast(file.path("Freya/Results", paste0(species, "_fut.tif")))
comb_dist <- rast(file.path("Freya/Results", paste0(species, "_com.tif")))
comb_dist_lc <- rast(file.path("Freya/Results", paste0(species, "_com_lc.tif")))

################################################################################
### combine nature reserves and plant suitability
################################################################################

### overlay them:
crs(comb_dist_lc) <- crs(combined)

# find the max value within each sssi
combined_suit <- terra::extract(comb_dist_lc, combined, fun = "max") 
# also include current and future separately?

# combine them:
combined <- cbind(combined, combined_suit)
names(combined)[42] <- "comb_suit"

################################################################################
### Choose a threshold. This will be the 10% threshold of the minimum non-zero score within a 10km presence cell
################################################################################

# crop raw_rast to the extent of comb_dist
raw_rast <- crop(raw_rast, comb_dist)

# Set the CRS of comb_dist to match raw_rast (EPSG:27700)
crs(comb_dist) <- crs(raw_rast)

# multiply datasets
thresh_comb <- raw_rast * comb_dist

# extract values that are greater than zero
thresh_comb <- values(thresh_comb)[values(thresh_comb)>0]

# remove na values
thresh_comb <- na.omit(thresh_comb)

# set the 10th percentile value of the thresh_comb data
thresh_comb <-  quantile(thresh_comb, 0.1)

################################################################################
### Plot suitable nature reserves
################################################################################

# to do a quick plot of reserves with potential we can now filter by the 'band1' column of combined:
suitable_reserves <- combined[combined$comb_suit>thresh_comb,]  # this filters for anywhere above 0

################################################################################
### Compute reserves with existing populations:
################################################################################

# set the crs of raw_rast to be the same as combined
crs(raw_rast) <- crs(combined)

# find presence value within each reserve
reserves_with_sp <- terra::extract(raw_rast, combined, fun = function(x){any(x==1)}) 

# combine them:
reserves_with_sp <- cbind(combined, reserves_with_sp)

# rename columns
names(reserves_with_sp)[43:44] <- c( "reserveID", "ActualPresence")

# create new column to indicate if above suitability threshold
reserves_with_sp$suitable <- reserves_with_sp$comb_suit > thresh_comb

################################################################################
### Save vector
################################################################################

# save three column for each species in the order of the combined data
# will cbind them back in later
for_saving <- data.frame(comb_suit = reserves_with_sp$comb_suit, 
                         act_pres = reserves_with_sp$ActualPresence,
                         suitable = reserves_with_sp$suitable)

# filter reserves with sp to only include when suitable is true and actual presence is above zero
reserves_with_sp <- reserves_with_sp[reserves_with_sp$suitable | reserves_with_sp$ActualPresence > 0,]

################################################################################
### Add species name to the end of each column
################################################################################

# change col names to end with the species names
current_col_names <- names(for_saving)
new_col_names <- paste0(current_col_names, "_", species)
colnames(for_saving) <- new_col_names

################################################################################
### Save
################################################################################

# save
write.csv(for_saving, file = file.path('Freya/Results', paste0(species, "_results_PL.csv")))

}