################################################################################
### set working directory and load packages
################################################################################

#setwd("R:/rsrch/cb751/lab/Freya") 
#setwd("/tmp/.x2go-frr512/media/disk/_Volumes_Biology_rsrch_cb751_lab/Freya")
setwd("/Volumes/Biology/rsrch/cb751/lab/Freya")

library(dplyr)
library(terra)
library(ggplot2)
library(geodata)
library(maps)
library(raster)
library(readxl)

################################################################################
### Select species
################################################################################

# import species_list data set
species_list <- read_excel("Data/Species_list.xlsx", sheet = "Sheet1")

# extract the species_name column from species_list data set
species_names <- species_list$species_name

# extract the habitats column from the species_list data set
habitats <- species_list$habitats

# convert the habitats into lists of numeric values
habitats <- lapply(habitats, function(x) as.numeric(unlist(strsplit(x, ","))))

# assign the species names to the habitat types
names(habitats) <- species_names

################################################################################
### Concept codes
################################################################################

# this is a csv of the species names from the bsbi data
# they link the species id to a species name
concepts <- read.csv(paste0(getwd(),"/Data/Taxon register export.csv"))

################################################################################
### Species data 
################################################################################

# load the species data in a full GB raster format
# current and future projections are loaded at this point
load("Data/current_distributions.Rdata") 
load("Data/allPs2_GB_cur_fut_fullmatrix.Rdata")

# start a loop so the code goes through every species
for ( i in 1:NROW(species_names)) {
  full_species <- species_names[i]
  species <- paste0(substr(full_species,1,1), ".", strsplit(full_species, " ")[[1]][2])
  modified_species <- gsub(" ", ".", full_species)
  print(paste("starting species", species))
  
  ################################################################################
  ### Provide the vector of habitat codes from CEH data for the focal species:
  ################################################################################
  
  # retrieve habitat codes for the current species
  hab_codes <- habitats[[full_species]]
  
  ################################################################################
  ### Species code
  ################################################################################
  
  # this selects the species name
  concept_code <- concepts %>% filter(NAME == full_species)
  concept_code <- concept_code[,1]
  
  if(concept_code %in% dimnames(all.ps)[[2]]) {
    
    ################################################################################
    ### Colours for climate suitability maps 
    ################################################################################
    
    # colour palettes for climate suitability plots
    col_cur <- colorRampPalette(c("lightblue", "darkblue"))
    col_fut <- colorRampPalette(c("#FF7276", "darkred"))
    col_com <- colorRampPalette(c("pink", "purple"))
    col_hab <- colorRampPalette(c("lightgreen", "darkgreen"))
    
    ################################################################################
    ### Current climate suitability
    ################################################################################
    
    # 1 = current climate suitability
    GB.raster[] <- all.ps[,concept_code,1] 
    
    # create a raster object
    cur_dist <- rast(GB.raster)
    
    ################################################################################
    ### Future climate suitability
    ################################################################################
    
    GB.raster[] <- all.ps[,concept_code,2] #2 = future climate suitability
    fut_dist <- rast(GB.raster) #rename per species so it doesn't get confusing 
    
    ################################################################################
    ### Combined climate suitability
    ################################################################################
    
    # create a mask indicating cells present in both rasters 
    comb_dist <- cur_dist * fut_dist 
    
    ################################################################################
    ### UKCEH habitat land cover
    ################################################################################
    
    # import UKCEH land cover raster
    CEH_rast <- rast("Data/gblcm25m2021.tif")
    CEH_rast <- CEH_rast[[1]]
    
    ################################################################################
    ### Combined climate and habitat suitability
    ################################################################################
    
    # create a map of just the right landcover and multiply by the cliamte suitability 
    
    # First disaggregate the suitability map to the finer resolution:
    comb_dist_25 <- disagg(comb_dist, fact=400)
    
    #check resolution is 25m
    res(comb_dist_25) 
    
    ## align crs and extents:
    crs(comb_dist_25) <- crs(CEH_rast)
    CEH_rast <- crop(CEH_rast, comb_dist_25)
    
    # identify habitat suitability by checking if land cover codes match species' habitat codes
    species_hab <- (CEH_rast %in% hab_codes)
    
    # combine them
    comb_dist_lc <- comb_dist_25 * species_hab
    
    ################################################################################
    ### Plot with all maps
    ################################################################################
    
    # plot all figures together
    pdf(file = paste0("Figures/", species, ".pdf"), height = 12, width = 8)
    par(mfrow = c(2, 2))
    plot(cur_dist, col=col_cur(7), main = "Current climate suitability")
    plot(fut_dist, col=col_fut(7), main = "Future climate suitability")
    plot(comb_dist, col=col_com(7), main = "Current and future climate suitability")
    plot(comb_dist_lc, col=col_hab(7), main = "Current and future climate suitability, good habitat")
    dev.off()
    
    ################################################################################
    ### Save
    ################################################################################
    
    writeRaster(cur_dist_lc, file.path("Results", paste0(species, "_cur_lc.tif")), overwrite=TRUE)
    writeRaster(fut_dist_lc, file.path("Results", paste0(species, "_fut_lc.tif")), overwrite=TRUE)
    writeRaster(cur_dist, file.path("Results", paste0(species, "_cur.tif")), overwrite=TRUE)
    writeRaster(fut_dist, file.path("Results", paste0(species, "_fut.tif")), overwrite=TRUE)
    writeRaster(comb_dist, file.path("Results", paste0(species, "_com.tif")), overwrite=TRUE)
    writeRaster(comb_dist_lc, file.path("Results", paste0(species, "_com_lc.tif")), overwrite=TRUE)
  } else { print(paste0("Concept code ", concept_code, " for species ", full_species, " not in modelled outputs."))}
}