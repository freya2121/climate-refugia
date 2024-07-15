################################################################################
### Working directory and load packages
################################################################################

setwd("R:/rsrch/cb751/lab") 
#setwd("/tmp/.x2go-frr512/media/disk/_Volumes_Biology_rsrch_cb751_lab")
#setwd("/Volumes/Biology/rsrch/cb751/lab/Freya")

library(readxl)
library(dplyr)
library(terra)
library(raster)

################################################################################
### 
################################################################################

# load a model species data from Charles' folder 
model_species <- rast("Charles/PhD/Chapter2/Data/Species_datasets/Vascular_plants/period2.mod.raw/Cerastium.alpinum.tif")

# load missing plant current distributions
BSBI_Raw_Data_Full <- read_excel("Freya/Data/BSBI Raw Data Freya Read.xlsx")

# filter so only includes presence if between 1970 and 1986
BSBI_Raw_Data <- BSBI_Raw_Data_Full %>%
  filter(`year from` >= 1970 & `year from` <= 1986 | `year to` >= 1970 & `year to` <= 1986) %>%
  filter(`precision` < 10000)
  
# filter so only one species
BSBI_S.romanzoffiana <- BSBI_Raw_Data %>%
  filter(taxon == "Spiranthes romanzoffiana")

# Convert to point data - do I need to specify CRS?
BSBI_S.romanzoffiana_points <- vect(BSBI_S.romanzoffiana, geom=c("easting", "northing"), crs = crs(model_species))
head(BSBI_S.romanzoffiana_points)

# Save aas tif
S.romanzoffiana_rast <- rasterize(BSBI_S.romanzoffiana_points, model_species, background=NA, touches=FALSE, update=FALSE, 
          cover=FALSE, by=NULL)
values(S.romanzoffiana_rast)[is.na(values(S.romanzoffiana_rast)) & !is.na(values(model_species))] <- 0

# Save the raster object to a .tif file
writeRaster(S.romanzoffiana_rast, filename = "Freya/Data/Current_distributions/S.romanzoffiana.tif", overwrite = TRUE)
