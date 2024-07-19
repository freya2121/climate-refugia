################################################################################
### This script combines the results of different species analyses and runs the
### main analysis
################################################################################

################################################################################
### set working directory and load packages
################################################################################

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
results_list <- list.files('Results', pattern='results.csv')

# read in the combined reserves data:
combined <- vect("Data/Reserves/CombinedReserves.shp")

# import the species files
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
all_sp_data$N_suit <- rowSums(all_sp_data[,suitable_columns], na.rm = TRUE)  ## should be how many species find suitable habitat in each reserve
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
### Plot all species on one map
################################################################################

uk <- gadm(country = "GBR", level = 1, resolution = 2,
           path = "data")
england <- uk[uk$NAME_1 == "England" | is.na(uk$NAME_1),]

# plot map - current suitability
pdf(file = "Figures/N_suit.pdf", height = 12, width = 8)
print(ggplot() +
        geom_spatvector(data = uk, fill = "white") +
        geom_spatvector(data = all_data, mapping = aes(fill = N_suit, color = N_suit)) +
        scale_fill_gradient(low = "grey", high = "green") +
        scale_color_gradient(low = "grey", high = "green"))
dev.off()

# plot map - current presence 
pdf(file = "Freya/Figures/N_act.pdf", height = 12, width = 8)
print(ggplot() +
        geom_spatvector(data = uk, fill = "white") +
        geom_spatvector(data = all_data, mapping = aes(fill = N_act, color = N_act)) +
        scale_fill_gradient(low = "blue", high = "green") +
        scale_color_gradient(low = "blue", high = "green"))
dev.off()

# can also make a graph of only the reintroduction
# remove locations where they are not present
pdf(file = "Figures/N_act_suit.pdf", height = 12, width = 8)
print(ggplot() +
        geom_spatvector(data = uk, fill = "white") +
        geom_spatvector(data = all_data, mapping = aes(fill = N_act_suit, color = N_act_suit)) +
        scale_fill_gradient(low = "azure4", high = "chartreuse3") +
        scale_color_gradient(low = "azure4", high = "chartreuse3") +
        labs(title = "Number of Species") +
        theme(panel.background = element_rect(fill = "white", colour = "white"),
              plot.background = element_rect(fill = "white", colour = NA)))
dev.off()

# current suitability - but remove areas that have current presence
# only areas for reintroduction / assisted colonization
pdf(file = "Figures/N_target.pdf", height = 12, width = 8)
print(ggplot() +
        geom_spatvector(data = uk, fill = "white") +
        geom_spatvector(data = all_data, mapping = aes(fill = N_target, color = N_target)) +
        scale_fill_gradient(low = "azure4", high = "blue2") +
        scale_color_gradient(low = "azure4", high = "blue2") +
        labs(title = "Number of Species") +
        theme(panel.background = element_rect(fill = "white", colour = "white"),
              plot.background = element_rect(fill = "white", colour = NA)))
dev.off()

################################################################################
### save all_data
################################################################################

# save
writeVector(all_data, filename = "Results/AllData/AllData.shp", overwrite = TRUE)

################################################################################
### create a table of the sites
################################################################################

# create a table with results
species <- sapply(results_list, FUN = function(x) strsplit(x, "_")[[1]][1])
species_tab <- data.frame(species = rep(species[1], NROW(all_data)),
                          site = all_data[,"name"], source = all_data[,"source"],
                          'Suitable' = c(values(all_data[, paste0("suitable_", species[1])])),
                          'Suitable_score' = c(values(all_data[, paste0("comb_suit_", species[1])])),
                          'Current_pres' = c(values(all_data[, paste0("act_pres_", species[1])])))
names(species_tab)[4:6] <- c("Suitable", "Suitable_score", "Presence")

# carry this out for all species 
for(i in 2:NROW(species)){
  temp_tab <- data.frame(species = rep(species[i], NROW(all_data)),
                         site = all_data[,"name"], source = all_data[,"source"],
                         'Suitable' = c(values(all_data[, paste0("suitable_", species[i])])),
                         'Suitable_score' = c(values(all_data[, paste0("comb_suit_", species[i])])),
                         'Presence' = c(values(all_data[, paste0("act_pres_", species[i])])))  # could add the same 
  names(temp_tab)[4:6] <- c("Suitable", "Suitable_score", "Presence")
  species_tab <- rbind(species_tab, temp_tab)
}

################################################################################
### create a table summarising results
################################################################################

# filter table and add columns for introduction and at risk
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
  mutate(Risk = Suitable == FALSE & Presence == TRUE)

################################################################################
### table of summary
################################################################################

species_summary <- filtered_species_tab %>%
  group_by(species) %>%
  summarize(In_Situ_Refugia = sum(Suitable == TRUE & Presence == TRUE, na.rm = TRUE),
            Ex_Situ_Refugia = sum(Introduction == TRUE, na.rm = TRUE),
            At_Risk = sum(Risk == TRUE, na.rm = TRUE))

# rename column
species_summary <- species_summary %>% 
  rename(Species = species)

# print the results
print(species_summary)

# save data frame
write.csv(species_summary, file = "Results/summary_table.csv")