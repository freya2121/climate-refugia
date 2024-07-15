################################################################################
### set working directory and load packages
################################################################################

#setwd("R:/rsrch/cb751/lab/Freya") 
#setwd("/tmp/.x2go-frr512/media/disk/_Volumes_Biology_rsrch_cb751_lab/Freya")
setwd("/Volumes/Biology/rsrch/cb751/lab/Freya")

#library(raster)
library(terra)
library(ggplot2)
library(magrittr)
library(tidyterra)
library(dplyr)
library(glmmfields)

################################################################################
### import data needed
################################################################################

# import all_data 
all_data_nact <- vect("Results/AllData/AllData_glm.shp")

################################################################################
### analysis - by glm
################################################################################

# glm test
model1 <- glm(N_act_suit ~ elevat_lo0 + elevat_lo1 + lat_dec + long_dec + area_log + hab_count, 
              family = poisson, data = all_data_nact)
# anova chi-squared test
anova(model1, test = "Chi")

# scale 
all_data_nact$lat_dec_s <- scale(all_data_nact$lat_dec)
all_data_nact$long_dec_s <- scale(all_data_nact$long_dec)
all_data_nact$hab_count_s <- scale(all_data_nact$hab_count)
all_data_nact$elevat_m_s <- scale(all_data_nact$elevat_lo0)
all_data_nact$area_log_s <- scale(all_data_nact$area_log)
all_data_nact$elevat_r_s <- scale(all_data_nact$elevat_lo1)

# model with scaled data
model2 <- glm(N_act_suit ~ elevat_m_s + elevat_r_s + lat_dec_s + long_dec_s + area_log_s + hab_count_s,
              family = poisson, data = all_data_nact)
anova_results <- anova(model2, test = "Chi")

# summary of model
model2_summary <- summary(model2)

################################################################################
### create results table
################################################################################

# create a data frame from ANOVA results
anova_table <- data.frame(
  Parameter = row.names(anova_results),  
  Df = anova_results$Df,           
  Resid.Df = anova_results$"Resid. Df", 
  Resid.Dev = anova_results$"Resid. Dev",  
  `P value` = anova_results$`Pr(>Chi)`    
)

# extract parts for  table
coefficients_table <- data.frame(
  Parameter = row.names(model2_summary$coefficients),
  Estimate = model2_summary$coefficients[, "Estimate"],
  `Std Error` = model2_summary$coefficients[, "Std. Error"]
)

# merge tables
combined_results <- merge(coefficients_table, anova_table, all = FALSE)  

# replace the Parameter column values
new_names <- c(area_log_s = "Log(Area) (km2)",
               elevat_m_s = "Log(Mean Elevation) (m)",
               hab_count_s = "Habitat Count",
               lat_dec_s = "Latitude",
               long_dec_s = "Longitude",
               elevat_r_s = "Log(Elevational Range) (m)")
combined_results$Parameter <- unname(sapply(combined_results$Parameter, function(x) new_names[x]))

# Change P.value to 3dp
combined_results$P.value <- ifelse(combined_results$P.value < 0.001, "<0.001", sprintf("%.3f", combined_results$P.value))

# change number of decimal places
combined_results$Estimate <- round(combined_results$Estimate, 3)
combined_results$Std.Error <- round(combined_results$Std.Error, 3)
combined_results$Resid.Df <- round(combined_results$Resid.Df, 3)
combined_results$Resid.Dev <- round(combined_results$Resid.Dev, 3)

# save table
write.csv(combined_results, file = "Results/Stats_Results_Nact.csv")

################################################################################
### Glmmfields
################################################################################

# make all_data_nact a data frame
all_data_nact_df <- as.data.frame(all_data_nact)

# remove NA values
all_data_nact_df <- na.omit(all_data_nact_df)

# carry out glmm
m_spatial <- glmmfields(N_act_suit ~ elevat_m_s + elevat_r_s + lat_dec_s + long_dec_s + area_km2_s + hab_count_s,
                        data = all_data_nact_df, family = poisson(link = "log"),
                        lat = "lat_dec", lon = "long_dec", nknots = 5, iter = 10000, chains = 1,
                        prior_intercept = student_t(3, 0, 10), 
                        prior_beta = student_t(3, 0, 3),
                        prior_sigma = half_t(3, 0, 3),
                        prior_gp_theta = half_t(3, 0, 10),
                        prior_gp_sigma = half_t(3, 0, 3),
                        seed = 123,
                        cluster = "kmeans",
                        verbose = TRUE)

# save results from glmm
saveRDS(m_spatial, file = "Results/m_spatial_model_Nact.rds")

################################################################################
### create a table to save results
################################################################################

# insert results
m_spatial <- readRDS(file = "Results/m_spatial_model_Nact.rds")

# Get summary statistics for the stanfit object
stat_summary <- summary(m_spatial$model)

# The summary object typically includes a list of summary statistics
# Extract mean and sd from the summary output
Mean <- stat_summary$summary[,"mean"]
Sd <- stat_summary$summary[,"sd"]
Lower <- stat_summary$summary[,"2.5%"]
Upper <- stat_summary$summary[,"97.5%"]

# Create a dataframe with statistics and filter for specific parameters
summary_table <- data.frame(Mean = Mean, Sd = Sd, Lower = Lower, Upper = Upper)
filtered_table <- summary_table[grep("B\\[[2-7]\\]", rownames(summary_table)), ]

# Set full names, row names, and round numerical values in one go
filtered_table <- filtered_table %>%
  # Add full names as a new column temporarily
  mutate(Parameter = c("Log(Mean Elevation)", "Log(Elevational Range)", "Latitude", "Longitude", "Log(Area)", "Habitat Count")) %>%
  # Round numeric columns
  mutate(across(where(is.numeric), round, digits = 2)) %>%
  # convert current rownames to a column
  rownames_to_column(var = "Original_Index") %>%
  # move parameters to first in the table
  select(Parameter, Original_Index, everything()) %>%
  # remove original rownames
  select(-Original_Index)

# save results table 
#write.csv(filtered_table, file = "Figures/Final_Analysis/GLMM_Nact.csv")

################################################################################
### create a forest plot
################################################################################

# Add a column to the data indicating whether the Mean is above zero
filtered_table$Colour <- ifelse(filtered_table$Mean > 0.06, "Above Zero", "Not Above Zero")


filtered_table$Parameter <- factor(filtered_table$Parameter, 
                                   levels = c("Habitat Count",   
                                              "Longitude", "Latitude", "Log(Area)", "Log(Elevational Range)", "Log(Mean Elevation)"))

# create the plot
ggplot(filtered_table, aes(x = Mean, y = Parameter, color = Colour)) +
  geom_point() +
  geom_errorbarh(aes(xmin = Lower, xmax = Upper), height = 0) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  scale_color_manual(values = c("Above Zero" = "chartreuse3", "Not Above Zero" = "black")) +
  theme_classic() +
  xlab("Mean and 90% credible interval for covariate coefficient") + ylab("Covariate") +
  theme(legend.title = element_blank(), legend.position = "none")
