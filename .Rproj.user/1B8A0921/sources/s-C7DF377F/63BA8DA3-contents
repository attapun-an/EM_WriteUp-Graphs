# load packages---- 
library(tidyverse)
library(ggplot2)
library(ggrepel)

# import and process data ----
tree_data <- read.csv("TreeData.csv") %>% slice(-211) %>%                       # removes empty row
  rename(Plot_Code = ï..Plot_Code) %>%                                          # renames header which didn't import correctly
  dplyr::select(-c(X, X.1))                                                     # removes empty columns 

tree_data_Asia <- tree_data %>% 
  dplyr::filter(Plot_Code %in% c("SG01", "SG02", "THA01")) %>%                  # filter only plots in Asia
  mutate(Biomass = as.numeric(as.character(Biomass)))                           # turn biomass into a number instead of factor

tree_data_Eurp <- tree_data %>% 
  dplyr::filter(Plot_Code %in% c("SK01", "SK02")) %>%                           # filter only plots in Europe
  separate(., Biomass, into = c("Bio1", "Bio2"), sep = ",") %>%                 # Split by comma into two variables
  mutate(Biomass = as.numeric(paste(Bio1,".",Bio2, sep = ""))) %>%              # turn them into numbers and convert to tonnes
  dplyr::select(-c("Bio1", "Bio2"))
  
tree_data <- rbind(tree_data_Asia, tree_data_Eurp) %>%                          # combine the two data sets
  mutate(Biomass = Biomass/1000) %>%                                            # convert to tonnes 
  group_by(Plot_Code)

plot_averages <- 