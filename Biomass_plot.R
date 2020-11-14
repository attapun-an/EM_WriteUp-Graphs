# load packages---- 
library(tidyverse)
library(ggplot2)
library(ggrepel)

# import and process data ----
tree_data <- read.csv("Resources/TreeData.csv") %>% slice(-211) %>%                       # removes empty row
  rename(Plot_code = ï..Plot_Code) %>%                                          # renames header which didn't import correctly
  dplyr::select(-c(X, X.1))                                                     # removes empty columns 

tree_data_Asia <- tree_data %>% 
  dplyr::filter(Plot_code %in% c("SG01", "SG02", "THA01")) %>%                  # filter only plots in Asia
  mutate(Biomass = as.numeric(as.character(Biomass)))                           # turn biomass into a number instead of factor
 
# The raw data for Europe was broken (had commas instead of decimal points for biomass)
tree_data_Eurp <- tree_data %>% 
  dplyr::filter(Plot_code %in% c("SK01", "SK02")) %>%                           # filter only plots in Europe
  separate(., Biomass, into = c("Bio1", "Bio2"), sep = ",") %>%                 # Split by comma into two variables
  mutate(Biomass = as.numeric(paste(Bio1,".",Bio2, sep = ""))) %>%              # Combine them again as numbers
  dplyr::select(-c("Bio1", "Bio2"))
  
tree_data <- rbind(tree_data_Asia, tree_data_Eurp) %>%                          # combine the two data 
  mutate(Country = case_when(
    grepl("SK", Plot_code) ~ "SK",
    grepl("SG", Plot_code) ~ "SG",
    grepl("THA", Plot_code) ~ "THA"),
    Biomass = Biomass/1000) %>%                                                 # from kg to tonnes
    group_by(Plot_code) %>% 
    mutate(Biomass_per_plot = sum(Biomass)*5.09294626942)                       # sum biomass for each plot, scale data up to biomass per hectare

head(tree_data)
#  Generate plot data ----

# biomass per plot 
p_Biomass_vs_Site <- tree_data %>% 
   dplyr::select(Plot_code, Biomass_per_plot, Country) %>%                      # removed unneeded columns
   distinct()                                                                   # only keep unique rows

# mean biomass for each plot  
p_Biomass_within_site <- tree_data %>%
   dplyr::select(Plot_code, Biomass, Country, Biomass_per_plot) 
   
# mean DBH for each plot
p_DBH_within_site <- tree_data %>% 
   dplyr::select(Plot_code, DBH_cm, Country, Biomass_per_plot)

# colors
colr <- c("#f4a261","#2a9d8f","#e9c46a")

# Generate plots ----

(graph_Biomass_per_plot <- ggplot(p_Biomass_vs_Site, 
                                 aes(x = reorder(Plot_code, -Biomass_per_plot),
                                     y = Biomass_per_plot, fill = Country))+
  geom_bar(stat = "Identity", alpha = 0.75)+
  theme_bw()+
  scale_fill_manual(values = colr)+
  labs(x= "\n Plot code", y = "Biomass per plot (tonnes/ha) \n")+
  theme(axis.text = element_text(size = 12), 
        axis.title = element_text(size = 12), 
        plot.title = element_text(size = 14, hjust = 0.5, face = 'bold'), 
        plot.margin = unit(c(0.5,0.5,0.5,0.5), units = , 'cm'), 
        legend.position = '0')
 )
  
  
(graph_Biomass_in_plot <- ggplot(p_Biomass_within_site, aes(x = reorder(Plot_code, -Biomass_per_plot),y = Biomass, fill = Country, alpha = 0.75 ))+
  geom_violin()+ 
  # geom_point(aes(alpha = 0.5))+
  # geom_count(inherit.aes = TRUE,)+
  # geom_dotplot(binaxis="y", dotsize = 0.1, stackdir = "center")+
  geom_jitter(width = 0.02, size = 2, alpha = 0.3, aes(colour = Country))+
  theme_bw()+
  scale_fill_manual(values = colr)+
  scale_color_manual(values = colr)+
  labs(x= "\n Plot code", y = "Biomass (tonnes) \n")+
  theme(axis.text = element_text(size = 12), 
        axis.title = element_text(size = 12), 
        plot.title = element_text(size = 14, hjust = 0.5, face = 'bold'), 
        plot.margin = unit(c(0.5,0.5,0.5,0.5), units = , 'cm'), 
        legend.position = '0')
    )

(graph_DBH_in_plot <- ggplot(p_DBH_within_site, aes(x = reorder(Plot_code, -Biomass_per_plot),y = DBH_cm, fill = Country, alpha = 0.75 ))+
    geom_violin(scale = "count")+ 
    geom_jitter(width = 0.02, size = 2, alpha = 0.3, aes(colour = Country))+
    theme_bw()+
    scale_fill_manual(values = colr)+
    scale_color_manual(values = colr)+
    labs(x= "\n Plot code", y = "Diameter at Breast Height (cm) \n")+
    theme(axis.text = element_text(size = 12), 
          axis.title = element_text(size = 12), 
          plot.title = element_text(size = 14, hjust = 0.5, face = 'bold'), 
          plot.margin = unit(c(0.5,0.5,0.5,0.5), units = , 'cm'), 
          legend.position = '0')
)

 # save plots ----
ggsave("Output/Mean-Biomass-Plot.png", plot = graph_Biomass_per_plot, device = "png")
ggsave("Output/DBH-within-plot.png", plot = graph_DBH_in_plot, device = "png")

  
  
  
  

