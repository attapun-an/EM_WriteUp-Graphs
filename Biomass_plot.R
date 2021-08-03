install.packages("dplyr")

# load packages---- 
library(dplyr)
library(tidyr)
library(ggrepel)
library(ggplot2)
library(patchwork)



# import and process data ----
tree_data <- read.csv("Resources/TreeData.csv") %>% slice(-211) %>%                       # removes empty row                                         
  rename(Plot_code = Plot_Code) %>% 
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


# stocking density

Stk_dns_count <- summary(tree_data$Plot_code)*5.09294626942                     # note, this is stems/ha and not trees/ha

p_Stocking_density <- tree_data %>% 
  mutate(Stk_dns = case_when(
    grepl("SK01", Plot_code) ~ Stk_dns_count[4],
    grepl("SK02", Plot_code) ~ Stk_dns_count[5],
    grepl("SG01", Plot_code) ~ Stk_dns_count[2],
    grepl("SG02", Plot_code) ~ Stk_dns_count[3],
    grepl("THA", Plot_code) ~ Stk_dns_count[6]),
  ) %>% 
  dplyr::select(Plot_code, Stk_dns, Country, Biomass_per_plot) %>% 
  distinct()


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

(graph_DBH_in_plot_scaled <- ggplot(p_DBH_within_site, aes(x = reorder(Plot_code, -Biomass_per_plot),y = DBH_cm, fill = Country, alpha = 0.75 ))+
    geom_violin()+ 
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

(graph_Stocking_density <- ggplot(p_Stocking_density, 
                                  aes(x = reorder(Plot_code, -Biomass_per_plot),
                                      y = Stk_dns, fill = Country))+
    geom_bar(stat = "Identity", alpha = 0.75)+
    theme_bw()+
    scale_fill_manual(values = colr)+
    labs(x= "\n Plot code", y = "Stocking density (stems/ha) \n")+
    theme(axis.text = element_text(size = 12), 
          axis.title = element_text(size = 12), 
          plot.title = element_text(size = 14, hjust = 0.5, face = 'bold'), 
          plot.margin = unit(c(0.5,0.5,0.5,0.5), units = , 'cm'), 
          legend.position = '0')
)

(graph_stacked <- graph_Biomass_per_plot+labs(x = "") + graph_Stocking_density+labs(x= "") + graph_DBH_in_plot_scaled + plot_layout(ncol = 1))

 # save plots ----
ggsave("Output/Mean-Biomass-Plot.png", plot = graph_Biomass_per_plot, device = "png")
ggsave("Output/DBH-within-plot.png", plot = graph_DBH_in_plot, device = "png")
ggsave("Output/DBH-within-plot_scaled.png", plot = graph_DBH_in_plot_scaled, device = "png")
ggsave("Output/Stocking-Density.png", plot = graph_Stocking_density, device = "png")
ggsave("Output/Stacked.png", plot = graph_stacked, device = "png", width = 6.71, height = 10, units = "in")
  
  
  
  

