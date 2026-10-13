# Hotspot review paper code to build the main dataset from the spreadsheet
# and plot the global distribution of hotspot work by geographic location
# and depth
# Written by Dan Palance with plot aesthetic assistance from Anthropic's Claude
# Last updated 12 Oct 2026

# read in required packages
library(tidyverse)
library(rnaturalearth)
library(rnaturalearthdata)
library(sf)
library(cowplot)

#### Read in and organize data ####
# read in the spreadsheet csv as a dataframe
# The commented out dataframe is my version which has more private notes
#hs_data <- read.csv("data/hsr_final.csv")[,2:19] # Remove the first column and last four columns
hs_data <- read.csv("data/hsr_final_subvers.csv")

# Rename columns to something more R friendly
colnames(hs_data) <- c("Authors","Year","Methods","Title","Def_clarity","Type",
                      "Definition","Taxa","Journal","Lat","Lon","Location",
                      "Depth","Summary","Persistence","Drivers_examined", 
                      "Drivers_examined_condensed", "Condensed_var") 

# Remove these two types since they're too new to define well
hsr_df <- hs_data %>% 
  filter(Type != "Metabolic Production" & Type != "Socio-ecological") %>% 
  filter(Year != 1988) # remove the Myers 1988 study since its not marine

# Organize the types by three broad categories: Biophysical, Anthropogenic, and
# Ecoimpact using the forcat package in tidyverse
hsr_df$Category <- forcats::fct_collapse(hsr_df$Type, 
                                         Biophysical=c("Foraging","Habitat","Nutrients & Biogeochemical-Cycling",
                                                       "Abundance/Density","Diversity & Endemism",
                                                       "Freq of Occurence","Reproduction & Recruitment","Mortality"),
                                         Anthropogenic = c("Pollution"),
                                         "Ecological Impact" = c("Invasives", "Eutrophication & Acidification",
                                                       "Fisheries & Bycatch","Multi-Risk & Threat",
                                                       "Bioaccumulation","Warming"))

# Combine these two into habitat since they're very similar
hsr_df$Type <- forcats::fct_collapse(hsr_df$Type, 
                                     Habitat=c("Habitat","Freq of Occurence")) 

# Get all unique lat lons from each study into individual rows 
hs_locations <- hsr_df %>% 
  separate_rows(c(Lat,Lon), sep =",") %>% 
  drop_na() # remove the studies that were global (NAs) to make plotting easier and the Myers 1988 study
# fix column data types
hs_locations$Lat <- as.numeric(hs_locations$Lat)
hs_locations$Lon <- as.numeric(hs_locations$Lon)

# import worldmap from rnaturalearth
world <- ne_countries(scale = "medium", returnclass = "sf") # load in spatial data of the earth

# Read in Marine Eco Regions of the World (MEOW) file from Spalding et al. 2007 using rgdal
meow <- read_sf("data/Marine Ecoregions/",layer="meow_ecos") # read in shapefile
#meow_realm <- tidy(meow, region = "REALM") # extract a dataframe of the realms from the shape file, this is defunct and replaced by line below
meow_realm <- sf::st_as_sf(meow, region = "REALM") # extract a dataframe of the realms from the shape file

########
#make spatial polygons for realms

realms <- meow_realm %>% 
  group_by(REALM) %>% 
  summarize(do_union = TRUE) %>% 
  st_wrap_dateline()
  
# Extract how many hotspots in a given realm #
meow_shp <- read_sf("data/Marine Ecoregions/",layer="meow_ecos") # read in meow as a shapefile using sf package
hs_locations_sdf <- st_as_sf(hs_locations, coords = c("Lon", "Lat")) # convert hs_locations to a spatial df
st_crs(hs_locations_sdf) <- st_crs(meow_shp) # set the coordinate system for the hs_locations_sdf to the meow shapefile coordinate system

# Import MEOW shapefile as a polygon so it works with overlay to identify studies outsides of the MEOWs
meow_poly <- st_read("data/Marine Ecoregions/",layer="meow_ecos")
# Get the coordinate projection
st_crs(meow_poly) 

# IMPORTANT NOTE: This dataframe will not contain studies that fall outside the marine realms,
meow_hs <- st_intersection(hs_locations_sdf, meow_shp)
#saveRDS(meow_hs, file ="output/meow_hs.RDS")

# Create a dataframe for those studies that fall outside of marine realms from Spalding et al. 2007
outside_meow <- anti_join(hsr_df,meow_hs,by="Title") %>% 
  mutate(REALM = ifelse(Location=="Global","Global", "Open Ocean")) %>% # Add additional options for studies that were either global (Global) or fall outside of realms (Other)
  select(-c(Lat,Lon)) # verify the few studies that had lat lons in known regions are accurate

global_count <- outside_meow %>% 
  group_by(REALM) %>% 
  count()
  
# Combine meow_hs (spatial df) and the studies outside of eco realms to create a master dataframe for non spatial analysis
# THIS DATASET IS USED IN ALL SUBSEQUENT ANALYSES!!!! #
main <- as.data.frame(meow_hs) %>% 
  select(-c(ECO_CODE,ECOREGION,PROV_CODE,PROVINCE,RLM_CODE,ALT_CODE,ECO_CODE_X,Lat_Zone,geometry)) %>% 
  rbind(outside_meow, .) %>% 
  distinct() %>% 
  mutate(Type = forcats::fct_recode(Type, "Water Chemistry" = "Eutrophication & Acidification",
                                    "Human Threat" = "Multi-Risk & Threat",
                                    "Fisheries" = "Fisheries & Bycatch",
                                    "Biodiversity & Endemism" = "Diversity & Endemism",
                                    "Invasive Species" = "Invasives"))
saveRDS(main, file ="output/main_hs.RDS")
main <- readRDS(file ="output/main_hs.RDS")

# figure out how many studies spanned multiple realms, open ocean, or global
multi_realm <- main %>% 
  separate_rows(REALM, sep=",") 
307-296

open_ocean <- main %>% 
  filter(REALM == "Open Ocean")

global <- main %>% 
  filter(REALM =="Global")

# Create dataframe that sums number of studies per type
hs_types <- main %>% 
  filter(Year!="1988") %>% 
  distinct(Title, .keep_all = TRUE) %>% # remove duplicates from multirealm studies
  group_by(Type, Category) %>% 
  count(Type) %>% 
  arrange(Category,-n) %>% 
  mutate(Percentage = (n/296)*100)
saveRDS(hs_types, file ="output/hs_types.rds")

hs_cats <- hs_types %>% 
  group_by(Category) %>% 
  summarize(Total = sum(n),
            Percent = Total/296)

# Create hotspot map of # of studies by realm and add density plots of # of studies by lat and lon on sides -----
# colorblidn friendly palette thanks to Claude
realm_cols <- c("Arctic" = "#88CCEE",
                "Southern Ocean"= "#332288",
                "Temperate Northern Pacific" = "#117733",
                "Temperate Northern Atlantic" = "#44AA99",
                "Tropical Atlantic" = "#DDCC77",
                "Tropical Eastern Pacific" = "#999933",
                "Central Indo-Pacific" = "#CC6677",
                "Eastern Indo-Pacific" = "#882255",
                "Western Indo-Pacific" = "#AA4499",
                "Temperate Australasia" = "#E69F00",
                "Temperate South America" = "#4477AA",
                "Temperate Southern Africa"  = "#F4A582")

# Color blind friendly categories thanks to claude
pt_cols   <- c("Anthropogenic" = "#F2C100", "Biophysical" = "#0072B2", "Ecological Impact" = "#009E73")

global_dist <- ggplot() +
  geom_sf(data = realms, aes(fill = REALM), color = "black") +
  scale_fill_manual(name = bquote(bold("Realm")), values = realm_cols) +
  geom_sf(data = world, color = "black", fill = "black") +
  
  # white halo underneath so points stand out on any background
  geom_point(data = hs_locations, aes(x = Lon, y = Lat),
             shape = 20, size = 7, colour = "white") +
  geom_point(data = hs_locations, aes(x = Lon, y = Lat, color = Category),
             shape = 20, size = 5.5) +
  scale_color_manual(name = "Hotspot Category", values = pt_cols) +
  labs(tag = "A)") +
  theme_classic() +
  theme(axis.title = element_blank(),
        axis.text  = element_blank(),   
        axis.ticks = element_blank(),   
        axis.line  = element_blank(),    
        legend.title = element_text(face = "bold", size = 22),
        legend.text = element_text(size = 20),
        plot.tag = element_text(face = "bold", size = 24),
        legend.box = "vertical",
        legend.position = "bottom",
        legend.direction = "horizontal",
        legend.background = element_rect(fill = "transparent", colour = "transparent"),
        plot.margin = margin(t = 10, r = 20, b = 50, l = 20)) +
  guides(color = guide_legend(nrow = 1, order = 1, override.aes = list(size = 10)),
         fill  = guide_legend(nrow = 2, byrow = TRUE, order = 2))
                         

# Add probability density plots for lat-lon
global_dist_dens <- ggExtra::ggMarginal(global_dist,fill = "black",
                                        adjust = 0.45, size = 12, alpha = 0.8)
ggsave(plot = global_dist_dens,"figs/global_distcb.png", width = 9, height = 6,
       units = "in", scale = 3.35, dpi = 600)

# Create dataframe that sums number of studies per type for the map legend
realmstudies_df <- main %>% 
  group_by(REALM) %>% 
  count(REALM) %>% 
  mutate(Percent = (n/296)*100) %>% 
  arrange(REALM,-n) 

realms_plot <- ggplot(data = realmstudies_df) +
  geom_bar(aes(x = reorder(REALM, n), y = n, fill = REALM), 
           stat = "identity", color = "black") +
  scale_fill_manual(name = bquote(bold("Realm")), 
                     values = c("Arctic" = "#88CCEE",
                               "Southern Ocean" = "#332288",
                               "Temperate Northern Pacific" = "#117733",
                               "Temperate Northern Atlantic"= "#44AA99",
                               "Tropical Atlantic" = "#DDCC77",
                               "Tropical Eastern Pacific" = "#999933",
                               "Central Indo-Pacific" = "#CC6677",
                               "Eastern Indo-Pacific" = "#882255",
                               "Western Indo-Pacific" = "#AA4499",
                               "Temperate Australasia" = "#E69F00",
                               "Temperate South America" = "#4477AA",
                               "Temperate Southern Africa"  = "#F4A582",
                               "Global" = "lightsteelblue4",
                               "Open Ocean" = "black")) +
  labs(x = "Realm", y = "Number of Studies", tag = "B)") +
  theme_classic() +
  theme(legend.position="none",
        axis.text = element_text(size = 12,
                                 face = "bold", 
                                 color = "black"), 
        axis.text.x = element_text(angle = 90, hjust = 1),
        axis.title = element_text(size = 14,
                                  face = "bold", 
                                  color = "black"),
        plot.tag = element_text(face = "bold",
                                size = 16)) +
  scale_y_continuous(expand = c(0,0)) +
  coord_flip()

ggsave(plot = realms_plot, "figs/studiesbyrealm_plot_cb.png", units="in", 
       height = 6, width = 6, dpi = 600)
# ggsave(plot = realms_plot, "figs/studiesbyrealm_plot.svg", units="in", 
#        height = 6, width = 6, dpi = 600)

