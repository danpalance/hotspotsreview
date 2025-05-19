# Hotspot review paper code
# read in required packages
library(tidyverse)
library(rnaturalearth)
library(rnaturalearthdata)
library(sf)
library(glue)

#### Read in and organize data ####
# read in the csv as a dataframe
hs_data <- read.csv("data/hsr_final.csv")[,2:19] # Remove the first column and last four columns

# Rename columns to something more R friendly
colnames(hs_data) <- c("Authors","Year","Methods","Title","Def_clarity","Type",
                      "Definition","Taxa","Journal","Lat","Lon","Location",
                      "Depth","Summary","Persistence","Drivers_examined", 
                      "Drivers_examined_condensed", "Condensed_var") 

# Remove these two types since they're too new to define well
hsr_df <- hs_data %>% 
  filter(Type != "Metabolic Production" & Type != "Socio-ecological") 

# Organize the types by three broad categories: Biophysical, Anthropogenic, and
# Ecoimpact using the forcat package in tidyverse
hsr_df$Category <- forcats::fct_collapse(hsr_df$Type, 
                                         Biophysical=c("Foraging","Habitat","Nutrients & Biogeochemical-Cycling",
                                                       "Abundance/Density","Diversity & Endemism",
                                                       "Freq of Occurence","Reproduction & Recruitment","Mortality"),
                                         Anthropogenic = c("Pollution"),
                                         Ecoimpact = c("Invasives", "Eutrophication & Acidification",
                                                       "Fisheries & Bycatch","Multi-Risk & Threat",
                                                       "Bioaccumulation","Warming"))

# Combine these two into habitat since they're very similar
hsr_df$Type <- forcats::fct_collapse(hsr_df$Type, 
                                     Habitat=c("Habitat","Freq of Occurence")) 

# Get all unique lat lons from each study into individual rows 
hs_locations <- hsr_df %>% 
  separate_rows(c(Lat,Lon), sep =",") %>% 
  drop_na() # remove the studies that were global (NAs) to make plotting easier
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
main <- as.data.frame(meow_hs) %>% 
  select(-c(ECO_CODE,ECOREGION,PROV_CODE,PROVINCE,RLM_CODE,ALT_CODE,ECO_CODE_X,Lat_Zone,geometry)) %>% 
  rbind(outside_meow, .) %>% 
  distinct() %>% 
  mutate(Type = forcats::fct_recode(Type, "Water Chemistry" = "Eutrophication & Acidification",
                                    "Threat" = "Multi-Risk & Threat",
                                    "Fisheries" = "Fisheries & Bycatch",
                                    "Biodiversity & Endemism" = "Diversity & Endemism",
                                    "Invasive Species" = "Invasives"))
saveRDS(main, file ="output/main_hs.RDS")
#main <- readRDS(file ="output/main_hs.RDS")

# Create dataframe that sums number of studies per type
hs_types <- hsr_df %>% 
  group_by(Type, Category) %>% 
  count(Type) %>% 
  arrange(Category,-n) %>% 
  mutate(Percentage = (n/292)*100)
saveRDS(hs_types, file ="output/hs_types.rds")

sum(hs_types$n)

hs_loc_count <- main %>% 
  group_by(Title) %>% 
  count() %>% 
  filter(n >1)


# Figure 1: Create hotspot map of # of studies by realm and add density plots of # of studies by lat and lon on sides ------
realm_plot <- ggplot() +
  geom_sf(data=realms, aes(fill=REALM), color="black") +
  scale_fill_manual(name=bquote(bold("Realm")),values=c("Arctic"="skyblue1", "Southern Ocean"="cornflowerblue", "Temperate Northern Pacific"="mediumseagreen", 
                                                        "Tropical Atlantic"="gold2","Central Indo-Pacific"="sienna2", "Temperate Australasia"="maroon4", 
                                                        "Temperate South America"="turquoise", "Tropical Eastern Pacific"="olivedrab1",
                                                        "Eastern Indo-Pacific"="palevioletred1","Temperate Northern Atlantic"="palegreen4",
                                                        "Temperate Southern Africa"="slategray2", "Western Indo-Pacific"="thistle1")) +
  geom_sf(data=world, color="black",fill="burlywood3") +
  # guides(fill = guide_legend(position = "inside")) +
  theme_classic() +
  theme(axis.title = element_blank(),
        legend.title = element_blank(),
        legend.text = element_text(size=28),
        legend.position = "bottom",
        legend.direction = "horizontal",
        legend.background = element_rect(fill = "transparent", colour = "transparent"))
ggsave(plot=realm_plot,"figs/realm_plot.png",scale=3.3)

global_dist <- ggplot() +
  geom_sf(data=realms, aes(fill=REALM), color="black") +
  scale_fill_manual(name=bquote(bold("Realm")),values=c("Arctic"="skyblue1", "Southern Ocean"="cornflowerblue", "Temperate Northern Pacific"="mediumseagreen", 
                                                        "Tropical Atlantic"="gold2","Central Indo-Pacific"="sienna2", "Temperate Australasia"="maroon4", 
                                                        "Temperate South America"="turquoise", "Tropical Eastern Pacific"="olivedrab1",
                                                        "Eastern Indo-Pacific"="palevioletred1","Temperate Northern Atlantic"="palegreen4",
                                                        "Temperate Southern Africa"="slategray2", "Western Indo-Pacific"="thistle1")) +
  geom_sf(data=world, color="black",fill="burlywood3") +
  geom_point(data=hs_locations, aes(x=Lon,y=Lat),size=5,color="black",fill="black",alpha=0.5) +
  theme_classic() +
  theme(axis.title = element_blank(),
        legend.title = element_blank(),
        legend.text = element_text(size=24),
        legend.position = "bottom",
        legend.direction = "horizontal",
        legend.background = element_rect(fill = "transparent", colour = "transparent"))

# Add probability density plots for lat-lon
global_dist_dens <- ggExtra::ggMarginal(global_dist,fill = "black",adjust=0.45,size=12, alpha = 0.75)
ggsave(plot=global_dist_dens,"figs/global_dist.png",scale=3.3)

# Take a look at depth around the world
depth_by_location <- hs_locations %>% 
  separate_rows(Depth,sep=",") %>% 
  drop_na() 

percents_depth <- depth_by_location %>% 
  filter(Depth == "mid")

# First look at surface oriented studies
surface_depth_plot <- ggplot() +
  geom_sf(data=world, color="black",fill="burlywood3") +
  geom_point(data=depth_by_location %>% filter(Depth == "surface"), aes(x=Lon,y=Lat, color=Category), size=5, alpha=0.75) +
  scale_color_manual(values=c("Anthropogenic"="#CD950C","Biophysical"="#0000CD","Ecoimpact"="#228B22")) +
  theme_classic() +
  theme(axis.title = element_blank(),
        legend.title = element_blank(),
        legend.text = element_text(size=24),
        legend.position = "bottom",
        legend.direction = "horizontal",
        legend.background = element_rect(fill = "transparent", colour = "transparent"))
ggsave(plot=surface_depth_plot,"figs/surface_depth_plot.png",scale=3.3)

# Add probability density plots for lat-lon
surface_depth_plot <- ggExtra::ggMarginal(surface_depth_plot , 
                                          groupFill = FALSE,
                                          fill = "black",
                                          adjust=0.45,size=12, alpha = 0.75)
ggsave(plot=surface_depth_plot,"figs/surface_depth_plot.png",scale=3.3)

# Make the same plot for midwater
mid_depth_plot <- ggplot() +
  geom_sf(data=world, color="black",fill="burlywood3") +
  geom_point(data=depth_by_location %>% filter(Depth == "mid"), aes(x=Lon,y=Lat, color=Category), size=5, alpha=0.75) +
  scale_color_manual(values=c("Anthropogenic"="#CD950C","Biophysical"="#0000CD","Ecoimpact"="#228B22")) +
  theme_classic() +
  theme(axis.title = element_blank(),
        legend.title = element_blank(),
        legend.text = element_text(size=24),
        legend.position = "bottom",
        legend.direction = "horizontal",
        legend.background = element_rect(fill = "transparent", colour = "transparent"))

# Add probability density plots for lat-lon
mid_depth_plot <- ggExtra::ggMarginal(mid_depth_plot , 
                                      groupFill = FALSE,
                                      fill = "black",
                                      adjust=0.45,size=12, alpha = 0.75)
ggsave(plot=mid_depth_plot,"figs/mid_depth_plot.png",scale=3.3)

# Make the same plot for demersal
demersal_depth_plot <- ggplot() +
  geom_sf(data=world, color="black",fill="burlywood3") +
  geom_point(data=depth_by_location %>% filter(Depth == "demersal"), aes(x=Lon,y=Lat, color=Category), size=5, alpha=0.75) +
  scale_color_manual(values=c("Anthropogenic"="#CD950C","Biophysical"="#0000CD","Ecoimpact"="#228B22")) +
  theme_classic() +
  theme(axis.title = element_blank(),
        legend.title = element_blank(),
        legend.text = element_text(size=24),
        legend.position = "bottom",
        legend.direction = "horizontal",
        legend.background = element_rect(fill = "transparent", colour = "transparent"))

# Add probability density plots for lat-lon
demersal_depth_plot <- ggExtra::ggMarginal(demersal_depth_plot , 
                                      groupFill = FALSE,
                                      fill = "black",
                                      adjust=0.45,size=12, alpha = 0.75)
ggsave(plot=demersal_depth_plot,"figs/demersal_depth_plot.png",scale=3.3)

# Make bottom panel bar plot for for different depths

hs_depth <- hsr_df %>% 
  separate_rows(Depth,sep=",") %>% 
  drop_na() %>% 
  count(Depth,Category) #%>% 
 # mutate(Depth=as.factor(Depth))

depth_plot <- ggplot(data=hs_depth) +
      geom_bar(aes(x=Depth,y=n,fill=Category), width=.95,stat="identity") +
      labs(x="Depth Zone",y="Number of Studies") +
      theme_classic() +
      theme(axis.title = element_blank(),
            legend.title = element_text(size=14,face="bold"),
            legend.text = element_text(size=12,face="bold"),
            legend.position = "inside",
            legend.position.inside = c(0.8,0.35),
            legend.background = element_rect(fill = "transparent", colour = "transparent"),
            axis.text.x = element_text(size=12,face="bold",color="black"),
            axis.title.x = element_text(size=14,face="bold",color="black"),
            axis.text.y = element_text(size=12,face="bold",color="black"),
            axis.title.y = element_text(size=14,face="bold",color="black")) +
      scale_fill_manual(values=c("Anthropogenic"="#CD950C","Biophysical"="#0000CD","Ecoimpact"="#228B22")) +
      scale_y_continuous(expand=c(0,0)) +
      coord_flip() 
ggsave(plot=depth_plot,"figs/depth_plot.png",units="in",height=3.5,width=8)


# Create dataframe that sums number of studies per type
realmstudies_df <- main %>% 
  group_by(REALM) %>% 
  count(REALM) %>% 
  arrange(REALM,-n) 

ggplot(data = realmstudies_df) +
  geom_bar(aes(x = REALM, y = n, fill = REALM), stat = "identity") +
  scale_fill_manual(name=bquote(bold("Realm")),values=c("Arctic"="skyblue1", "Southern Ocean"="cornflowerblue", "Temperate Northern Pacific"="mediumseagreen", "Tropical Atlantic"="gold2",
                                                        "Central Indo-Pacific"="sienna2", "Temperate Australasia"="maroon4", "Temperate South America"="turquoise", "Tropical Eastern Pacific"="olivedrab1",
                                                        "Eastern Indo-Pacific"="palevioletred1","Temperate Northern Atlantic"="palegreen4","Temperate Southern Africa"="slategray2", "Western Indo-Pacific"="thistle1",
                                                        "Global"="lightsteelblue4","Open Ocean"="gray59")) +
  labs(x = "Realm", y = "Number of Studies") +
  theme_classic() +
  theme(legend.position="none",
        axis.text = element_text(size = 12,
                                   face = "bold", 
                                   color = "black"), 
        axis.text.x = element_text(angle = 90, hjust = 1),
        axis.title = element_text(size = 14,
                                  face = "bold", 
                                  color = "black")) +
  scale_y_continuous(expand = c(0,0)) +
  coord_flip()
ggsave("figs/studiesbyrealm_plot.png", units="in", height = 3.5, width = 8)
  
