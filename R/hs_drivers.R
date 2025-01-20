# Hotspot review paper code
# read in required packages
library(tidyverse)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
#library(rgeos)

#### Read in and organize data ####
# read in the csv as a dataframe
hsr_df <- read.csv("data/hsr_final.csv")
hsr_df <- hsr_df[,2:20] # Remove the first column and last two columns

# Rename columns to something more R friendly
colnames(hsr_df) <- c("Authors","Year","Methods","Title","Def_clarity","Type",
                      "Definition","Taxa","Journal","Lat","Lon","Location",
                      "Depth","Summary","Persistence","Drivers_examined", 
                      "Drivers_examined_condensed", "Drivers_suggested", 
                      "Drivers_suggested_condensed") 

# Organize the types by three broad categories: Biophysical, Anthropogenic, and
# Ecoimpact using the forcat package in tidyverse
hsr_df$Category <- forcats::fct_collapse(hsr_df$Type, 
                   Biophysical=c("Foraging","Habitat","Nutrients & Biogeochemical-Cycling",
                                "Abundance/Density","Diversity & Endemism",
                                "Freq of Occurence","Reproduction & Recruitment"),
                   Anthropogenic = c("Pollution"),
                   Ecoimpact = c("Invasives", "Mortality","Eutrophication & Acidification",
                                 "Fisheries & Bycatch","Multi-Risk & Threat","Socio-ecological",
                                 "Bioaccumulation","Metabolic Production","Warming"))

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
#######

#make realms shapefile
########
#make spatial polygons for realms
realms <- meow_realm %>% 
  group_by(REALM) %>% 
  summarize(do_union = TRUE) %>% 
  st_wrap_dateline()

ggplot(realms) + 
  geom_sf(data=realms, aes(fill=REALM), color="black") 


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

# Combine meow_hs (spatial df) and the studies outside of eco realms to create a main dataframe for non spatial analysis, note there are several (5?) studies that spread across mutliple
# ecoregions so there will be a few more rows than the HSR_DF (307 vs 296)
main <- as.data.frame(meow_hs) %>% 
  select(-c(ECO_CODE,ECOREGION,PROV_CODE,PROVINCE,RLM_CODE,ALT_CODE,ECO_CODE_X,Lat_Zone,geometry)) %>% 
  rbind(outside_meow, .) %>% 
  distinct()
saveRDS(main, file ="output/main_hs.RDS")
main <- readRDS(file ="output/main_hs.RDS")



  

