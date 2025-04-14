# This code creates a sankey plot of drivers
# written by Dan Palance on April 7, 2025

# Load required packages
library(tidyverse)
#library(networkD3)

# Read in the main dataframe created in earlier code
main <- readRDS(file ="output/main_hs.RDS")

# Create a dataframe with rows for each driver
hs_drivers <- main %>% 
  distinct() %>% # remove duplicates due to studies occurring in multiple regions
  separate_rows(Condensed_var, sep =", ") %>% 
  mutate_at(vars(Condensed_var), list(factor)) %>%
  filter(Condensed_var != "none") %>% 
  mutate(Drivercat = forcats::fct_collapse(Condensed_var,
                                           abiotic = c("dynamic physical", "static physical", "biogeochem"),
                                           biotic = c("ecological", "species attributes"),
                                           anthropogenic = c("human activity"))) %>%
  group_by(Condensed_var, Type) %>% 
  count(Condensed_var) %>% 
  ungroup() %>% 
  rename(Driver = Condensed_var) %>% 
  select(Type, Driver, n)
# do the above but use percentage of studies instead of straight number

hs_driver_components <- main %>% 
  distinct() %>% # remove duplicates due to studies occurring in multiple regions
  separate_rows(Drivers_examined, sep =", ")  %>% 
  filter(Drivers_examined != "none") %>% 
  mutate(Drivers_examined = as.factor(Drivers_examined)) %>% 
  mutate(Driver_comp = forcats::fct_collapse(Drivers_examined, # this is not working since habitat falls into multiple categories, remove from all but ecological?
                                           
                                           ## DYNAMIC PHYSICAL ##
          Temperature = c("bottom temperature", "SST", "temperature"),
          Circulation = c("currents", "current speed", "current velocity", 
                          "downwelling", "NPTZ", "eddies", "EKE", "Ekman transport",
                          "gyres", "hydrographic forcing", "surface currents", 
                          "tidal current", "tide", "upwelling", "water flow",
                          "rivers"),
          Atmospheric = c("climate", "hydrology", "La Niña", "cloud coverage", 
                          "precipitation", "pressure", "El Niño", "ENSO", "PDO", 
                          "storms","shear stress", "wind", "wind speed", "wind stress"),
          "Distance to features" = c("distance from edge of slope", "distance from estuary mouth", 
                                   "distance from shelf break", "distance from shore", 
                                   "distance to coast", "distance to continental slope", 
                                   "distance to eddies", "distance to estuary mouth", 
                                   "distance to fronts", "distance to iceberg", 
                                   "distance to ocean", "distance to plume", 
                                   "distance to shelf break", "proximity to estuary", 
                                   "proximity to rivers", "proximity to tidal channels"),
          Ice = c("ice", "ice coverage", "icebergs", "glaciers"),
          "Sea state" = c("dynamic height", "sea level anomaly", "SSH", "SSHA", "swell",
                        "wave action", "wave exposure", "wave velocity", "waves",
                        "tsunamis", "fetch"),
          "Water column structure" = c("Gulfstream North Wall", "fronts", 
                             "mixed layer thickness", "mixing", "mixing line", 
                             "isothermality", "mixed layer depth", "pycnocline depth", 
                             "stratification", "thermal stratification", "density","turbidity"),
          "Misc dyphys" = c("day length", "light", "moon illuminance", "gravitational sinking", 
                           "island mass effect", "protrusion of surf zone", 
                           "tectonic events"),
         
                                ## STATIC PHYSICAL - RENAME TO BATHY & TOPO ##
         "General bathy/topo" = c("bathymetry", "depth", "bottom depth", 
                                  "topography", "land area", "continental width", 
                        "islands"),
         "Seabed characteristics" = c("ridges", "roughness", "rugosity", "seabed composition",
                                    "slope", "substrate", "sediment grain size", "aspect",
                                    "bottom type", "wood debris"),
         "Bathy structures" = c("banks", "canyons", "fjords", "guyots", "seamounts", 
                              "shelf break", "shelfs"),
         
                                            ## BIOGEOCHEM ##
         Nutrients = c("nitrate", "nitrogen", "nutrients", "silica", "silicate", 
                       "silicon", "phosphate", "phosphorous", "pCO2", 
                       "nitrogen fixation"),
         "Carbon cycle" = c("carbon cycling", "particulate organic carbon", 
                    "sediment organic content", "DIC", "PIC", "POC"),
         "Oxygen & acidifcation" = c("dissolved oxygen", "DO", "alkalinity", "aragonite saturation", 
                           "conductivity", "dissolution", "pH", "salinity", "oxygen", 
                           "oxygen saturation"),
         
         
                                        ## SPECIES ATTRIBUTES ##
         "Life history" = c("dispersal mechanisms", "life history", "life stage",
                          "reproduction rate", "wean mass", "prey recruitment"),
         "Physio & morph" = c("enzymatic responses", "phytoplankton fluorescence", 
                              "animal sensitivity", "body length", "body shape", "diet",
                              "prey size", "heat wave stress"),
         Behavior = c("behavior", "migratory behavior", "DVM", "social cues"),
         Demography = c("population size", "range size", "prey mortality", "social group size"),
         "Misc spp" = c("taxonomic group"), # Is this really a driver component?
         
         
                                          ## ECOLOGICAL ##
         "Habitat components" = c("habitat", "habitat complexity", "habitat quality", 
                     "habitat type", "adult cover", "canopy height", "reef area", 
                     "reef structure", "rhizome layer depth", "prey habitat", "habitat structure"),
         "Community composition" = c("biodiversity", "community composition", "taxa", 
                                   "species composition", "ecological niche", 
                                   "relative nekton density-distribution"),
         "Primary production" = c("chl-a", "chlorophyll", "phytoplankton", 
                                "phytoplankton biomass", "primary productivity", 
                                "productivity", "proximity to phytoplankton"),
         "Misc interactions" = c("intraspecific interactions", "disease"), 
         Predation = c("predator abundance", "predator density", "predators", 
                       "prey abundance", "prey biomass", "prey density", 
                       "prey distribution", "prey type", "trophic category", 
                       "distance to feeding areas"),
         
         ## HUMAN ACTIVITY - RENAME TO ANTHROPOGENIC ##
         Fishing = c("aquaculture", "fishing", "fishing gear", 
                       "fishing gear type", "fishing pressure", "gear material",
                       "live trade", "trawl depth"),
         Production = c("agriculture", "desalinization", 
                        "industry", "reclamation"),
         "Habitat alteration" = c("biofouling", "deforestation", 
                                "dredging", "climate change", "climate velocity"),
         Shipping = c("vessel track", "vessel type", "ship breaking", "shipping", 
                      "shipwrecks", "ballast water","proximity to ports", 
                      "visitation rate", "port proximity", "recreational boating"),
         Pollutants = c("bulk discharge", "dumping", "eutrophication",
                       "heavy metal effluent", "heavy metal exposure",
                       "sewage", "sewage discharge", "industrial effluent", 
                       "litter", "metal pollution", "noise", "oil pollution", 
                       "oil release rates", "PCB exposure", "plastic density", 
                       "pollution"),
         "Misc human" = c("city", "consumer demand", "human activity", 
                        "human impact", "human population", 
                        "proximity to urban areas", "threat type", "tourism"))) %>% 
  mutate(Driver = forcats::fct_collapse(Driver_comp,
                    "Dynamic physical" = c("Temperature", "Circulation", "Atmospheric",
                    "Distance to features", "Ice", "Sea state", "Water column structure",
                    "Misc dyphys"),
                    "Static physical" = c("General bathy/topo", "Seabed characteristics",
                                          "Bathy structures"),
                    Biogeochem = c("Nutrients", "Carbon cycle", "Oxygen & acidifcation"),
                    "Species attributes" = c("Life history", "Physio & morph",
                                             "Behavior", "Demography", "Misc spp"),
                    Ecological = c("Habitat components", "Community composition",
                                   "Primary production", "Misc interactions",
                                   "Predation"),
                    "Anthropogenic" = c("Fishing", "Production", "Habitat alteration",
                                         "Shipping", "Pollutants", "Misc human"))) %>%
  group_by(Driver, Type) %>% 
  count(Driver_comp) %>%
  ungroup() 


# nodes <- data.frame(
#   name=c(as.character(hs_drivers$Driver),
#          as.character(hs_drivers$Type)) %>% unique()
# )
# 
# hs_drivers$IDsource <- match(hs_drivers$Driver, nodes$name)-1
# hs_drivers$IDtarget <- match(hs_drivers$Type, nodes$name)-1
# 
# p <- sankeyNetwork(Links = hs_drivers, Nodes = nodes,
#                    Source = "IDsource", Target = "IDtarget",
#                    Value = "n", NodeID = "name",
#                    sinksRight=FALSE, fontSize = 12, nodeWidth = 30)
# p
# 
# 
# 
# nodes2 <- data.frame(
#   name=c(as.character(hs_driver_components$Driver),
#          as.character(hs_driver_components$Driver_comp)) %>% unique()
# )
# 
# hs_driver_components$IDsource <- match(hs_driver_components$Driver_comp, nodes2$name)-1
# hs_driver_components$IDtarget <- match(hs_driver_components$Driver, nodes2$name)-1
# 
# p2 <- sankeyNetwork(Links = hs_driver_components, Nodes = nodes2,
#                    Source = "IDsource", Target = "IDtarget",
#                    Value = "n", NodeID = "name",
#                    sinksRight=FALSE, fontSize = 12, nodeWidth = 30)
# p2
# 
# 
# df <- structure(list(
#   network_name = c("YAGHO", "YAGHO", "YAGHO", "YAGHO","YAGHO", "YAGHO", "YAGHO", "YAGHO", "YAGHO", "YAGHO", "YAGHO","YAGHO", "YAGHO", "YAGHO", "YAGHO", "YAGHO", "YAGHO", "YAGHO","YAGHO", "YAGHO", "YAGHO", "YAGHO", "YAGHO", "YAGHO"),
#   type = c("deposits", "deposits", "withdrawals", "withdrawals", "trf_outgoing", "trf_outgoing","trf_incoming", "trf_incoming", "trf_incoming", "trf_incoming","trf_outgoing", "trf_incoming", "trf_outgoing", "trf_outgoing","chk_issued", "chk_issued", "chk_issued", "chk_issued", "chk_received","chk_received", "chk_received", "chk_received", "chk_received","chk_received"),
#   thirdparty = c("Christine", "Mike", "Patrick","Natalie", "Renee", "Jacob", "Renee", "Kathy", "John", "Ahmad", "Ahmad", "Tito", "Tito", "John", "Sally", "Tito", "John", "Ahmad", "Mohamad", "Tito", "John", "Sally", "Tito", "John"),
#   amount = c(2038472, 683488, 38765, 123413, 94543234, 20948043, 34842843, 218864, 6468486, 384684, 5348687, 34684687, 6936937, 16841287, 1584587, 1901504.4, 2281805.28, 2738166.34, 295910.77, 4114374.62, 26680528.46, 5336105.38, 12954836.15, 1218913.08)))
# df <- bind_cols(df)   # or: as.data.frame(df)

library(plotly)

# Prepare node and link data for plotting
nodes <- df %>%
  pivot_longer(-amount, values_to = "name_node") %>%
  distinct(name_node) %>%
  mutate(idx = (1:n()) - 1)

nodes <- hs_driver_components %>%
  pivot_longer(-n, values_to = "name_node") %>%
  distinct(name_node) %>%
  mutate(idx = (1:n()) - 1)

links <- bind_rows(
  hs_driver_components %>% select(source = Driver_comp, target = Driver, n),
  hs_driver_components %>% select(source = Driver, target = Type, n)) %>%
  group_by(source, target) %>%
  summarise(value = sum(n), .groups = "drop") %>%
  mutate(across(c(source, target), ~ nodes$idx[match(.x, nodes$name_node)]))

# Plot
plot_ly(
  type = "sankey",
  orientation = "h",
  node = list(label = nodes$name_node, pad = 15, thickness = 15),
  link = as.list(links))

