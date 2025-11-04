# This code creates a sankey plot of hotspot covariates and indicators which are
# referred to as drivers from an earlier draft and kept here for simplicity
# written by Dan Palance 
# last modified on 09 June 2025

# Load required packages
library(tidyverse)
library(plotly)

# Read in the main dataframe created in hs_globaldist.R
main <- readRDS(file ="output/main_hs.RDS")

# Create a dataframe with rows for each driver
hs_drivers <- main %>% 
  filter(Year != "1988") %>% 
  distinct(Title, .keep_all = TRUE) %>% # remove duplicates due to studies occurring in multiple regions
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
  filter(Year != "1988") %>% 
distinct(Title, .keep_all = TRUE) %>% # remove duplicates due to studies occurring in multiple regions
  separate_rows(Drivers_examined, sep =", ")  %>% 
  filter(Drivers_examined != "none") %>% 
  mutate(Drivers_examined = as.factor(Drivers_examined)) %>% 
  mutate(Driver_comp = forcats::fct_collapse(Drivers_examined, 
                                             ## DYNAMIC PHYSICAL ##
                                             Temperature = c("bottom temperature", "SST", "temperature", "heat"),
                                             Circulation = c("currents", "current speed", "current velocity", 
                                                             "downwelling", "NPTZ", "eddies", "EKE", "Ekman transport",
                                                             "gyres", "hydrographic forcing", "surface currents", 
                                                             "tidal current", "tide", "upwelling", "water flow",
                                                             "rivers", "freshwater input"),
                                             Atmospheric = c("climate", "hydrology", "La Niña", "cloud coverage", 
                                                             "precipitation", "pressure", "El Niño", "ENSO", "PDO", 
                                                             "storms","shear stress", "wind", "wind speed", "wind stress"),
                                             "Distance to Features" = c("distance from edge of slope", "distance from estuary mouth", 
                                                                        "distance from shelf break", "distance from shore", 
                                                                        "distance to coast", "distance to continental slope", 
                                                                        "distance to eddies", "distance to estuary mouth", 
                                                                        "distance to fronts", "distance to iceberg", 
                                                                        "distance to ocean", "distance to plume", 
                                                                        "distance to shelf break", "proximity to estuary", 
                                                                        "proximity to rivers", "proximity to tidal channels", "distance to ledges",
                                                                        "distance to canyons"),
                                             Ice = c("ice", "ice coverage", "icebergs", "glaciers"),
                                             "Sea State" = c("dynamic height", "sea level anomaly", "SSH", "SSHA", "swell",
                                                             "wave action", "wave exposure", "wave velocity", "waves",
                                                             "tsunamis", "fetch"),
                                             "Water Column Structure" = c("Gulfstream North Wall", "fronts", 
                                                                          "mixed layer thickness", "mixing", "mixing line", 
                                                                          "isothermality", "mixed layer depth", "pycnocline depth", 
                                                                          "stratification", "thermal stratification", "density","turbidity"),
                                             "Misc Dyphys" = c("day length", "light", "moon illuminance", "gravitational sinking", 
                                                               "island mass effect", "protrusion of surf zone", 
                                                               "tectonic events"),
                                             
                                             ## STATIC PHYSICAL 
                                             "General Bathy/topo" = c("bathymetry", "depth", "bottom depth", 
                                                                      "topography", "land area", "continental width", 
                                                                      "islands"),
                                             "Seabed Characteristics" = c("ridges", "roughness", "rugosity", "seabed composition",
                                                                          "slope", "substrate", "sediment grain size", "aspect",
                                                                          "bottom type", "wood debris"),
                                             "Bathy Structures" = c("banks", "canyons", "fjords", "guyots", "seamounts", 
                                                                    "shelf break", "shelfs"),
                                             
                                             ## BIOGEOCHEM ##
                                             Nutrients = c("nitrate", "nitrogen", "nutrients", "silica", "silicate", 
                                                           "silicon", "phosphate", "phosphorous", "pCO2", 
                                                           "nitrogen fixation"),
                                             "Carbon Cycle" = c("carbon cycling", "particulate organic carbon", 
                                                                "sediment organic content", "DIC", "PIC", "POC"),
                                             "Oxygen & Acidifcation" = c("dissolved oxygen", "DO", "alkalinity", "aragonite saturation", 
                                                                         "conductivity", "dissolution", "pH", "salinity", "oxygen", 
                                                                         "oxygen saturation"),
                                             
                                             
                                             ## SPECIES ATTRIBUTES ##
                                             "Life History" = c("dispersal mechanisms", "life history", "life stage",
                                                                "reproduction rate", "wean mass", "prey recruitment"),
                                             "Physio & Morph" = c("enzymatic responses", "phytoplankton fluorescence", 
                                                                  "animal sensitivity", "body length", "body shape", "diet",
                                                                  "prey size", "heat wave stress"),
                                             Behavior = c("behavior", "migratory behavior", "DVM", "social cues"),
                                             Demography = c("population size", "range size", "prey mortality", "social group size"),
                                             "Misc Spp" = c("taxonomic group"), # Is this really a driver component?
                                             
                                             
                                             ## ECOLOGICAL ##
                                             "Habitat Components" = c("habitat", "habitat complexity", "habitat quality", 
                                                                      "habitat type", "adult cover", "canopy height", "reef area", 
                                                                      "reef structure", "rhizome layer depth", "prey habitat", "habitat structure"),
                                             "Community Composition" = c("biodiversity", "community composition", "taxa", 
                                                                         "species composition", "ecological niche", 
                                                                         "relative nekton density-distribution"),
                                             "Primary Production" = c("chl-a", "chlorophyll", "phytoplankton", 
                                                                      "phytoplankton biomass", "primary productivity", 
                                                                      "productivity", "proximity to phytoplankton"),
                                             "Misc Interactions" = c("intraspecific interactions", "disease"), 
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
                                             "Habitat Alteration" = c("biofouling", "deforestation", 
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
                                             "Misc Human" = c("city", "consumer demand", "human activity", 
                                                              "human impact", "human population", 
                                                              "proximity to urban areas", "threat type", "tourism"))) %>% 
  mutate(Driver = forcats::fct_collapse(Driver_comp,
                                        "Dynamic Physical" = c("Temperature", "Circulation", "Atmospheric",
                                                               "Distance to Features", "Ice", "Sea State", "Water Column Structure",
                                                               "Misc Dyphys"),
                                        "Bathy & Topo" = c("General Bathy/topo", "Seabed Characteristics",
                                                           "Bathy Structures"),
                                        Biogeochem = c("Nutrients", "Carbon Cycle", "Oxygen & Acidifcation"),
                                        "Species Attributes" = c("Life History", "Physio & Morph",
                                                                 "Behavior", "Demography", "Misc Spp"),
                                        Ecological = c("Habitat Components", "Community Composition",
                                                       "Primary Production", "Misc Interactions",
                                                       "Predation"),
                                        "Anthropogenic" = c("Fishing", "Production", "Habitat Alteration",
                                                            "Shipping", "Pollutants", "Misc Human"))) %>% 
  group_by(Driver, Type) %>% 
  count(Driver_comp) %>%
  ungroup() 


#### Create sankey plot ####
node_color <- c("coral", "blue", "coral","coral", "coral", "green", "coral", "blue",
                "blue", "darkgreen", "blue", "coral", "blue", "darkgreen", "darkgreen",
                "blue", "goldenrod", "blue", "darkgreen", "yellow", "yellow", "yellow",
                "yellow", "yellow", "yellow", "yellow", "darkgreen", "aquamarine", "aquamarine",
                "aquamarine", "aquamarine", "orchid", "orchid", "orchid", "orchid", "orchid",
                "orchid", "saddlebrown", "saddlebrown", "saddlebrown", "saddlebrown", "ivory", "ivory",
                "ivory", "ivory", "ivory", "ivory", "ivory", "ivory", "ivory")

nodes <- hs_driver_components %>%
  pivot_longer(-n, values_to = "name_node") %>%
  distinct(name_node) %>%
  mutate(idx = (1:n()) - 1,
         color = node_color)

links <- bind_rows(
  hs_driver_components %>% select(source = Driver_comp, target = Driver, n),
  hs_driver_components %>% select(source = Driver, target = Type, n)) %>%
  group_by(source, target) %>%
  summarise(value = sum(n), .groups = "drop") %>%
  mutate(across(c(source, target), ~ nodes$idx[match(.x, nodes$name_node)])) %>% 
  mutate(color = c("lightpink", "khaki", "lightblue", "plum", "khaki", "rosybrown", "khaki", "rosybrown", "rosybrown", "plum", 
                   "lightpink", "khaki", "wheat", "khaki", "lightblue", "lightpink", "khaki", "wheat", "wheat", "wheat", 
                   "wheat", "lightpink", "plum", "wheat", "lightpink", "wheat", "wheat", "lightblue", "plum", "plum",
                   "lightpink", "lightpink", "lightpink", "lightpink", "lightpink", "lightpink", "lightpink", "lightpink", "lightpink", "lightpink",
                   "lightpink", "lightpink", "lightpink", "khaki", "khaki", "khaki", "khaki", "khaki", "khaki", "khaki",
                   "khaki", "khaki", "khaki", "khaki", "lightblue", "lightblue", "lightblue", "lightblue", "lightblue", "lightblue",
                   "lightblue", "lightblue", "lightblue", "lightblue", "plum", "plum", "plum", "plum", "plum", "plum",
                   "plum", "rosybrown", "rosybrown", "rosybrown", "rosybrown", "rosybrown", "rosybrown", "rosybrown", "rosybrown", "rosybrown",
                   "rosybrown", "rosybrown", "rosybrown", "wheat", "wheat", "wheat", "wheat", "wheat", "wheat", "wheat", 
                   "wheat", "wheat", "wheat", "wheat", "wheat", "wheat", "wheat"))

p <- plot_ly(
  type = "sankey",
  orientation = "h",
  textfont = list(size = 24),
  node = list(label = nodes$name_node, pad = 15, thickness = 15, color = node_color),
  link = as.list(links))
p

# To save a png, use the save option in the plots window