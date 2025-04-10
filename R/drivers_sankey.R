# This code creates a sankey plot of drivers
# written by Dan Palance on April 7, 2025

# Load required packages
library(tidyverse)
library(networkD3)
library(cowplot)

# Read in the main dataframe created in earlier code
main <- readRDS(file ="output/main_hs.RDS")

# Create a dataframe with rows for each driver
hs_drivers <- main %>% 
  distinct() %>% # remove duplicates due to studies occurring in multiple regions
  separate_rows(Condensed_var, sep =", ") %>% 
  mutate_at(vars(Condensed_var), list(factor)) %>%
  filter(Condensed_var != "none") %>% 
  mutate(Drivercat = forcats::fct_collapse(Condensed_var,
                                           abiotic=c("dynamic physical", "static physical", "biogeochem"),
                                           biotic = c("ecological", "species attributes"),
                                           anthropogenic = c("human activity"))) %>%
  group_by(Condensed_var, Type, Category, Drivercat) %>% 
  count(Condensed_var)
# do the above but use percentage of studies instead of straight number

hs_components <- main %>% 
  distinct() %>% # remove duplicates due to studies occurring in multiple regions
  separate_rows(Drivers_examined, sep =", ")  %>% 
  mutate(Drivercat = forcats::fct_collapse(Drivers_examined, # this is not working since habitat falls into multiple categories, remove from all but ecological?
                                           
                                           ## DYNAMIC PHYSICAL ##
          Temperature = c("bottom temperature", "SST", "temperature"),
          Circulation = c("currents", "current speed", "current velocity", 
                          "downwelling", "NPTZ", "eddies", "EKE", "Ekman transport",
                          "gyres", "hydrographic forcing", "surface currents", 
                          "tidal current", "tide", "upwelling", "water flow",
                          "rivers"),
          Atmospheric = c("climate", "hydrology", "La Niña", "cloud coverage", 
                          "precipitation", "pressure", "El Niño", "ENSO", "PDO", storms,
                          "shear stress", "wind", "wind speed", "wind stress"),
          Distance_to_features = c("distance from edge of slope", "distance from estuary mouth", 
                                   "distance from shelf break", "distance from shore", 
                                   "distance to coast", "distance to continental slope", 
                                   "distance to eddies", "distance to estuary mouth", 
                                   "distance to fronts", "distance to iceberg", 
                                   "distance to ocean", "distance to plume", 
                                   "distance to shelf break", "proximity to estuary", 
                                   "proximity to rivers", "proximity to tidal channels"),
          Ice = c("ice", "ice coverage", "icebergs", "glaciers"),
          Sea_state = c("dynamic height", "sea level anomaly", "SSH", "SSHA", "swell",
                        "wave action", "wave exposure", "wave velocity", "waves",
                        "tsunamis", "fetch"),
          Water_column_structure = c("Gulfstream North Wall", "fronts", 
                             "mixed layer thickness", "mixing", "mixing line", 
                             "isothermality", "mixed layer depth", "pycnocline depth", 
                             "stratification", "thermal stratification", "density","turbidity"),
          Misc_dyphys_ = c("day length", "light", "moon illuminance", "gravitational sinking", 
                           "island mass effect", "protrusion of surf zone", 
                           "tectonic events", "habitat"),
         
                                ## STATIC PHYSICAL - RENAME TO BATHY & TOPO ##
         General_bt = c("bathymetry", "depth", "bottom depth", 
                                  "topography", "land area", "continental width", 
                        "islands", "habitat structure", "habitat", "habitat type", 
                        "wood debris"),
         Seabed_characteristics = c("ridges", "roughness", "rugosity", "seabed composition",
                                    "slope", "substrate", "sediment grain size", "aspect",
                                    "bottom type"),
         Bathy_structures = c("banks", "canyons", "fjords", "guyots", "seamounts", 
                              "shelf break", "shelfs"),
         
                                            ## BIOGEOCHEM ##
         Nutrients = c("nitrate", "nitrogen", "nutrients", "silica", "silicate", 
                       "silicon", "phosphate", "phosphorous", "pCO2", 
                       "nitrogen fixation"),
         Carbon_cycle = c("carbon cycling", "particulate organic carbon", "PIC, POC", 
                    "sediment organic content", "DIC"),
         "Oxygen & acidifcation" = c("dissolved oxygen", "DO", "alkalinity", "aragonite saturation", 
                           "conductivity", "dissolution", "pH", "salinity", "oxygen", 
                           "oxygen saturation"),
         
         
                                        ## SPECIES ATTRIBUTES ##
         Life_history = c("dispersal mechanisms", "life history", "life stage",
                          "reproduction rate", "wean mass", "prey recruitment"),
         "Physio & morph" = c("enzymatic responses", "phytoplankton fluorescence", 
                              "animal sensitivity", "body length", "body shape", "diet",
                              "prey size", "heat wave stress"),
         Behavior = c("behavior", "migratory behavior", "DVM", "social cues"),
         Demography = c("population size", "range size", "prey mortality", "social group size"),
         Spp_misc = c("taxonomic group", "habitat"), # Is this really a driver component?
         
         
                                          ## ECOLOGICAL ##
         Habitat = c("habitat", "habitat complexity", "habitat quality", 
                     "habitat type", "adult cover", "canopy height", "reef area", 
                     "reef structure", "rhizome layer depth", "prey habitat"),
         Community_composition = c("biodiversity", "community composition", "taxa", 
                                   "species composition", "ecological niche", 
                                   "relative nekton density-distribution"),
         Primary_production = c("chl-a", "chlorophyll", "phytoplankton", 
                                "phytoplankton biomass", "primary productivity", 
                                "productivity", "proximity to phytoplankton"),
         Misc_interactions = c("intraspecific interactions", "disease"), 
         Predation = c("predator abundance", "predator density", "predators", 
                       "prey abundance", "prey biomass", "prey density", 
                       "prey distribution", "prey type", "trophic category", 
                       "distance to feeding areas"),
         
         ## HUMAN ACTIVITY - RENAME TO ANTHROPOGENIC ##
         Fisheries = c("aquaculture", "fishing", "fishing gear", 
                       "fishing gear type", "fishing pressure", "gear material",
                       "live trade", "trawl depth"),
         Production = c("agriculture", "desalinization", 
                        "industry", "reclamation"),
         Habitat_alteration = c("biofouling", "deforestation", 
                                "dredging", "climate change", "climate velocity"),
         Shipping = c("vessel track", "vessel type", "ship breaking", "shipping", 
                      "shipwrecks", "ballast water","proximity to ports", 
                      "visitation rate", "port proximity", "recreational boating"),
         Pollution = c("bulk discharge", "dumping", "eutrophication",
                       "heavy metal effluent", "heavy metal exposure",
                       "sewage", "sewage discharge", "industrial effluent", 
                       "litter", "metal pollution", "noise", "oil pollution", 
                       "oil release rates", "PCB exposure", "plastic density", 
                       "pollution"),
         Misc_human = c("city", "consumer demand", "human activity", 
                        "human impact", "human population", 
                        "proximity to urban areas", "threat type", "tourism")
         ))

nodes <- data.frame(
  name=c(as.character(hs_drivers$Condensed_var), 
         as.character(hs_drivers$Type)) %>% unique()
)

hs_drivers$IDsource <- match(hs_drivers$Condensed_var, nodes$name)-1 
hs_drivers$IDtarget <- match(hs_drivers$Type, nodes$name)-1

p <- sankeyNetwork(Links = hs_drivers, Nodes = nodes,
                   Source = "IDsource", Target = "IDtarget",
                   Value = "n", NodeID = "name", 
                   sinksRight=FALSE)
p


