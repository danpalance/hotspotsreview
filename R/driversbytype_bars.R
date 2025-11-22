# This code creates the hotspot driver components by type bar graphs
# Modified original code from https://r-graph-gallery.com/297-circular-barplot-with-groups.html
# Written by Dan Palance
# Last modified 05 Nov 2025

# read in required packages
library(tidyverse)
library(cowplot)

# Read in the main dataframe created in hs_globaldist.R
main <- readRDS(file ="output/main_hs.RDS")

#### Individual Subplots ####
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
                                             
                                             ## STATIC PHYSICAL - RENAME TO BATHY & TOPO ##
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
                                        "Static Physical" = c("General Bathy/topo", "Seabed Characteristics",
                                                           "Bathy Structures"),
                                        Biogeochem = c("Nutrients", "Carbon Cycle", "Oxygen & Acidifcation"),
                                        "Species Attributes" = c("Life History", "Physio & Morph",
                                                                 "Behavior", "Demography", "Misc Spp"),
                                        Ecological = c("Habitat Components", "Community Composition",
                                                       "Primary Production", "Misc Interactions",
                                                       "Predation"),
                                        "Anthropogenic" = c("Fishing", "Production", "Habitat Alteration",
                                                            "Shipping", "Pollutants", "Misc Human"))) %>% 
  group_by(Driver) %>% 
  count(Driver_comp) %>% 
  group_split(Driver)

# Plot the driver components by driver type
plot_fn <- function(df, driver_type){
  # plot the components of the driver
ggplot(data = df) +
  geom_bar(aes(x = reorder(Driver_comp, n), y = n), 
           stat = "identity",
           color = "black",
           fill = "black") +
  labs(x = "",
       y = "",
       title = driver_type) +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 90,
                                   hjust = 1),
        axis.text = element_text(size = 12),
        plot.title = element_text(size = 16,
                                  face = "bold",
                                  hjust = 0.5)) +
  scale_y_continuous(expand = c(0,0)) 

}

plot_list <- imap(hs_driver_components, function(df, i) {
  driver_name <- as.character(df$Driver[1])
  plot_fn(df, driver_name)
})


comp_plots <- cowplot::plot_grid(plot_list[[1]], plot_list[[2]],
                                 plot_list[[3]], plot_list[[4]],
                                 plot_list[[5]], plot_list[[6]],
                                 labels = "AUTO",
                                 label_size = 14,
                                 label_fontface = "bold",
                                 label_x = 0.06,
                                 align="hv",
                                 ncol = 2)

final_plot <- cowplot::ggdraw(comp_plots) +
  cowplot::draw_label("Driver Components", 
                      x = 0.5, y = 0.02,   # centered below
                      vjust = 1, hjust = 0.5,
                      size = 16, fontface = "bold") +
  cowplot::draw_label("# of Studies", 
                      x = 0.0, y = 0.5,   # centered left
                      angle = 90, fontface = "bold",
                      vjust = 1, hjust = 0.5,
                      size = 16)
final_plot
ggsave(plot = final_plot,"figs/drivercomp_plots.png", dpi = 600, 
       width = 7.5, height = 11)



