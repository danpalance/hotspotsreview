# This code creates a sankey plot of drivers
# written by Dan Palance on April 7, 2025

# Load required packages
library(tidyverse)
library(plotly)
library(networkD3)

# Read in the main dataframe created in earlier code
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
  mutate(Driver_comp = forcats::fct_collapse(Drivers_examined, # this is not working since habitat falls into multiple categories, remove from all but ecological?
                                             
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
                                             "Distance to features" = c("distance from edge of slope", "distance from estuary mouth", 
                                                                        "distance from shelf break", "distance from shore", 
                                                                        "distance to coast", "distance to continental slope", 
                                                                        "distance to eddies", "distance to estuary mouth", 
                                                                        "distance to fronts", "distance to iceberg", 
                                                                        "distance to ocean", "distance to plume", 
                                                                        "distance to shelf break", "proximity to estuary", 
                                                                        "proximity to rivers", "proximity to tidal channels", "distance to ledges",
                                                                        "distance to canyons"),
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
                                        "Bathy & topo" = c("General bathy/topo", "Seabed characteristics",
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
#orca(p, file = "figs/driver_sankey.png")
p






# Make the same plot using networkD3
sankeyNetwork(Links = links, Nodes = nodes, Source = "source",
              Target = "target", Value = "value", NodeID = "name_node",
              units = "TWh", fontSize = 12, nodeWidth = 30)

taxa_list <- hs_driver_components %>% 
  separate_rows(Taxa, sep=",") %>% 
  mutate(Taxa2 = fct_collapse(Taxa,"Fish"=c("bony fish","cart fish","reef fish"),
                              "Seabirds"="seabirds",
                              "Mammals"=c("cetaceans","pinnipeds","fissipeds","sirenians","marine mammals","jaguars"),
                              "Reptiles"=c("sea turtles","sea snake"),
                              "Plankton"=c("microalgae","nekton","plankton"),
                              "Krill"="krill",
                              "Invertebrates"=c("crustaceans","inverts","mollusks","coral","sponges","seastars","urchins"),
                              "Plants & Seaweed"=c("macroalgae","plants"),
                              "Microbes"="microbes",
                              "Misc"="many",
                              "None"="none")) %>% 
  group_by(Driver_comp, Driver, Taxa2) %>% 
  count(Taxa2) %>% 
  ungroup() %>% 
  group_by(Taxa2) %>% 
  mutate(Total=sum(n)) %>% 
  ungroup() %>% 
  mutate(Taxa2 = as.factor(Taxa2),
         Percent = (n/Total)*100) %>%
  group_split(Taxa2)


# make plotting function to do individual taxa plots
driver_plot <- function(df){
  ggplot(data = df) +
    geom_bar(aes(x = reorder(Driver_comp, Percent), y = Percent, fill = Driver), col = "black", stat = "identity") +
    scale_fill_manual(values = c("Ecological" = "coral",
                                 "Dynamic physical" = "ivory",
                                 "Bathy & topo" = "saddlebrown",
                                 "Biogeochem" = "aquamarine",
                                 "Species attributes" = "orchid",
                                 "Anthropogenic" = "yellow"),
                      name = "Indicator/Covariate\n Category") + # LEFT OFF HERE, NEED TO FINISH THIS LINE AND GET THE ORDER OF BARS IN DESCENDING
    scale_y_continuous(expand = c(0,0),
                       limits = c(0,25)) +
    theme_classic() +
    labs(x = "Indicator/Covariate Component", y = "% of Studies", title = df$Taxa2[1]) +
    theme(plot.title = element_text(face = "bold",
                                    size = 16,
                                    hjust = 0.5),
          axis.text = element_text(face = "bold",
                                   size = 12),
          axis.title = element_text(face = "bold",
                                    size = 14),
          legend.position = "inside",
          legend.position.inside = c(0.75,0.25),
          legend.text = element_text(face = "bold",
                                     size = 10),
          legend.title = element_text(face = "bold",
                                      size = 12,
                                      hjust = 0.5)) +
    coord_flip()
}

# Now build and export the plots by taxa in a loop
for (i in 1:length(taxa_list)){
  temp_df <- taxa_list[[i]]
  temp_df$Taxa2 <- as.character(temp_df$Taxa2)
  taxa_name <- temp_df$Taxa2[1] # get the decade name
  driver_plot(df = temp_df)
  ggsave(file = file.path("figs/", paste0(taxa_name,"_drivers.png")))
}



