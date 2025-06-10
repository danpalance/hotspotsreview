# This code creates the hotspot covariates and indicators by taxa starburst  plot in the hotspot manuscript
# Covariates and indicators were originally referred to as drivers, and I have kept it his way in the code for simplicity
# Modified original code from https://r-graph-gallery.com/297-circular-barplot-with-groups.html
# Written by Dan Palance
# Last modified 09 June 2025

# read in required packages
library(tidyverse)
library(cowplot)

# Read in the data from hs_globaldist.R
main <- readRDS(file ="output/main_hs.RDS")

# Create a dataframe with rows for each driver
hs_drivers <- main %>% 
  distinct(Title, .keep_all = TRUE) %>% # remove duplicates due to studies occurring in multiple realms
  separate_rows(Condensed_var, sep =", ") %>% 
  mutate_at(vars(Condensed_var), list(factor)) %>%
  filter(Condensed_var != "none") %>% 
  mutate(Drivercat = forcats::fct_collapse(Condensed_var,
                                           abiotic = c("dynamic physical", "static physical"),
                                           biotic = c("ecological", "species attributes", "biogeochem"),
                                           anthropogenic = c("human activity"))) 


# Make dataframe seprating taxa and hotspot drivers into rows and condensing them into broader categories
taxa_df <- hs_drivers %>% 
  separate_rows(Taxa, sep=",") %>% 
  filter(Taxa != "many") %>% # remove the one study where it was not possible to delineate taxa (Moran and Kanemoto 2007)
  filter(Taxa != "none") %>% # remove studies that didn't look at taxa
  mutate(Taxa2 = fct_collapse(Taxa,"Fish"=c("bony fish","cart fish","reef fish","fish"),
                              "Seabirds"="seabirds",
                              "Mammals"=c("cetaceans","pinnipeds","fissipeds","sirenians","marine mammals","jaguars","mammals"),
                              "Reptiles"=c("sea turtles","sea snake","reptiles"),
                              "Plankton"=c("microalgae","nekton","plankton"),
                              "Krill"="krill",
                              "Invertebrates"=c("crustaceans","inverts","mollusks","coral","sponges","seastars","urchins","invertebrates"),
                              "Plants & Seaweed"=c("macroalgae","plants"),
                              "Microbes"="microbes")) %>% 
  group_by(Condensed_var, Drivercat, Taxa2) %>% 
  count(Taxa2) %>% 
  ungroup() %>% 
  group_by(Taxa2) %>% 
  mutate(Total=sum(n)) %>% 
  ungroup() %>% 
  mutate(Taxa2 = as.factor(Taxa2)) 
  


# Setup empty bar spacers between realms
empty_bar <- 1
to_add <- data.frame(matrix(NA,empty_bar*nlevels(taxa_df$Taxa2),ncol(taxa_df)))
colnames(to_add) <- colnames(taxa_df)
to_add$Taxa2 <- rep(levels(taxa_df$Taxa2), each = empty_bar)
taxa_df <- rbind(taxa_df, to_add)
taxa_df$Taxa2 <- as.factor(taxa_df$Taxa2)
taxa_df <- as.data.frame(taxa_df)
taxa_df <- taxa_df %>% arrange(Taxa2,-n) # Sort by Taxa2 and then number of obs so the bar graphs are ordered from highest to smallest
taxa_df$id <- seq(1, nrow(taxa_df))

# Get the name and the y position of each label
label_data <- taxa_df
number_of_bar <- nrow(label_data)
angle <- 90 - 360 * (label_data$id-0.5) / number_of_bar     # subtract 0.5 because so the letter matches the angle of the center bars
label_data$hjust <- ifelse(angle < -90, 1, 0)
label_data$angle <- ifelse(angle < -90, angle+180, angle)

# prepare a data frame for base lines
base_data <- taxa_df %>% 
  group_by(Taxa2) %>% 
  summarize(start=min(id)-0.5, end=max(id)-0.5) %>% 
  rowwise() %>% 
  mutate(title=mean(c(start, end)))
number_of_bar.base <- nrow(base_data)
base_data$id <- seq(1, nrow(base_data))
angle <- 90 - 360 * (base_data$id-0.5) /number_of_bar.base

# Create angle for the labels in the circle using the median value from the grouped bar charts labels
base_data_angle <- label_data %>% 
  group_by(Taxa2) %>% 
  summarise(median = median(angle, na.rm = FALSE)) %>%  
  rename(angle=median)  
base_data <- merge(base_data,base_data_angle)
base_data$hjust <- ifelse(angle < 70, 0.5, 0) # if the angle is less than -70, make it 1, otherwise make it 0

# prepare a data frame for the gray scale bars in between each category of bar plot
grid_data <- base_data
grid_data$end <- grid_data$start
grid_data$start <- grid_data$start - 1
#grid_data <- grid_data[-13,] # remove the scale bars for the area where the scale labels will go

# Assemble graph 
# Make the plot 
p1 <- ggplot(taxa_df, aes(x=Condensed_var, y=n)) +       
  geom_bar(aes(x=as.factor(id), y=n, fill=Drivercat), stat="identity", color = "black") + 
  scale_fill_manual(values=c("abiotic"="saddlebrown","biotic"="green4", "anthropogenic"="goldenrod")) +
  # Add a val=20/15/10/5 lines. I do it at the beginning to make sure barplots are OVER it.
  geom_segment(data=grid_data, aes(x = end, y = 20, xend = start, yend = 20), colour = "grey", alpha=1, linewidth=0.3, inherit.aes = FALSE ) +
  geom_segment(data=grid_data, aes(x = end, y = 15, xend = start, yend = 15), colour = "grey", alpha=1, linewidth=0.3, inherit.aes = FALSE ) +
  geom_segment(data=grid_data, aes(x = end, y = 10, xend = start, yend = 10), colour = "grey", alpha=1, linewidth=0.3, inherit.aes = FALSE ) +
  geom_segment(data=grid_data, aes(x = end, y = 5, xend = start, yend = 5), colour = "grey", alpha=1, linewidth=0.3, inherit.aes = FALSE ) +
  
  # Add text showing the value of each 100/75/50/25 lines
  annotate("text", x = rep(max(taxa_df$id),5), y = c(0,5, 10, 15, 20), label = c("0","5", "10", "15", "20") , color="grey", size=3 , angle=0, fontface="bold", hjust=1) +
  ylim(-100,120) +
  theme_minimal() +
  theme(legend.position = "none",
        axis.text = element_blank(),
        axis.title = element_blank(),
        panel.grid = element_blank(),
        plot.margin = unit(rep(-1,4),"cm")) +
  coord_polar() + 
  geom_text(data=label_data, aes(x=id, y=n+10, label=Condensed_var, hjust=hjust), color="black", fontface="bold",alpha=0.6, size=4, angle=label_data$angle, inherit.aes = FALSE ) +
  
  # Add base line information
  geom_segment(data=base_data, aes(x = start, y = -5, xend = end, yend = -5), colour = "black", alpha=0.8, linewidth=0.6 , inherit.aes = FALSE )  +
  geom_text(data=base_data, aes(x = title, y = -17, label=Taxa2, hjust=hjust), angle = base_data$angle,
            colour = "black", alpha=0.8, size=3.5, fontface="bold", inherit.aes = FALSE)

p2 <- ggplot(taxa_df) +       
  geom_col(aes(x=as.factor(id), y=Total, fill=Taxa2), col=NA, width=1.5) + 
  scale_fill_brewer(palette = "Set3") +
  coord_polar() +
  theme_minimal() +
  theme(legend.position = "none",
        axis.text = element_blank(),
        axis.title = element_blank(),
        panel.grid = element_blank(),
        plot.margin = unit(rep(-1,4),"cm")) 

# Nest the circular bar charts 
ggdraw() +
  draw_plot(p2,scale=0.38) +
  draw_plot(p1,scale=1)
ggsave(file ="Figs/driversbytaxa_burst.svg", scale = 2.5)
ggsave(file ="Figs/driversbytaxa_burst.png", scale = 2.5)


#### Make plots for each taxa broken down by driver components
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
                                        "Bathy & Topo" = c("General Bathy/topo", "Seabed Characteristics",
                                                           "Bathy Structures"),
                                        Biogeochem = c("Nutrients", "Carbon Cycle", "Oxygen & Acidifcation"),
                                        "Species Attributes" = c("Life History", "Physio & Morph",
                                                                 "Behavior", "Demography", "Misc Spp"),
                                        Ecological = c("Habitat Components", "Community Composition",
                                                       "Primary Production", "Misc Interactions",
                                                       "Predation"),
                                        "Anthropogenic" = c("Fishing", "Production", "Habitat Alteration",
                                                            "Shipping", "Pollutants", "Misc Human")))


taxa_list <- hs_driver_components %>% 
  separate_rows(Taxa, sep=",") %>% 
  filter(Taxa != "many") %>% # remove the one study where it was not possible to delineate taxa (Moran and Kanemoto 2007)
  filter(Taxa != "none") %>% # remove studies that didn't look at taxa
  mutate(Taxa2 = fct_collapse(Taxa,"Fish"=c("bony fish","cart fish","reef fish","fish"),
                              "Seabirds"="seabirds",
                              "Mammals"=c("cetaceans","pinnipeds","fissipeds","sirenians","marine mammals","jaguars","mammals"),
                              "Reptiles"=c("sea turtles","sea snake","reptiles"),
                              "Plankton"=c("microalgae","nekton","plankton"),
                              "Krill"="krill",
                              "Invertebrates"=c("crustaceans","inverts","mollusks","coral","sponges","seastars","urchins","invertebrates"),
                              "Plants & Seaweed"=c("macroalgae","plants"),
                              "Microbes"="microbes")) %>% 
  group_by(Driver_comp, Driver, Taxa2) %>% 
  count(Taxa2) %>% 
  ungroup() %>% 
  group_by(Taxa2) %>% 
  mutate(Total=sum(n)) %>% 
  ungroup() %>% 
  mutate(Taxa2 = as.factor(Taxa2),
         Percent = (n/Total)*100) %>%
  group_split(Taxa2)




# Create a plotting function
driver_plot <- function(df){
  ggplot(data = df) +
    geom_bar(aes(x = reorder(Driver_comp, Percent), y = Percent, fill = Driver), col = "black", stat = "identity") +
    scale_fill_manual(values = c("Ecological" = "coral",
                                 "Dynamic Physical" = "ivory",
                                 "Bathy & Topo" = "saddlebrown",
                                 "Biogeochem" = "aquamarine",
                                 "Species Attributes" = "orchid",
                                 "Anthropogenic" = "yellow"),
                      name = "Indicator/Covariate\n Category") + 
    # scale_y_continuous(expand = c(0,0),
    #                    limits = c(0,35)) +
    scale_y_continuous(expand = expansion(mult = c(0,.1))) + #create continouos y scale and set relative expansion
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
          legend.position.inside = c(0.8,0.2),
          legend.text = element_text(face = "bold",
                                     size = 12),
          legend.title = element_text(face = "bold",
                                      size = 14,
                                      hjust = 0.5),
          legend.background = element_rect(fill = 'transparent'),
          panel.background = element_rect(fill = 'transparent'),
          plot.background = element_rect(fill = 'transparent',
                                         color = NA)
    ) +
    coord_flip()
}


# Make and group plots into a list 
taxacomp_plots <- lapply(taxa_list, driver_plot)

# Save plots in a grid, 4 to a page
taxacomp_plots1 <- cowplot::plot_grid(taxacomp_plots[[1]], taxacomp_plots[[2]],
                                       taxacomp_plots[[3]], taxacomp_plots[[4]],
                                       labels = c('A)', 'B)', 'C)', 'D)'),
                                       label_size = 18,
                                       label_fontface = "bold",
                                       align="hv") 
ggsave(plot = taxacomp_plots1,"figs/taxacomp_plots1.png", dpi = 600, 
       width = 7.5, height = 7.5, scale = 2)

taxacomp_plots2 <- cowplot::plot_grid(taxacomp_plots[[5]], taxacomp_plots[[6]],
                                       taxacomp_plots[[7]], taxacomp_plots[[8]],
                                       labels = c('E)', 'F)', 'G)', 'H)'),
                                       label_size = 18,
                                       label_fontface = "bold",
                                       align="hv")
ggsave(plot = taxacomp_plots2,"figs/taxacomp_plots2.png", dpi = 600, 
       width = 7.5, height = 7.5, scale = 2)

taxacomp_plots3 <- cowplot::plot_grid(taxacomp_plots[[9]],NULL, NULL, NULL, # add dummy plots to keep plot dimensions
                                       labels = c('I)'),
                                       label_size = 18,
                                       label_fontface = "bold",
                                       align="hv")
ggsave(plot = taxacomp_plots3,"figs/taxacomp_plots3.png", dpi = 600, 
       width = 7.5, height = 7.5, scale = 2)

