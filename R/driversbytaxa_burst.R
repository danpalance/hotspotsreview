# This code creates the starburst figure by realm for Taxa in the hotspot manuscript
library(tidyverse)
library(cowplot)


# Read in the main dataframe created in earlier code
main <- readRDS(file ="output/main_hs.RDS")

# Create a dataframe with rows for each driver
hs_drivers <- main %>% 
  distinct(Title, .keep_all = TRUE) %>% # remove duplicates due to studies occurring in multiple regions
  separate_rows(Condensed_var, sep =", ") %>% 
  mutate_at(vars(Condensed_var), list(factor)) %>%
  filter(Condensed_var != "none") %>% 
  mutate(Drivercat = forcats::fct_collapse(Condensed_var,
                                           abiotic = c("dynamic physical", "static physical"),
                                           biotic = c("ecological", "species attributes", "biogeochem"),
                                           anthropogenic = c("human activity"))) 


# Make dataframe seprating Taxa and hotspot drivers into rows and condensing them into broader categories
taxa_df <- hs_drivers %>% 
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
  summarize(start=min(id)-0.5, end=max(id)-0.5) %>% # need to fix this since those that only have one occurrence aren't getting a line
  rowwise() %>% 
  mutate(title=mean(c(start, end)))
number_of_bar.base <- nrow(base_data)
base_data$id <- seq(1, nrow(base_data))
angle <- 90 - 360 * (base_data$id-0.5) /number_of_bar.base

# Create angle for the Taxa labels in the circle using the median value from the grouped bar charts realm labels
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
  geom_bar(aes(x=as.factor(id), y=n, fill=Drivercat), stat="identity") + 
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
  geom_col(aes(x=as.factor(id), y=Total, fill=Taxa2), col=NA,width=1.5) + 
  scale_fill_brewer(palette = "Set3") +
  coord_polar() +
  theme_minimal() +
  theme(legend.position = "none",
        axis.text = element_blank(),
        axis.title = element_blank(),
        panel.grid = element_blank(),
        plot.margin = unit(rep(-1,4),"cm")) 

# Nest the circular bar charts with total taxa inside Taxa by realm
ggdraw() +
  draw_plot(p2,scale=0.38) +
  draw_plot(p1,scale=1)
ggsave(file ="Figs/driversbytaxa_burst.svg", scale = 2.5)
ggsave(file ="Figs/driversbytaxa_burst.png", scale = 2.5)


#### Make plots for each taxa broken down by driver components
hs_driver_components <- main %>% 
  distinct(Title, .keep_all = TRUE) %>% # remove duplicates due to studies occurring in multiple regions
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
                                        "Bathy & topo" = c("General bathy/topo", "Seabed characteristics",
                                                           "Bathy structures"),
                                        Biogeochem = c("Nutrients", "Carbon cycle", "Oxygen & acidifcation"),
                                        "Species attributes" = c("Life history", "Physio & morph",
                                                                 "Behavior", "Demography", "Misc spp"),
                                        Ecological = c("Habitat components", "Community composition",
                                                       "Primary production", "Misc interactions",
                                                       "Predation"),
                                        "Anthropogenic" = c("Fishing", "Production", "Habitat alteration",
                                                            "Shipping", "Pollutants", "Misc human"))) 


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














 #### heatmap dataframe and plot ####
heatmap_df <- hs_drivers %>% 
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
  group_by(Condensed_var, Drivercat, Taxa2) %>% 
  count(Taxa2) %>% 
  ungroup() %>% 
  group_by(Condensed_var) %>% 
  mutate(Total=sum(n)) %>% 
  ungroup() %>% 
  mutate(Taxa2 = as.factor(Taxa2),
         Percent = n/Total * 100,
         Condensed_var = str_to_title(Condensed_var)) # captialize first letter like to match taxa

# make the plot
ggplot(heatmap_df, aes(x = Taxa2, y = Condensed_var, fill = Percent)) +
  geom_tile(color = "black") +
  scale_fill_gradient(low = "white", high = "skyblue", name = "% of Studies",
                      guide = guide_colorbar(frame.colour = "gray", ticks.colour = "black")) +
  labs(x = "Taxa", y = "Driver") +
  geom_text(aes(x = Taxa2, y = Condensed_var, label = n)) +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        axis.text = element_text(face = "bold",
                                  size = 12),
        axis.title = element_text(face = "bold",
                                 size = 14),
        legend.text = element_text(face = "bold",
                                   size = 12),
        legend.title = element_text(face = "bold",
                                    size = 14)) +
  scale_x_discrete(expand = c(0,0)) +
  scale_y_discrete(expand = c(0,0)) 
ggsave(file ="figs/driversbytaxa_percheatmap.png", scale = 1.5)
