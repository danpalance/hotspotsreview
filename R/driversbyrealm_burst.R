# This code creates the starburst figure by realm for Taxa (figure X) in the hotspot manuscript
library(tidyverse)
library(cowplot)

# Read in the main dataframe created in earlier code
main <- readRDS(file ="output/main_hs.RDS")

# Create a dataframe with rows for each driver, this time keep all rows to represent studies that spanned multiple realms
hs_drivers <- main %>% 
  filter(Year != "1988") %>% 
  separate_rows(Condensed_var, sep =", ") %>% 
  mutate_at(vars(Condensed_var), list(factor)) %>%
  filter(Condensed_var != "none") %>% 
  mutate(Drivercat = forcats::fct_collapse(Condensed_var,
                                           abiotic = c("dynamic physical", "static physical"),
                                           biotic = c("ecological", "species attributes", "biogeochem"),
                                           anthropogenic = c("human activity"))) 
# circulation <- hs_drivers %>% 
#   filter(Driver == "Circulation")
# eddies <- main %>% filter(grepl(pattern = "eddies", Drivers_examined))

# hs_driver_comps <- main %>% 
#   separate_rows(Drivers_examined, sep =", ") %>% 
#   separate_rows(Drivers_examined, sep =",") %>% 
#   group_by(Drivers_examined) %>% 
#   count()
# 
# ggplot(hs_driver_comps) +
#   geom_bar(aes(x = Drivers_examined)) +
#   labs(x = "Driver component", y = "# of Studies") +
#   theme_classic() +
#   scale_y_continuous(expand = c(0,0)) +
#   theme(axis.text.x = element_text(angle = 90))
#   
# 
# unique(hs_driver_comps$Drivers_examined)



# Make dataframe seprating Taxa and hotspot drivers into rows and condensing them into broader categories
realm_df <- hs_drivers %>% 
  separate_rows(REALM, sep=",") %>% 
  group_by(Condensed_var, Drivercat, REALM) %>% 
  count(REALM) %>% 
  ungroup() %>% 
  group_by(REALM) %>% 
  mutate(Total=sum(n)) %>% 
  ungroup() %>% 
  mutate(REALM = as.factor(REALM))


# Setup empty bar spacers between realms
empty_bar <- 1
to_add <- data.frame(matrix(NA,empty_bar*nlevels(realm_df$REALM),ncol(realm_df)))
colnames(to_add) <- colnames(realm_df)
to_add$REALM <- rep(levels(realm_df$REALM), each = empty_bar)
realm_df <- rbind(realm_df, to_add)
realm_df$REALM <- as.factor(realm_df$REALM)
realm_df <- as.data.frame(realm_df)
realm_df <- realm_df %>% arrange(REALM,-n) # Sort by REALM and then number of obs so the bar graphs are ordered from highest to smallest
realm_df$id <- seq(1, nrow(realm_df))

# Get the name and the y position of each label
label_data <- realm_df
number_of_bar <- nrow(label_data)
angle <- 90 - 360 * (label_data$id-0.5) / number_of_bar     # subtract 0.5 because so the letter matches the angle of the center bars
label_data$hjust <- ifelse(angle < -90, 1, 0)
label_data$angle <- ifelse(angle < -90, angle+180, angle)

# prepare a data frame for base lines
base_data <- realm_df %>% 
  group_by(REALM) %>% 
  summarize(start=min(id)-0.5, end=max(id)-0.5) %>% # need to fix this since those that only have one occurrence aren't getting a line
  rowwise() %>% 
  mutate(title=mean(c(start, end)))
number_of_bar.base <- nrow(base_data)
base_data$id <- seq(1, nrow(base_data))
angle <- 90 - 360 * (base_data$id-0.5) /number_of_bar.base

# Create angle for the Taxa labels in the circle using the median value from the grouped bar charts realm labels
base_data_angle <- label_data %>% 
  group_by(REALM) %>% 
  summarise(median = median(angle, na.rm = FALSE)) %>%  
  rename(angle=median)  
base_data <- merge(base_data,base_data_angle)
base_data$hjust <- ifelse(angle < 70, 0.5, 0) # if the angle is less than -70, make it 1, otherwise make it 0

# prepare a data frame for the gray scale bars in between each category of bar plot
grid_data <- base_data
grid_data$end <- grid_data$start
grid_data$start <- grid_data$start - 1
grid_data <- grid_data[-13,] # remove the scale bars for the area where the scale labels will go

# Assemble graph 
# Make the plot 
p1 <- ggplot(realm_df, aes(x = Condensed_var, y=n)) +       
  geom_bar(aes(x = as.factor(id), y=n, fill = Drivercat), stat="identity") + 
  scale_fill_manual(values=c("abiotic"="saddlebrown","biotic"="green4", "anthropogenic"="goldenrod")) +
  # Add a val=20/15/10/5 lines. I do it at the beginning to make sure barplots are OVER it.
  geom_segment(data=grid_data, aes(x = end, y = 20, xend = start, yend = 20), colour = "grey", alpha=1, linewidth=0.3, inherit.aes = FALSE ) +
  geom_segment(data=grid_data, aes(x = end, y = 15, xend = start, yend = 15), colour = "grey", alpha=1, linewidth=0.3, inherit.aes = FALSE ) +
  geom_segment(data=grid_data, aes(x = end, y = 10, xend = start, yend = 10), colour = "grey", alpha=1, linewidth=0.3, inherit.aes = FALSE ) +
  geom_segment(data=grid_data, aes(x = end, y = 5, xend = start, yend = 5), colour = "grey", alpha=1, linewidth=0.3, inherit.aes = FALSE ) +
  
  # Add text showing the value of each 100/75/50/25 lines
  annotate("text", x = rep(max(realm_df$id),5), y = c(0,5, 10, 15, 20), label = c("0","5", "10", "15", "20") , color="grey", size=3 , angle=0, fontface="bold", hjust=1) +
  ylim(-100,120) +
  theme_minimal() +
  theme(legend.position = "none",
        axis.text = element_blank(),
        axis.title = element_blank(),
        panel.grid = element_blank(),
        plot.margin = unit(rep(-1,4),"cm")) +
  coord_polar() + 
  geom_text(data=label_data, aes(x=id, y=n+10, label= Condensed_var, hjust=hjust), color="black", fontface="bold",alpha=0.6, size=3.5, angle=label_data$angle, inherit.aes = FALSE ) +
  
  # Add base line information
  geom_segment(data=base_data, aes(x = start, y = -5, xend = end, yend = -5), colour = "black", alpha=0.8, linewidth=0.6 , inherit.aes = FALSE )  +
  geom_text(data=base_data, aes(x = title, y = -17, label=REALM, hjust=hjust), angle = base_data$angle,
            colour = "black", alpha=0.8, size=3.5, fontface="bold", inherit.aes = FALSE)

p2 <- ggplot(realm_df) +       
  geom_col(aes(x = as.factor(id), y = Total, fill = REALM), col = NA,width = 1.5) + 
  scale_fill_manual(name = bquote(bold("Realm")),
                    values = c("Arctic"="skyblue1", "Southern Ocean"="cornflowerblue", "Temperate Northern Pacific"="mediumseagreen", 
                              "Tropical Atlantic"="gold2","Central Indo-Pacific"="sienna2", "Temperate Australasia"="maroon4", 
                              "Temperate South America"="turquoise", "Tropical Eastern Pacific"="olivedrab1",
                              "Eastern Indo-Pacific"="palevioletred1","Temperate Northern Atlantic"="palegreen4",
                              "Temperate Southern Africa"="slategray2", "Western Indo-Pacific"="thistle1")) +
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
ggsave(file ="Figs/driversbyrealm_burst.png", scale = 2.5)
ggsave(file ="Figs/driversbyrealm_burst.svg", scale = 2.5)

#### Make plots for each realm broken down by driver components
hs_driver_components <- main %>% 
  filter(Year != "1988") %>% 
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
                                                            "Shipping", "Pollutants", "Misc human"))) 

realm_list <- hs_driver_components %>% 
  separate_rows(REALM, sep=",") %>% 
  group_by(Driver_comp, Driver, REALM) %>% 
  count(REALM) %>% 
  ungroup() %>% 
  group_by(REALM) %>% 
  mutate(Total=sum(n)) %>% 
  ungroup() %>% 
  mutate(REALM = as.factor(REALM),
         Percent = (n/Total)*100) %>% 
  group_split(REALM)


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
                       limits = c(0,35)) +
    theme_classic() +
    labs(x = "Indicator/Covariate Component", y = "% of Studies", title = df$REALM[1]) +
    theme(plot.title = element_text(face = "bold",
                                    size = 16,
                                    hjust = 1.5),
          axis.text = element_text(face = "bold",
                                   size = 12),
          axis.title = element_text(face = "bold",
                                    size = 14),
          legend.position = "inside",
          legend.position.inside = c(0.6,0.15),
          legend.text = element_text(face = "bold",
                                     size = 10),
          legend.title = element_text(face = "bold",
                                      size = 12,
                                      hjust = 0.5)) +
    coord_flip()
}

# Now build and export the plots by taxa in a loop
for (i in 1:length(realm_list)){
  temp_df <- realm_list[[i]]
  temp_df$REALM <- as.character(temp_df$REALM)
  realm_name <- temp_df$REALM[1] # get the decade name
  driver_plot(df = temp_df)
  ggsave(file = file.path("figs/", paste0(realm_name,"_drivers.png")))
}


