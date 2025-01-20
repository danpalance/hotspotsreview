# Random stats like number of studies that looked at persistence, different methods, etc.
library(tidyverse)

# read in data and remove duplicates from multirealm studies
main <- readRDS("output/main_hs.RDS") %>% distinct(Title, .keep_all = TRUE) 

# count number of studies that looked at persistence
# NOte Moreno & Matthews doesn't currently have a Y for persistence but they 
# did actually look at it, so add 1 to the total
table(main$Persistence)
pers_studs <- main %>% 
  filter(Persistence == "Y")

pers_realms <- main %>% 
  filter(Persistence == "Y") %>% 
  separate_rows(REALM, sep=", ") %>% 
  group_by(REALM) %>% 
  count() %>% 
  ungroup() %>% 
  mutate(Percentage = (n/36)*100)
  
methods_df <- main %>% 
  separate_rows(Methods, sep=", ") %>% 
  group_by(Methods) %>% 
  count() %>% 
  ungroup() %>% 
  mutate(Percentage = (n/291)*100)


categories_df <- main %>% 
  group_by(Category) %>% 
  count()

eco_subset <- main %>% 
  filter(Year >= 2000 & Year < 2010,
         Category == "Ecoimpact")

realm_df <- main %>% 
  separate_rows(REALM, sep=", ") %>% 
  group_by(REALM) %>% 
  count() %>% 
  ungroup() %>% 
  mutate(Percentage = (n/291)*100)

depth_df <- main %>% 
  separate_rows(Depth, sep=",") %>% 
  group_by(Depth) %>% 
  count() %>% 
  ungroup() %>% 
  mutate(Percentage = (n/291)*100)


taxa_df <- main %>% 
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
  group_by(Taxa2) %>% 
  count() %>% 
  ungroup() %>% 
  mutate(Percentage = (n/291)*100)

taxacat_df <- main %>% 
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
  group_by(Taxa2, Category) %>% 
  count() 

ecotax_tot <- taxacat_df %>% 
  filter(Category == "Ecoimpact") %>% 
  mutate(Percentage = (n/85)*100)

bptax_tot <- taxacat_df %>% 
  filter(Category == "Biophysical") %>% 
  mutate(Percentage = (n/198)*100)


taxatype_df <- main %>% 
  filter(Year!="1988") %>% 
  distinct(Title, .keep_all = TRUE) %>% # remove duplicates from multirealm studies
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
  group_by(Taxa2, Type, Category) %>% 
  count(Type) %>% 
  ungroup() %>% 
  group_by(Type, Category) %>% 
  mutate(Total=sum(n)) %>% 
  ungroup() %>% 
  mutate(Type = as.factor(Type),
         Percentage = (n/Total)*100)


# Create a dataframe with rows for each driver
hs_drivers <- main %>% 
  #distinct() %>% # remove duplicates due to studies occurring in multiple regions
  separate_rows(Drivers_examined_condensed, sep =", ") %>% 
  mutate_at(vars(Drivers_examined_condensed),list(factor)) %>%
  mutate(Drivers = forcats::fct_collapse(Drivers_examined_condensed,
                                         "bathy & topo" = c("bathymetry","topography"),
                                         "climate & hydrology" = c("climate","hydrology"))) %>% 
  filter(Drivers != "none") %>% 
  mutate(Drivercat = forcats::fct_collapse(Drivers,
                                           abiotic=c("bathy & topo","circulation",
                                                     "distance to physical features",
                                                     "fronts", "habitat","climate & hydrology",
                                                     "water biogeochemistry","pollution","salinity",
                                                     "temperature"),
                                           biotic = c("primary productivity","biodiversity",
                                                      "species attributes"),
                                           anthropogenic = c("human activity","fishing","shipping","pollution")))



# Make dataframe seprating Type and hotspot type into rows and condensing them into broader categories
drivers_df <- hs_drivers %>% 
  separate_rows(Type, sep=",") %>% 
  group_by(Drivers) %>% 
  count()

eddies <- main %>% 
  separate_rows(Drivers_examined, sep =", ") %>% 
  group_by(Drivers_examined) %>% 
  count()

  
