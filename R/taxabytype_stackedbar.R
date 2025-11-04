# This code creates the sideways stacked bar graph for taxa by type in the hotspot manuscript
# Written by Dan Palance
# Last modified 09 June 2025

# Load required packages
library(tidyverse)
library(cowplot)


# Make dataframe seprating taxa and hotspot type into rows and condensing them into broader categories
main <- readRDS("output/main_hs.RDS")

# Create dataframe for cumulative number of studies per year by type
taxa_df <- main %>% 
  filter(Year!="1988") %>% 
  filter(Type != "Pollution") %>% # remove this type since it doesn't have any taxa
  distinct(Title, .keep_all = TRUE) %>% # remove duplicates from multirealm studies
  separate_rows(Taxa, sep=",") %>% 
  filter(Taxa != "many") %>% # remove the one study where it was not possible to delineate taxa (Moran and Kanemoto 2007)
  filter(Taxa != "none") %>% # remove studies that didn't look at taxa
  # mutate(Taxa2 = fct_collapse(Taxa,"Fish"=c("bony fish","cart fish","reef fish","fish"),
  #                             "Seabirds"="seabirds",
  #                             "Mammals"=c("cetaceans","pinnipeds","fissipeds","sirenians","marine mammals","jaguars","mammals"),
  #                             "Reptiles"=c("sea turtles","sea snake","reptiles"),
  #                             "Plankton"=c("microalgae","nekton","plankton"),
  #                             "Krill"="krill",
  #                             "Invertebrates"=c("crustaceans","inverts","mollusks","coral","sponges","seastars","urchins","invertebrates"),
  #                             "Plants & Seaweed"=c("macroalgae","plants"),
  #                             "Microbes"="microbes")) %>% 
  mutate(Taxa2 = fct_collapse(Taxa,
                              "Ectothermic Verts" = c("bony fish","cart fish",
                                                      "reef fish","fish", "sea turtles",
                                                      "sea snake","reptiles"),
                              "Endothermic Verts" = c("seabirds","cetaceans","pinnipeds",
                                                      "fissipeds","sirenians",
                                                      "marine mammals","jaguars","mammals"),
                              "Plankton"=c("microalgae","nekton","plankton"),
                              "Macro Invertebrates"=c("krill","crustaceans","inverts",
                                                      "mollusks","coral","sponges","seastars",
                                                      "urchins","invertebrates"),
                              "Plants & Seaweed"=c("macroalgae","plants"),
                              "Microbes"="microbes")) %>% 
  group_by(Taxa2, Type, Category) %>% 
  count(Type) %>% 
  ungroup() %>% 
  group_by(Type, Category) %>% 
  mutate(Total=sum(n)) %>% 
  ungroup() %>% 
  mutate(Type = as.factor(Type))

taxa_tot_percentage <- taxa_df %>% 
  select(Taxa2, n) %>% 
  group_by(Taxa2) %>% 
  summarize(Taxtot = sum(n),
            Taxper = Taxtot/268) # 268 is the total number of studies that looked at taxa (excludes none and many)

taxa_type_percentage <- taxa_df %>% 
  select(Taxa2, n, Type, Category) %>% 
  group_by(Type) %>% 
  mutate(Taxa_tot = sum(n),
         Taxa_pctg = (n/Taxa_tot)*100) %>% 
  ungroup() %>%
  #Reorder the type factor for the plot by category
  mutate(Type = factor(Type,
                       levels = c("Water Chemistry", "Warming", "Threat",
                                  "Mortality", "Invasive Species", "Fisheries",
                                  "Bioaccumulation", "Reproduction & Recruitment",
                                  "Nutrients & Biogeochemical-Cycling", "Habitat",
                                  "Foraging", "Biodiversity & Endemism", "Abundance/Density"),
                       ordered = T))


# Plot for stacked bar graph to go near hotspot definition panel
ggplot() +
  geom_bar(data = taxa_type_percentage,
           aes(x = Type, y = Taxa_pctg, fill = Taxa2), color = "black", stat = "identity") +
  scale_fill_brewer(name = "Taxa",
                    palette = "Set3") +
  labs(x = "Hotspot Type", y = "Percentage") +
  theme_classic() +
  theme(axis.text = element_text(face = "bold",
                                 size = 12),
        axis.title = element_text(face = "bold",
                                  size = 16),
        legend.position = "top",
        legend.text = element_text(size = 20,
                                   face = "bold"),
        legend.title = element_text(size = 16,
                                    face = "bold")) +
  scale_y_continuous(expand = c(0,0),
                     limits = c(0,101)) +
  coord_flip() 
ggsave(file ="figs/taxabytype_stackedbar.png",scale = 0.6, width = 25, height = 15, units = "in")
ggsave(file ="figs/taxabytype_stackedbar.svg",scale=2)


 # get info on inverts
inverts <- main %>% 
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
  filter(Taxa2 == "Invertebrates") %>% 
  distinct(Title, .keep_all = TRUE) # remove duplicates from multirealm studies


# Plot the breakdown of taxa
taxa_df2 <- taxa_df <- hs_drivers %>% 
  separate_rows(Taxa, sep=",") %>% 
  filter(Taxa != "many") %>% # remove the one study where it was not possible to delineate taxa (Moran and Kanemoto 2007)
  filter(Taxa != "none") %>% # remove studies that didn't look at taxa
  mutate(Taxa2 = fct_collapse(Taxa,
                              "Ectothermic Verts" = c("bony fish","cart fish",
                                                      "reef fish","fish", "sea turtles",
                                                      "sea snake","reptiles"),
                              "Endothermic Verts" = c("seabirds","cetaceans","pinnipeds",
                                                      "fissipeds","sirenians",
                                                      "marine mammals","jaguars","mammals"),
                              "Plankton"=c("microalgae","nekton","plankton"),
                              "Macro Invertebrates"=c("krill","crustaceans","inverts",
                                                      "mollusks","coral","sponges","seastars",
                                                      "urchins","invertebrates"),
                              "Plants & Seaweed"=c("macroalgae","plants"),
                              "Microbes"="microbes")) %>% 
  group_by(Taxa2) %>% 
  count(Taxa) %>% 
  ungroup() %>% 
  group_by(Taxa2) %>% 
  mutate(Total=sum(n),
         Perc = round(((n/Total)*100), 1)) %>% 
  ungroup() %>% 
  mutate(Taxa2 = as.factor(Taxa2)) 

ggplot(data = taxa_df2, aes(x = "x", y = Perc, fill = Taxa)) +
  geom_bar(stat = "identity", position = "stack") + 
  coord_polar(theta = "y", direction = -1) +
  scale_fill_discrete(name = NULL) +  # Remove legend title
  labs(title = "Number of Cylinders") +  # Add plot title
  theme_void() +  # Empty theme
  theme(plot.title = element_text(hjust = 0.5, size = 15)) +
  facet_wrap(~Taxa2)

