# This code creates the starburst figure by realm for taxa (figure 3) in the hotspot manuscript
library(tidyverse)
library(cowplot)
#library(patchwork)

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
  mutate(Taxa2 = fct_collapse(Taxa,"Fish"=c("bony fish","cart fish","reef fish","fish"),
                              "Seabirds"="seabirds",
                              "Mammals"=c("cetaceans","pinnipeds","fissipeds","sirenians","marine mammals","jaguars","mammals"),
                              "Reptiles"=c("sea turtles","sea snake","reptiles"),
                              "Plankton"=c("microalgae","nekton","plankton"),
                              "Krill"="krill",
                              "Invertebrates"=c("crustaceans","inverts","mollusks","coral","sponges","seastars","urchins","invertebrates"),
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
            Taxper = Taxtot/296) # 268 is the total number of studies that looked at taxa (excludes none and many)

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
  scale_y_continuous(expand = c(0,0)) +
  coord_flip() 
ggsave(file ="figs/taxabytype_stackedbar.png",scale=2)
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


