# Random stats like number of studies that looked at persistence, different methods, etc.
library(tidyverse)

# read in data and remove duplicates from multirealm studies
main <- readRDS("output/main_hs.RDS") %>% distinct(Title, .keep_all = TRUE) 

type_perc <- main %>% 
  filter(Year!="1988") %>% 
  distinct(Title, .keep_all = TRUE) %>% # remove duplicates from multirealm studies
  group_by(Type) %>% 
  count() %>% 
  mutate(Type_perc = (n/nrow(main) * 100))

# count number of studies that looked at persistence
table(main$Persistence)
pers_studs <- main %>% 
  filter(Persistence == "Y")

pers_realms <- main %>% 
  filter(Persistence == "Y") %>% 
  separate_rows(REALM, sep=", ") %>% 
  group_by(REALM) %>% 
  count() %>% 
  ungroup() %>% 
  mutate(Percentage = (n/40)*100)
  
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
         Category == "Ecological Impact")

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
  filter(Taxa != "many") %>% 
  filter(Taxa != "none") %>% 
  filter(Year!="1988") %>% 
  distinct(Title, .keep_all = TRUE) %>% # remove duplicates from multirealm studies
  separate_rows(Taxa, sep=",") %>% 
  mutate(Taxa2 = fct_collapse(Taxa,"Fish"=c("bony fish","cart fish","reef fish","fish"),
                              "Seabirds"="seabirds",
                              "Mammals"=c("cetaceans","pinnipeds","fissipeds","sirenians","marine mammals","jaguars","mammals"),
                              "Reptiles"=c("sea turtles","sea snake","reptiles"),
                              "Plankton"=c("microalgae","nekton","plankton"),
                              "Krill"="krill",
                              "Invertebrates"=c("crustaceans","inverts","mollusks","coral","sponges","seastars","urchins","invertebrates"),
                              "Plants & Seaweed"=c("macroalgae","plants"),
                              "Microbes"="microbes")) %>% 
  group_by(Taxa2) %>% 
  count() %>% 
  ungroup() %>% 
  mutate(Percentage = (n/268)*100) # 268 is total number of studies that had specific taxa

taxacat_df <- main %>% 
  filter(Taxa != "many") %>% 
  filter(Taxa != "none") %>% 
  filter(Year!="1988") %>% 
  distinct(Title, .keep_all = TRUE) %>% # remove duplicates from multirealm studies
  separate_rows(Taxa, sep=",") %>% 
  mutate(Taxa2 = fct_collapse(Taxa,"Fish"=c("bony fish","cart fish","reef fish","fish"),
                              "Seabirds"="seabirds",
                              "Mammals"=c("cetaceans","pinnipeds","fissipeds","sirenians","marine mammals","jaguars","mammals"),
                              "Reptiles"=c("sea turtles","sea snake","reptiles"),
                              "Plankton"=c("microalgae","nekton","plankton"),
                              "Krill"="krill",
                              "Invertebrates"=c("crustaceans","inverts","mollusks","coral","sponges","seastars","urchins","invertebrates"),
                              "Plants & Seaweed"=c("macroalgae","plants"),
                              "Microbes"="microbes")) %>% 
  group_by(Taxa2, Category) %>% 
  count() %>% 
  ungroup()  




ecoimptax_tot <- taxacat_df %>% 
  filter(Category == "Ecological Impact") %>% 
  mutate(Percentage = (n/83)*100) # 85 is number of ecoimpact studies

biophystax_tot <- taxacat_df %>% 
  filter(Category == "Biophysical") %>% 
  mutate(Percentage = (n/205)*100)


taxatype_df <- main %>% 
  filter(Taxa != "many") %>% 
  filter(Taxa != "none") %>% 
  filter(Year!="1988") %>% 
  distinct(Title, .keep_all = TRUE) %>% # remove duplicates from multirealm studies
  separate_rows(Taxa, sep=",") %>% 
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
  mutate(Type = as.factor(Type),
         Percentage = (n/Total)*100)
