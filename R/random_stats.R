# Random stats used for percentages in manuscript like number of studies 
# that looked at persistence, different methods, etc.
# Written by Dan Palance
# Last updated 09 June 2025
library(tidyverse)

# read in data from hs_globaldist.R and remove duplicates from multirealm studies
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


# find how many studies used qualitative methods
# read in the csv as a dataframe
#hs_data <- read.csv("data/hsr_final.csv")[,2:19] # Remove the first column and last four columns
hs_data <- read.csv("data/hsr_final_subvers.csv") # Remove the first column and last four columns

# Rename columns to something more R friendly
colnames(hs_data) <- c("Authors","Year","Methods","Title","Def_clarity","Type",
                       "Definition","Taxa","Journal","Lat","Lon","Location",
                       "Depth","Summary","Persistence","Drivers_examined", 
                       "Drivers_examined_condensed", "Condensed_var") 
# First look in definition
quant_methods_def <- hs_data %>% 
  filter(Year!="1988") %>% # remove Myers study
  distinct(Title, .keep_all = TRUE) %>% 
  filter(str_detect(Definition, "KDE|SDM|Getis|kernel|standard deviation|
                      Standard deviation|Standard Deviation|
                      species distribution model|kde|sdm|
                      Species distribution model|getis|Kernel"))

# Then look in summary
quant_methods_smy <- hs_data %>% 
  filter(Year!="1988") %>% # remove Myers study
  distinct(Title, .keep_all = TRUE) %>% 
  filter(str_detect(Summary, "KDE|SDM|Getis|kernel|standard deviation|
                      Standard deviation|Standard Deviation|
                      species distribution model|kde|sdm|
                      Species distribution model|getis|Kernel"))

quant_methods <- bind_rows(quant_methods_def, quant_methods_smy) %>% 
  distinct(Title, .keep_all = TRUE)

# total number of quant studies
nrow(quant_methods)/301 

getis_smy <- quant_methods %>% 
  filter(str_detect(Summary, "Getis|getis"))
getis_def <- quant_methods %>% 
  filter(str_detect(Definition, "Getis|getis"))
getis <- bind_rows(getis_def, getis_smy) %>% 
  distinct(Title, .keep_all = TRUE) 
# Get percentage of KDE studies
nrow(getis)/301 # 

sdm_smy <- quant_methods %>% 
  filter(str_detect(Summary, "SDM|species distribution model|sdm|
                      Species distribution model|
                      species distribution model"))
sdm_def<- quant_methods %>% 
  filter(str_detect(Definition, "SDM|species distribution model|sdm|
                      Species distribution model|
                      species distribution model"))
sdm <- bind_rows(sdm_def, sdm_smy) %>% 
  distinct(Title, .keep_all = TRUE) 
# Get percentage of SDM studies
nrow(sdm)/301

kde_smy <- quant_methods %>% 
  filter(str_detect(Summary, "KDE|kernel|Kernel"))
kde_def <- quant_methods %>% 
  filter(str_detect(Definition, "KDE|kernel|Kernel"))
kde <- bind_rows(kde_smy, kde_def) %>% 
  distinct(Title, .keep_all = TRUE)
# Get percentage of KDE studies
(nrow(kde))/301 

stdev_smy <- quant_methods %>% 
  filter(str_detect(Summary, "standard deviation|Standard deviation|Standard Deviation"))
stdev_def <- quant_methods %>% 
  filter(str_detect(Definition, "standard deviation|Standard deviation|Standard Deviation"))
stdev <- bind_rows(stdev_def,stdev_smy) %>% 
  distinct(Title, .keep_all = TRUE)
nrow(stdev)/301

getis_sdm <- inner_join(getis, sdm) %>% mutate(ENS = "getis-sdm")
getis_sd <- inner_join(getis, stdev) %>% mutate(ENS = "getis-sd")
getis_kde <- inner_join(getis, kde) %>% mutate(ENS = "getis-kde")
sdm_kde <- inner_join(sdm, kde) %>% mutate(ENS = "sdm-kde")
sdm_sd <- inner_join(sdm, stdev ) %>% mutate(ENS = "sdm-sd")
kde_sd <- inner_join(kde, stdev) %>% mutate(ENS = "kde-sd")

ens_quant <- bind_rows(getis_sdm, getis_sd, getis_kde, sdm_kde, sdm_sd, kde_sd)
unique(ens_quant$Title)



