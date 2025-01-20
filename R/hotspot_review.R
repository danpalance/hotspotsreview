# Hotspot review paper code
# read in required packages
library(tidyverse)
#library(ggrepel)
#library(cowplot)
#library(lubridate)
#library(viridis)
library(rnaturalearth)
library(rnaturalearthdata)
library(rgdal)
library(sf)
#library(ggpubr)
#library(gridExtra)
#library(tidytext)
#library(scales)
#library(ggExtra)
#library(cowplot)
#library(egg)
#library(patchwork)
#library(ggnewscale)

#### Read in and organize data ####
# read in the csv as a dataframe
hsr_df <- read.csv("data/HSR.csv")
hsr_df <- hsr_df[,1:14] # Remove the last two columns
colnames(hsr_df) <- c("Authors","Year","Methods","Title","DefClarity","Type","Definition","Taxa","Journal","Lat","Lon","Location","Depth","Summary") # Rename columns to something more R friendly

# Organize the types by three broad categories: Biophysical, Anthropogenic, and Ecoimpact using the forcat package in tidyverse
hsr_df$Category <- forcats::fct_collapse(hsr_df$Type, Biophysical=c("Foraging","Habitat","Nutrients & Biogeochemical-Cycling",
                                                           "Abundance/Density","Diversity & Endemism","Freq of Occurence","Reproduction & Recruitment",
                                                           "Anatomical & Physiological","Evolution & Genetics"),
                                Anthropogenic = c("Pollution"),
                                Ecoimpact = c("Invasives", "Mortality","Eutrophication & Acidification","Fisheries & Bycatch","Multi-Risk & Threat","Socio-ecological",
                                              "Bioaccumulation","Metabolic Production","Warming"))

# Combine some types and remove others to reflect types in paper
hsr_df <- hsr_df %>% 
  filter(Type != "Anatomical & Physiological", Type != "Evolution & Genetics" )

hsr_df$Type <- forcats::fct_collapse(hsr_df$Type, Habitat=c("Habitat","Freq of Occurence")) # Combine these two into habitat since they're very similar
  

  
# Get all unique lat lons from each study into individual rows 
hs_locations <- hsr_df %>% 
  separate_rows(c(Lat,Lon), sep =",") %>% 
  drop_na() # remove the studies that were global (NAs) to make plotting easier
# fix column data types
hs_locations$Lat <- as.numeric(hs_locations$Lat)
hs_locations$Lon <- as.numeric(hs_locations$Lon)

# import worldmap from rnaturalearth
world <- ne_countries(scale = "medium", returnclass = "sf") # load in spatial data of the earth

# Read in Marine Eco Regions of the World (MEOW) file from Spalding et al. 2007 using rgdal
meow <- readOGR("data/Marine Ecoregions/",layer="meow_ecos") # read in shapefile
#meow_realm <- tidy(meow, region = "REALM") # extract a dataframe of the realms from the shape file, this is defunct and replaced by line below
meow_realm <- sf::st_as_sf(meow, region = "REALM") # extract a dataframe of the realms from the shape file
# meow_realm$id <- as.factor(meow_realm$id) # Convert the id columns to factor for plotting, this is no longer needed thanks to the new line above

# Extract how many hotspots in a given realm #
meow_shp <- read_sf("data/Marine Ecoregions/",layer="meow_ecos") # read in meow as a shapefile using sf package
hs_locations_sdf <- st_as_sf(hs_locations, coords = c("Lon", "Lat")) # convert hs_locations to a spatial df
st_crs(hs_locations_sdf) <- st_crs(meow_shp) # set the coordinate system for the hs_locations_sdf to the meow shapefile coordinate system

# Import MEOW shapefile as a polygon so it works with overlay to identify studies outsides of hte MEOWs
meow_poly <- readOGR("data/Marine Ecoregions/",layer="meow_ecos")
# Get the coordinate projection
meow_poly@proj4string #### '@' = '$' in spatial data objects

# IMPORTANT NOTE: This dataframe will not contain studies that fall outside the marine realms,
meow_hs <- st_intersection(hs_locations_sdf, meow_shp)
#saveRDS(meow_hs, file ="output/meow_hs.RDS")

# Create a dataframe for those studies that fall outside of marine realms from Spalding et al. 2007
outside_meow <- anti_join(hsr_df,meow_hs,by="Title") %>% 
  mutate(REALM = ifelse(Location=="Global","Global", "Open Ocean")) %>% # Add additional options for studies that were either global (Global) or fall outside of realms (Other)
  select(-c(Lat,Lon)) # verify the few studies that had lat lons in known regions are accurate

# Combine meow_hs (spatial df) and the studies outside of eco realms to create a master dataframe for non spatial analysis, note there are several (5?) studies that spread across mutliple
# ecoregions so there will be a few more rows than the HSR_DF (307 vs 296)
master <- as.data.frame(meow_hs) 
master <- master %>% 
  select(-c(ECO_CODE,ECOREGION,PROV_CODE,PROVINCE,RLM_CODE,ALT_CODE,ECO_CODE_X,Lat_Zone,geometry))
master <- unique(rbind(outside_meow,master))
saveRDS(master, file ="output/master_hs.RDS")

# Create dataframe that sums number of studies per type
hs_types <- hsr_df %>% 
  group_by(Type, Category) %>% 
  count(Type) %>% 
  arrange(Category,-n)
write.csv(hs_types, file ="output/hs_types.csv")

sum(hs_types$n)


# Figure 1: Create hotspot map of # of studies by realm and add density plots of # of studies by lat and lon on sides ------
#cat_colors <-c("#CD950C","#0000CD","#228B22")
global_dist <- ggplot() +
  geom_polygon(data=meow_realm,aes(x=long, y=lat,group=group,fill=id),color="black") +
  scale_fill_manual(name=bquote(bold("Realm")),values=c("Arctic"="skyblue1", "Southern Ocean"="cornflowerblue", "Temperate Northern Pacific"="mediumseagreen", "Tropical Atlantic"="gold2",
                             "Central Indo-Pacific"="sienna2", "Temperate Australasia"="maroon4", "Temperate South America"="turquoise", "Tropical Eastern Pacific"="olivedrab1",
                             "Eastern Indo-Pacific"="palevioletred1","Temperate Northern Atlantic"="palegreen4","Temperate Southern Africa"="slategray2", "Western Indo-Pacific"="thistle1")) +
  #scale_fill_brewer(palette = "Set3") +
  geom_sf(data=world, color="black",fill="burlywood3") +
  geom_point(data=hs_locations, aes(x=Lon,y=Lat),size=5,color="black",fill="black",alpha=0.5) +
  theme_classic() +
  theme(axis.title = element_blank(),
        legend.title = element_text(size=17),
        legend.text = element_text(size=14),
        legend.position = c(0.67,0.08),
        legend.direction = "horizontal",
        legend.background = element_rect(fill = "transparent", colour = "transparent"))

global_dist

# Add probability density plots for lat-lon
global_dist_dens <- ggMarginal(global_dist,fill = "darkslateblue",adjust=0.45,size=12)
ggsave(plot=global_dist_dens,"figs/global_dist.png",scale=3.3)

# Create stacked bar chart of hotspot type by realm
type_spatial <- master %>% 
  distinct(Title, .keep_all=TRUE) %>%  # remove the duplicates due to studies spanning multiple realms
  group_by(Year,REALM) %>% 
  count(Type)

ggplot()+
  geom_bar(data=type_spatial,aes(x=reorder(Type,n,sum),y=n,fill=REALM), stat="identity") +
  theme_classic()+
  scale_y_continuous(expand=c(0,0)) +
  labs(x="Type",y="# of Studies") +
  theme(axis.text.x = element_text(angle = 90,hjust=1,vjust = 0.5))
#ggsave(file ="~/Documents/UCSC/Dissertation/Hot Spots Review Paper/Hotspot Manuscript/Hotspot Review/Figures/TypeRealm.png")

# Facet by realm to see the distribution of hotspot types
ggplot(data = type_spatial) +
  geom_bar(data=type_spatial,aes(x=Type,y=n), stat="identity") +
  facet_wrap(~REALM) +
  theme(axis.text.x = element_text(angle = 90,hjust=1,vjust = 0.5)) 
#ggsave(file ="~/Documents/UCSC/Dissertation/Hot Spots Review Paper/Hotspot Manuscript/Hotspot Review/Figures/TypeRealm_Facet.png")

# Make dataframe to count number of studies per taxa
taxa_spatial <- master %>% 
  separate_rows(Taxa, sep=",") %>% 
  group_by(Authors,REALM) %>% 
  count(Taxa)

# Make stacked bar graph to verify no typos or duplicates in taxa
ggplot(data=taxa_spatial) +
  geom_bar(aes(Taxa,y=n,fill=REALM),stat="identity") +
  theme_classic() +
  scale_y_continuous(expand = c(0, 0)) +
  labs(x="Taxa",y="# of Studies") +
  theme(axis.text.x = element_text(angle = 90,hjust=1,vjust = 0.5))+ 
  scale_fill_brewer(palette = "Set3") + # add two more colors to this palette for Global and Open Ocean
  coord_flip()
#ggsave(file ="~/Documents/UCSC/Dissertation/Hot Spots Review Paper/Hotspot Manuscript/Hotspot Review/Figures/TaxaRealm.png")

# Create Circular Bar Graph by taxa and realm, bar charts will be taxa, with each bar a 
# given realm, then insert a pie chart in the middle of total taxa regardless of Realm, make donut plot by larger phylogenetic groups (inverts, mammals,etc)
# Consider adding a species diversity layer by eco region (number of taxa studied per realm)
# Set a number of 'empty bar' to add at the end of each group

#taxa_df <- as.data.frame(taxa_spatial) %>% 
#  filter(Taxa!="none",Taxa!="many") %>% # omit those that had no taxa or many, consider re-adding these in some capacity
#  select(REALM,Taxa,n) %>% 
#  group_by(Taxa,REALM) %>% 
#  mutate(Taxa = as.factor(Taxa),REALM = as.factor(REALM)) %>% 
#  count(Taxa,REALM) %>% 
#  group_by(Taxa) %>% 
#  mutate(Total = sum(n)) %>% # create the sum column for the inset plot
#  ungroup()


# Create order of taxa 
# Setup empty bar spacers between taxonomic groups
#empty_bar <- 1
#to_add <- data.frame(matrix(NA,empty_bar*nlevels(taxa_df$Taxa),ncol(taxa_df)))
#colnames(to_add) <- colnames(taxa_df)
#to_add$Taxa <- rep(levels(taxa_df$Taxa), each = empty_bar)
#taxa_df <- rbind(taxa_df, to_add)
#taxa_df$Taxa <- as.factor(taxa_df$Taxa)
#taxa_df <- as.data.frame(taxa_df)
#taxa_df$Taxa <- factor(taxa_df$Taxa , levels=c("1"="microbes","2"="microalgae","3"="macroalgae","4"="inverts","5"="sponges","6"="coral","7"="urchins","8"="seastars",
#                                               "9"="crustaceans","10"="krill","11"="mollusks","12"="plankton","13"="nekton","14"="cart fish","15"="bony fish","16"="reef fish","17"="plants",
#                                               "18"="sea turtles","19"="sea snake","20"="seabirds","21"="sirenians","22"="pinnipeds","23"="fissipeds","24"="cetaceans","25"="marine mammals", "26"="jaguars"))
#taxa_df <- taxa_df %>% arrange(Taxa,-n) # Sort by Taxa and then number of obs so the bar graphs are ordered from highest to smallest
#taxa_df$id <- seq(1, nrow(taxa_df))

# Get the name and the y position of each label
#label_data <- taxa_df
#number_of_bar <- nrow(label_data)
#angle <- 90 - 360 * (label_data$id-0.5) / number_of_bar     # I substract 0.5 because the letter must have the angle of the center of the bars. Not extreme right(1) or extreme left (0)
#label_data$hjust <- ifelse( angle < -90, 1, 0)
#label_data$angle <- ifelse(angle < -90, angle+180, angle)

# prepare a data frame for base lines
#base_data <- taxa_df %>% 
#  group_by(Taxa) %>% 
#  summarize(start=min(id)-0.5, end=max(id)-0.5) %>% # need to fix this since those that only have one occurence aren't getting a line
#  rowwise() %>% 
#  mutate(title=mean(c(start, end)))
#number_of_bar.base <- nrow(base_data)
#base_data$id <- seq(1, nrow(base_data))
#angle <- 90 - 360 * (base_data$id-0.5) /number_of_bar.base

# Create angle for the taxa labels in the circle using the median value from the grouped bar charts realm labels
#base_data_angle <- label_data %>% 
#  group_by(Taxa) %>% 
#  summarise(median = median(angle, na.rm = FALSE)) %>%  
#  rename(angle=median)  
#base_data <- merge(base_data,base_data_angle)
#base_data$hjust <- ifelse(angle < 70, 0.5, 0) # if the angle is less than -90, make it 1, otherwise make it 0

# prepare a data frame for the gray scale bars in between each category of bar plot
#grid_data <- base_data
#grid_data$end <- grid_data$start
#grid_data$start <- grid_data$start - 1
#grid_data <- grid_data[-13,] # remove the scale bars for the area where the scale labels will go

# could add another taxa category called label to trick it into thinking there are labels there and then use annotate to put the text in
# Assemble graph 
# Make the plot reorder(Type, n, sum)
#p1 <- ggplot(taxa_df, aes(x=REALM, y=n, fill=Taxa)) +       
#  geom_bar(aes(x=as.factor(id), y=n, fill=Taxa), stat="identity") + # need to get taxa ordered and colored by grouping
  
  # Add a val=20/15/10/5 lines. I do it at the beginning to make sure barplots are OVER it.
#  geom_segment(data=grid_data, aes(x = end, y = 20, xend = start, yend = 20), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) +
#  geom_segment(data=grid_data, aes(x = end, y = 15, xend = start, yend = 15), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) +
#  geom_segment(data=grid_data, aes(x = end, y = 10, xend = start, yend = 10), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) +
#  geom_segment(data=grid_data, aes(x = end, y = 5, xend = start, yend = 5), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) +
  
  # Add text showing the value of each 100/75/50/25 lines
#  annotate("text", x = rep(max(taxa_df$id),5), y = c(0,5, 10, 15, 20), label = c("0","5", "10", "15", "20") , color="grey", size=3 , angle=0, fontface="bold", hjust=1) +
  #annotate("text", x = rep(max(taxa_df$id)+2.25,5), y = c(0,5, 10, 15, 20), label = c(" ") , color="grey", size=3 , angle=0, fontface="bold", hjust=1) +
#  ylim(-100,120) +
#  theme_minimal() +
#  theme(legend.position = "none",
#        axis.text = element_blank(),
#        axis.title = element_blank(),
#        panel.grid = element_blank(),
#        plot.margin = unit(rep(-1,4),"cm")) +
#  coord_polar() + 
#  geom_text(data=label_data, aes(x=id, y=n+10, label=REALM, hjust=hjust), color="black", fontface="bold",alpha=0.6, size=2.5, angle=label_data$angle, inherit.aes = FALSE ) +
  
  # Add base line information
#  geom_segment(data=base_data, aes(x = start, y = -5, xend = end, yend = -5), colour = "black", alpha=0.8, linewidth=0.6 , inherit.aes = FALSE )  +
#  geom_text(data=base_data, aes(x = title, y = -17, label=Taxa,hjust=0.5), angle = base_data$angle, 
#            colour = "black", alpha=0.8, size=2, fontface="bold", inherit.aes = FALSE)

#p2 <- ggplot(taxa_df) +       
#  geom_col(aes(x=as.factor(id), y=Total, fill=Taxa), col=NA,width=1.5) +
#  coord_polar() +
#  theme_minimal() +
#  theme(legend.position = "none",
#        axis.text = element_blank(),
#        axis.title = element_blank(),
#        panel.grid = element_blank(),
#        plot.margin = unit(rep(-1,4),"cm")) 

# Nest the circular bar charts with total taxa inside taxa by realm
#ggdraw() +
#  draw_plot(p2,scale=0.38) +
#  draw_plot(p1,scale=1)
#ggsave(file ="~/Documents/UCSC/Dissertation/Hot Spots Review Paper/Hotspot Manuscript/Hotspot Review/Figures/TaxaRealm_starburst.png",scale=1.65)
# place to grab animal silhouttes from

# Create Circular Bar Graph with realm as the big internal bars rather than taxa #####
# Set a number of 'empty bar' to add at the end of each group

realm_df <- master %>% 
  separate_rows(Taxa, sep=",") %>% 
  mutate(Taxa2 = fct_collapse(Taxa,"Fish"=c("bony fish","cart fish","reef fish"),"Seabirds"="seabirds",
                              "Mammals"=c("cetaceans","Cetaceans","pinnipeds","fissipeds","sirenians","marine mammals","jaguars"),
                              "Reptiles"=c("sea turtles","sea snake"),
                              "Plankton"=c("microalgae","nekton","plankton"),
                              "Krill"="krill",
                              "Invertebrates"=c("crustaceans","inverts","mollusks","coral","sponges","seastars","urchins"),
                              "Plants & Seaweed"=c("macroalgae","plants"),
                              "Microbes"="microbes",
                              "Misc"="many",
                              "None"="none")) %>% 
  group_by(Taxa2) %>% 
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
realm_df$Taxa2 <- as.factor(realm_df$Taxa2)
realm_df <- as.data.frame(realm_df)
realm_df <- realm_df %>% arrange(REALM,-n) # Sort by Taxa and then number of obs so the bar graphs are ordered from highest to smallest
realm_df$id <- seq(1, nrow(realm_df))

# Get the name and the y position of each label
label_data <- realm_df
number_of_bar <- nrow(label_data)
angle <- 90 - 360 * (label_data$id-0.5) / number_of_bar     # I subtract 0.5 because the letter must have the angle of the center of the bars. Not extreme right(1) or extreme left (0)
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

# Create angle for the taxa labels in the circle using the median value from the grouped bar charts realm labels
base_data_angle <- label_data %>% 
  group_by(REALM) %>% 
  summarise(median = median(angle, na.rm = FALSE)) %>%  
  rename(angle=median)  
base_data <- merge(base_data,base_data_angle)
base_data$hjust <- ifelse(angle < 80, 1, 0) # if the angle is less than 80, make it 1, otherwise make it 0

# prepare a data frame for the gray scale bars in between each category of bar plot
grid_data <- base_data
grid_data$end <- grid_data$start
grid_data$start <- grid_data$start - 1
grid_data <- grid_data[-13,] # remove the scale bars for the area where the scale labels will go

# could add another taxa category called label to trick it into thinking there are labels there and then use annotate to put the text in
# Assemble graph 
# Make the plot reorder(Type, n, sum)
p1 <- ggplot(realm_df, aes(x=Taxa2, y=n, fill=REALM)) +       
  geom_bar(aes(x=as.factor(id), y=n, fill=REALM), stat="identity") + # need to get taxa ordered and colored by grouping
  scale_fill_manual(name=bquote(bold("Realm")),values=c("Arctic"="skyblue1", "Southern Ocean"="cornflowerblue", "Temperate Northern Pacific"="mediumseagreen", "Tropical Atlantic"="gold2",
                                                        "Central Indo-Pacific"="sienna2", "Temperate Australasia"="maroon4", "Temperate South America"="turquoise", "Tropical Eastern Pacific"="olivedrab1",
                                                        "Eastern Indo-Pacific"="palevioletred1","Temperate Northern Atlantic"="palegreen4","Temperate Southern Africa"="slategray2", "Western Indo-Pacific"="thistle1",
                                                        "Global"="lightsteelblue4","Open Ocean"="gray59")) +
  
  # Add a val=20/15/10/5 lines. I do it at the beginning to make sure barplots are OVER it.
  geom_segment(data=grid_data, aes(x = end, y = 20, xend = start, yend = 20), colour = "grey", alpha=1, size=0.3, inherit.aes = FALSE ) +
  geom_segment(data=grid_data, aes(x = end, y = 15, xend = start, yend = 15), colour = "grey", alpha=1, size=0.3, inherit.aes = FALSE ) +
  geom_segment(data=grid_data, aes(x = end, y = 10, xend = start, yend = 10), colour = "grey", alpha=1, size=0.3, inherit.aes = FALSE ) +
  geom_segment(data=grid_data, aes(x = end, y = 5, xend = start, yend = 5), colour = "grey", alpha=1, size=0.3, inherit.aes = FALSE ) +
  
  # Add text showing the value of each 100/75/50/25 lines
  annotate("text", x = rep(max(realm_df$id),5), y = c(0,5, 10, 15, 20), label = c("0","5", "10", "15", "20") , color="grey", size=3 , angle=0, fontface="bold", hjust=1) +
  #annotate("text", x = rep(max(taxa_df$id)+2.25,5), y = c(0,5, 10, 15, 20), label = c(" ") , color="grey", size=3 , angle=0, fontface="bold", hjust=1) +
  ylim(-100,120) +
  theme_minimal() +
  theme(legend.position = "none",
        axis.text = element_blank(),
        axis.title = element_blank(),
        panel.grid = element_blank(),
        plot.margin = unit(rep(-1,4),"cm")) +
  coord_polar() + 
  geom_text(data=label_data, aes(x=id, y=n+10, label=Taxa2, hjust=hjust), color="black", fontface="bold",alpha=0.6, size=2.5, angle=label_data$angle, inherit.aes = FALSE ) +
  
  # Add base line information
  geom_segment(data=base_data, aes(x = start, y = -5, xend = end, yend = -5), colour = "black", alpha=0.8, linewidth=0.6 , inherit.aes = FALSE )  +
  geom_text(data=base_data, aes(x = title, y = -17, label=REALM,hjust=base_data$hjust), angle = base_data$angle, # Need to get the hjust working around the circle for the realm labels 
            colour = "black", alpha=0.8, size=2, fontface="bold", inherit.aes = FALSE)

p2 <- ggplot(realm_df) +       
  geom_col(aes(x=as.factor(id), y=Total, fill=REALM), col=NA,width=1.5) + 
  scale_fill_manual(name=bquote(bold("Realm")),values=c("Arctic"="skyblue1", "Southern Ocean"="cornflowerblue", "Temperate Northern Pacific"="mediumseagreen", "Tropical Atlantic"="gold2",
                                                        "Central Indo-Pacific"="sienna2", "Temperate Australasia"="maroon4", "Temperate South America"="turquoise", "Tropical Eastern Pacific"="olivedrab1",
                                                        "Eastern Indo-Pacific"="palevioletred1","Temperate Northern Atlantic"="palegreen4","Temperate Southern Africa"="slategray2", "Western Indo-Pacific"="thistle1",
                                                        "Global"="lightsteelblue4","Open Ocean"="gray59")) +
  coord_polar() +
  theme_minimal() +
  theme(legend.position = "none",
        axis.text = element_blank(),
        axis.title = element_blank(),
        panel.grid = element_blank(),
        plot.margin = unit(rep(-1,4),"cm")) 

# Nest the circular bar charts with total taxa inside taxa by realm
ggdraw() +
  draw_plot(p2,scale=0.38) +
  draw_plot(p1,scale=1)
ggsave(file ="Figs/RealmcentricTaxa_starburst.png",scale=1.65)
# place to grab animal silhouttes from

# Create timeline dataframes by adapting code from here: https://benalexkeen.com/creating-a-timeline-graphic-using-r-and-ggplot2/
# For type
Type_timeline_df <- meow_hs[with(meow_hs, order(Year)), ] # organize dataframe by year
Type_timeline_df <- Type_timeline_df[match(unique(Type_timeline_df$Type), Type_timeline_df$Type),] # match returns indices of the first match in the compared vectors
Type_timeline_df <- Type_timeline_df %>% 
  select(Year,Authors,Category,Type,REALM,PROVINCE,ECOREGION) %>% 
  mutate("Paper" = paste0(Type," (",Authors, " ",Year,")"))
positions <- (c(0.5, -0.5, 1.0, -1.0, 1.5, -1.5, 4, -0.5, 5, -1, 6.25, -1.5,6.5,-2)*4)
directions <- c(1,-1)

line_pos <- data.frame(
  "Year"=unique(Type_timeline_df$Year),
  "position"=rep(positions, length.out=length(unique(Type_timeline_df$Year))),
  "direction"=rep(directions, length.out=length(unique(Type_timeline_df$Year)))
)

Type_timeline_df <- merge(x=Type_timeline_df, y=line_pos, by="Year", all = TRUE)
text_offset <- .6 # controls vertical stacking space for text when multiples occur at same time

Type_timeline_df$Year_count <- ave(Type_timeline_df$Year==Type_timeline_df$Year, Type_timeline_df$Year, FUN=cumsum) # assigns increasing numeric value for each duplicate year
Type_timeline_df$Text_position <- ((Type_timeline_df$Year_count * text_offset * Type_timeline_df$direction) + Type_timeline_df$position)

year_date_range <- as.data.frame(seq(1990,2025,by=5))
colnames(year_date_range)[1] ="Year"

# Dataframe for number of studies total per year
type_peryear <- hsr_df %>% 
  filter(Year!="1988") %>% 
  count(Year,Type,Category)

#Create the timeline ggplot
Type_colors_df <- data.frame(Type=c("Diversity & Endemism","Foraging","Anatomical & Physiological","Abundance/Density","Freq of Occurence","Reproduction & Recruitment",
                                    "Habitat","Nutrients & Biogeochemical-Cycling","Evolution & Genetics","Invasives","Acidification","Mortality","Multi-Risk & Threat",
                                    "Fisheries & Bycatch","Bioaccumulation","Socio-ecological","Metabolic Production","Pollution","Warming"),
                             Color=c("darkblue","slateblue4","mediumblue","royalblue","cornflowerblue","deepskyblue","turquoise","aquamarine1","cyan",
                                     "darkgreen","forestgreen","limegreen","springgreen3","chartreuse3","lawngreen","greenyellow", "darkolivegreen1",
                                     "darkgoldenrod2","darkorange1"))
# Join the type_colors_df and type_per_year to make stacked bar plot with 3 color schemes representing the categories
Type_colors_df <- inner_join(Type_timeline_df,Type_colors_df,by="Type")
Type_colors_df <- Type_colors_df %>% 
  select(Year,Type,Color)
Type_colors_df$Y <-c(25,24,23,22,21,20,19,18,17,25,24,23,22,21,20,19,18,25,24)
Type_colors_df$X <- c(1990.3,1990.3,1990.3,1990.3,1990.3,1990.3,1990.3,1990.3,1990.3,1995.3,1995.3,1995.3,1995.3,1995.3,1995.3,1995.3,1995.3,2000.3,2000.3)

cat_colors <-c("#CD950C","#0000CD","#228B22")
# Try putting the regular timeline below the bar graph and remove the x axis of the bar graph
# also consider faceting by type, with the bar graph by category instead of type
# Make combined timeline and stacked bar graph
ggplot(Type_timeline_df,aes(x=Year,y=0,col=Category,label=Type)) +
  xlim(1989,2025) + # adjust the west end of the x axis so it isn't cutting off text labels
  scale_color_manual(values = cat_colors) +
  labs(col="Category") +
  theme_classic() +
  # Plot horizontal black line for timeline
  geom_hline(yintercept=0,color="black", linewidth=0.3) +
  geom_segment(data=Type_timeline_df[Type_timeline_df$Year_count == 1,], aes(y=Text_position,yend=0,xend=Year), color='black', size=0.2) + # to show where two type overlap in time
  geom_point(data=subset(Type_timeline_df,Type=="Nutrients & Biogeochemical-Cycling"),aes(y=0), size=6) +
  geom_point(aes(y=0), size=3) +
  theme(axis.line.y=element_blank(),
        axis.text.y=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        axis.ticks.y=element_blank(),
        axis.text.x =element_blank(),
        axis.ticks.x =element_blank(),
        axis.line.x =element_blank(),
        legend.position = "none",#c(0.2,0.8),
        legend.direction = "horizontal") +
  geom_text(data=year_date_range, aes(x=Year,y=-0.5,label=Year,fontface="bold"),size=5, color='black') +
  # label lines with each hotspot type
  geom_bar(data=type_peryear,aes(x=Year,y=n,fill=Type),color="black",stat="identity") +
  scale_fill_manual(breaks = Type_colors_df$Type,
                    values=Type_colors_df$Color) +
  guides(fill = guide_legend(ncol = 3)) + # control number of columns in legend for the fill by type
  geom_text(aes(y=Text_position,label=Paper,fontface="bold"),size=3.5) +
  # Create Manual Legend
  # Biophysical colors for legend
  geom_point(aes(x=1990,y=25),color="black",fill="darkblue",size=7.5,pch=22) +
  geom_text(aes(x=1990.3,y=25,label="Diversity & Endemism"),color="black",size=4,hjust=0,check_overlap = TRUE) +
  geom_point(aes(x=1990,y=24),color="black",fill="slateblue4",size=7.5,pch=22) +
  geom_text(aes(x=1990.3,y=24,label="Foraging"),color="black",size=4,hjust=0) +
  geom_point(aes(x=1990,y=23),color="black",fill="mediumblue",size=7.5,pch=22) +
  geom_text(aes(x=1990.3,y=23,label="Anatomical & Physiological"),color="black",size=4,hjust=0) +
  geom_point(aes(x=1990,y=22),color="black",fill="royalblue",size=7.5,pch=22) +
  geom_text(aes(x=1990.3,y=22,label="Abundance/Density"),color="black",size=4,hjust=0) +
  geom_point(aes(x=1990,y=21),color="black",fill="cornflowerblue",size=7.5,pch=22) +
  geom_text(aes(x=1990.3,y=21,label="Freq of Occurence"),color="black",size=4,hjust=0) +
  geom_point(aes(x=1990,y=20),color="black",fill="deepskyblue",size=7.5,pch=22) +
  geom_text(aes(x=1990.3,y=20,label="Nutrients & \nBiogeochemical-Cycling"),color="black",size=4,hjust=0) +
  geom_point(aes(x=1990,y=19),color="black",fill="turquoise",size=7.5,pch=22) +
  geom_text(aes(x=1990.3,y=19,label="Reproduction & Recruitment"),color="black",size=4,hjust=0) +
  geom_point(aes(x=1990,y=18),color="black",fill="aquamarine1",size=7.5,pch=22) +
  geom_text(aes(x=1990.3,y=18,label="Habitat"),color="black",size=4,hjust=0) +
  geom_point(aes(x=1990,y=17),color="black",fill="cyan",size=7.5,pch=22) +
  geom_text(aes(x=1990.3,y=17,label="Evolution & Genetics"),color="black",size=4,hjust=0) +
  # EcoImpact colors for legend
  geom_point(aes(x=1995,y=25),color="black",fill="darkgreen",size=7.5,pch=22) +
  geom_text(aes(x=1995.3,y=25,label="Invasives"),color="black",size=4,hjust=0) +
  geom_point(aes(x=1995,y=24),color="black",fill="forestgreen",size=7.5,pch=22) +
  geom_text(aes(x=1995.3,y=24,label="Acidification"),color="black",size=4,hjust=0) +
  geom_point(aes(x=1995,y=23),color="black",fill="limegreen",size=7.5,pch=22) +
  geom_text(aes(x=1995.3,y=23,label="Mortality"),color="black",size=4,hjust=0) +
  geom_point(aes(x=1995,y=22),color="black",fill="springgreen3",size=7.5,pch=22) +
  geom_text(aes(x=1995.3,y=22,label="Multi-Risk & Threat"),color="black",size=4,hjust=0) +
  geom_point(aes(x=1995,y=21),color="black",fill="chartreuse3",size=7.5,pch=22) +
  geom_text(aes(x=1995.3,y=21,label="Bioaccumulation"),color="black",size=4,hjust=0) +
  geom_point(aes(x=1995,y=20),color="black",fill="lawngreen",size=7.5,pch=22) +
  geom_text(aes(x=1995.3,y=20,label="Fisheries & Bycatch"),color="black",size=4,hjust=0) +  
  geom_point(aes(x=1995,y=19),color="black",fill="greenyellow",size=7.5,pch=22) +
  geom_text(aes(x=1995.3,y=19,label="Metabolic Production"),color="black",size=4,hjust=0) +
  geom_point(aes(x=1995,y=18),color="black",fill="darkolivegreen1",size=7.5,pch=22) +
  geom_text(aes(x=1995.3,y=18,label="Socio-ecological"),color="black",size=4,hjust=0) +
  # Anthropogenic colors for legend
  geom_point(aes(x=1999,y=25),color="black",fill="darkorange1",size=7.5,pch=22) +
  geom_text(aes(x=1999.3,y=25,label="Warming"),color="black",size=4,hjust=0) +
  geom_point(aes(x=1999,y=24),color="black",fill="darkgoldenrod2",size=7.5,pch=22) +
  geom_text(aes(x=1999.3,y=24,label="Pollution"),color="black",size=4,hjust=0) 


# Figure 4: The timeline of methods evolution -----
# Create dataframe that sums number of studies by method type
methods_total <- hsr_df %>% # use the original raw dataframe to avoid counting duplicates with different realms in the master df
  separate_rows(Methods, sep=", ") %>% 
  count(Methods)

# Plot the data to see general methods trends
ggplot() +
  geom_bar(data=methods_total, aes(x=reorder(Methods,-n,sum),y=n),color="black",fill="black",stat="identity") +
  scale_y_continuous(expand=c(0,0)) +
  theme_classic() +
  labs(x="Method",y= "# of Studies") +
  theme(axis.text.x = element_text(angle = 90,hjust=1,vjust = 0.5)) 
#ggsave(file ="figs/Methods_total.png")  


# Create dataframe for studies that used more than one method
methods_mutli <- master %>% 
  mutate("Paper" = paste0(Authors, ", ",Year)) %>%
  separate_rows(Methods, sep=", ") %>% 
  count(Paper) %>% 
  separate(Paper, into = c('Author', 'Year'), sep = ", ") %>% 
  filter(n>1) %>% 
  count(Year)
#methods_multi$Year <- as.factor(methods_multi$Year)

# Create dataframe for studies that employed just one method
methods_single <- master %>% 
  mutate("Paper" = paste0(Authors, ", ",Year)) %>%
  separate_rows(Methods, sep=", ") %>% 
  count(Paper) %>% 
  separate(Paper, into = c('Author', 'Year'), sep = ", ") %>% 
  filter(n==1) %>% 
  count(Year)
#methods_multi$Year <- as.factor(methods_multi$Year)

methods_spatial <- master %>% 
  separate_rows(Methods, sep=", ") %>% 
  count(Methods,REALM)
method_timeline_df <- meow_hs[with(meow_hs, order(Year)), ] # organize dataframe by year
method_timeline_df <- method_timeline_df %>% 
  separate_rows(Methods, sep=", ") %>% 
  select(Year,Authors,Category,Methods,REALM,PROVINCE,ECOREGION) %>% 
  mutate("Paper" = paste0(Methods," (",Authors, " ",Year,")")) 
method_timeline_df <- method_timeline_df[match(unique(method_timeline_df$Methods), method_timeline_df$Methods),] # match returns indices of the first match in the compared vectors
method_timeline_df <- method_timeline_df %>% 
  select(Year,Authors,Methods,Category,REALM,PROVINCE,ECOREGION) %>% 
  mutate("Paper" = paste0(Methods," (",Authors, " ",Year,")"))
positions <- c(0.5, -0.5, 1.0, -1.0, 1.5, -1.5, 0.5, -0.5, 1.0, -1.0, 1.5, -1.5, 0.5)
directions <- c(1, -1)

method_line_pos <- data.frame(
  "Year"=unique(method_timeline_df$Year),
  "position"=rep(positions, length.out=length(unique(method_timeline_df$Year))),
  "direction"=rep(directions, length.out=length(unique(method_timeline_df$Year)))
)


method_timeline_df <- merge(x=method_timeline_df, y=method_line_pos, by="Year", all = TRUE)
method_timeline_df$Methods <- as.factor(method_timeline_df$Methods) # make methods a factor so ggplot works correctly
text_offset <- .6 # controls vertical stacking space for text when multiples occur at same time

method_timeline_df$Year_count <- ave(method_timeline_df$Year==method_timeline_df$Year, method_timeline_df$Year, FUN=cumsum) # assigns increasing numeric value for each duplicate year
method_timeline_df$Text_position <- ((method_timeline_df$Year_count * text_offset * method_timeline_df$direction) + method_timeline_df$position)

year_date_range <- as.data.frame(seq(1990,2025,by=5))
colnames(year_date_range)[1] ="Year"

# Dataframe for number of studies total per year
methods_peryear <- hsr_df %>% 
  filter(Year!="1988") %>% 
  separate_rows(Methods, sep=", ") %>%
  count(Year,Methods)

#Create the timeline ggplot - switch this to methods
method_colors_df <- data.frame(Methods=c("Survey","Acoustics","Review","Database","Fishery","Model","Lab","Satellite","Biologging","Paleontology","Experiment","Social Survey","Radar"),
                               Color=c("darkblue","cornflowerblue","forestgreen","lightgreen","darkorchid","coral2","firebrick4","darkgoldenrod2","plum","aquamarine","sienna4",
                                       "slategrey","aliceblue"))
# Join the type_colors_df and type_per_year to make stacked bar plot with 3 color schemes representing the categories
method_colors_df <- inner_join(as.data.frame(method_timeline_df),method_colors_df,by="Methods")
method_colors_df <- method_colors_df %>% 
  select(Year,Methods,Color)
method_colors_df$Y <-c(25,24,23,22,21,20,19,18,17,25,24,23,22)
method_colors_df$X <- c(1990.3,1990.3,1990.3,1990.3,1990.3,1990.3,1990.3,1990.3,1990.3,1995.3,1995.3,1995.3,1995.3)


# Fix the color scheme for the categories
method_timeline_plot <- ggplot(method_timeline_df,aes(x=Year,y=0, col=Category,label=Methods)) +
  xlim(1989,2025) +
  scale_color_manual(values = cat_colors) +
  labs(col="Category") +
  theme_classic() +
  # Plot horizontal black line for timeline
  geom_hline(yintercept=0,color="black", linewidth=0.3) +
  geom_segment(data=method_timeline_df[method_timeline_df$Year_count == 1,], aes(y=position,yend=0,xend=Year), color='black', size=0.2) + 
  geom_point(aes(y=0), size=3) + 
  # scale_color_brewer(palette = "Dark2") +
  theme(axis.line.y=element_blank(),
        axis.text.y=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        axis.ticks.y=element_blank(),
        axis.text.x =element_blank(),
        axis.ticks.x =element_blank(),
        axis.line.x =element_blank(),
        legend.position = "bottom"
  ) +
  geom_text(data=year_date_range, aes(x=Year,y=-0.1,label=Year,fontface="bold"),size=5, color='black') +
  # label lines with each hotspot type
  geom_text(aes(y=Text_position,label=Paper,fontface="bold"),size=3.5) 

method_stackedbar <- ggplot(method_timeline_df,aes(x=Year,y=0,label=Methods)) +
  xlim(1989,2025) + # adjust the west end of the x axis so it isn't cutting off text labels
  scale_y_continuous(c(0,0)) +
  theme_classic() +
  # Plot horizontal black line for timeline
  theme(axis.line.x =element_blank(),
        #axis.line.y=element_blank(),
        axis.text.x =element_blank(),
        #axis.text.y=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        #axis.ticks.y=element_blank(),
        axis.ticks.x =element_blank(),
        legend.position = "none",
        plot.margin = unit(c(0, 0, 0, 0), "cm")) +
  geom_text(data=year_date_range, aes(x=Year,y=-3.5,label=Year,fontface="bold"),size=5, color='black') +
  # label lines with each hotspot type
  geom_bar(data=methods_peryear,aes(x=Year,y=n,fill=Methods),color="black",stat="identity") +
  scale_fill_manual(breaks=method_colors_df$Methods,
                    values=method_colors_df$Color) +
  guides(fill = guide_legend(ncol = 3)) +  # control number of columns in legend for the fill by type
  geom_hline(yintercept=0,color="black", linewidth=0.3) +
  # Make custom legend
  geom_point(aes(x=1990,y=25),color="black",fill="darkblue",size=7.5,pch=22) +
  geom_text(aes(x=1990.3,y=25,label="Survey"),color="black",size=4,hjust=0,check_overlap = TRUE) +
  geom_point(aes(x=1990,y=24),color="black",fill="cornflowerblue",size=7.5,pch=22) +
  geom_text(aes(x=1990.3,y=24,label="Acoustics"),color="black",size=4,hjust=0) +
  geom_point(aes(x=1990,y=23),color="black",fill="forestgreen",size=7.5,pch=22) +
  geom_text(aes(x=1990.3,y=23,label="Review"),color="black",size=4,hjust=0) +
  geom_point(aes(x=1990,y=22),color="black",fill="lightgreen",size=7.5,pch=22) +
  geom_text(aes(x=1990.3,y=22,label="Database"),color="black",size=4,hjust=0) +
  geom_point(aes(x=1990,y=21),color="black",fill="darkorchid",size=7.5,pch=22) +
  geom_text(aes(x=1990.3,y=21,label="Fishery"),color="black",size=4,hjust=0) +
  geom_point(aes(x=1990,y=20),color="black",fill="coral2",size=7.5,pch=22) +
  geom_text(aes(x=1990.3,y=20,label="Model"),color="black",size=4,hjust=0) +
  geom_point(aes(x=1990,y=19),color="black",fill="firebrick4",size=7.5,pch=22) +
  geom_text(aes(x=1990.3,y=19,label="Lab"),color="black",size=4,hjust=0) +
  geom_point(aes(x=1990,y=18),color="black",fill="darkgoldenrod2",size=7.5,pch=22) +
  geom_text(aes(x=1990.3,y=18,label="Satellite"),color="black",size=4,hjust=0) +
  geom_point(aes(x=1990,y=17),color="black",fill="plum",size=7.5,pch=22) +
  geom_text(aes(x=1990.3,y=17,label="Biologging"),color="black",size=4,hjust=0) +
  geom_point(aes(x=1995,y=25),color="black",fill="aquamarine",size=7.5,pch=22) +
  geom_text(aes(x=1995.3,y=25,label="Paleontology"),color="black",size=4,hjust=0) +
  geom_point(aes(x=1995,y=24),color="black",fill="sienna4",size=7.5,pch=22) +
  geom_text(aes(x=1995.3,y=24,label="Experiment"),color="black",size=4,hjust=0) +
  geom_point(aes(x=1995,y=23),color="black",fill="slategrey",size=7.5,pch=22) +
  geom_text(aes(x=1995.3,y=23,label="Social Survey"),color="black",size=4,hjust=0) +
  geom_point(aes(x=1995,y=22),color="black",fill="aliceblue",size=7.5,pch=22) +
  geom_text(aes(x=1995.3,y=22,label="Radar"),color="black",size=4,hjust=0) 

method_timebar <- plot_grid(method_stackedbar,method_timeline_plot,ncol=1) # consider adding numbers to some of the histo bars for reference
ggsave(method_timebar,file ="~/Documents/UCSC/Dissertation/Hot Spots Review Paper/Hotspot Manuscript/Hotspot Review/Figures/method_timebar.png",scale=3)

# Create Figure 5 to show relationship between taxa diversity, methods, and hotspot types
# Need to add in global and many taxa studies still
diversity_df <- meow_hs %>% 
  separate_rows(Taxa, sep=",") %>% 
  separate_rows(Methods,sep=", ") %>% 
  group_by(Methods,Taxa) %>% 
  count() %>% 
  mutate(Taxa = as.factor(Taxa),Methods = as.factor(Methods)) %>% 
  group_by(Methods) %>% 
  mutate(Total = sum(n)) # create the sum column for the inset plot
diversity_df <- as.data.frame((diversity_df)) %>% 
  select(-geometry)

#Make initial stacked bar plot for QC
ggplot(diversity_df) +
  geom_bar(aes(x=Methods, y=n, fill=Taxa),stat="identity")

# Now make the starburst plot like Figure 3

diversity_empty_bar <- 1
diversity_to_add <- data.frame(matrix(NA,diversity_empty_bar*nlevels(diversity_df$Methods),ncol(diversity_df)))
colnames(diversity_to_add) <- colnames(diversity_df)
diversity_to_add$Methods <- rep(levels(diversity_df$Methods), each = diversity_empty_bar)
diversity_df <- rbind(diversity_df, diversity_to_add)
diversity_df <- diversity_df %>% arrange(Methods,-n)
diversity_df$Subphy <- fct_collapse(diversity_df$Methods, vertebrates=c("bony fish","cart fish","reef fish","sea turtles","sea snake","seabirds","sirenians","pinnipeds","fissipeds",
                                                                        "cetaceans","jaguars"),
                                    invertebrates=c("coral","crustaceans","inverts","krill","macroalgae","microalgae","microbes","mollusks","plankton","plants",
                                                    "seastars","sponges","urchins")) #,
# unknown="many",none="none")
diversity_df$Grouping <- fct_collapse(diversity_df$Taxa, fish=c("bony fish","cart fish","reef fish"),reptiles=c("sea turtles","sea snake"),seabirds="seabirds",
                                      "marine mammals"=c("sirenians","pinnipeds","fissipeds","cetaceans","jaguars"),
                                      algae=c("macroalgae","microalgae"),zooplankton="plankton",shellfish=c("krill","mollusks","crustaceans"),echinoderms=c("urchins","seastars"),
                                      sponges="sponges",microbes="microbes",plants="plants",coral="coral",inverts="inverts")#,none="none",unknown="many")

diversity_df$Evolution <- fct_collapse(diversity_df$Taxa,b="microbes",c="microalgae",d="macroalgae",e="sponges",f="plants",g="inverts",h="coral",i="seastars",j="urchins",
                                       k="mollusks",l="crustaceans",m="krill",n="plankton",o="cart fish",p="bony fish",q="reef fish",r="sea turtles",
                                       s="sea snake",t="seabirds",u="sirenians",v="cetaceans",w="pinnipeds",x="fissipeds",y="jaguars") #, a="none",z="many"
diversity_df$id <- seq(1, nrow(diversity_df))

#taxa$Color

# Get the name and the y position of each label
diversity_label_data <- diversity_df
diversity_number_of_bar <- nrow(diversity_label_data)
diversity_angle <- 90 - 360 * (diversity_label_data$id-0.5) / diversity_number_of_bar # I substract 0.5 because the letter must have the angle of the center of the bars. Not extreme right(1) or extreme left (0)
diversity_label_data$hjust <- ifelse(diversity_angle < -90, 1, 0)
diversity_label_data$angle <- ifelse(diversity_angle < -90, diversity_angle+180, diversity_angle)

# prepare a data frame for base lines
diversity_base_data <- diversity_df %>% 
  group_by(Methods) %>% 
  summarize(start=min(id)-0.5, end=max(id)-0.5) %>% # need to fix this since those that only have one occurence aren't getting a line
  rowwise() %>% 
  mutate(title=mean(c(start, end)))
diversity_number_of_bar.base <- nrow(diversity_base_data)
diversity_base_data$id <- seq(1, nrow(diversity_base_data))
diversity_base_angle <- 90 - 360 * (diversity_base_data$id-0.5) / diversity_number_of_bar.base

# Create angle for the Methods labels in the circle using the median value from the grouped bar charts realm labels
diversity_base_data_angle <- diversity_label_data %>% 
  group_by(Methods) %>% 
  summarise(median = median(angle, na.rm = FALSE)) %>%  
  rename(angle=median)  
diversity_base_data <- merge(diversity_base_data,diversity_base_data_angle)
diversity_base_data$hjust <- ifelse(diversity_base_angle < 70, 0.5, 0) # if the angle is less than -90, make it 1, otherwise make it 0

# prepare a data frame for grid (scales)
diversity_grid_data <- diversity_base_data
diversity_grid_data$end <- diversity_grid_data$end[c( nrow(diversity_grid_data), 1:nrow(diversity_grid_data)-1)] + 1
diversity_grid_data$start <- diversity_grid_data$start - 1
diversity_grid_data <- diversity_grid_data[-1,]


# Make the plot
diversity_p1 <- ggplot(diversity_df, aes(x=Taxa, y=n, fill=Methods)) +       
  geom_bar(aes(x=as.factor(id), y=n, fill=Methods), stat="identity") + # need to get taxa ordered and colored by grouping
  
  # Add a val=20/15/10/5 lines. I do it at the beginning to make sure barplots are OVER it.
  geom_segment(data=diversity_grid_data, aes(x = end, y = 20, xend = start, yend = 20), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) +
  geom_segment(data=diversity_grid_data, aes(x = end, y = 15, xend = start, yend = 15), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) +
  geom_segment(data=diversity_grid_data, aes(x = end, y = 10, xend = start, yend = 10), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) +
  geom_segment(data=diversity_grid_data, aes(x = end, y = 5, xend = start, yend = 5), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) +
  
  # Add text showing the value of each 100/75/50/25 lines
  annotate("text", x = rep(max(diversity_df$id)+0.3,5), y = c(0,5, 10, 15, 20), label = c("0","5", "10", "15", "20") , color="grey", size=3 , angle=0, fontface="bold", hjust=1) +
  ylim(-100,120) +
  theme_minimal() +
  theme(legend.position = "none",
        axis.text = element_blank(),
        axis.title = element_blank(),
        panel.grid = element_blank(),
        plot.margin = unit(rep(-1,4),"cm")) +
  coord_polar() + 
  geom_text(data=diversity_label_data, aes(x=id, y=n+10, label=Taxa, hjust=hjust), color="black", fontface="bold",alpha=0.6, size=2.5, angle=diversity_label_data$angle, inherit.aes = FALSE ) +
  
  # Add base line information
  geom_segment(data=diversity_base_data, aes(x = start, y = -5, xend = end, yend = -5), colour = "black", alpha=0.8, linewidth=0.6 , inherit.aes = FALSE )  +
  geom_text(data=diversity_base_data, aes(x = title, y = -17, label=Methods,hjust=0.5), angle = diversity_base_data$angle, 
            colour = "black", alpha=0.8, size=2, fontface="bold", inherit.aes = FALSE)

diversity_p2 <- ggplot(diversity_df) +       
  geom_col(aes(x=as.factor(id), y=Total, fill=Methods), col=NA,width=1.5) +
  coord_polar() +
  theme_minimal() +
  theme(legend.position = "none",
        axis.text = element_blank(),
        axis.title = element_blank(),
        panel.grid = element_blank(),
        plot.margin = unit(rep(-1,4),"cm")) 

# Nest the circular bar charts with total taxa inside taxa by realm
ggdraw() +
  draw_plot(diversity_p2,scale=0.38) +
  draw_plot(diversity_p1,scale=1)
ggsave(file ="~/Documents/UCSC/Dissertation/Hot Spots Review Paper/Hotspot Manuscript/Hotspot Review/Figures/Diversity_starburst.png",scale=1.65)












##### OLD CODE TO BE REPURPOSED ABOVE #

# Create dataframe grouped by category of hotspot
hsr_cattypes <- hsr_df %>% 
  group_by(Category) %>% 
  count(Type)

ggplot(hsr_cattypes) +
  geom_bar(aes(x=Category,y=n,fill=factor(Type,levels=c("Nutrients & Biogeochemical-Cycling","Evolution & Genetics","Habitat",
                                                        "Anatomical & Physiological","Reproduction & Recruitment","Freq of Occurence",
                                                        "Foraging","Diversity & Endemism","Abundance/Density","Pollution","Warming","Invasives", "Mortality","Acidification","Fisheries & Bycatch","Multi-Risk & Threat","Socio-ecological",
                                                        "Bioaccumulation","Metabolic Production"))),stat="identity") +
  theme_classic() +
  labs(x="Category",y="# of Publications",fill="Hotspot Type") +
  scale_y_continuous(expand = c(0, 0)) +
  theme(legend.position=c(0.85,0.6)) 
ggsave(file ="~/Documents/UCSC/Dissertation/Hot Spots Review Paper/Hotspot Manuscript/Hotspot Review/Figures/Cattypes.png",scale=1.25)

hs_year <- hsr_df %>% 
  count(Year,Type) %>% 
  mutate(CumType = cumsum(n))
#filter(Year!="2023")

biophys_annual <- hsr_df %>% 
  filter(Category=="Biophysical") %>% 
  count(Year,Type) %>% 
  mutate(CumType = cumsum(n))

ggplot(biophys_annual,aes(x=Year,y=n,fill=factor(Type,levels=c("Nutrients & Biogeochemical-Cycling","Evolution & Genetics","Habitat",
                                                               "Anatomical & Physiological","Reproduction & Recruitment","Freq of Occurence",
                                                               "Foraging","Diversity & Endemism","Abundance/Density"))))+ # levels argument allows you to organize bar graph stacking
  geom_bar(stat="identity")+ # could add position=stack argument but it won't let you choose the order of stacking
  #stat_smooth(method="loess")+
  labs(x="Year",y="Number of Biophysical Publications", fill="Hotspot Type")+
  scale_y_continuous(expand = c(0, 0)) +
  scale_fill_viridis(discrete = TRUE, option = "turbo")+
  theme_classic() +
  theme(legend.position=c(0.25,0.8))
ggsave(file ="~/Documents/UCSC/Dissertation/Hot Spots Review Paper/Hotspot Manuscript/Hotspot Review/Figures/Biophys_cumsum.png")

ecoimp_annual <- hsr_df %>% 
  filter(Category=="Ecoimpact") %>% 
  count(Year,Type) %>% 
  mutate(CumType = cumsum(n))

ggplot(ecoimp_annual,aes(x=Year,y=n,fill=factor(Type,levels=c("Invasives", "Mortality","Acidification","Fisheries & Bycatch","Multi-Risk & Threat","Socio-ecological",
                                                              "Bioaccumulation","Metabolic Production"))))+ # levels argument allows you to organize bar graph stacking
  geom_bar(stat="identity")+ # could add position=stack argument but it won't let you choose the order of stacking
  #stat_smooth(method="loess")+
  labs(x="Year",y="Number of Ecological Impact Publications", fill="Hotspot Type")+
  scale_y_continuous(expand = c(0, 0)) +
  scale_fill_viridis(discrete = TRUE, option = "turbo")+
  theme_classic() +
  theme(legend.position=c(0.25,0.8))
ggsave(file ="~/Documents/UCSC/Dissertation/Hot Spots Review Paper/Hotspot Manuscript/Hotspot Review/Figures/Ecoimp_cumsum.png")

anthro_annual <- hsr_df %>% 
  filter(Category=="Anthropogenic") %>% 
  count(Year,Type) %>% 
  mutate(CumType = cumsum(n))

ggplot(anthro_annual,aes(x=Year,y=n,fill=factor(Type,levels=c("Pollution","Warming"))))+ # levels argument allows you to organize bar graph stacking
  geom_bar(stat="identity")+ # could add position=stack argument but it won't let you choose the order of stacking
  #stat_smooth(method="loess")+
  labs(x="Year",y="Number of Ecological Impact Publications", fill="Hotspot Type")+
  scale_y_continuous(expand = c(0, 0)) +
  scale_fill_viridis(discrete = TRUE, option = "turbo")+
  theme_classic() +
  theme(legend.position=c(0.25,0.8))
ggsave(file ="~/Documents/UCSC/Dissertation/Hot Spots Review Paper/Hotspot Manuscript/Hotspot Review/Figures/Anthro_cumsum.png")


# Make dataframe to tell how many papers clearly defined hotspot type each year
hs_defs <- hsr_df %>% 
  select(Year,DefClarity) %>% 
  group_by(Year) %>% 
  count(DefClarity) %>% 
  pivot_wider(names_from = DefClarity,values_from = n)

# Plot the definition clarity dataframe
ggplot(data=hs_defs)+
  geom_line(aes(x=Year,y=N), color="blue")+
  geom_line(aes(x=Year,y=Y), color="orange")+
  theme_classic()

# Create dataframe for timeline of hotspot definitions to see when each type emerged (consider overlaying this on annual hotspot pubs figure)            
timeline_df <- hsr_df[match(unique(hsr_df$Type), hsr_df$Type),] # identify year each definition emerged
timeline_df$Year <- as.Date(as.character(timeline_df$Year), format = "%Y")
#timeline_df$Year <- lubridate::year(timeline_df$Year)

timeline_df <- timeline_df %>%  # sort the dataframe by order of appearance for the category
  arrange(Category) %>% 
  select(Authors,Year,Type,Category) %>% 
  mutate(Posn = c(1,0.7,0.5,-0.5,0.3,0.5,-0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,1,-1))

# Source code for timeline plot: https://stackoverflow.com/questions/7492274/draw-a-chronological-timeline-with-ggplot2

# Function to shift x-axis to 0 adapted from link shown above
shift_axis <- function(p, xmin, xmax, y=0){
  g <- ggplotGrob(p)
  dummy <- data.frame(y=y)
  ax <- g[["grobs"]][g$layout$name == "axis-b"][[1]]
  p + annotation_custom(grid::grobTree(ax, vp = grid::viewport(y=1, height=sum(ax$height))), 
                        ymax=y, ymin=y) +
    annotate("segment", y = 0, yend = 0, x = xmin, xend = xmax, 
             arrow = arrow(length = unit(0.1, "inches"))) +
    theme(axis.text.x = element_blank(), 
          axis.ticks.x=element_blank())
  
}

#Conditionally set whether text will be above or below the point
vjust = ifelse(timeline_df$Posn > 0, -1, 1.5)

#plot
p1 <- timeline_df %>% 
  ggplot(aes(Year, Type)) +
  geom_lollipop(point.size = 1) +
  geom_text(data = timeline_df, aes(x = Year, y = Posn, label = Type),
            hjust = 0, vjust = vjust, size = 2.5) +
  theme_classic()+
  theme(axis.title = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.line = element_blank(),
        axis.text.x = element_text(size = 8)) +
  expand_limits(x = c(ymd(19880101), ymd(20230201)), y = 3) #+
scale_x_date(breaks = scales::pretty_breaks(n = 9))


# run the function from above
timeline <- shift_axis(p1, ymd(19880101), ymd(20230201))

# Create a types dataframe
hs_types <- hsr_df %>% 
  count(Type)

yearly_hs_types <- hsr_df %>% 
  group_by(Year) %>% 
  count(Type) %>% 
  group_by(Type) %>% 
  mutate(Total=cumsum(n))


# Create a category dataframe (biophys, anthro, eco impact)
yearly_hs_cats <- hsr_df %>% 
  group_by(Year) %>% 
  count(Category) %>% 
  group_by(Category) %>% 
  mutate(Total=cumsum(n))

# Create dataframe for each category
yearly_bio <- hsr_df %>% 
  filter(Category=="Biophysical") %>% 
  group_by(Year) %>%
  count(Type) %>% 
  group_by(Type) %>% 
  mutate(Total=cumsum(n))

yearly_eco <- hsr_df %>% 
  filter(Category=="Ecoimpact") %>% 
  group_by(Year) %>% 
  count(Type) %>% 
  group_by(Type) %>% 
  mutate(Total=cumsum(n))

yearly_anthro <- hsr_df %>% 
  filter(Category=="Anthropogenic") %>% 
  group_by(Year) %>% 
  count(Type) %>% 
  group_by(Type) %>% 
  mutate(Total=cumsum(n))

# Create area plots of each category with its types by year
ggplot(data=yearly_bio) +
  geom_line(aes(x=Year,y=Total,color=Type)) +
  theme_classic() +
  scale_y_continuous(expand = c(0, 0))  # removes white space between axis and data (ggplot default setting)
ggsave(file ="~/Documents/UCSC/Dissertation/Hot Spots Review Paper/Hotspot Manuscript/Hotspot Review/Figures/YearlyBio.png")

ggplot(data=yearly_anthro) +
  ylim(0,90) +
  geom_line(aes(x=Year,y=Total,color=Type)) +
  theme_classic() +
  scale_y_continuous(expand = c(0, 0))  # removes white space between axis and data (ggplot default setting)
ggsave(file ="~/Documents/UCSC/Dissertation/Hot Spots Review Paper/Hotspot Manuscript/Hotspot Review/Figures/YearlyAnthro.png")

ggplot(data=yearly_eco) +
  ylim(0,90) +
  geom_line(aes(x=Year,y=Total,color=Type)) +
  theme_classic() +
  scale_y_continuous(expand = c(0, 0))  # removes white space between axis and data (ggplot default setting)
ggsave(file ="~/Documents/UCSC/Dissertation/Hot Spots Review Paper/Hotspot Manuscript/Hotspot Review/Figures/YearlyEco.png")

# Create a categories dataframe
hsr_cats <- hsr_df %>% 
  count(Category)

ggplot(yearly_hs_cats) +
  geom_line(aes(x=Year,y=Total,color=Category),linewidth=0.75) +
  scale_color_manual(values=c("#0000CD","#228B22","#CD950C")) +
  theme_classic() +
  scale_y_continuous(expand = c(0, 0)) +  # removes white space between axis and data (ggplot default setting) 
  theme(axis.text = element_text(size=14),
        axis.title = element_text(size=15,face="bold"),
        legend.position = c(0.2, 0.8),
        legend.title = element_text(size=13,face="bold"),
        legend.title.align = 0.5,
        legend.text = element_text(size=13)) +
  labs(x="Year", y="Cumulative # of Publications")
ggsave(file ="~/Documents/UCSC/Dissertation/Hot Spots Review Paper/Hotspot Manuscript/Hotspot Review/Figures/YearlyCats.png",scale=0.5)

hs_journals <- hsr_df %>% 
  count(Journal) 

hs_locations <- hsr_df %>% 
  separate_rows(Location, sep =",") %>% 
  count(Location) 

ggplot(data=hs_locations) +
  geom_bar(aes(x=reorder(Location, n, sum),y=n),stat='identity') + # needed to add stat='identity' to get the columns right
  scale_y_continuous(expand = c(0, 0)) + # removes white space between axis and data (ggplot default setting)
  labs(x="Location",y="Total Publications") +
  coord_flip() +
  theme_classic()  # this needs to go before any axis text rotation modification%
ggsave(file ="~/Documents/UCSC/Dissertation/Hot Spots Review Paper/Hotspot Manuscript/Hotspot Review/Figures/Locations.png")


# Create a bar graph of total number of publications per year
ggplot(data=hs_year)+
  geom_point(aes(x=Year,y=n))+
  stat_smooth(aes(x=Year,y=n),se = FALSE)+
  labs(x="Year",y="Total Publications")+
  theme_classic()
ggsave(file ="~/Documents/UCSC/SeaGrant/Full Proposal/TotalPubs.png")

# Create dataframe for number of studies per type that were global for location
global_type <- hsr_df %>% 
  group_by(Type) %>% 
  count(Location="Global") 

# Create bar plot of global studies by type
ggplot(data=global_type) +
  geom_bar(aes(reorder(Type, n, sum),y=n), stat="identity") + # reorder the bars from highest to lowest
  scale_y_continuous(expand = c(0, 0)) +
  labs(x="Hotspot Type",y="# of Global Publications") +
  theme_classic() +
  coord_flip()
ggsave(file ="~/Documents/UCSC/Dissertation/Hot Spots Review Paper/Hotspot Manuscript/Hotspot Review/Figures/Globaltype.png")


ggplot(data=hsr_df) +
  geom_bar(aes(x=Year)) +
  scale_y_continuous(expand = c(0, 0)) + # removes white space between axis and data (ggplot default setting)
  labs(x="Year",y="Total Publications") +
  theme_classic() + # this needs to go before any axis text rotation modification%
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 


# Calculate positions for labels on pie chart
hs_journals <- hs_journals %>% 
  mutate(csum = rev(cumsum(rev(n))), 
         pos = n/2 + lead(csum, 1),
         pos = if_else(is.na(pos), n/2, pos))

# Create the pie plot for journals for the laughs
ggplot(data=hs_journals, aes(x="",y=n,fill=Journal)) +
  geom_bar(stat="identity", width=1, color= "white") +
  coord_polar("y", start=0) +
  geom_label_repel(data = hs_journals,
                   aes(y = pos, label = paste0(Journal, " ","(",n,"%",")")),
                   size = 4.5, nudge_x = 1, show.legend = FALSE) +
  theme_void() + # remove background, axes, and labels
  theme(legend.position="none") # remove legend
ggsave(file ="~/Documents/UCSC/Dissertation/Hot Spots Review Paper/Hotspot Manuscript/Hotspot Review/Figures/PieJournals.png")

# Create a bar plot of journals
ggplot(data=hsr_df) +
  geom_bar(aes(x=Journal)) +
  scale_y_continuous(expand = c(0, 0)) + # removes white space between axis and data (ggplot default setting)
  labs(x="Journal",y="Total Publications") +
  theme_classic() + # this needs to go before any axis text rotation modification%
  coord_flip()
ggsave(file ="~/Documents/UCSC/Dissertation/Hot Spots Review Paper/Hotspot Manuscript/Hotspot Review/Figures/TotalJournal.png",scale=2.5)
