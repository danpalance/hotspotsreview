# This code creates the timeline of hotspot types for the hotspot manuscript
# Create timeline dataframes by adapting code from here: 
# https://benalexkeen.com/creating-a-timeline-graphic-using-r-and-ggplot2/ 
# Written by Dan Palance
# Last modified on 09 June 2025

# Load required packages
library(tidyverse)
library(ggbreak)

# read in data created in hs_globaldist.R
main <- readRDS("output/main_hs.RDS")

# Create dataframe for cumulative number of studies per year by type
tsum_type <- main %>% 
  filter(Year!="1988") %>% 
  distinct(Title, .keep_all = TRUE) %>% # remove duplicates from multirealm studies
  group_by(Year, Category) %>% 
  count(Year,Type) %>% 
  ungroup() %>% 
  group_by(Type) %>% 
  mutate(Csum = cumsum(n))

tsum_type <- main %>%
  filter(Year != "1988") %>%
  distinct(Title, .keep_all = TRUE) %>%  # remove duplicates from multirealm studies
  group_by(Year, Category, Type) %>%
  summarise(n = n(), .groups = "drop") %>%
  
  # Ensure every combination of Year and Type exists, fill missing with 0
  complete(Year = full_seq(as.integer(Year), 1), 
           Type, 
           fill = list(n = 0)) %>%
  
  arrange(Type, Year) %>%
  group_by(Type) %>%
  mutate(Csum = cumsum(n)) %>%
  ungroup() %>% 
  mutate(Category = if_else(is.na(Category) & 
      Type == "Biodiversity & Endemism | Foraging | Nutrients & Biogeochemical Cycling | Reproduction & Recruitment | Abundance/Density | Habitat", 
                            "Biophysical", 
                            Category))
        
# Create dataframe for cumulative number of studies per year by category
csum_type <- main %>% 
  filter(Year!="1988") %>% 
  distinct(Title, .keep_all = TRUE) %>% # remove duplicates from multirealm studies
  group_by(Year) %>% 
  count(Year,Category) %>% 
  ungroup() %>% 
  group_by(Category) %>% 
  mutate(Csum = cumsum(n))

# Add dummy row to fix anthro data at end given lack of studies
add_data <- data.frame(Year = as.integer(2023), Category = as.factor("Anthropogenic"), 
                       n = as.integer(0), Csum = as.integer(8))
csum_final <- bind_rows(csum_type, add_data)

# Create a dataframe containing the first study for each  hotspot type
type_timeline_df <- main %>% 
  filter(Year!="1988") %>% 
  separate_rows(Type, sep=", ") %>% 
  arrange(Year) %>% 
  select(Year,Authors,Category,Type,REALM) %>%
  group_by(Type) %>% 
  slice(1) %>% 
  ungroup() %>% 
  mutate("Paper" = paste0(Type," (", Authors, ", ", Year, ")")) %>% 
  arrange(Year) %>% # ensures the alternating labels above and below axis work later
  mutate(Position = c(5, 15, 30, 45, 60, 75, 90, 105, 120, 135, 150, 165, 180, 195),
         Text_position = c(5, 15, 30, 45, 60, 75, 90, 105, 120, 135, 150, 165, 180, 195)) %>% 
  group_by(Category) %>% 
  slice_head() # keep only the first paper from each category

# Create year labels for false x-axis
year_date_range <- as.data.frame(seq(1990,2025,by=5))
colnames(year_date_range)[1] ="Year"

# Create timeline showing cumulative sum of studies with first study of each type labeled
# catsum_plot <- ggplot(data=csum_final) +
ggplot(data=csum_final) +
  xlim(1988,2023) + # adjust the west end of the x axis so it isn't cutting off text labels
  labs(x="Year", y="Cumulative # of Studies") +
  geom_area(aes(x=Year,y=Csum,fill=Category),position=position_identity(), show.legend = FALSE) + # remove legend argument to show it
  scale_fill_manual(values=c("Anthropogenic" = "#F2C100",
                             "Biophysical"="#0072B2",
                             "Ecological Impact"="#009E73")) +
  # Plot horizontal black line for timeline
  geom_hline(yintercept=0,color="black", linewidth = 0.5) +
  scale_color_manual(values=c("Anthropogenic" = "#F2C100",
                              "Biophysical"="#0072B2",
                              "Ecological Impact"="#009E73")) +
  geom_text(data = year_date_range,
            aes(x = Year, y = -5, label = Year, fontface = "bold"),
            size = 5, color='black') +
  scale_x_break(c(1991, 2000)) +
  theme_classic() +
  annotate("point", x = 2000, y = 175, pch = 15, size = 5, color = "#0072B2") +
  annotate("point", x = 2000, y = 167, pch = 15, size = 5, color = "#009E73") +
  annotate("point", x = 2000, y = 159, pch = 15, size = 5, color = "#F2C100") +
  annotate("text", x = 2000.3, y = 175, label = "Biophysical", size = 5, fontface = "bold", hjust = 0) +
  annotate("text", x = 2000.3, y = 167, label = "Ecological Impact", size = 5, fontface = "bold", hjust = 0) +
  annotate("text", x = 2000.3, y = 159, label = "Anthropogenic", size = 5, fontface = "bold", hjust = 0) +
  theme(legend.position = "none",
        axis.text.y = element_text(face="bold",
                                   size = 14,
                                   color="black"),
        axis.title.y = element_text(face="bold",
                                    size = 16,
                                    color="black"),
        axis.title.x = element_text(face="bold",
                                    size = 16,
                                    color="black"),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.line.x = element_blank()) 
ggsave(filename="./figs/catsum_time.png", units = "in", 
       width = 12, height = 4, scale = 1.4)

