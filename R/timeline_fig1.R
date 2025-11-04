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
  geom_area(aes(x=Year,y=Csum,fill=Category),position=position_identity(), alpha = 0.6, show.legend = FALSE) + # remove legend argument to show it
  scale_fill_manual(values=c("Anthropogenic"="#CD950C","Biophysical"="#0000CD","Ecological Impact"="#228B22")) +
  # Plot horizontal black line for timeline
  geom_hline(yintercept=0,color="black", linewidth = 0.5) +
  # geom_segment(data = type_timeline_df,
  #              aes(x = Year, y = Text_position, yend = 0, xend = Year, color = Category), 
  #              linewidth=0.5) + # to show where two type overlap in time
  scale_color_manual(values=c("Anthropogenic"="#CD950C","Biophysical"="#0000CD","Ecological Impact"="#228B22")) +
  # geom_label(data = type_timeline_df,
  #           aes(x = Year, y = Text_position + 5, label = Paper, fontface = "bold", fill = Category), 
  #           alpha = 0.5, color = "white", size = 4) +
  geom_text(data = year_date_range,
            aes(x = Year, y = -5, label = Year, fontface = "bold"),
            size = 5, color='black') +
  scale_x_break(c(1991, 2000)) +
  theme_classic() +
  annotate("point", x = 2000, y = 175, pch = 15, size = 5, color = "#0000CD") +
  annotate("point", x = 2000, y = 167, pch = 15, size = 5, color = "#228B22") +
  annotate("point", x = 2000, y = 159, pch = 15, size = 5, color = "#CD950C") +
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

# Show rpaid expansion of types for each category
ggplot(data=csum_final %>% filter(Category == "Biophysical")) +
  xlim(1988,2023) + # adjust the west end of the x axis so it isn't cutting off text labels
  labs(x="Year", y="Cumulative # of Studies") +
  geom_area(aes(x = Year,y = Csum), fill = "#0000CD", position = position_identity(), alpha = 0.6, show.legend = FALSE) + # remove legend argument to show it
  # Plot horizontal black line for timeline
  geom_hline(yintercept=0,color="black", linewidth = 0.5) +
  geom_text(data = year_date_range, 
            aes(x = Year, y = -5, label = Year, fontface = "bold"),
            size = 5, color='black') +
  theme_classic() +
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
        axis.line.x = element_blank()) +
  geom_line(data = tsum_type %>% filter(Category == "Biophysical"), aes(x = Year, y = Csum, fill = Type), color = "white") 
ggsave(filename="./figs/biophys_timeline.png", units = "in", 
       width = 12, height = 4, scale = 1.4)

# Show rapid expansion of types for each category
ggplot(data=csum_final %>% filter(Category == "Ecological Impact")) +
  xlim(1988,2023) + # adjust the west end of the x axis so it isn't cutting off text labels
  labs(x="Year", y="Cumulative # of Studies") +
  geom_area(aes(x = Year,y = Csum), fill = "#228B22", position = position_identity(), alpha = 0.6, show.legend = FALSE) + # remove legend argument to show it
  # Plot horizontal black line for timeline
  geom_hline(yintercept=0,color="black", linewidth = 0.5) +
  geom_text(data = year_date_range, 
            aes(x = Year, y = -5, label = Year, fontface = "bold"),
            size = 5, color='black') +
  theme_classic() +
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
        axis.line.x = element_blank()) +
  geom_line(data = tsum_type %>% filter(Category == "Ecological Impact"), aes(x = Year, y = Csum, fill = Type), color = "white") 
ggsave(filename="./figs/ecoimp_timeline.png", units = "in", 
       width = 12, height = 4, scale = 1.4)

# Show rpaid expansion of types for each category
ggplot(data=csum_final) +
  xlim(1988,2023) + # adjust the west end of the x axis so it isn't cutting off text labels
  labs(x="Year", y="Cumulative # of Studies") +
  # Plot horizontal black line for timeline
  geom_hline(yintercept=0,color="black", linewidth = 0.5) +
  geom_text(data = year_date_range, 
            aes(x = Year, y = -5, label = Year, fontface = "bold"),
            size = 5, color='black') +
  theme_classic() +
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
        axis.line.x = element_blank()) +
  geom_line(data = tsum_type, aes(x = Year, y = Csum, color = Category, fill = Type)) +
  scale_color_manual(values=c("Anthropogenic"="#CD950C","Biophysical"="#0000CD","Ecological Impact"="#228B22")) 
ggsave(filename="./figs/type_timeline.png", units = "in", 
       width = 12, height = 4, scale = 1.4)
 ##### Additional data visualizations not included in paper ####
# Make the dataframe summarizing type
type_total <- main %>% 
  filter(Year!="1988") %>% 
  distinct(Title, .keep_all = TRUE) %>% # remove duplicates from multirealm studies
  separate_rows(Type, sep=", ") %>% 
  count(Type)

type_total$Cat <-  forcats::fct_collapse(type_total$Type, 
                                         Biophysical=c("Foraging","Habitat","Nutrients & Biogeochemical-Cycling",
                                                       "Abundance/Density","Biodiversity & Endemism","Reproduction & Recruitment", "Mortality"),
                                         Anthropogenic = c("Pollution"),
                                         Ecoimpact = c("Invasive Species", "Water Chemistry","Fisheries","Threat",
                                                       "Bioaccumulation","Warming"))


# Create dataframe for methods timeline #####
method_timeline_df <- main %>% 
  filter(Year!="1988") %>% 
  separate_rows(Methods, sep=", ") %>% 
  arrange(Year) %>% 
  select(Year,Authors,Category,Methods,REALM) %>%
  group_by(Methods) %>% 
  slice(1) %>% 
  ungroup() %>% 
  mutate("Paper" = paste0(Methods," (",Authors, ", ",Year,")")) %>% 
  arrange(Year) # ensures the alternating labels above and below axis work later
  

method.positions <- c(-0.5, 0.5, -1.0, 1.0, -1.5, 1.5, -0.5, 0.5, -1.0, 1.0)
method.directions <- c(1, -1)

method_line_pos <- data.frame(
  "Year"=unique(method_timeline_df$Year),
  "position"=rep(method.positions, length.out=length(unique(method_timeline_df$Year))),
  "direction"=rep(method.directions, length.out=length(unique(method_timeline_df$Year)))
)


method_timeline_df <- merge(x=method_timeline_df, y=method_line_pos, by="Year", all = TRUE)
method_timeline_df$Methods <- as.factor(method_timeline_df$Methods) # make methods a factor so ggplot works correctly
text_offset <- .08 # controls vertical stacking space for text when multiples occur at same time

method_timeline_df$Year_count <- ave(method_timeline_df$Year==method_timeline_df$Year, method_timeline_df$Year, FUN=cumsum) # assigns increasing numeric value for each duplicate year
method_timeline_df$Text_position <- ((method_timeline_df$Year_count * text_offset * method_timeline_df$direction) + method_timeline_df$position)

year_date_range <- as.data.frame(seq(1990,2025,by=5))
colnames(year_date_range)[1] ="Year"

# Make the dataframe summarizing methods
methods_total <- main %>% 
  filter(Year!="1988") %>% 
  distinct(Title, .keep_all = TRUE) %>% # remove duplicates from multirealm studies
  separate_rows(Methods, sep=", ") %>% 
  count(Methods,Category)

# Make the methods timeline plot
methods_timeplot <- ggplot(method_timeline_df,aes(x=Year,y=0,col=Category)) +
  xlim(1988,2023) + # adjust the west end of the x axis so it isn't cutting off text labels
  scale_color_manual(values=c("Anthropogenic"="#CD950C","Biophysical"="#0000CD","Ecological Impact"="#228B22")) +
  labs(col="Category") +
  theme_classic() +
  # Plot horizontal black line for timeline
  geom_hline(yintercept=0,color="black", size=0.3) +
  geom_segment(data=method_timeline_df[method_timeline_df$Year_count == 1,], aes(y=Text_position,yend=0,xend=Year), color='black', size=0.2) + # to show where two type overlap in time
  geom_point(aes(y=0), size=3) +
  theme(axis.line.y=element_blank(),
        axis.text.y=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        axis.ticks.y=element_blank(),
        axis.text.x =element_blank(),
        axis.ticks.x =element_blank(),
        axis.line.x =element_blank(),
        legend.position = "none") +
  geom_text(data=year_date_range, aes(x=Year,y=-0.05,label=Year,fontface="bold"),size=5, color='black') +
  geom_text(aes(y=Text_position,label=Paper,fontface="bold"),size=4) 

methods_totplot <- ggplot(data=methods_total) +
  geom_bar(aes(x=reorder(Methods,-n,sum),y=n,fill=Category),stat="identity") +
  scale_fill_manual(values=c("Anthropogenic"="#CD950C","Biophysical"="#0000CD","Ecological Impact"="#228B22")) +
  labs(x="Methods",y="# of Studies") +
  theme_classic() +
  scale_y_continuous(expand=c(0,0)) +
  theme(panel.background = element_rect(fill='transparent'), #transparent panel bg
        plot.background = element_rect(fill='transparent', color=NA),
        legend.position = "none",
        axis.text.x = element_text(vjust = 0.5, 
                                   hjust=1,
                                   face = "bold",
                                   size = 12),
        axis.text.y = element_text(face="bold",
                                   size = 12),
        axis.title.x = element_text(size=16, 
                                    face = "bold"),
        axis.title.y = element_text(size=16, 
                                    face = "bold")) +
  coord_flip() 


# Now create the taxa timeline #####
taxa_timeline_df <- main %>% 
  filter(Year!="1988") %>% 
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
  arrange(Year) %>% 
  select(Year,Authors,Category,Taxa2,REALM) %>%
  group_by(Taxa2) %>% 
  slice(1) %>% 
  ungroup() %>% 
  mutate("Paper" = paste0(Taxa2," (",Authors, " ",Year,")")) %>% 
  arrange(Year) # ensures the alternating labels above and below axis work later


taxa.positions <- c(-0.5, 0.5, -1.0, 1.0, -1.5, 1.5, -0.5, 0.5, -1.0, 1.0)
taxa.directions <- c(1, -1)

taxa_line_pos <- data.frame(
  "Year"=unique(taxa_timeline_df$Year),
  "position"=rep(taxa.positions, length.out=length(unique(taxa_timeline_df$Year))),
  "direction"=rep(taxa.directions, length.out=length(unique(taxa_timeline_df$Year)))
)


taxa_timeline_df <- merge(x=taxa_timeline_df, y=taxa_line_pos, by="Year", all = TRUE)
taxa_timeline_df$Taxa2 <- as.factor(taxa_timeline_df$Taxa2) # make methods a factor so ggplot works correctly
text_offset <- .08 # controls vertical stacking space for text when multiples occur at same time

taxa_timeline_df$Year_count <- ave(taxa_timeline_df$Year==taxa_timeline_df$Year, taxa_timeline_df$Year, FUN=cumsum) # assigns increasing numeric value for each duplicate year
taxa_timeline_df$Text_position <- ((taxa_timeline_df$Year_count * text_offset * taxa_timeline_df$direction) + taxa_timeline_df$position)

year_date_range <- as.data.frame(seq(1990,2025,by=5))
colnames(year_date_range)[1] ="Year"

# Make the dataframe summarizing taxa
taxa_total <- main %>% 
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
  count(Taxa2,Category)

# Make the methods timeline plot
taxa_timeplot <- ggplot(taxa_timeline_df,aes(x=Year,y=0,col=Category)) +
  xlim(1988,2023) + # adjust the west end of the x axis so it isn't cutting off text labels
  scale_color_manual(values=c("Anthropogenic"="#CD950C","Biophysical"="#0000CD","Ecological Impact"="#228B22")) +
  labs(col="Category") +
  theme_classic() +
  # Plot horizontal black line for timeline
  geom_hline(yintercept=0,color="black", linewidth=0.3) +
  geom_segment(data=taxa_timeline_df[taxa_timeline_df$Year_count == 1,], aes(y=Text_position,yend=0,xend=Year), color='black', size=0.2) + # to show where two type overlap in time
  geom_point(aes(y=0), size=3) +
  theme(#legend.position = "none",
        legend.title = element_text(size=14, face = "bold"),
        legend.text = element_text(size=12),
        legend.position = c(0.9,0.8),
        axis.line.y=element_blank(),
        axis.text.y=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        axis.ticks.y=element_blank(),
        axis.text.x =element_blank(),
        axis.ticks.x =element_blank(),
        axis.line.x =element_blank()) +
  geom_text(data=year_date_range, aes(x=Year,y=-0.05,label=Year,fontface="bold"),size=5, color='black') +
  geom_text(aes(y=Text_position,label=Paper,fontface="bold"),size=4) 


taxa_totplot <- ggplot(data=taxa_total) +
  geom_bar(aes(x=reorder(Taxa2,-n,sum),y=n,fill=Category),stat="identity") +
  scale_fill_manual(values=c("Anthropogenic"="#CD950C","Biophysical"="#0000CD","Ecological Impact"="#228B22")) +
  labs(x="Taxa",y="# of Studies") +
  theme_classic() +
  scale_y_continuous(expand=c(0,0)) +
  theme(panel.background = element_rect(fill='transparent'), #transparent panel bg
        plot.background = element_rect(fill='transparent', color=NA),
        legend.position = "none",
        axis.text.x = element_text(vjust = 0.5, 
                                   hjust=1,
                                   face = "bold",
                                   size = 12),
        axis.text.y = element_text(face="bold",
                                   size = 12),
        axis.title.x = element_text(size=16, 
                                    face = "bold"),
        axis.title.y = element_text(size=16, 
                                    face = "bold")) +
  coord_flip() 

# Create timeline showing cumulative sum of studies 
typesum_plot <- ggplot(data=tsum_type) +
  xlim(1989,2023) + # adjust the west end of the x axis so it isn't cutting off text labels
  labs(x="Year", y="Cumulative # of Studies") +
  geom_area(aes(x = Year, y = Csum, fill = Category), position = position_identity()) +
  scale_fill_manual(values=c("Anthropogenic" = "#CD950C", 
                             "Biophysical" = "#0000CD",
                             "Ecological Impact"="#228B22")) +
  scale_y_continuous(expand = c(0,0)) +
  theme_classic() +
  theme(legend.position = "inside",
        legend.position.inside = c(0.9,0.9),
        legend.title = element_text(size=14),
        legend.text = element_text(size=12),
        legend.background = element_rect(fill = "transparent",
                                         colour = "transparent"),
        axis.text.x = element_text(face = "bold",
                                   size = 17,
                                   color="black"),
        axis.text.y = element_text(face="bold",
                                   size = 17,
                                   color="black"),
        axis.title.x = element_text(face="bold",
                                    size = 19,
                                    color="black"),
        axis.title.y = element_text(face="bold",
                                    size = 19,
                                    color="black")) +
  facet_wrap(~Type, ncol = 4)
ggsave(plot=typesum_plot, filename="./figs/type_timeseries.png",units="in",width=12,height=12)
