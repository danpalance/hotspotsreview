# This code creates the starburst figure by realm for Taxa (figure X) in the hotspot manuscript
library(tidyverse)
library(cowplot)


# Read in the main dataframe created in earlier code
main <- readRDS(file ="output/main_hs.RDS")

# Create a dataframe with rows for each driver
hs_drivers <- main %>% 
  #distinct() %>% # remove duplicates due to studies occurring in multiple regions
  separate_rows(Drivers_examined_condensed, sep =", ") %>% 
  mutate_at(vars(Drivers_examined_condensed),list(factor)) %>%
  mutate(Drivers = forcats::fct_collapse(Drivers_examined_condensed,
                                         "bathy & topo" = c("bathymetry","topography"),
                                         "misc water properties" = c("DO","light","misc water properties"),
                                         "climate & hydrology" = c("climate","hydrology"))) %>% 
  filter(Drivers != "none") %>% 
  mutate(Drivercat = forcats::fct_collapse(Drivers,
                                           abiotic=c("bathy & topo","substrate","circulation",
                                                     "distance to physical features","misc water properties",
                                                     "fronts", "habitat","climate & hydrology",
                                                     "nutrients","pollution","salinity","shipping",
                                                     "temperature"),
                                           biotic = c("primary productivity","biodiversity","fishing",
                                                      "human activity","species attributes")))


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
  group_by(Drivers, Taxa2) %>% 
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
grid_data <- grid_data[-13,] # remove the scale bars for the area where the scale labels will go

# Assemble graph 
# Make the plot 
p1 <- ggplot(taxa_df, aes(x=Drivers, y=n, fill=Taxa2)) +       
  geom_bar(aes(x=as.factor(id), y=n, fill=Taxa2), stat="identity") + 
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
  geom_text(data=label_data, aes(x=id, y=n+10, label=Drivers, hjust=hjust), color="black", fontface="bold",alpha=0.6, size=3.5, angle=label_data$angle, inherit.aes = FALSE ) +
  
  # Add base line information
  geom_segment(data=base_data, aes(x = start, y = -5, xend = end, yend = -5), colour = "black", alpha=0.8, linewidth=0.6 , inherit.aes = FALSE )  #+
  geom_text(data=base_data, aes(x = title, y = -17, label=Taxa2, hjust=hjust), angle = base_data$angle,
            colour = "black", alpha=0.8, size=3.5, fontface="bold", inherit.aes = FALSE)

p2 <- ggplot(taxa_df) +       
  geom_col(aes(x=as.factor(id), y=Total, fill=Taxa2), col=NA,width=1.5) + 
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
ggsave(file ="Figs/taxabydrivers_burst_blank.png", scale = 2.5)
