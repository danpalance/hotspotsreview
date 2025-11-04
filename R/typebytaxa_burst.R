# This code creates the hotspot covariates and indicators by taxa starburst  plot in the hotspot manuscript
# Covariates and indicators were originally referred to as drivers, and I have kept it his way in the code for simplicity
# Modified original code from https://r-graph-gallery.com/297-circular-barplot-with-groups.html
# Written by Dan Palance
# Last modified 03 Nov 2025
# read in required packages
library(tidyverse)
library(cowplot)

# Read in the data from hs_globaldist.R
main <- readRDS(file ="output/main_hs.RDS")

# Make dataframe seprating taxa and hotspot type into rows and condensing them into broader categories
taxa_df <- main %>% 
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
  group_by(Category, Type, Taxa2) %>% 
  count(Taxa2) %>% 
  ungroup() %>% 
  group_by(Taxa2) %>% 
  mutate(Total=sum(n)) %>% 
  ungroup() %>% 
  mutate(Taxa2 = as.factor(Taxa2)) 


# The code below for the circle bar plot is adapted from 
# https://r-graph-gallery.com/297-circular-barplot-with-groups.html
# Set a number of 'empty bar' to add at the end of each group
empty_bar <- 2
to_add <- data.frame( matrix(NA, empty_bar*nlevels(taxa_df$Taxa2), ncol(taxa_df)) )
colnames(to_add) <- colnames(taxa_df)
to_add$Taxa2 <- rep(levels(taxa_df$Taxa2), each=empty_bar)
taxa_df <- rbind(taxa_df, to_add)
taxa_df <- taxa_df %>% arrange(Taxa2, -n) # order alphabetically by taxa and then highest to lowest n for each taxa
taxa_df$id <- seq(1, nrow(taxa_df))

# Get the name and the y position of each label
label_data <- taxa_df
number_of_bar <- nrow(label_data)
angle <- 90 - 360 * (label_data$id-0.5) /number_of_bar     # I substract 0.5 because the letter must have the angle of the center of the bars. Not extreme right(1) or extreme left (0)
label_data$hjust <- ifelse( angle < -90, 1, 0)
label_data$angle <- ifelse(angle < -90, angle+180, angle)

# prepare a data frame for base lines
base_data <- taxa_df %>% 
  group_by(Taxa2) %>% 
  summarize(start=min(id), end=max(id) - empty_bar) %>% 
  rowwise() %>% 
  mutate(title=mean(c(start, end)))

# prepare a data frame for grid (scales)
grid_data <- base_data
grid_data$end <- grid_data$end[ c( nrow(grid_data), 1:nrow(grid_data)-1)] + 1
grid_data$start <- grid_data$start - 1
grid_data <- grid_data[-1,]

# Make the plot
p1 <- ggplot(taxa_df, 
             aes(x = as.factor(id), y = n)) +       # Note that id is a factor. If x is numeric, there is some space between the first bar
  geom_bar(aes(x = as.factor(id), y = n, fill = Category), col = "black", stat="identity") +
  scale_fill_manual(values=c("NA" = NA, "Biophysical"="#0000CD","Ecological Impact"="#228B22")) +
  geom_segment(data=grid_data, aes(x = end, y = 80, xend = start, yend = 80), 
               colour = "grey", alpha = 1, linewidth = 0.3 , inherit.aes = FALSE) +
  geom_segment(data=grid_data, aes(x = end, y = 60, xend = start, yend = 60), 
               colour = "grey", alpha = 1, linewidth = 0.3 , inherit.aes = FALSE) +
  geom_segment(data=grid_data, aes(x = end, y = 40, xend = start, yend = 40), 
               colour = "grey", alpha = 1, linewidth = 0.3 , inherit.aes = FALSE) +
  geom_segment(data=grid_data, aes(x = end, y = 20, xend = start, yend = 20), 
               colour = "grey", alpha = 1, linewidth = 0.3 , inherit.aes = FALSE) +
  # Add text showing the value of each 100/75/50/25 lines, subtracting 0.25 to align the labels in the middle of white space
  annotate("text", x = rep(max(taxa_df$id),4) - 0.25, y = c(20, 40, 60, 80), label = c("20", "40", "60", "80") , color="grey", size=3 , angle=0, fontface="bold", hjust=1) +
  geom_bar(aes(x = as.factor(id), y = n, fill = Taxa2), stat = "identity", alpha = 0.5) +
  ylim(-100,120) +
  theme_minimal() +
  theme(legend.position = "none",
        axis.text = element_blank(),
        axis.title = element_blank(),
        panel.grid = element_blank(),
        plot.margin = unit(rep(-1,4), "cm")) +
  coord_polar() + 
  geom_text(data = label_data, aes(x = id, y = n + 10, label = stringr::str_to_title(Type), hjust = hjust), 
            color = "black", fontface = "bold", alpha = 0.6, size = 4, 
            angle = label_data$angle, inherit.aes = FALSE ) +
  # Add base line information
  geom_segment(data=base_data, aes(x = start-0.5, y = -5, xend = end + 0.5, yend = -5), 
               colour = "black", alpha = 0.8, size=0.6 , inherit.aes = FALSE )  +
  geom_text(data=base_data, aes(x = title, y = -18, label = Taxa2), hjust = 1, 
            colour = "black", alpha = 0.8, size = 4, fontface="bold", inherit.aes = FALSE)


# Inner bar plot
p2 <- ggplot(taxa_df, aes(x = as.factor(id), y = n, fill = Taxa2)) +       
  geom_col(aes(x=as.factor(id), y = Total, fill = Taxa2), col = NA, width = 1.1) + 
  scale_fill_brewer(palette = "Set3") +
  coord_polar() +
  theme_minimal() +
  theme(legend.position = "none",
        axis.text = element_blank(),
        axis.title = element_blank(),
        panel.grid = element_blank(),
        plot.margin = unit(rep(-1,4),"cm")) 

# Nest the circular bar charts 
ggdraw() +
  draw_plot(p2,scale=0.38) +
  draw_plot(p1,scale=1)
ggsave(file ="Figs/driversbytaxa_burst.svg", scale = 2.5)
#ggsave(file ="Figs/driversbytaxa_burst.png", scale = 2.5)

legend_plot <- ggplot(taxa_df, 
                      aes(x = as.factor(id), y = n)) +       # Note that id is a factor. If x is numeric, there is some space between the first bar
  geom_bar(aes(x = as.factor(id), y = n, fill = Category), col = "black", stat="identity") +
  scale_fill_manual(name = "Hotspot Category", 
                    values=c("NA" = NA, "Biophysical"="#0000CD","Ecological Impact"="#228B22")) +
  theme(legend.title = element_text(size = 14,
                                    face = "bold"),
        legend.text = element_text(size = 12))
ggsave(file ="Figs/driversbytaxa_legend.svg", scale = 2.5)


