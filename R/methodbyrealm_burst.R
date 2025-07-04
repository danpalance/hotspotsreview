# This code creates the hotspot method by realm starburst  plot in the hotspot manuscript
# Modified original code from https://r-graph-gallery.com/297-circular-barplot-with-groups.html
# Written by Dan Palance
# Last modified 09 June 2025

# read in required packages
library(tidyverse)
library(cowplot)

# Read in the data from hs_globaldist.R
main <- readRDS(file ="output/main_hs.RDS")

realm_df <- main %>% 
  filter(Year != "1988") %>% 
  separate_rows(Methods, sep=", ") %>% 
  mutate(Methclass = fct_collapse(Methods, 
                         "Field" = c("Survey", "Fishery", "Biologging", 
                                     "Acoustics", "Social Survey",
                                     "Paleontology", "Radar", "Experiment"),
                         "Non-field" = c("Satellite", "Model", "Lab",
                                         "Review", "Database"))) %>%
  group_by(Methods, Methclass) %>% 
  count(REALM) %>% 
  ungroup() %>% 
  group_by(REALM) %>% 
  mutate(Total=sum(n)) %>% 
  ungroup() %>% 
  mutate(REALM = as.factor(REALM))

realm_stats <- main %>% 
  filter(Year!="1988") %>% 
  group_by(REALM,Type) %>% 
  count() %>% 
  ungroup() %>% 
  mutate(Percent = (n/296)*100)


# Setup empty bar spacers between realms
empty_bar <- 1
to_add <- data.frame(matrix(NA,empty_bar*nlevels(realm_df$REALM),ncol(realm_df)))
colnames(to_add) <- colnames(realm_df)
to_add$REALM <- rep(levels(realm_df$REALM), each = empty_bar)
realm_df <- rbind(realm_df, to_add)
realm_df$Methods <- as.factor(realm_df$Methods)
realm_df <- as.data.frame(realm_df)
realm_df <- realm_df %>% arrange(REALM,-n) # Sort by Methods and then number of obs so the bar graphs are ordered from highest to smallest
realm_df$id <- seq(1, nrow(realm_df))

# Get the name and the y position of each label
label_data <- realm_df
number_of_bar <- nrow(label_data)
angle <- 90 - 360 * (label_data$id-0.5) / number_of_bar     # subtract 0.5 because so the letter matches the angle of the center bars
label_data$hjust <- ifelse(angle < -90, 1, 0)
label_data$angle <- ifelse(angle < -90, angle+180, angle)

# prepare a data frame for base lines
base_data <- realm_df %>% 
  group_by(REALM) %>% 
  summarize(start=min(id)-0.5, end=max(id)-0.5) %>% 
  rowwise() %>% 
  mutate(title=mean(c(start, end)))
number_of_bar.base <- nrow(base_data)
base_data$id <- seq(1, nrow(base_data))
angle <- 90 - 360 * (base_data$id-0.5) /number_of_bar.base  # subtract 0.5 because so the letter matches the angle of the center bars

# Create angle for the Methods labels in the circle using the median value from the grouped bar charts realm labels
base_data_angle <- label_data %>% 
  group_by(REALM) %>% 
  summarise(median = median(angle, na.rm = FALSE)) %>%  
  rename(angle=median)  
base_data <- merge(base_data,base_data_angle)
base_data$hjust <- ifelse(angle < 70, 0.5, 0) # if the angle is less than -70, make it 1, otherwise make it 0

#base_data$REALM2 <- c("Southern\nOcean")
#base_data$angle <- base_data$angle + 90

# prepare a data frame for the gray scale bars in between each category of bar plot
grid_data <- base_data
grid_data$end <- grid_data$start
grid_data$start <- grid_data$start - 1
#grid_data <- grid_data[-13,] # remove the scale bars for the area where the scale labels will go

# Assemble graph 
# Make the plot reorder(Type, n, sum)
p1 <- ggplot(realm_df, aes(x = Methods, y = n, fill = Methclass)) +       
  geom_bar(aes(x = as.factor(id), y = n, fill = Methclass), stat = "identity", col = "black") + 
  scale_fill_manual(values=c("Field"="beige", "Non-field"="darkslategrey")) +
  
  # Add a val=20/15/10/5 lines. I do it at the beginning to make sure barplots are OVER it.
  geom_segment(data=grid_data, aes(x = end, y = 20, xend = start, yend = 20), colour = "grey", alpha=1, linewidth=0.3, inherit.aes = FALSE ) +
  geom_segment(data=grid_data, aes(x = end, y = 15, xend = start, yend = 15), colour = "grey", alpha=1, linewidth=0.3, inherit.aes = FALSE ) +
  geom_segment(data=grid_data, aes(x = end, y = 10, xend = start, yend = 10), colour = "grey", alpha=1, linewidth=0.3, inherit.aes = FALSE ) +
  geom_segment(data=grid_data, aes(x = end, y = 5, xend = start, yend = 5), colour = "grey", alpha=1, linewidth=0.3, inherit.aes = FALSE ) +
  
  # Add text showing the value of each 100/75/50/25 lines
  #annotate("text", x = rep(max(realm_df$id)-1,5), y = c(0,5, 10, 15, 20), label = c("0","5", "10", "15", "20") , color="grey", size=3 , angle=0, fontface="bold", hjust=1) +
  ylim(-100,120) +
  theme_minimal() +
  theme(legend.position = "none",
        axis.text = element_blank(),
        axis.title = element_blank(),
        panel.grid = element_blank(),
        plot.margin = unit(rep(-1,4),"cm")) +
  coord_polar() + 
  geom_text(data=label_data, aes(x=id, y=n+10, label=Methods, hjust=hjust), color="black", fontface="bold",alpha=0.6, size=4, angle=label_data$angle, inherit.aes = FALSE ) +
  
  # Add base line information
  geom_segment(data=base_data, aes(x = start, y = -5, xend = end, yend = -5), colour = "black", alpha=0.8, linewidth=0.6 , inherit.aes = FALSE )  +
  geom_text(data=base_data, aes(x = title, y = -17, label=REALM, hjust=base_data$hjust), angle = base_data$angle, 
            colour = "black", alpha=0.8, size=3, fontface="bold", inherit.aes = FALSE) 

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

# Nest the circular bar charts with total Methods inside Methods by realm
ggdraw() +
  draw_plot(p2,scale=0.38) +
  draw_plot(p1,scale=1)
ggsave(file ="Figs/methodsbyrealm_burst.png",scale=2.5)
ggsave(file ="Figs/methodsbyrealm_burst.svg",scale=2.5)


