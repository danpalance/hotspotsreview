# This code creates a sankey plot of hotspot covariates and indicators which are
# referred to as drivers from an earlier draft and kept here for simplicity
# written by Dan Palance 
# last modified on 26 Sep 2025

# Load required packages
library(tidyverse)
library(stringr)
library(ggsankey)

# Repurposed code from this tutorial
# https://rpubs.com/techanswers88/sankey-with-own-data-in-ggplot

# Read in the main dataframe created in hs_globaldist.R
main <- readRDS(file ="output/main_hs.RDS")

# Create a dataframe with rows for each driver
hs_drivers <- main %>% 
  filter(Year != "1988") %>% 
  distinct(Title, .keep_all = TRUE) %>% # remove duplicates due to studies occurring in multiple regions
  separate_rows(Condensed_var, sep =", ") %>% 
  filter(Condensed_var != "none") %>% 
  mutate(Drivercat = forcats::fct_collapse(Condensed_var,
                                           abiotic = c("dynamic physical", "static physical", "biogeochem"),
                                           biotic = c("ecological", "species attributes"),
                                           anthropogenic = c("human activity"))) %>%
  mutate_at(vars(Drivercat), as.character) %>%
  rename(Driver = Condensed_var,
         "Hotspot Category" = Category) %>% 
  select("Hotspot Category", Driver) %>% 
  mutate(Driver = str_to_title(Driver)) # capitalize to match the categories

drivers_san <- hs_drivers %>% 
  make_long(Driver, "Hotspot Category")

# get the percentage of each driver
drivers_perc <- drivers_san %>% 
  dplyr::group_by(node) %>% 
  tally() %>% 
  ungroup() %>%  
  filter(stringr::str_detect(node,"Dynamic Physical|Static Physical|Ecological|Biogeochem|Species Attributes|Human Activity")) %>% 
  filter(node != "Ecological Impact") %>% 
  dplyr::group_by(node) %>%
  dplyr::mutate(pct = round(((pct = n/457)*100),1)) %>% 
  ungroup()

# get the percentage of each category
cat_perc <- drivers_san %>% 
    dplyr::group_by(node) %>% 
    tally() %>% 
    ungroup() %>%  
    filter(stringr::str_detect(node,"Biophysical|Anthropogenic|Ecological Impact")) %>% 
    dplyr::group_by(node) %>%
    dplyr::mutate(pct = round(((pct = n/457)*100),1)) %>% 
    ungroup()

# cobmine the driver and category dfs into 1 for node labels
perc_df <- rbind(drivers_perc, cat_perc)

##### Left off here to make the plot for the type instead of category
# # get the percentage of each type
# type_perc <- drivers_san %>% 
#   dplyr::group_by(node) %>% 
#   tally() %>% 
#   ungroup() %>%  
# #  filter(stringr::str_detect(node,"Biophysical|Anthropogenic|Ecological Impact")) %>% 
#   dplyr::group_by(node) %>%
#   dplyr::mutate(pct = round(((pct = n/457)*100),1)) %>% 
#   ungroup()
# 
# # cobmine the driver and category dfs into 1 for node labels
# perc_df <- rbind(drivers_perc, cat_perc)

sk_df <- merge(drivers_san, perc_df, by.x = "node", by.y = "node", all.x = TRUE)


ggplot(sk_df, aes(x = x,
                     next_x = next_x,
                     node = node,
                     next_node = next_node,
                     fill = factor(node),
                     label = paste0(node,' (', pct , '%)'))) +
  geom_sankey(flow.alpha = 0.8,
            node.color = "black",
            show.legend = FALSE) +
  geom_sankey_label(size = 4, color = "black", 
                    fill= "white", hjust = 0,
                    fontface = "bold") +
  theme_classic() +
  theme(legend.position = "none") +
  theme(axis.title = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        panel.grid = element_blank(),
        axis.line = element_blank(),
        axis.text = element_text(face = "bold",
                                 size = 16,
                                 color = "black")) +
  scale_fill_manual(values = c("Biophysical" = "blue",
                               "Ecological Impact"  ="darkgreen",
                               "Anthropogenic"  = "goldenrod",
                               "Dynamic Physical" = "firebrick4",
                               "Static Physical" = "saddlebrown",
                               "Ecological" = "coral",
                               "Biogeochem" = "steelblue",
                               "Species Attributes" = "orchid",
                               "Human Activity" = "khaki")) +
  #labs(title = "Hotspot covariates by Category", hjust = 0.5) +
  labs(subtitle = ) +
  labs(caption = "ggsankey package\n David Sjobergs @techanswers88") +
  labs(fill = 'Nodes')+
  scale_x_discrete(position = "top") # Move axis labels to top
ggsave("figs/drivercat_sankey.png", height = 6, width = 18, units = "cm", scale = 3.5)
