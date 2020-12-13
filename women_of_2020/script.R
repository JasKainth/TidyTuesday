#' ---
#' title: "Women of 2020"
#' author: "Jas Kainth"
#' date: "11/12/2020"
#' output: pdf_document
#' editor_options: 
#'   chunk_output_type: console
#' ---
#' 
## ----setup, include=FALSE----------------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)
library(tidyverse)
library(ggsci)
library(showtext)
library(patchwork)
library(ggdark)
tt <- tidytuesdayR::tt_load(2020, week = 50)
women <- tt$women %>% 
  mutate(country = case_when(country == "US" ~ "USA",
                             TRUE ~ country))

#' 
#' 
## ----------------------------------------------------------------------------------
# Which words are used to describe women from around the world?
map <- map_data("world")
## CREATIVITY
p1 <- women %>% 
  select(category, region = country) %>%
  filter(region != "Worldwide",
         category != "All") %>% 
  mutate(for_sum = 1) %>%
  group_by(region, category) %>% 
  summarise(total = sum(for_sum)) %>% 
  ungroup() %>%
  filter(category == "Identity") %>%
  full_join(., map, by = "region") %>%
  mutate(total = case_when(is.na(total) ~ 0,
                           TRUE ~ total)) %>%
  ggplot(aes(x = long, y = lat)) +
  geom_polygon(aes(group = group, fill = total), color = "white", size = 0.1) +
  theme_void() +
  labs(title = "Identity") + 
  theme(legend.position = "none",
        plot.title = element_text(family = "Avenir Next Condensed",
                                  hjust = .5, size = 30, face = "bold.italic",
                                  color = "forestgreen", vjust = -25)) +
  scale_fill_material(palette = "green") 


## IDENTITY
p2 <- women %>% 
  select(category, region = country) %>%
  filter(region != "Worldwide",
         category != "All") %>% 
  mutate(for_sum = 1) %>%
  group_by(region, category) %>% 
  summarise(total = sum(for_sum)) %>% 
  ungroup() %>%
  filter(category == "Creativity") %>%
  full_join(., map, by = "region") %>%
  mutate(total = case_when(is.na(total) ~ 0,
                           TRUE ~ total)) %>%
  ggplot(aes(x = long, y = lat)) +
  geom_polygon(aes(group = group, fill = total), color = "white", size = 0.1) +
  theme_void() +
  labs(title = "Creativity") + 
  theme(legend.position = "none",
        plot.title = element_text(family = "Avenir Next Condensed",
                                  hjust = .5, size = 30, face = "bold.italic",
                                  color = "darkcyan", vjust = -25)) +
  scale_fill_material(palette = "cyan") 

## KNOWLEDGE
p3 <- women %>% 
  select(category, region = country) %>%
  filter(region != "Worldwide",
         category != "All") %>% 
  mutate(for_sum = 1) %>%
  group_by(region, category) %>% 
  summarise(total = sum(for_sum)) %>% 
  ungroup() %>%
  filter(category == "Knowledge") %>%
  full_join(., map, by = "region") %>%
  mutate(total = case_when(is.na(total) ~ 0,
                           TRUE ~ total)) %>%
  ggplot(aes(x = long, y = lat)) +
  geom_polygon(aes(group = group, fill = total), color = "white", size = 0.1) +
  theme_void() +
  labs(title = "Knowledge") + 
  theme(legend.position = "none",
        plot.title = element_text(family = "Avenir Next Condensed",
                                  hjust = .5, size = 30, face = "bold.italic",
                                  color = "darkorchid4", vjust = -25)) +
  scale_fill_material(palette = "deep-purple") 

## LEADERSHIP
p4 <- women %>% 
  select(category, region = country) %>%
  filter(region != "Worldwide",
         category != "All") %>% 
  mutate(for_sum = 1) %>%
  group_by(region, category) %>% 
  summarise(total = sum(for_sum)) %>% 
  ungroup() %>%
  filter(category == "Leadership") %>%
  full_join(., map, by = "region") %>%
  mutate(total = case_when(is.na(total) ~ 0,
                           TRUE ~ total)) %>%
  ggplot(aes(x = long, y = lat)) +
  geom_polygon(aes(group = group, fill = total), color = "white", size = 0.1) +
  theme_void() +
  labs(title = "Leadership") + 
  theme(legend.position = "none",
        plot.title = element_text(family = "Avenir Next Condensed",
                                  hjust = .5, size = 30, face = "bold.italic",
                                  color = "darkorange3", vjust = -25)) +
  scale_fill_material(palette = "deep-orange") 

layout <- "
AB
CD
"
pw <- p1 + p2 + p3 + p4  + plot_layout(design = layout)
pw + 
  plot_annotation(title = "What categories do women from different countries excel in?",
                  theme = theme(plot.title = 
                                  element_text(size = 25, hjust = 0.5, vjust = -1,
                                               face = "bold.italic",
                                               family = "Avenir Next Condensed"))) 

#' 
