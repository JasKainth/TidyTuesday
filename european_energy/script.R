#' ---
#' title: "European Energy"
#' author: "Jas Kainth"
#' date: "15/09/2020"
#' output: pdf_document
#' editor_options: 
#'   chunk_output_type: console
#' ---
#' 
## ----setup, include=FALSE-----------------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)
library(tidyverse)
library(ggdark)
library(ggsci)
library(ggflags)
library(gganimate)
library(maps)
library(ggthemes)
tuesdata <- tidytuesdayR::tt_load('2020-08-04')
types <- tuesdata$energy_types %>% 
  mutate("Renewable" = ifelse(type == "Conventional thermal", "No", "Yes")) %>% 
  pivot_longer(cols = `2016`:`2018`, names_to = "Year") %>% 
  filter(level == "Level 1") %>%
  mutate(country_name = ifelse(is.na(country_name), 
                               "United Kingdom", country_name))%>% 
  # Change UK to GB for ggflags
  mutate(country = ifelse(country == "UK", "GB", country)) %>% 
  # Change for Greece too
  mutate(country = ifelse(country == "EL", "GR", country))




totals <- tuesdata$country_totals %>% 
  pivot_longer(cols = `2016`:`2018`, names_to = "Year") %>% 
  mutate(country_name = ifelse(is.na(country_name), 
                               "United Kingdom", country_name)) %>% 
  # Change UK to GB for ggflags
  mutate(country = ifelse(country == "UK", "GB", country)) %>% 
  # Change for Greece too
  mutate(country = ifelse(country == "EL", "GR", country))



#' 
## -----------------------------------------------------------------------------------
# Which Countries produce the most energy (from 2016 to 2018)
test <- types %>% 
  select(country) %>% 
  distinct() %>% 
  pull()
temp <- types %>%
  select(country, country_name)
types %>% 
  group_by(country_name, Renewable) %>% 
  summarise(total = sum(value)) %>% 
  ungroup() %>% 
  left_join(., temp, by = "country_name") %>% 
  mutate(Renewable = ifelse(Renewable == "No", "Not Renewable", "Renewable"),
         country_name = fct_reorder(country_name, total)) %>% 
  ggplot() +
  geom_col(aes(x = total, y = country_name, fill = Renewable)) +
  dark_theme_minimal() +
  theme(
    strip.text = element_text(face = 'bold', hjust = 0),
    plot.subtitle = element_text(face = 'italic'),
    axis.ticks = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_blank(),
    axis.line = element_line('white', size = 0.5)
  ) + 
  labs(title = "Which Countries in Europe Produce the Most Energy?",
       subtitle = "From 2016 to 2018", 
       x = "Total Energy (GWh)",
       y = " ",
       fill = "Energy Type") +
  scale_fill_simpsons() +
  guides(fill = guide_legend(reverse = TRUE)) +
  scale_x_continuous(labels = scales::comma, 
                     breaks = c(10000000, 20000000, 30000000, 40000000),
                     limits = c(0, 40000000))




#' 
## -----------------------------------------------------------------------------------
# Let's look at the top 9 and bottom 9 countries
countries <- types %>% 
  group_by(country_name) %>% 
  summarise(total = sum(value)) %>% 
  ungroup() %>% 
  arrange(-total) %>% 
  filter(!is.na(country_name)) %>% 
  head(9) %>% 
  pull(country_name)

types %>% 
  filter(country_name %in% countries) %>% 
  mutate(Renewable = ifelse(Renewable == "No", "Not Renewable", "Renewable"),
         country_name = fct_reorder(country_name, value)) %>% 
  ggplot() + 
  geom_col(aes(x = Renewable, y = value, fill = Year), position = 'dodge') + 
  facet_wrap(~country_name) + 
  scale_y_continuous(labels = scales::comma) + 
  dark_theme_minimal() +
  theme(
    strip.text = element_text(face = 'bold', hjust = 0),
    plot.subtitle = element_text(face = 'italic'),
    axis.ticks = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_blank(),
    axis.line = element_line('white', size = 0.5),
    panel.ontop = TRUE
  ) +
  scale_fill_uchicago() + 
  labs(title = "Countries with the Most Energy Production in Europe",
       y = "Total Energy (GWh)",
       x = "Energy Type")

## How about the countries which use the least amount of energy?
countries <- types %>% 
  group_by(country_name) %>% 
  summarise(total = sum(value)) %>% 
  ungroup() %>% 
  arrange(total) %>% 
  filter(!is.na(country_name)) %>% 
  head(9) %>% 
  pull(country_name)

types %>% 
  filter(country_name %in% countries) %>% 
  mutate(Renewable = ifelse(Renewable == "No", "Not Renewable", "Renewable"),
         country_name = fct_reorder(country_name, value)) %>% 
  ggplot() + 
  geom_col(aes(x = Renewable, y = value, fill = Year), position = 'dodge') + 
  facet_wrap(~country_name) + 
  scale_y_continuous(labels = scales::comma) + 
  dark_theme_minimal() +
  theme(
    strip.text = element_text(face = 'bold', hjust = 0),
    plot.subtitle = element_text(face = 'italic'),
    axis.ticks = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_blank(),
    axis.line = element_line('white', size = 0.5),
    panel.ontop = TRUE
  ) +
  scale_fill_uchicago() + 
  labs(title = "Countries with the Least Energy Production in Europe",
       y = "Total Energy (GWh)",
       x = "Energy Type")

#' 
#' 
## -----------------------------------------------------------------------------------
# Are there countries which are attempting to use less energy?
# Note: Energy Supplied = net + import - export - energy absorbed by pumping
totals %>%
  filter(type == "Energy supplied") %>% 
  mutate(country_name = fct_reorder(country_name, value)) %>% 
  ggplot() + 
  geom_col(aes(x = value, y = country_name, fill = Year), position = 'dodge') +
  dark_theme_minimal() + 
  theme(
    strip.text = element_text(face = 'bold', hjust = 0),
    panel.grid.major = element_line('black', size = 0.5),
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.ontop = TRUE
  ) +
  labs(title = "Energy Supplied by Countries in the EU", 
       x = "Energy (GWh)", 
       y = "") + 
  scale_x_continuous(labels = scales::comma,
                     limits = c(0, 600000), 
                     breaks = c(0, 200000, 400000, 600000)) + 
  scale_fill_lancet() + 
  guides(fill = guide_legend(reverse = TRUE))

#' 
## -----------------------------------------------------------------------------------
# Which countries had the largest difference for energy supplied in the 
# 3 year period?
countries <- totals %>% 
  filter(type == "Energy supplied", 
         Year %in% c(2016, 2018)) %>% 
  pivot_wider(names_from = Year, values_from = value) %>% 
  mutate(diff = abs(`2018` - `2016`)) %>% 
  arrange(-diff) %>% 
  head(10) %>% 
  pull(country_name)
totals %>%
  filter(type == "Energy supplied", 
         country_name %in% countries) %>% 
  ggplot() + 
  geom_point(aes(x = Year, y = value)) + 
  geom_line(aes(x = Year, y = value, group = country_name)) + 
  geom_flag(aes(x = Year, y = value, country = str_to_lower(country))) + 
  labs(title = "Total Energy Supplied over 3 Years per Country",
       x = "Year", 
       y = "Energy (GWh)") +
  dark_theme_classic() +
  scale_y_continuous(labels = scales::comma) 
# In this plot we don't reall see much differences, so try to use a log scale 
# to see it a bit better

totals %>%
  filter(type == "Energy supplied", 
         country_name %in% countries) %>% 
  ggplot() + 
  geom_point(aes(x = Year, y = value)) + 
  geom_line(aes(x = Year, y = value, group = country_name)) + 
  geom_flag(aes(x = Year, y = value, country = str_to_lower(country))) + 
  labs(title = "Total Energy Supplied over 3 Years per Country",
       x = "Year", 
       y = "Energy (GWh) - Log Scale") +
  dark_theme_classic() +
  scale_y_log10(labels = scales::comma) 

#' 
## -----------------------------------------------------------------------------------
# Is there one energy type which is becoming more popular over the three years?
types %>% 
  group_by(type, Year) %>% 
  summarise(total = sum(value)) %>%
  ungroup() %>%
  # This line helps fix the order of the legend
  mutate(type = fct_reorder(type, -total)) %>% 
  ggplot(aes(x = Year, y = total, color = type)) + 
  geom_point() + 
  geom_line(aes(group = type)) +
  # Try a log scale, it might be better since there are some really low values
  scale_y_log10(labels = scales::comma) +
  # Looks a lot better with that 
  dark_theme_classic() + 
  scale_color_lancet() +
  labs(title = "Energy Produced by All Countries in the EU",
       color = "Energy Type", 
       y = "Enery Produced (GWh) - Log Scale")

# We can see there is a dip in conventional thermal from 2017 to 2018;
# Doesn't look like much but it should be noted that it is a log scale 
# (If we take the log scale away then it will be more noticeable but we won't see 
# a change in any other energy type)


#' 
#' 
#' 
## -----------------------------------------------------------------------------------
# Let's make a map!!
names <- types %>% 
  distinct(country_name) %>% 
  pull(country_name)

eu_map <- map_data("world", region = names)

for_labels <- eu_map %>%
  group_by(region) %>%
  summarise(long = mean(long), lat = mean(lat))

data <- types %>% 
  group_by(country_name, Renewable, Year) %>% 
  summarise(total = sum(value)) %>% 
  ungroup() %>% 
  left_join(., temp, by = "country_name") %>% 
  mutate(Renewable = ifelse(Renewable == "No", "Not Renewable", "Renewable")) %>% 
  select(country_name, Year, total, Renewable) %>% 
  rename("region" = "country_name") %>% 
  left_join(., eu_map, by = "region")

data %>% 
  ggplot(aes(x = long, y = lat)) +
  geom_polygon(aes(group = group, fill = total), color = "white", size = 0.1)+
  geom_text(aes(label = region), data = for_labels,  size = 1.5, hjust = 0.5)+
  theme_void() +
  facet_grid(Renewable ~ Year, switch = "y") + 
  scale_fill_material(palette = "cyan", labels = scales::comma) + 
  theme(
    strip.text = element_text(face = 'bold', hjust = 0), 
    strip.text.y = element_text(hjust = 0.5)
    ) + 
  labs(title = "Energy Production in the EU", 
       fill = "Energy Production (GWh)") 

#' 
#' 
