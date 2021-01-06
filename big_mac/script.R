#' ---
#' title: "Big Mac"
#' author: "Jas Kainth"
#' date: "22/12/2020"
#' output:
#'   html_document:
#'     df_print: paged
#' editor_options:
#'   chunk_output_type: console
#' ---
#' 
## ----setup, include=FALSE--------------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)
library(tidyverse)
library(ggsci)
library(scales)
library(lubridate)
library(countrycode)
library(rvest)
library(data.table)
library(gganimate)

#' 
## ----data------------------------------------------------------------------------
tt <- tidytuesdayR::tt_load(2020, week = 52)
big_mac <- tt$`big-mac`

#' 
## ----exp&cheap, fig.height=6, fig.width=10---------------------------------------
# Where are the most expensive Big Macs
big_mac %>% 
  group_by(name) %>%
  slice_max(order_by = dollar_price, n = 1, with_ties = FALSE) %>%
  arrange(desc(dollar_price)) %>%
  head(10) %>%
  ungroup() %>%
  mutate(year = year(date),
         for_plot = glue::glue("{ name } ({ year })")) %>%
  mutate(for_plot = fct_reorder(for_plot, dollar_price)) %>%
  ggplot(aes(x = dollar_price, y = for_plot, fill = for_plot)) +
  geom_col() + 
  theme_minimal() + 
  scale_fill_futurama() +
  theme(legend.position = "none") +
  labs(title = "Most Expensive regions for a Big Mac",
       x = "Price (USD)",
       y = " ") +
  scale_x_continuous(labels = dollar)

big_mac %>% 
  group_by(name) %>%
  slice_min(order_by = dollar_price, n = 1, with_ties = FALSE) %>%
  arrange(dollar_price) %>%
  head(10) %>%
  ungroup() %>%
  mutate(year = year(date),
         for_plot = glue::glue("{ name } ({ year })")) %>%
  mutate(for_plot = fct_reorder(for_plot, dollar_price)) %>%
  ggplot(aes(x = dollar_price, y = for_plot, fill = for_plot)) +
  geom_col() + 
  theme_minimal() + 
  scale_fill_futurama() +
  theme(legend.position = "none") +
  labs(title = "Cheapest regions for a Big Mac",
       x = "Price (USD)",
       y = " ") +
  scale_x_continuous(labels = dollar)


#' 
## ----wikipedia-------------------------------------------------------------------
# Scrape population data from wikipedia
url <- "https://en.wikipedia.org/wiki/List_of_countries_and_dependencies_by_population"
webpage <- read_html(url)
t <- webpage %>% html_nodes("table")
population <- t[[1]] %>% 
  html_table %>% 
  as_tibble() %>%
  # Select the country name
  select(name = 2, 3) %>% 
  # Clean it up a bit
  mutate(name = gsub("\\[.*", "", name)) %>%
  mutate(Population = gsub("\\,*", "", Population),
         Population = as.integer(Population))

#' 
#' 
## ----animation, fig.height=10, fig.width=10--------------------------------------
filtered_big_mac <- big_mac %>% 
  filter(name != "Euro area")
# How does GDP effect the Price of the Big Mac
big_mac_per_year <- filtered_big_mac %>%
  mutate(continent = countrycode(sourcevar = filtered_big_mac$name,
                                           origin = "country.name", 
                                           destination = "continent")) %>%
  left_join(., population, by = "name") %>% 
  select(name, Population, date, gdp_dollar, dollar_price, continent) %>% 
  drop_na() %>% 
  mutate(year = year(date)) %>% 
  group_by(name, year) %>%
  slice_max(order_by = dollar_price, with_ties = FALSE) %>%
  ungroup() 

big_mac_per_year %>%
  ggplot(aes(x = gdp_dollar, y = dollar_price, size = Population,
             color = continent)) +
  geom_point() + 
  theme_minimal() + 
  theme(text = element_text('Avenir Next Condensed')) +
  scale_x_continuous(labels = dollar) + 
  scale_y_continuous(labels = dollar,
                     limits = c(1, 9),
                     breaks = c(1, 3, 5, 7, 9)) + 
  scale_color_futurama() +
  scale_size_area(max_size = 6) +
  labs(title = "How does the price of a Big Mac vary over the years?",
       x = "GDP (USD)",
       y = "Price (USD)",
       size = "Population \n in 2020",
       color = "Continent") + 
  facet_wrap(~ year)

# Let's animate that to see the change per country each year

animation <- big_mac_per_year %>%
  ggplot(aes(x = gdp_dollar, y = dollar_price, size = Population,
             color = continent)) + 
  geom_point() + 
  theme_minimal() + 
  theme(text = element_text('Avenir Next Condensed')) +
  scale_x_log10(labels = dollar,
                limits = c(1000, 120000),
                breaks = c(1000, 10000, 100000)) + 
  scale_y_continuous(labels = dollar,
                     limits = c(1, 9),
                     breaks = c(1, 3, 5, 7, 9)) + 
  scale_color_futurama() +
  scale_size_area(max_size = 10) +
  labs(x = "GDP (USD)",
       y = "Price (USD)",
       size = "Population \n in 2020",
       color = "Continent") +
  ggtitle("Big Mac Prices in {closest_state}") +
  transition_states(states = year) + 
  ease_aes(x = 'cubic-in-out')

# anim_save("big_mac_prices.gif", animate(animation, fps = 45, duration = 20))


#' 
## ----compareLocalCurrency, fig.height=10, fig.width=10---------------------------
# How does the price of the Big Mac compare the the expexted price
# i.e. we will compare the price in local currency to the price when we 
# multiply the US price by the exchange rate
# We will only do this for the countries where we have full data 
big_mac %>%
  add_count(name, name = "total_points") %>%
  filter(total_points == max(total_points)) %>% 
  group_by(date) %>%
  mutate(usd_price = local_price[iso_a3 == "USA"]) %>%
  ungroup() %>%
  select(date, name, local_price, dollar_ex, usd_price) %>% 
  mutate(expected_price = usd_price * dollar_ex) %>% 
  # Remove the United States since that will always have the same local price
  # and expected price
  filter(name != "United States") %>%
  ggplot(aes(x = date)) + 
  geom_line(aes(y = local_price, color = "Price of Big Mac")) + 
  geom_line(aes(y = expected_price, color = "Expected price of Big Mac")) +
  theme_minimal() + 
  theme(text = element_text('Avenir Next Condensed')) +
  facet_wrap(~ name, scales = "free_y") +
  expand_limits(y = 0) + 
  scale_color_manual(values = c("#FF6F00FF", "#008EA0FF")) + 
  labs(title = "Local Price of Big Mac vs Expected Price",
       subtitle = "Expected Price = Price of Big Mac in USD * Exchange Rate",
       x = "Date",
       y = "Price in Local Currency",
       color = " ")


#' 
#' 
## ----index, fig.height=10, fig.width=10------------------------------------------
inbetween_values <- big_mac %>% 
  add_count(name, name = "total_points") %>%
  filter(total_points == max(total_points),
         !is.na(usd_adjusted),
         !name %in% c("United States", "China", "South Korea")) %>%
  group_by(name) %>% 
  mutate(
    date = as.numeric(as_datetime(date)),  
    usd_lead = lead(usd_adjusted),
    date_lead = lead(date),
    xzero = -((usd_adjusted * (date_lead - date)) / 
                (usd_lead - usd_adjusted)) + date, 
    is_valid = xzero > date & xzero < date_lead,
    yzero = 0,
    xzero = replace(xzero, !is_valid, NA),
    yzero = replace(yzero, !is_valid, NA)
  ) %>% 
  ungroup() %>% 
  filter(!is.na(xzero)) %>% 
  select(date = xzero, name, usd_adjusted = yzero)


big_mac_area <- big_mac %>% 
  add_count(name, name = "total_points") %>%
  filter(total_points == max(total_points),
         !is.na(usd_adjusted),
         !name %in% c("United States", "China", "South Korea")) %>%
  mutate(date = as.numeric(as_datetime(date))) %>% 
  select(date, name, usd_adjusted) %>% 
  rbind(., inbetween_values) %>% 
  mutate(date = as_datetime(date))

big_mac_area %>%
  ggplot(aes(x = date, y = usd_adjusted)) + 
  geom_area(data = filter(big_mac_area, usd_adjusted >= 0), 
            fill = "#d70000") + 
  geom_area(data = filter(big_mac_area, usd_adjusted <= 0), 
            fill = "#f7ac17") +
  facet_wrap(~ name, ncol = 6) + 
  labs(title = "Big Mac Index",
       subtitle = " How expensive is the Big Mac relative to it's expected price? \n Higher positive values means that once the Big Mac has been adjusted for the GDP, \n it is more expensive than anticipated.") + 
  theme_void() + 
  theme(text = element_text('Avenir Next Condensed'),
        plot.title = element_text(hjust = .5, size = 25, color = "#d70000",
                                  face = "bold.italic"),
        plot.subtitle = element_text(hjust = .5, size = 12, color = "#f7ac17",
                                  face = "bold.italic"),
        strip.text = element_text(face = "bold", vjust = 1)) 

  

#' 
