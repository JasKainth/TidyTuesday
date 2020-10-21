#' ---
#' title: "Datasaurus"
#' author: "Jas Kainth"
#' date: "13/10/2020"
#' output: pdf_document
#' editor_options: 
#'   chunk_output_type: console
#' ---
#' 
## ----setup, include=FALSE-------------------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)
library(tidyverse)
library(ggsci)
library(ggthemes)
library(patchwork)
library(gtsummary)
library(gganimate)
tt <- tidytuesdayR::tt_load(2020, week = 42)
datasaurus <- tt$datasaurus

#' 
## -------------------------------------------------------------------------------------
datasaurus %>% 
  count(dataset) # We have 13 different datasets

# Let's take a look at some key statistics of all of them
datasaurus %>% 
  group_by(dataset) %>% 
  summarise("Min x" = min(x),
            "Min y" = min(y),
            "1st Quartile x" = quantile(x, 0.25),
            "1st Quartile y" = quantile(y, 0.25),
            "Median x" = median(x),
            "Median y" = median(y),
            "Mean x" = mean(x),
            "Mean y" = mean(y),
            "3rd Quartile x" = quantile(x, 0.75),
            "3rd Quartile y" = quantile(y, 0.75),
            "Max x" = max(x),
            "Max y" = max(y)) %>% 
  pivot_longer(cols = -dataset, names_to = "statistic", values_to = "value") %>%
  mutate(dataset = str_replace(dataset, "_", " "),
         dataset = str_to_title(dataset)) %>% 
  ggplot(aes(x = dataset, y = value, color = statistic)) + 
  geom_line(aes(group = statistic)) + 
  geom_point() + 
  facet_wrap(~ statistic) + 
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        legend.position = "none") + 
  scale_color_igv() + 
  labs(title = "Comparing statistics for each dataset",
       y = " ",
       x = "Dataset")
# As we see the values are pretty consistent, let's see what the actual datasets 
# actually look like when we plot them 


#' 
## -------------------------------------------------------------------------------------
# Create a plot of all the datasets
# We will use patchworks, so we will add two plots (highlighting the dino plot)
# This is a personal choice to make the dino one bigger but creating it like this
# will allow us to get a nice matrix of plots
dinoPlot <- datasaurus %>% 
  filter(dataset == "dino") %>%
  mutate(dataset = str_replace(dataset, "_", " "),
         dataset = str_to_title(dataset)) %>% 
  ggplot(aes(x = x, y = y)) +
  geom_point(color = "darkgreen") +
  theme_void() +
  facet_wrap(~ dataset)

otherPlot <- datasaurus %>% 
  filter(dataset != "dino") %>% 
  mutate(dataset = str_replace(dataset, "_", " "),
         dataset = str_to_title(dataset)) %>% 
  ggplot(aes(x = x, y = y)) +
  geom_point(aes(color = dataset)) +
  theme_void() +
  theme(legend.position = "none") +
  scale_color_manual(values = pal_futurama("planetexpress")(12)) +
  facet_wrap(~ dataset)

dinoPlot + otherPlot

#' 
#' 
## -------------------------------------------------------------------------------------
# Make a heat map so we can see where most of the points are concentrated
datasaurus %>% 
  mutate(dataset = str_replace(dataset, "_", " "),
         dataset = str_to_title(dataset)) %>%
  ggplot(aes(x = x, y = y)) + 
  stat_density_2d(aes(fill = ..density..), geom = "raster", contour = FALSE) +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  facet_wrap(~ dataset) +
  scale_fill_gradient(low = "darkorchid4", high = "white") +
  theme_minimal() + 
  theme(legend.position = "none") 

# For most of them we see heavy concentration around some areas to change the mean &
# medians
  

#' 
## -------------------------------------------------------------------------------------
datasaurus %>% 
  left_join(., local_mean, by = "dataset") %>%
  mutate(dataset = str_replace(dataset, "_", " "),
         dataset = str_to_title(dataset)) %>% 
  ggplot(aes(x = x, y = y)) + 
  geom_point(aes(color = dataset), alpha = 0.5, shape = 16) + 
  facet_wrap(~ dataset) +
  theme_void() + 
  scale_color_manual(values = pal_simpsons("springfield")(13)) + 
  theme(legend.position = "none")

#' 
#' 
## -------------------------------------------------------------------------------------
# Get information on the means and sds for each dataset to use for the gif
stats <- datasaurus %>% 
  group_by(dataset) %>% 
  summarise(mean_x = mean(x),
            mean_y = mean(y),
            sd_x = sd(x),
            sd_y = sd(y)) %>% 
  mutate(dataset = str_replace(dataset, "_", " "),
         dataset = str_to_title(dataset)) 

# Create the gif (use gganimate)
p <- datasaurus %>% 
  mutate(dataset = str_replace(dataset, "_", " "),
         dataset = str_to_title(dataset)) %>% 
  ggplot(aes(x = x, y = y)) +
  geom_point() +
  theme_void() +
  theme(legend.position = "none",
        plot.title = element_text(hjust = 0.5),
        text = element_text('Avenir Next Condensed'),
        strip.text = element_text(face = 'bold', hjust = 0)) +
  geom_text(data = stats, x = 105, y = 50, hjust = 0, 
            vjust = 0.5, size = 5, family = "Avenir Next Condensed",
            aes(label = paste0("\nMean x = ",
                                "\nMean y = ",
                                "\nSD x = ",
                                "\nSD y = ")), 
            check_overlap = T, color = "black") +
  geom_text(data = stats, x = 126, y = 50, hjust = 0,
            vjust = 0.5, size = 5, family = "Avenir Next Condensed",
            aes(label = paste0("\n", mean_x,
                               "\n", mean_y,
                               "\n", sd_x,
                               "\n", sd_y)), 
            check_overlap = T, color = "grey50") +
  scale_x_continuous(limits = c(0, 175)) +
  transition_states(dataset,
                    transition_length = 2,
                    state_length = 1) +
  ggtitle("{closest_state}") 
# Code line if you want to save the gif
# anim_save("datasaurus.gif", animate(p, fps = 45, duration = 20))

#' 
#' 
#' 
