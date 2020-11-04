#' ---
#' title: "Canadian Wind Turbine"
#' author: "Jas Kainth"
#' date: "26/10/2020"
#' output: pdf_document
#' editor_options: 
#'   chunk_output_type: console
#' ---
#' 
## ----setup, include=FALSE------------------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)
library(tidyverse)
library(ggsci)
library(tidytext)
library(ggdark)
library(patchwork)
library(strex)
library(tidymodels)
library(vip)
library(parttree)

tt <- tidytuesdayR::tt_load(2020, week = 44)
wind_turbine <- tt$`wind-turbine` %>% 
  mutate(year = str_first_number(commissioning_date))



#' 
## ------------------------------------------------------------------------------------
# Where are the most wind turbines found?
wind_turbine %>% 
  count(province_territory) %>% 
  mutate(province_territory = fct_reorder(province_territory, n)) %>%
  ggplot(aes(x = n, y = province_territory, fill = province_territory)) + 
  geom_col() + 
  theme_minimal() + 
  theme(
    panel.grid.major = element_line('white', size = 0.5),
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_blank(),
    legend.position = "none",
    panel.ontop = TRUE
  ) + 
  scale_fill_igv() + 
  labs(title = "Where are most of the wind turbines located?",
       x = "# of Turbines", 
       y = " ")

# What are some of the largest projects?
wind_turbine %>% 
  separate(turbine_number_in_project, into = c("Turbine_No", "Total"), 
           sep = "/", convert = TRUE) %>% 
  distinct(province_territory, Total, project_name) %>% 
  arrange(desc(Total)) %>% 
  head(20) %>% 
  mutate(project_name = fct_reorder(project_name, Total)) %>% 
  ggplot(aes(x = Total, y = project_name, fill = province_territory)) + 
  geom_col() + 
  theme_minimal() + 
  scale_fill_futurama() +
  labs(title = "What are the largest wind turbine projects in Canada?",
       y = "Project", 
       x = "# of Turbines",
       fill = "Province")

#' 
#' 
#' 
#' # Are there in trends over time?
#' 
## ------------------------------------------------------------------------------------
# Average height and rotor size
wind_turbine %>% 
  group_by(year) %>% 
  summarise(avg_height = mean(hub_height_m)) %>% 
  ggplot(aes(x = year, y = avg_height)) + 
  geom_col(aes(fill = avg_height), color = "white") +
  theme_minimal() + 
  geom_text(aes(label = round(avg_height), color = avg_height), 
            vjust = -0.5, size = 3) +
  scale_fill_material(palette = "green") + 
  scale_color_material(palette = "green") + 
  theme(legend.position = "none") +
  labs(title = "Average hub height of wind turbines",
       x = "Year",
       y = "Height (m)")


wind_turbine %>% 
  group_by(year) %>% 
  summarise(avg_height = mean(rotor_diameter_m)) %>% 
  ggplot(aes(x = year, y = avg_height)) + 
  geom_col(aes(fill = avg_height), color = "white") +
  theme_minimal() + 
  geom_text(aes(label = round(avg_height), color = avg_height), 
            vjust = -0.5, size = 3) +
  scale_fill_material(palette = "green") + 
  scale_color_material(palette = "green") + 
  theme(legend.position = "none") +
  labs(title = "Average rotor diameter of wind turbines",
       x = "Year",
       y = "Diameter (m)")

#' 
## ------------------------------------------------------------------------------------
# What projects are the largest (for energy capacity)?
wind_turbine %>%
  distinct(project_name, total_project_capacity_mw, province_territory) %>%
  arrange(desc(total_project_capacity_mw)) %>% 
  head(15) %>%
  mutate(project_name = fct_reorder(project_name, total_project_capacity_mw)) %>%
  ggplot(aes(x = total_project_capacity_mw, y = project_name, 
             fill = province_territory)) +
  geom_col() +
  theme_minimal() +
  scale_fill_aaas(breaks = c("Quebec", "Ontario", "Alberta")) +
  labs(title = "Which projects have the largest energy capacity?",
       x = "Capacity (MW)",
       y = "",
       fill = "")
# Make a df indicating the first wind turbine that was commissioned for each project
first_year <- wind_turbine %>% 
  group_by(project_name) %>% 
  summarise(first_year = min(year, na.rm = TRUE)) %>% 
  select(project_name, first_year)
# Have there been more projects over time?
wind_turbine %>% 
  separate(turbine_number_in_project, into = c("Turbine_No", "Total"), 
           sep = "/", convert = TRUE) %>%
  distinct(project_name, Total, total_project_capacity_mw, province_territory) %>% 
  left_join(., first_year, by = "project_name") %>% 
  ggplot(aes(x = first_year, y = total_project_capacity_mw, 
             size = Total, color = province_territory)) +
  geom_point(alpha = 0.5, shape = 16) + 
  theme_minimal() +
  scale_color_futurama() +
  theme(legend.position = "bottom", 
        legend.box = "horizontal",
        strip.text = element_text(face = "bold", hjust = 0),
        plot.caption = element_text(face = "italic"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major.y = element_blank()) +
  guides(size = guide_legend(override.aes = list(colour = "black"))) +
  labs(title = "Are the wind turbine projects getting larger over time?",
       size = "# of Turbines in Project",
       color = "Province/Territory",
       x = "Year of First Wind Turbine Commissioned in Project",
       y = "Total Project Capacity (MW)") +
  scale_size_area(breaks = c(40, 80, 120, 160), max_size = 12) +
  scale_x_continuous(limits = c(1990, 2020),
                     breaks = c(1990, 2000, 2010, 2020))
  
# How much energy is each provine creating?
wind_turbine %>% 
  mutate(decade = year %/% 10 * 10) %>% 
  count(province_territory, decade) %>% 
  ggplot(aes(x = decade, y = n, fill = province_territory)) +
  geom_col() +
  theme_minimal() + 
  scale_fill_futurama() +
  facet_wrap(~ province_territory) +
  theme(legend.position = "none",
    strip.text = element_text(face = "bold", hjust = 0),
    plot.caption = element_text(face = "italic"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_blank()) +
  labs(title = "Which provinces are creating more wind turbines?", 
       x = "Decade", 
       y = "# of Turbines")


#' 
#' # How much power are the provinces generating from wind?
#' 
## ------------------------------------------------------------------------------------
complete <- tibble(province_territory = wind_turbine %>% 
                     select(province_territory) %>% 
                     distinct() %>% 
                     as.character())

running_sum <- wind_turbine %>% 
  group_by(province_territory, year) %>% 
  summarise(energy = sum(turbine_rated_capacity_k_w)) %>% 
  mutate(total = cumsum(energy)) %>% 
  filter(!is.na(year),
         !is.na(total)) 

running_sum %>% 
  select(total) %>%
  slice_max(order_by = total, n = 1, with_ties = FALSE) %>% 
  ungroup() %>% 
  left_join(., complete, by = "province_territory") %>% 
  mutate(year = 2020) %>% 
  union(., running_sum %>% select(province_territory, year, total)) %>%
  ggplot(aes(x = year, y = total/1000, color = province_territory)) +
  geom_line() +
  geom_point() + 
  theme_minimal() + 
  scale_color_futurama() + 
  labs(title = "Total energy produced by each province by wind turbines",
       subtitle = "Assumes that wind turbines run 24 hours a day",
       x = "Year",
       y = "Total Energy (MW)",
       color = "Province/Territory") +
  facet_wrap(~ province_territory) 
  

#' 
#' ## Creating a model to predict the capacity of a wind turbine
#' We will use tidymodels (creating a decision tree)
#' 
## ------------------------------------------------------------------------------------
# Create a df of our predictors
wind_turbine_df <- wind_turbine %>% 
  select(capacity = turbine_rated_capacity_k_w,
         year,
         rotor_diameter = rotor_diameter_m,
         hub_height = hub_height_m,
         model,
         province_territory) %>%
  # Remove the rows that have NA as our dependent variable (what we are predicting)
  filter(!is.na(capacity)) %>%
  # Lump the models (top 8)
  mutate(model = fct_lump_n(model, 8)) %>%
  # Convert characters into factors
  mutate_if(is.character, factor)

# Do we see any trend between the numerical variables?
wind_turbine_df %>%
  pivot_longer(year:hub_height) %>% 
  mutate(name = str_replace(name, "_", " "),
         name = str_to_title(name)) %>% 
  ggplot(aes(x = value, y = capacity, color = name)) + 
  geom_point(alpha = 0.1) + 
  geom_smooth(method = "loess") + 
  theme_minimal() + 
  scale_color_igv() +
  facet_wrap(~ name, 
             scales = "free_x", 
             strip.position = "bottom") + 
  theme(legend.position = "none") + 
  labs(x = " ",
       y = "Capacity (KW)")
# Hex plot (can see the overlap a bit better)
wind_turbine_df %>%
  pivot_longer(year:hub_height) %>% 
  mutate(name = str_replace(name, "_", " "),
         name = str_to_title(name)) %>% 
  ggplot(aes(x = value, y = capacity)) + 
  geom_hex(bins = 15) + 
  geom_smooth(method = "loess", color = "darkgreen") + 
  theme_minimal() + 
  scale_fill_material(palette = "green") +
  facet_wrap(~ name, 
             scales = "free_x", 
             strip.position = "bottom") + 
  labs(x = " ",
       y = "Capacity (KW)")

#' 
#' 
## ------------------------------------------------------------------------------------
# Split the data
set.seed(1998)
split <- initial_split(wind_turbine_df, strata = capacity)
training_set <- training(split)
test_set <- testing(split)

# Get more splits for cross-validation 
# This is for tuning our model
cv <- vfold_cv(training_set, strata = capacity) # Default is 10-fold

#' 
## ------------------------------------------------------------------------------------
# Start the modelling 
# We will use a decision tree
# Has 3 main tuning parameters; Cost Parameter, Tree Depth & Minimum n (how many
# data points we need to keep splitting)
# We will tune all of these
tree_specification <- decision_tree(
  cost_complexity = tune(),
  tree_depth = tune(),
  min_n = tune()
  ) %>%
  set_engine("rpart") %>%
  set_mode("regression")

# Create the set of possible parameters
# Leave levels at 3 (otherwise takes too long to run since it considers every
# possibility)
grid <- grid_regular(cost_complexity(), tree_depth(), min_n())

#' 
## ------------------------------------------------------------------------------------
doParallel::registerDoParallel()
set.seed(1998)
results <- tune_grid(
  tree_specification,
  capacity ~ .,
  resamples = cv,
  grid = grid
)

#' 
## ------------------------------------------------------------------------------------
# Let's explore the results
collect_metrics(results)

# Let's plot the results
autoplot(results) + 
  theme_light() + 
  scale_color_uchicago()

# What is the best tree?
show_best(results, metric = "rmse")
show_best(results, metric = "rsq")

# It's the same for both of them, that's good so let's stick with that tree
select_best(results, metric = "rmse")

#' 
## ------------------------------------------------------------------------------------
final_tree <- finalize_model(tree_specification, 
                            select_best(results, metric = "rmse"))

final_fit <- fit(final_tree, capacity ~ ., training_set)

# We will create this new workflow for our predictions (which we will take a look
# at later)
final_results <- last_fit(final_tree, capacity ~ ., split)

#' 
## ------------------------------------------------------------------------------------
final_fit %>%
  vip(geom = "col", aesthetics = list(fill = "darkgreen")) +
  theme_minimal() + 
  scale_y_continuous(expand = c(0,0))
# So we see the most important parameters are rotor_diameter and the year it was
# commissioned in
# Let's use parttree to see another visualization
# NOTE: For this we can only use two predictors so we will use the two most
# important

new_fit <- fit(final_tree, capacity ~ year + rotor_diameter, training_set)

training_set %>% 
  ggplot(aes(x = rotor_diameter, y = year, color = capacity)) + 
  geom_jitter(alpha = 0.6, width = 1.5, height = 0.5, shape = 16) + 
  geom_parttree(data = new_fit, aes(fill = capacity), alpha = 0.3) +
  dark_theme_minimal() + 
  scale_color_viridis_c(aesthetics = c("color", "fill")) + 
  labs(title = "Visualization of Decision Tree",
       subtitle = "Decision Tree Based Only on Commision Year & Rotor Diameter",
       x = "Rotor Diameter (m)",
       y = "Commission Date",
       color = "Turbine Capacity \n(KW)", 
       fill = "Turbine Capacity \n(KW)") + 
  theme(plot.caption = element_text(face = 'italic'))


#' 
## ------------------------------------------------------------------------------------
# Let's take a look at the testing data 
collect_predictions(final_results) %>% 
  ggplot(aes(x = capacity, y = .pred)) + 
  geom_point(alpha = 0.4, color = "black") + 
  theme_minimal() + 
  geom_abline(slope = 1, intercept = 0, color = "red", lty = 2) + 
  labs(title = "How do the predicted values compare to the actual values?",
       subtitle = "Taking a look at the test set",
       x = "Actual Capacity (KW)", 
       y = "Predicted Capacity (KW)") 


#' 
