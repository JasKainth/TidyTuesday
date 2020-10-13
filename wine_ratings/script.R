#' ---
#' title: "Wine Ratings"
#' author: "Jas Kainth"
#' date: "16/09/2020"
#' output: pdf_document
#' editor_options: 
#'   chunk_output_type: console
#' ---
#' 
## ----setup, include=FALSE-----------------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)
library(tidyverse)
library(ggsci)
library(tvthemes)
library(ggdark)
library(tidytext)
library(widyr)
library(ggthemes)
library(broom)
library(ggraph)
library(igraph)
library(Matrix)
library(glmnet)
wine_ratings <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-05-28/winemag-data-130k-v2.csv") %>% 
  # Add wine id; almost like X1 but it starts at 1
  mutate(id = row_number())
# The description is actually kind of cool, we can ignore it for now but 
# at the end we can make a network plot where we can visualize which words appear
# together within the same description

#' 
#' 
## ----popular------------------------------------------------------------------------
# Which countries make the most wines?
wine_ratings %>%
  mutate(country = fct_lump_n(country, 8)) %>% 
  # Remove the NA
  filter(!is.na(country)) %>% 
  count(country) %>% 
  mutate(country = fct_reorder(country, n)) %>% 
  ggplot() + 
  geom_col(aes(x = n, y = country, fill = country)) + 
  labs(title = "Which Countries Produce the Most Wines?", 
       x = "# of Wines",
       y = " ") + 
  theme_minimal() + 
  scale_fill_lancet() + 
  theme(
    plot.caption = element_text(face = 'italic'),
    panel.grid.major = element_line('white', size = 0.5),
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.ontop = TRUE,
    legend.position = "none"
  )

# What are the most popular types of grapes?
wine_ratings %>%
  mutate(variety = fct_lump_n(variety, 15)) %>% 
  # Remove the NA
  filter(!is.na(variety)) %>% 
  count(variety) %>% 
  mutate(variety = fct_reorder(variety, n)) %>% 
  ggplot() + 
  geom_col(aes(x = n, y = variety, fill = variety)) + 
  labs(title = "What are the Most Popular Grape Types?", 
       x = "",
       y = " ") + 
  theme_minimal() + 
  scale_fill_igv() + 
  theme(
    plot.caption = element_text(face = 'italic'),
    panel.grid.major = element_line('white', size = 0.5),
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.ontop = TRUE,
    legend.position = "none"
  )

# Cleary there is a large variety of grapes

#' 
## ----best_wine----------------------------------------------------------------------
# Which Countries produce the 'best' wine?
wine_ratings %>% 
  group_by(country) %>% 
  summarise(avg_rating = mean(points, na.rm = TRUE),
            n = n()) %>%
  ungroup() %>% 
  # Remove the countries which have fewer than 100 wines made
  filter(n > 100) %>% 
  arrange(desc(avg_rating)) %>% 
  head(10) %>% 
  mutate(country = fct_reorder(country, avg_rating)) %>% 
  ggplot(aes(avg_rating, country)) + 
  geom_point(aes(size = n, color = country)) +
  dark_theme_minimal() +
  scale_color_igv() + 
  guides(color = FALSE) +
  labs(title = "Which Countries make the best wine?",
       x = "Average Rating", 
       y = "",
       size = "# of Wines Made") + 
  theme(
    strip.text = element_text(face = 'bold', hjust = 0),
    plot.caption = element_text(face = 'italic'),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_blank(),
  ) + 
  scale_x_continuous(limits = c(88, 91))

#' 
#' 
## ----distribution-------------------------------------------------------------------
# How are prices and points distributed?
wine_ratings %>% 
  ggplot(aes(points)) + 
  geom_bar(fill = "darkred") + 
  theme_classic()

wine_ratings %>% 
  ggplot(aes(price)) + 
  geom_bar() + 
  theme_classic() 
# There are some really expensive wines; for now filter them out to see a rough 
# distribution
wine_ratings %>% 
  filter(price < 500) %>% 
  ggplot(aes(price)) + 
  geom_bar() + 
  theme_classic() 
# Points seems normally distributed and price seems log-normal

# Is there a trend between the price of the wine and the points it recieves?
wine_ratings %>% 
  filter(!is.na(price) & !is.na(points)) %>%
  ggplot() + 
  geom_point(aes(x = price, y = points), alpha = 0.3, color = "black") + 
  geom_smooth(aes(x = price, y = points), method = "lm") + 
  theme_classic() + 
  scale_x_log10() +
  labs(title = "Is there a correlation between price and points?", 
       x = "Price", 
       y = "Points")
# It does seem like it; price might be a good predictor to use as well


#' 
## ----tasters------------------------------------------------------------------------
# Do some tasters give a higher rating than others? 
# Check with the top 5 tasters (most tasted)
top_5 <- wine_ratings %>% 
  count(taster_name, sort = TRUE) %>% 
  filter(!is.na(taster_name)) %>% 
  head(5) %>% 
  pull(taster_name)

wine_ratings %>% 
  filter(taster_name %in% top_5) %>% 
  mutate(taster_name = fct_reorder(taster_name, points)) %>% 
  ggplot() + 
  geom_boxplot(aes(x = taster_name, y = points, fill = taster_name),
               color = "black") + 
  theme_classic() + 
  scale_fill_uchicago() + 
  labs(title = "How do Ratings Compare Across Tasters?",
       x = "Taster Name", 
       y = "Points") + 
  theme(legend.position = "none")

# Do some tasters prefer specific grape wines?
top_grapes <- 
  wine_ratings %>% 
  count(variety, sort = TRUE) %>%
  head(5) %>% 
  pull(variety)

wine_ratings %>% 
  filter(taster_name %in% top_5 & variety %in% top_grapes) %>% 
  group_by(taster_name, variety) %>% 
  summarise(avg_rating = mean(points),
            total = n()) %>%
  ggplot(aes(x = taster_name, y = avg_rating, color = variety)) +
  geom_line(aes(group = variety), alpha = 0.5) +
  geom_point(aes(size = total), alpha = 0.5) + 
  dark_theme_classic() + 
  scale_color_lancet() + 
  labs(title = "Which Variety do the Top Tasters Prefer",
       subtitle = "Tasters who have tasted the most wines were picked",
       x = "Taster Name", 
       y = "Points",
       size = "Total Wines Tasted",
       color = "Grape Variety") + 
  theme(
    plot.caption = element_text(face = 'italic'),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_blank()
  )

# Some interesting points - Kerin O' Keefe is the only one without TWO of the
# top FIVE varities tasted

#' 
## ----linearmodel--------------------------------------------------------------------
# Let's try making a model to try and predict the wine ratings 
# We will use country (top 5)
# We will also use price (log2 scale)
# We will also use variety (grape type/ top 5 and name the rest other)
# We will also use the taster name (top 5)
for_model <- wine_ratings %>% 
  mutate(variety = fct_lump(variety, 5), 
        country = fct_lump(country, 5),
        taster_name = fct_lump(taster_name, 5))

# Note that a lm is probably not the best model type since we are constrained 
# by 0 and 100
ratings_model <- lm(points ~ log2(price) + country + taster_name + variety,
                    data = for_model) 

# Baseline - France, Kerin O’Keefe, Bordeaux-style Red Blend
ratings_model %>% 
  tidy(conf.int = TRUE) %>% 
  # Want to remove the intercept term from out plot
  # We only want the plot to show us the effects of the predictors
  filter(term != "(Intercept)") %>% 
  mutate(term = str_replace(term, "country", "Country: "),
         term = str_replace(term, "taster_name", "Taster: "),
         term = str_replace(term, "variety", "Grape: "), 
         term = fct_reorder(term, estimate)) %>%
  ggplot(aes(estimate, term)) + 
  geom_errorbar(aes(xmin = conf.low, xmax = conf.high), color = "gray72") +
  geom_point(color = "black") + 
  theme_minimal() + 
  theme(
    strip.text = element_text(face = 'bold', hjust = 0),
    plot.caption = element_text(face = 'italic'), 
    panel.grid.minor = element_blank()
  ) + 
  labs(title = "Which predictors effect wine ratings?",
       subtitle = "Error bars provided with point estimates",
       x = "Point Estimates", 
       y = "Covariates")

# Note: The interpertation of the price predictor is that doubling the price will
# increase the points by approximately 2 (i.e. the point estimate)

#' 
## ----network------------------------------------------------------------------------
# Make a network plot of words that appear together
words <- wine_ratings %>% 
  select(id, description) %>% 
  unnest_tokens(word, description) %>% 
  anti_join(stop_words, by = "word") %>% 
  # Get rid of some more words
  filter(! word %in% c("wine", "drink", "flavors")) 

words_cor <- words %>% 
  # Take away words if the repeat multiple times in the same description
  distinct(word, id) %>% 
  # We want each word to appear at least 1000 times 
  add_count(word) %>% 
  filter(n >= 1000, 
         str_detect(word, "[a-z]")) %>% 
  pairwise_cor(word, id)

set.seed(1998)
words_cor %>%
  filter(correlation > .25) %>%
  graph_from_data_frame() %>%
  ggraph(layout = "fr") +
  geom_edge_link(aes(edge_alpha = correlation, edge_width = correlation),
                 color = "darkred") +
  geom_node_point(color = "black", size = 5) +
  geom_node_text(aes(label = name), repel = TRUE, color = "black") +
  theme_void() +
  labs(title = "Which words often appear together in the same wine description?") + 
  theme(
    strip.text = element_text(face = 'bold', hjust = 0),
    plot.caption = element_text(face = 'italic'))

# What if we make the word restriction more strict
words %>% 
  # Take away words if the repeat multiple times in the same description
  distinct(word, id) %>% 
  # We want each word to appear at least 5000 times 
  add_count(word) %>% 
  filter(n >= 5000, 
         str_detect(word, "[a-z]")) %>% 
  # Allow more words by changing the correlation threshold
  pairwise_cor(word, id) %>% 
  filter(correlation > .1) %>%
  graph_from_data_frame() %>%
  ggraph(layout = "fr") +
  geom_edge_link(aes(edge_alpha = correlation, edge_width = correlation),
                 color = "darkred") +
  geom_node_point(color = "black", size = 5) +
  geom_node_text(aes(label = name), repel = TRUE, color = "black") +
  theme_void() +
  labs(title = "Which words often appear together in the same wine description?") + 
  theme(
    strip.text = element_text(face = 'bold', hjust = 0),
    plot.caption = element_text(face = 'italic'))
  

#' 
#' 
## ----correlation_among--------------------------------------------------------------
# Make a plot showing the correlation among the 9 most popular words with each other
top_9 <- words %>% 
  count(word, sort = TRUE) %>% 
  head(9) %>% 
  pull(word)

words_cor %>% 
  filter(item1 %in% top_9,
         item2 %in% top_9) %>% 
  mutate(item1 = str_to_title(item1),
         item2 = str_to_title(item2)) %>% 
  mutate(item2 = reorder_within(item2, correlation, item1)) %>% 
  ggplot(aes(correlation, item2)) +
  geom_segment(aes(x = 0, xend = correlation, y = item2, yend = item2, 
                   color = correlation > 0)) +
  geom_point(aes(color = correlation > 0)) + 
  facet_wrap(~ item1, scales = "free_y") + 
  scale_y_reordered() + 
  theme_minimal() + 
  scale_color_westeros(palette = "Targaryen") + 
  labs(title = "How often do the top 9 words used to describe wine appear in the same description?",
       x = "Correlation", 
       y = " ") + 
  theme(
    strip.text = element_text(face = 'bold', hjust = 0),
    plot.caption = element_text(face = 'italic'),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_blank(),
    legend.position = "none") 


# What about words are most likely and least likely to show up with these 9 words?
words_cor %>% 
  filter(item1 %in% top_9) %>% 
  mutate(direction = ifelse(correlation > 0, 1, 0)) %>% 
  group_by(item1, direction) %>% 
  # Using top_n with grouped data will allow us to take 6 within each group
  top_n(n = 6, wt = abs(correlation)) %>% 
  ungroup() %>% 
  mutate(item1 = str_to_title(item1),
         item2 = str_to_title(item2)) %>% 
  mutate(item2 = reorder_within(item2, correlation, item1)) %>%
  ggplot(aes(x = correlation, y = item2)) + 
  geom_segment(aes(x = 0, xend = correlation, y = item2, yend = item2, 
                   color = correlation > 0)) +
  geom_point(aes(color = correlation > 0)) +
  facet_wrap(~ item1, scales = "free_y") + 
  theme_minimal() + 
  scale_color_westeros(palette = "Targaryen") +
  labs(title = "Which words are most and least likely to appear within the same description of the 9 most common words?",
       x = "Correlation", 
       y = " ") + 
  scale_y_reordered() + 
  theme(
    strip.text = element_text(face = 'bold', hjust = 0),
    plot.caption = element_text(face = 'italic'),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_blank(),
    legend.position = "none") 


#' 
## ----text_regression----------------------------------------------------------------
# Try to do text regression 
words_for_regression <- words %>%
  # Want a lot of words, but don't want only numbers (i.e. 10 or 2016)
  distinct(word, id) %>% 
  add_count(word) %>% 
  filter(n >= 250,
         str_detect(word, "[a-z]"))
  # Have about 1350 words

sparse_matrix <- words_for_regression %>% 
  # Create a sparse matrix where every row is an id and column is a word
  cast_sparse(id, word)

# Create our y vector
index <- rownames(sparse_matrix) %>% as.integer()
# Line up the points with the wine_ratings matrix
points <- wine_ratings$points[index]

# Create a model 
# We could've added price as an estimator but I wanted this to be only a text 
# regression model
model_sparse <- glmnet(sparse_matrix, points)


# What does our model look like?
model_sparse %>% 
  tidy() %>% 
  arrange(desc(lambda)) 
# Here lambda is the penalty, the higher the lambda to fewer the terms we have 
# for our model. This can help us pick an optimal model so we don't overfit. 
# As we can see, with each step, or change in lambda, we increase the number of 
# predictors (or they stay the same, but never decrease)

# Let's take a few words and see how they effect the wine ratings as a function
# of lambda
model_sparse %>% 
  tidy() %>% 
  filter(term %in% c("rich", "black", "simple", "vineyard", "concentrated", 
                     "complex", "age", "structured", "dark", "elegant")) %>% 
  mutate(term = str_to_title(term),
         term = fct_reorder(term, -estimate)) %>% 
  ggplot(aes(lambda, estimate)) +
  geom_line(aes(color = term)) +
  # Normally want a log scale for penalty
  scale_x_log10() + 
  dark_theme_classic() + 
  scale_color_igv() + 
  labs(title = "How does the penalty term effect the point estimate of the terms?",
       x = "Penalty (Lambda) - Log Scale", 
       y = "Point Estimate", 
       color = "Term") + 
  geom_hline(linetype = 2, color = "red", yintercept = 0) 

# As we can see, 'simple' is a negative term whereas the rest of the words
# are generally positive
# Also, not all the words start at the same penalty because when the penalty is too
# high, some words are not taken into consideration

# Pick the optimal lambda (cross validation)
# Takes a while to run
cv_sparse_model <- cv.glmnet(sparse_matrix, points)
plot(cv_sparse_model)
# Since the MSE is always decreasing, we see that we don't overfit with more 
# predictors
# The left-most black dotted line is where the MSE is at it's minimum so it does
# go upwards a little bit but we will take the model with the most predictors
# so the next graphic has the most predictors as possible (normally this is
# probably not a good idea)

final_model <- cv_sparse_model$glmnet.fit %>% 
  tidy() %>% 
  filter(step == 77) # The lowest lambda value 

term_estimates <- final_model %>% 
  # We note the intercept is 84.9, this is important 
  select(term, estimate)

# How do the words effect the wine ratings? 
for_effect_plot <- wine_ratings %>% 
  select(id, description, title, points) %>%
  unnest_tokens(word, description) %>% 
  anti_join(stop_words, by = "word") %>% 
  # Get rid of some more words
  filter(! word %in% c("wine", "drink", "flavors")) %>%
  mutate(header = paste0(str_trunc(title, 25), " (Points: ", points, ")")) %>% 
  rename("term" = "word")

# See how the words effect the ratings for random wines
random_wine_df <- for_effect_plot %>% 
  filter(id %in% sample(for_effect_plot$id, 6)) %>% 
  left_join(., term_estimates, by = "term") %>% 
  # Remove the words that don't have an estimate 
  filter(!is.na(estimate))
random_wine_df %>% 
  mutate(term = str_to_title(term),
         term = reorder_within(term, estimate, header)) %>%
  ggplot(aes(estimate, term)) +
  geom_col(aes(fill = estimate > 0)) + 
  facet_wrap(~ header, scales = "free_y") + 
  scale_y_reordered() + 
  theme_minimal() + 
  scale_fill_westeros(palette = "Targaryen") + 
  theme(
    strip.text = element_text(face = 'bold', hjust = 0),
    plot.caption = element_text(face = 'italic'),
    panel.grid.major = element_line('white', size = 0.5),
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.ontop = TRUE, 
    legend.position = "none"
  ) + 
  labs(title = "How do the words used to describe the wines by experts effect the wine ratings?",
       subtitle = "Baseline rating is 84.9",
       x = "Effect to Wine Rating", 
       y = " ")

# For some reason the reorder_within isn't working

# What are the most positive and most negative words used to describe wine?
term_estimates %>% 
  filter(term != "(Intercept)") %>% 
  mutate(direction = ifelse(estimate > 0, 1, 0)) %>% 
  group_by(direction) %>% 
  top_n(n = 12, wt = abs(estimate)) %>% 
  mutate(term = str_to_title(term),
         term = fct_reorder(term, estimate)) %>% 
  ggplot(aes(x = estimate, y = term)) + 
  geom_col(aes(fill = estimate > 0)) +
  theme_minimal() + 
  scale_fill_westeros(palette = "Targaryen") + 
  theme(
    strip.text = element_text(face = 'bold', hjust = 0),
    plot.caption = element_text(face = 'italic'),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.ontop = TRUE, 
    legend.position = "none"
  ) +
  labs(title = "What are the most positive and most negative words used to describe wine?",
       x = "Effect to wine rating", 
       y = " ") + 
  scale_x_continuous(limits = c(-2, 3), 
                     breaks = c(-2, -1, 0, 1, 2, 3))

#' 
