#' ---
#' title: "Friends"
#' author: "Jas Kainth"
#' date: "07/09/2020"
#' output: pdf_document
#' editor_options: 
#'   chunk_output_type: console
#' ---
#' 
## ----setup, include=FALSE-----------------------------------------------------------
knitr::opts_chunk$set(echo = FALSE)
library(tidyverse)
library(lubridate)
library(ggdark)
library(ggsci)
library(glue)
library(widyr)
library(friends)
library(tidylo)
library(tidytext)

#' 
## -----------------------------------------------------------------------------------
tuesdata <- tidytuesdayR::tt_load('2020-09-08')
friends <- tuesdata$friends
info <- tuesdata$friends_info
emotions <- tuesdata$friends_emotions
entity <- friends_entities

#' 
## -----------------------------------------------------------------------------------
info %>% 
  ggplot() +
  geom_line(aes(x = air_date, y = imdb_rating, color = factor(season)), 
            alpha = 0.5) +
  geom_point(aes(x = air_date, y = imdb_rating, color = factor(season))) + 
  dark_theme_classic() + 
  scale_color_simpsons() + 
  labs(x = "", 
       y = "IMDb Rating", 
       title = "Rating for Each Episode of 'Friends'",
       subtitle = "Different color represents different season") + 
  theme(legend.position = 'none',
        strip.text = element_text(face = 'bold', hjust = 0),
        plot.caption = element_text(face = 'italic')) + 
  scale_y_continuous(limits = c(7, 10), breaks = c(7, 8, 9, 10))

#' 
#' ## What are the 10 best episodes? 
#' 
## -----------------------------------------------------------------------------------
info %>% 
  arrange(-imdb_rating) %>% 
  head(10) %>% 
  mutate(name = glue("{ title } ({ season }.{ episode })"), 
         name = factor(name, levels = name),
         name = fct_reorder(name, imdb_rating)) %>% 
  ggplot() + 
  geom_point(aes(y = name, x = imdb_rating, color = factor(season))) + 
  geom_segment(aes(y = name, yend = name, x = 7, xend = imdb_rating, 
               color = factor(season))) + 
  scale_x_continuous(limits = c(7, 10), breaks = c(7, 8, 9, 10)) + 
  theme_classic() + 
  labs(title = "What are the 10 best episodes in Friends?", 
       x = "IMDb Rating", 
       y = "") +
  scale_color_uchicago(palette = "dark") + 
  theme(legend.position = "none")

#' # Comparing the episode rating to their season average
## -----------------------------------------------------------------------------------
info <- info %>% 
  group_by(season) %>% 
  summarise(season_avg = mean(imdb_rating)) %>% 
  left_join(., info, by = "season") %>% 
  ungroup() 
info %>% 
  group_by(season) %>% 
  summarise(x = min(air_date), 
            xend = max(air_date)) %>% 
  left_join(., info, by = "season") %>% 
  ungroup() %>% 
  ggplot() + 
  geom_segment(aes(x = air_date, xend = air_date, 
                   y = season_avg, yend = imdb_rating), 
               color = "lightgrey") + 
  geom_segment(aes(x = x, xend = xend, y = season_avg, yend = season_avg,
                   color = factor(season)), size = 1.5) +
  geom_point(aes(x = air_date, y = imdb_rating, color = factor(season))) + 
  geom_text(data = subset(info, imdb_rating < 8 | imdb_rating > 9), 
            aes(x = air_date, y = imdb_rating, label = title, 
                color = factor(season)), 
            check_overlap = TRUE, vjust = 1, hjust = 1.04, size = 2) + 
  theme_classic() + 
  theme(legend.position = "none", 
        axis.text.x = element_blank(), 
        axis.ticks = element_blank()) + 
  scale_y_continuous(limits = c(7, 10)) +
  labs(title = "How do the episode ratings compare to their season average?", 
       y = "IMDb Rating",
       x = " ") + 
  scale_color_futurama()

#' 
#' ## What about viewers?
## -----------------------------------------------------------------------------------
info %>% 
  ggplot() + 
  geom_line(aes(x = air_date, y = us_views_millions, color = factor(season)), 
            alpha = 0.6) +
  geom_point(aes(x = air_date, y = us_views_millions, color = factor(season))) +
  dark_theme_classic() + 
  scale_color_simpsons() + 
  labs(x = "", 
       y = "US Viewers (Millions)", 
       title = "Viewers for Each Episode of 'Friends'",
       subtitle = "Different color represents different season") + 
  theme(legend.position = 'none',
        strip.text = element_text(face = 'bold', hjust = 0),
        plot.caption = element_text(face = 'italic')) + 
  scale_y_continuous(limits = c(15, 55), breaks = c(15, 25, 35, 45, 55))

#' 
## -----------------------------------------------------------------------------------
info %>% 
  arrange(-us_views_millions) %>% 
  head(10) %>% 
  mutate(name = glue("{ title } ({ season }.{ episode })"), 
         name = factor(name, levels = name),
         name = fct_reorder(name, us_views_millions)) %>% 
  ggplot() + 
  geom_point(aes(y = name, x = us_views_millions, color = factor(season))) + 
  geom_segment(aes(y = name, yend = name, x = 25, xend = us_views_millions,
                   color = factor(season))) + 
  theme_classic() + 
  labs(title = "What are the 10 most viewed episodes in Friends?", 
       x = "Views (Millions)", 
       y = "") + 
  scale_x_continuous(limits = c(25, 55), breaks = c(25, 35, 45, 55)) +
  scale_color_uchicago(palette = "dark") + 
  theme(legend.position = "none")

#' 
#' ## How about writters and directors? 
## -----------------------------------------------------------------------------------
`%not_in%` <- purrr::negate(`%in%`)
info %>% 
  separate(directed_by, into = c("d1", "d2"), sep = "&") %>% 
  filter(!grepl("Story", written_by, fixed = TRUE)) %>% 
  separate(written_by, into = c("w1", "w2", "w3", "w4"), sep = "&")

# Needs some cleaning

#' 
## -----------------------------------------------------------------------------------
# What are the most common moods in the first 4 seasons
emotions %>% 
  group_by(season, emotion) %>% 
  summarise(total = n()) %>% 
  mutate(emotion = fct_reorder(emotion, total)) %>% 
  ggplot() + 
  geom_col(aes(x = season, y = total, fill = emotion), position = 'fill') +
  dark_theme_classic() + 
  scale_fill_uchicago(palette = "dark") + 
  labs(title = "How do the speech emotions vary over the first 4 seasons?", 
       x = "Season", 
       y = "", 
       fill = "Emotion") +
  scale_y_continuous(labels = scales::percent) +
  theme(strip.text = element_text(face = 'bold', hjust = 0),
        plot.caption = element_text(face = 'italic'))

#' 
#' # Which emotions are often together in the same scene? 
#' 
## -----------------------------------------------------------------------------------
emotions %>% 
  mutate(name = glue("{ season }.{ episode }.{ scene }")) %>% 
  pairwise_cor(emotion, name, sort = TRUE) %>% 
  rowid_to_column() %>% 
  filter(rowid %% 2 == 0) %>% 
  select(-1) %>% 
  mutate(pair = glue("{ item1 } - { item2 }"),
         pair = fct_reorder(pair, correlation)) %>% 
  ggplot() +
  geom_point(aes(y = pair, x = correlation, color = correlation < 0)) + 
  geom_segment(aes(y = pair, yend = pair, x = 0, xend = correlation, 
               color = correlation < 0)) +
  dark_theme_classic() +
  scale_color_simpsons() + 
  theme(legend.position = 'none',
        strip.text = element_text(face = 'bold', hjust = 0),
        plot.caption = element_text(face = 'italic')) +
  labs(title = "Correlation Among Emotions in the Same Scene", 
       x = "Correlation", 
       y = "Emotions") + 
  scale_x_continuous(limits = c(-0.2, 0.3), 
                     breaks = c(-0.2, -0.1, 0, 0.1, 0.2, 0.3)) 
  

#' 
#' ## Which character speaks the most? 
## -----------------------------------------------------------------------------------
six <- c("Rachel Green", "Ross Geller", "Chandler Bing", "Joey Tribbiani",
         "Phoebe Buffay", "Monica Geller")
friends %>% 
  filter(speaker %in% six) %>% 
  separate(speaker, into = c("name", "Last_Name"), sep = " ") %>% 
  count(name) %>% 
  mutate(name = fct_reorder(name, n)) %>% 
  ggplot() + 
  geom_col(aes(x = n, y = name, fill = name)) + 
  theme_minimal() + 
  scale_fill_futurama() + 
  theme(legend.position = 'none',
        strip.text = element_text(face = 'bold', hjust = 0),
        plot.caption = element_text(face = 'italic'),
        panel.grid.major = element_line('white', size = 0.5),
        panel.grid.minor = element_blank(),
        panel.grid.major.y = element_blank(), 
        panel.ontop = TRUE) + 
  labs(title = "Which Character has the Most Lines?", 
       x = "# of Lines", 
       y = "") + 
  scale_x_continuous(limits = c(0, 10000), 
                     breaks = c(0, 2500, 5000, 7500, 10000))

#' 
## -----------------------------------------------------------------------------------
a <- info %>% 
  mutate(name = glue("{ season }.{ episode }")) %>% 
  select(name, air_date)
b <- friends %>% 
  filter(speaker %in% six) %>% 
  separate(speaker, into = c("First", "Last_Name"), sep = " ") %>% 
  mutate(name = glue("{ season }.{ episode }")) 
c <- left_join(a, b, by = "name")

#' 
#' 
## -----------------------------------------------------------------------------------
c %>% 
  group_by(First, air_date) %>% 
  summarise(total = n(),
            season = season, 
            episode = episode) %>% 
  ungroup() %>% 
  ggplot() +
  geom_line(aes(x = air_date, y = total, color = factor(season)), alpha = 0.5) +
  geom_point(aes(x = air_date, y = total, color = factor(season)),
             alpha = 0.3) +
  facet_wrap(~ First) + 
  labs(title = "How Much Does Each Character Talk per Episode?", 
       x = "", 
       y = "Total Lines", 
       subtitle = "Seperated by Season") +
  scale_color_simpsons() + 
  dark_theme_minimal() + 
  scale_x_date(breaks = c()) + 
  theme(legend.position = "none",
    strip.text = element_text(hjust = 0),
    plot.caption = element_text(face = 'italic'),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_blank()
  ) + 
  scale_y_continuous(limits = c(0, 125), breaks = c(0, 25, 50, 75, 100, 125))
  

#' 
#' 
#' ## Which characters are most likely to be on screen together? 
## -----------------------------------------------------------------------------------
c %>% 
  mutate(fullscene = glue("{ name }.{ scene }")) %>% 
  pairwise_cor(First, fullscene, sort = TRUE) %>% 
  ggplot() + 
  geom_col(aes(x = correlation, y = item2, fill = item2)) + 
  facet_wrap(~ item1, scales = "free_y") +
  dark_theme_minimal() + 
  scale_fill_uchicago() + 
  theme(legend.position = 'none',
        strip.text = element_text(hjust = 0),
        plot.caption = element_text(face = 'italic'),
        panel.grid.major = element_line('black', size = 0.5),
        panel.grid.minor = element_blank(),
        panel.grid.major.y = element_blank(), 
        panel.ontop = TRUE) +
  scale_x_continuous(limits = c(-0.1, 0.4), 
                     breaks = c(-0.1, 0, 0.1, 0.2, 0.3, 0.4)) + 
  labs(title = "Which Characters are Most Likely to be in a Scene Together?",
       x = "Correlation", 
       y = "")

#' 
## -----------------------------------------------------------------------------------
## Are there words that some friends (i.e the main 6) are more likely to say
## than others? 

words <- friends %>% 
  select(speaker, text, season, episode) %>% 
  unnest_tokens(word, text) %>% 
  anti_join(stop_words, by = "word") %>% 
  filter(speaker %in% six)
  # There are still some filler words that we might want to remove 
  # like 'yeah', 'hey' & 'gonna' but removing them would take too long 
  # so leave them in for now
words %>% 
  count(speaker, word) %>% 
  # Get rid of some words that barely come up
  filter(n > 50) %>% 
  # Fits the log odds with a prior
  # Uses emperical bayes
  # Don't really know what it does though
  bind_log_odds(speaker, word, n) %>% 
  group_by(speaker) %>% 
  top_n(15, log_odds_weighted) %>% 
  ungroup() %>% 
  separate(speaker, into = c("First", "Last"), sep = " ") %>% 
  mutate(word = str_to_title(word),
         word = reorder_within(word, log_odds_weighted, First)) %>% 
  # For the plot we probably would want odds rather than log-odds 
  # to make it a bit more interpretable but I don't know which base the log
  # is for the column provided by bind_log_odds (my guess is base e but I will
  # just leave it as log-odds)
  ggplot() + 
  geom_col(aes(x = log_odds_weighted, y = word, fill = First)) +
  facet_wrap(~ First, scales = "free") + 
  scale_y_reordered() +
  labs(title = "Which words are some characters more likely to use than others?",
       x = "Log-Odds", 
       y = " ") + 
  dark_theme_minimal() + 
  theme(legend.position = 'none',
        strip.text = element_text(hjust = 0),
        plot.caption = element_text(face = 'italic'),
        panel.grid.major = element_line('black', size = 0.5),
        panel.grid.minor = element_blank(),
        panel.grid.major.y = element_blank(), 
        panel.ontop = TRUE) + 
  scale_fill_uchicago()

#' 
#' 
#' 
