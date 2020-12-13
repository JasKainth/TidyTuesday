# TidyTuesday

## Summary

TidyTuesday is a weekly social data project that focusses on understanding how to summarize and arrange data to make meaningful plots using  ````dplyr```` , ````tidyr```` , ````ggplot2```` & other tools in the ````tidyverse```` universe.

For each dataset, we include a file with the code (.R file), which can be run using [RStudio](https://rstudio.com/products/rstudio/download/) or command line. See [How to Run](#how-to-run).

# How to Run  

## Dependencies

[Install R](https://cran.rstudio.com/)

## Run via Command Line

After installing R, you can run a script in terminal, like so:
```shell
R < european_energy/script.R --no-save
```

# Datasets  

## Women of 2020  
This data comes from [BBC](https://www.bbc.com/news/world-55042935).  

For this data, BBC has revealed its list of 100 inspiring and influential women from around the world for 2020. We take a look at which categories do women from different categories excel in. 

![Women Plot](https://github.com/JasKainth/TidyTuesday/blob/master/women_of_2020/women_plot.jpg)

## European Energy Dataset  
The data comes from [Eurostat](https://ec.europa.eu/eurostat/statistics-explained/index.php/Electricity_generation_statistics_%E2%80%93_first_results).

For this data, we explore the energy supplied and produced by the countries in the European Union. Also, we attempt to examine which countries are trying to be more green and also which types of energy is becoming more/less popular from a period of 2016 to 2018.  

The following map shows energy produced by the countries in Europe over 3 years, seperated by Renewable and Non-Renewable energy.

![European Energy Map](https://github.com/JasKainth/TidyTuesday/blob/master/european_energy/european_energy_map.jpg)  

## Canadian Wind Turbine Dataset  
The data comes from the [Government of Canada](https://open.canada.ca/data/en/dataset/79fdad93-9025-49ad-ba16-c26d718cc070).  
For this data, we explore the wind turbines which are commissioned in Canada. We see which provinces are attempting to create more energy using wind energy.  

The following plot shows a relationship between the energy capacity of all the wind turbines in the project with respect to when the first wind turbine of that project was commissioned, and showing how many turbines are in that project.

![Wind Turbine Plot](https://github.com/JasKainth/TidyTuesday/blob/master/canadian_wind_turbine/wind_turbine.jpg) 

Later, we also attempt to predict the capacity of each turbine, using a decision tree. The following is a visualization of the decision tree.  
Note: For this visualization, we can only use two predictors so, we picked the two more important predictors.  

![Decision Tree Plot](https://github.com/JasKainth/TidyTuesday/blob/master/canadian_wind_turbine/decision_tree.jpg)  


## Friends Dataset

This data comes from the ```friends``` [R package](https://github.com/EmilHvitfeldt/friends).

For this data, we look at the ratings and viewers for the show. Also, later we examine the correlation among the emotions of utterance (line of dialogue) in scenes and also the correlation among which *friends* are in scenes together.

The following plot compares the episode rating to the average rating of the season.  

![Friends Plot](https://github.com/JasKainth/TidyTuesday/blob/master/friends/friends_avg_ratings.jpg)

## Wine Ratings Dataset  

This data comes from [Kaggle](https://www.kaggle.com/zynicide/wine-reviews).  

For this data, we explore wine ratings and attempt to discover what factors might play role in the wine's rating. We use a text regression to determine the effect of each word in the description of the wine. 

The plot below shows the effect of each word to the overall rating of the word for six different wines.  

![Wine Ratings Word Plot](https://github.com/JasKainth/TidyTuesday/blob/master/wine_ratings/wine_rating_term_effect.jpg)

The plot below is a netword plot showing which words often appear together in the same wine description. 

![Wine Ratings Network Plot](https://github.com/JasKainth/TidyTuesday/blob/master/wine_ratings/wine_ratings_network_plot.jpg)

## Datasaurus Dataset

This data comes from [Albert Cairo](http://www.thefunctionalart.com/2016/08/download-datasaurus-never-trust-summary.html).

This dataset signifies the importance of plotting data rather than relying on common statistics. For this data, we plot the datasets and visually see the differences in the datasets. Also, we create a heatmap to see where the points are concentrated to impact the statistics.  

Below is a plot of all the datasets.  

![Full dataset Plot](https://github.com/JasKainth/TidyTuesday/blob/master/datasauraus/all_plots.jpg)

Below is a gif of the datasets, showing the summary statistics for each dataset. Notice how similar the values are as we go from one dataset to the other.   

![Datasaurus gif](https://github.com/JasKainth/TidyTuesday/blob/master/datasauraus/datasaurus.gif)
