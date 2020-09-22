# TidyTuesday
Fun data visualizations using the TidyTuesday datasets 

In these files, I will post some of my data visualizations from the R for Data Science: TidyTuesday project.
All the datasets are from that project and can be accessed from this URL: https://github.com/rfordatascience/tidytuesday.  

TidyTuesday is a weekly social data project that focusses on understanding how to summarize and arrange data to make meaningful plots using  ````dplyr```` , ````tidyr```` , ````ggplot2```` & other tools in the ````tidyverse```` universe.  
For each dataset, I will include a file with the code (.Rmd file) along with one plot which is used for the README file. To view the rest of the plots please run the code on your computer. 

## Friends (2020/ Week 37)
The data this week comes from ```friends``` R package (https://github.com/EmilHvitfeldt/friends).  
Credit for code: Jas Kainth  
For this data, I looked at the ratings and viewers for the show. Also, later I examined the correlation among the emotions of utterance (line of dialogue) in scenes and also the correlation among which *friends* were in scenes together.  
The following is a plot that compares the episode rating to the average rating of the season.  


![Friends Plot](https://github.com/JasKainth/TidyTuesday/blob/master/Friends/friends_avg_ratings.jpg)


## European Energy (2020/ Week 32)  
The data this week comes from Eurostat (https://ec.europa.eu/eurostat/statistics-explained/index.php/Electricity_generation_statistics_%E2%80%93_first_results)  
Credit for code: Jas Kainth  
For this weeks data, I explored the energy supplied and produced by the countries in the EU. Also, we try to see which countries are trying to be more green and also which types of energy is becoming more/less popular from a period of 2016 to 2018. 
The following is a map of energy produced by the countries in Europe over the 3 years, seperated by Renewable and Non-Renewable energy.

![European Energy Map](https://github.com/JasKainth/TidyTuesday/blob/master/European%20Energy/european_energy_map.jpg)
