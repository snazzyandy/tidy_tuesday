# Get the Data

# Read in with tidytuesdayR package 
# Install from CRAN via: install.packages("tidytuesdayR")
# This loads the readme and all the datasets for the week of interest

# Either ISO-8601 date or year/week works!

tuesdata <- tidytuesdayR::tt_load('2021-07-20')
tuesdata <- tidytuesdayR::tt_load(2021, week = 30)

drought <- tuesdata$drought

# Or read in the data manually

drought <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-07-20/drought.csv')

library(tidyverse)
library(usmap)
library(gganimate)

options(scipen=999)

# Base explore -------------------------------------------------------------
# Whats the go with the drought levels: The Descriptor in the dictionary
# Drought level (None, DO, D1, D2, D3, D4) which corresponds to 
# no drought, abnormally dry, moderate drought, severe drought, extreme drought or exceptional drought.

drought_distinct <- drought %>% 
  group_by(drought_lvl) %>% 
  count()


#Distinct One datapoint for each state to start
Pop_test <- drought %>% 
  group_by(state_abb) %>% 
#  filter(drought_lvl == "None") %>% 
#  distinct(state_abb, pop_total) %>% 
  mutate(max_pop = max(pop_total)) %>% 
  mutate(max_pop_int = as.integer(max_pop)) %>% 
  mutate(state = state_abb)

# maybe do some gganimate over the map?
# How responsive are drought restrictions to low reservoir levels
# We should assume that the more drought prone areas will be more proactive? Maybe California Idk how America works
# Lets have a punt with the library US map based off the tutorial https://cran.r-project.org/web/packages/usmap/vignettes/mapping.html

Population_Map<- plot_usmap(data = Pop_test, values = "area_pct" ,regions = "states") + 
  labs(title = "US states",
       subtitle = "This is a population map of the United States.") + 
  theme(panel.background = element_rect(color = "black", fill = "lightblue"))

Population_Map


# Cool the maps kicking out! Time to animate----------------------------------------------


