# Get the Data

# Read in with tidytuesdayR package 
# Install from CRAN via: install.packages("tidytuesdayR")
# This loads the readme and all the datasets for the week of interest

# Either ISO-8601 date or year/week works!

tuesdata <- tidytuesdayR::tt_load('2021-07-13')
tuesdata <- tidytuesdayR::tt_load(2021, week = 29)

scoobydoo <- tuesdata$scoobydoo

# Or read in the data manually

scoobydoo <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-07-13/scoobydoo.csv')

options(scipen=999)
#Load libraies
library(dplyr)
library(tidyverse)
#Initial look at number of snacks... its imporant
scooby_sort <- scoobydoo %>%
  group_by(number_of_snacks) %>%
  summarise(count = n())

#How much of each snack per episode
scooby_zoink <- scoobydoo %>%
  group_by(season, zoinks) %>%
  summarise(count = n()) %>%
  mutate(zoint_numeric = as.numeric(zoinks)) %>%
  arrange(zoint_numeric) %>%
  mutate(zoink_count = count*zoint_numeric)

#Clean up the data so that I can do a regression + GGplot
#What is the weight of a scooby snack? I must research this
  # average wheel barrow 80L
  # average truck - 32,350L
Snacks_clean <- scoobydoo %>%
  select(imdb, number_of_snacks, season) %>%
    mutate(number_of_snacks_clean = case_when(
    number_of_snacks %in% c("0", "1","2","3","4","5","6","7",'8',"9","10") ~ as.numeric(number_of_snacks),
    number_of_snacks == "wheel barrel full" ~ 20,
    number_of_snacks == "truck load" ~1000,
    number_of_snacks == "several boxes" ~ 100,
    number_of_snacks == "several" ~ 10,
    number_of_snacks == "a couple" ~ 2,
    number_of_snacks == "lifetime supply" ~ 10000,
    number_of_snacks == "NULL" ~ 0,
    number_of_snacks == "1 box" ~ 10,
    number_of_snacks == "2 boxes" ~ 20,
    number_of_snacks == "3 boxes" ~30
  ))%>%
  mutate(number_of_snacks_clean_numeric = as.integer(number_of_snacks_clean)) %>%
  mutate(imdb = case_when(
    imdb == "NULL" ~ 0,
    TRUE ~ as.double(imdb)
  )) %>%
filter(imdb > 3)

#Run linear regression
snack_regress = lm(imdb ~ number_of_snacks_clean, data = Snacks_clean) 
snack_regress

glimpse(scoobydoo)

#Plot - This tells us that scooby snacks are not associated with incredible ratings
#This is a tragedy and the critics have NO clue what they're talking about!
snacks_plot<- ggplot(Snacks_clean, aes(x= number_of_snacks_clean, y = imdb, colour = season))+
              geom_point()+
              xlim(0,10)+
              facet_wrap(~season)+
              labs(title = "More Scooby Snacks == Better Rating?", subtitle = "The results will disapoint you")

snacks_plot
