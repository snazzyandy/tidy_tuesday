# Get the Data

# Read in with tidytuesdayR package 
# Install from CRAN via: install.packages("tidytuesdayR")
# This loads the readme and all the datasets for the week of interest

# Either ISO-8601 date or year/week works!

tuesdata <- tidytuesdayR::tt_load('2021-07-06')
tuesdata <- tidytuesdayR::tt_load(2021, week = 28)

holidays <- tuesdata$holidays

# Or read in the data manually


#Load the data
holidays <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-07-06/holidays.csv')


#Cleaning raw data
library(polite)
library(rvest)
library(tidyverse)
library(httr)
library(lubridate)
library(janitor)



url <- "https://en.wikipedia.org/wiki/List_of_national_independence_days"
url_bow <- polite::bow(url)

ind_html <-
  polite::scrape(url_bow) %>%
  rvest::html_nodes("table.wikitable") %>%
  rvest::html_table(fill = TRUE)


ind_tab <-
  ind_html[[1]][1:6] %>%
  as_tibble() %>%
  clean_names()


raw_html <- polite::scrape(url_bow) 

raw_html %>%
  # rvest::html_nodes("table.wikitable") %>%
  rvest::html_nodes("span.flagicon") %>% 
  length()
  rvest::html_table(fill = TRUE)
  
  
  # Clean data --------------------------------------------------------------
  
  ind_clean <-
    ind_tab %>%
    # Cleaning up some dates
    mutate(
      date_of_holiday = case_when(
        country == "Croatia" ~ "May 30",
        country == "Mexico" ~ "September 16",
        country == "Mongolia" ~ "December 29",
        country == "Paraguay" ~ "May 14",
        country == "Israel" ~ "May 14", # Independence Day exists within a range, but this was the original date.
        country == "Slovenia" ~ "June 25", # Slovenia has two dates; this one is "Statehood Day".
        TRUE ~ date_of_holiday
      ),
      year = str_sub(year_of_event, start = 1, end = 4),
      date_mdy = case_when(
        date_of_holiday != "" ~ paste0(date_of_holiday, ", ", year),
        TRUE ~ ""
      ),
      date_parsed = mdy(date_mdy),
      weekday = weekdays(date_parsed),
      day = day(date_parsed),
      month = month(date_parsed, label = TRUE),
      year_of_event = as.integer(year_of_event),
      year = as.integer(year)
    ) %>%
    relocate(date_parsed:month, .after = country)
  
  ind_clean %>% 
    glimpse()
  
