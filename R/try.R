library(tidyverse)
library(lubridate)
library(stringr)
source('R/clean.R')

df <- read_tsv('data-raw/signif.txt.tsv')
df <- df %>%
  eq_clean_location %>%
  eq_clean_date %>%
  select(date, DEATHS, EQ_PRIMARY, COUNTRY, LOCATION_NAME) %>%
  na.omit %>%
  setNames(c('date', 'deaths', 'scale', 'country', 'location'))


congo <- df %>%
  filter(country == 'CONGO')
dd <- df %>%
  filter(country %in% c('CONGO', 'CROATIA'))


ggplot(dd, aes(date = date,
               xmin = as.Date('1960-09-22'),
               xmax = as.Date('2005-12-05'),
               y = country,
               colour = deaths,
               fill = deaths,
               size = scale,
               location = location)) +
  geom_timeline() +
  geom_timeline_label() +
  theme_timeline
