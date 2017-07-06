library(tidyverse)
library(lubridate)
library(stringr)
source('R/clean.R')

df <- read_tsv('data-raw/signif.txt.tsv')

df <- df %>%
  eq_clean_date() %>%
  select(date, DEATHS, EQ_PRIMARY, COUNTRY) %>%
  na.omit %>%
  setNames(c('date', 'deaths', 'scale', 'country'))


congo <- df %>%
  filter(country == 'CONGO')
dd <- df %>%
  filter(country %in% c('CONGO', 'CROATIA'))

dd %>%
  filter(date >= '1960-09-22 ', date <= '2005-12-05')


ggplot(dd, aes(date = date, xmin = as.Date('1960-09-22'), xmax = as.Date('2005-12-05'),
               y = country, colour = deaths, size = scale)) +
  geom_timeline() +
  theme(legend.position = 'top')

