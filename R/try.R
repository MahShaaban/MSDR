library(tidyverse)
library(lubridate)
library(stringr)
source('R/clean.R')

df <- read_tsv('data-raw/signif.txt.tsv')
names(df)
df <- df %>%
  eq_clean_date() %>%
  select(date, DEATHS, EQ_PRIMARY, COUNTRY, LOCATION_NAME) %>%
  na.omit %>%
  setNames(c('date', 'deaths', 'scale', 'country', 'location'))


congo <- df %>%
  filter(country == 'CONGO')
dd <- df %>%
  filter(country %in% c('CONGO', 'CROATIA'))

dd %>%
  filter(date >= '1960-09-22 ', date <= '2005-12-05')


ggplot(dd, aes(date = date,
                  xmin = as.Date('1960-09-22'),
                  xmax = as.Date('2005-12-05'),
                  y = country,
                  colour = deaths,
                  size = scale,
                  location = location)) +
  geom_timeline() +
  geom_timeline_label() +
  theme_classic() +
  labs(y = '', x = 'DATE') +
  theme(axis.line.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank())
