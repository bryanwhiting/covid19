library(renv)
library(dplyr)
library(distill)
library(tidyverse)
library(magrittr)
dir_data = '~/github/covid/csse_covid_19_data/csse_covid_19_daily_reports'

# Older data is missing latitude and logitude
prep_df <- function(df){
  if (ncol(df) == 6){
    stopifnot(!('Latitude' %in% colnames(df)))
    df$Latitude = NA
    df$Longitude = NA
  }
  
  date = as.data.frame(str_split_fixed(df$`Last Update`,pattern = ' ', n = 2))[,1] %>% as.character()
  
  if (str_detect(date[1], fixed('/'))){
    df$date = mdy(date)
  } else {
    df$date = ymd(date)
  }
  return(df)
}

dfs = list.files(dir_data, full.names = T, pattern='csv$') %>% 
  lapply(., read_csv) %>%
  lapply(., prep_df) 

df = do.call(rbind, dfs)

colnames(df) %<>% 
  tolower() %>% 
  str_replace_all(fixed('/'), '_') %>%
  str_replace_all(fixed(' '), '_')

View(df)
# Todo:
# Group by country/region
# Subtract from prior day
# Get the "new" cases
#
df %>%
  filter(country_region == 'China') %>%
  group_by(date) %>%
  summarize(confirmed = sum(confirmed)) %>% View
  ggplot(aes(x = date, y = confirmed)) +
  geom_line(type = 'bar')

  
# Not sure I trust that data above
fp_data = '~/github/covid/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Confirmed.csv'
dfc = read_csv(fp_data) %>%
  gather(date_raw, confirmed, -`Province/State`, -`Country/Region`, -Lat, -Long) %>%
  mutate(date = mdy(date_raw))
colnames(dfc) %<>% 
  tolower() %>% 
  str_replace_all(fixed('/'), '_') %>%
  str_replace_all(fixed(' '), '_')
dfc

dfc %>%
  filter(country_region == 'US') %>%
  group_by(date) %>%
  summarize(confirmed = sum(confirmed)) %>% 
  ggplot(aes(x = date, y = confirmed)) +
  geom_line(type = 'bar') +
  ggtitle('US')



# Day since 1
df_tmp <- dfc %>%
  group_by(country_region, date) %>%
  summarize(confirmed = sum(confirmed)) %>%  
  arrange(country_region, date) %>%
  filter(confirmed > 0) %>%
  mutate(day_since1 = row_number()) %>%
  ungroup() %>%
  #filter(country_region %in% c("China", 'US', 'Italy', 'France', 'Iran')) %>% #View
  highlight_key(~country_region) 
  
p <- ggplot(df_tmp, aes(x=day_since1, y=confirmed, color=country_region)) +
  geom_line() +
  ggtitle('Days Since first onset') + 
  theme(legend.position = "none") 

gg <- ggplotly(p)
highlight(gg, 'plotly_hover', 
          selectize = T, 
          dynamic=T,
          defaultValues = c('China', 'Italy', 'US'))
