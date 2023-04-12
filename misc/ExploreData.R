library(tidyverse)

library(hms)
library(jsonlite)
library(lubridate)

raw = read_file('sample.txt')

cooked = fromJSON(raw)[["powerDetails"]][["meters"]] |>
  unnest(values) |> 
  mutate(date=as_datetime(date))

cooked |> 
  filter(!is.na(value)) |> 
  mutate(day=date(date), time=as_hms(date)) |> 
  ggplot(aes(time, value, color=day, group=day)) +
    geom_line() +
 #   facet_wrap(~day, ncol=1) +
    theme_minimal()
