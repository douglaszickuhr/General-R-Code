library(tidyverse)

file <- "https://github.com/rfordatascience/tidytuesday/raw/master/data/week19_airline_safety.csv"
download.file(file, destfile = "tidytuesday/data/week19_airline_safety.csv")


airlines <- read_csv("tidytuesday/data/week19_airline_safety.csv")

levels(factor(airlines$type_of_event))

airlines %>%
  filter(type_of_event == "fatalities") %>%
  mutate(n = n_events/avail_seat_km_per_week) %>%
  arrange(airline) %>%
  ggplot() +
  geom_line(aes(x=year_range,y=n,group=airline))
