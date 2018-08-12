library(rvest)
library(tidyverse)
library(readxl)
library(DBI)
library(RMySQL)

mime <- read_html("https://www.freeformatter.com/mime-types-list.html")

mime %>%
  html_nodes("table") %>%
  .[[1]] %>%
  html_table() %>%
  rename(mime_type = `MIME Type / Internet Media Type`,
         file_extension = `File Extension`) %>%
  select(mime_type,file_extension) %>%
  separate(file_extension,
           into = c("ext1","ext2"),
           sep = " ",
           extra = "drop",
           remove = TRUE,
           fill = ) %>%
  gather(type,file_extension,-mime_type) %>%
  select(-type) %>%
  filter(!is.na(file_extension)) %>%
  mutate(file_extension = str_replace_all(file_extension,
                                  pattern = "[\\.\\,]",
                                  replacement = ""),
         mime_type = str_replace_all(mime_type,
                                     pattern = ",$",
                                     replacement = ""
                                     )) %>%
  filter(file_extension != "N/A") %>%
  arrange(mime_type) %>%
  as.tibble() %>%
  write_csv("ProvidentCRM/mime_types.csv")