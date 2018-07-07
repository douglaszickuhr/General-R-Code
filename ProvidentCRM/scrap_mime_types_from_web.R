library(rvest)
library(tidyverse)
library(DBI)
library(RMySQL)

mime <- read_html("https://www.freeformatter.com/mime-types-list.html")

mime_types <- mime %>%
  html_nodes("table") %>%
  .[[1]] %>%
  html_table() %>%
  select(c("MIME Type / Internet Media Type","File Extension")) %>%
  mutate(SUGAR_MODULE = "v10/Documents",
         SUGAR_FIELD = "file_mime_type") %>%
  rename(SUGAR_VALUE = "MIME Type / Internet Media Type",
         EXTERNAL_VALUE = "File Extension") %>%
  mutate(SUGAR_VALUE = str_replace_all(string = SUGAR_VALUE,
                                       pattern = "\\,", 
                                       replacement = ""),
         EXTERNAL_VALUE = str_replace_all(string = EXTERNAL_VALUE,
                                          pattern = "\\.",
                                          replacement = "")) %>%
  select(SUGAR_MODULE,
         SUGAR_FIELD,
         SUGAR_VALUE,
         EXTERNAL_VALUE)


con <- dbConnect(RMySQL::MySQL(), 
                 host = "127.0.0.1",
                 user = "root",
                 password = "root",
                 port = 8889,
                 dbname = "test")

dbWriteTable(con,
             name = "LOOKUP",
             mime_types,
             row.names = F,
             append = T)
