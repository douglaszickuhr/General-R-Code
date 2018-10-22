library(geniusR)
library(rvest)

hey <- read_html("https://www.vagalume.com.br/the-beatles/hey-jude.html")
hey %>%
  html_nodes("#lyr_original") %>%
  html_text() %>%
  .[[1]] %>%
  as.tibble() %>%
  unnest_tokens(word,value) %>%
  count(word,sort = TRUE)

heyjude <- geniusR::genius_lyrics("the beatles","hey jude")
heyjude %>%
  unnest_tokens(word,lyric) %>%
  count(word, sort = TRUE)
