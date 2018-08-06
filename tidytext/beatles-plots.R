library(tidyverse)
library(tidytext)
library(geniusR)

lyrics <- read_csv("tidytext/data/beatles_lyrics.csv")


top_words <- lyrics %>%
  filter(!is.na(word)) %>%
  anti_join(stop_words) %>%
  count(album_title,word,sort = TRUE)



top_words %>%
  filter(!is.na(word)) %>%
  group_by(album_title) %>%
  top_n(10) %>%
  mutate(word = fct_inorder(word,ordered = NA)) %>%
  ungroup() %>%
  ggplot(aes(x=word,y=n)) +
  geom_col() + 
  facet_wrap(~album_title, scales = "free") + 
  coord_flip()

relevant_words <- top_words %>%
  bind_tf_idf(word,album_title,n) %>%
  arrange(desc(tf_idf))


relevant_words %>%
  mutate(word = factor(word, levels = rev(unique(word)))) %>%
  group_by(album_title) %>%
  top_n(10) %>%
  ungroup() %>%
  ggplot(aes(word,tf_idf)) + 
  geom_col(show.legend = FALSE) + 
  facet_wrap(~album_title, scales = "free") + 
  coord_flip()



