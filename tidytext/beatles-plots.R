library(tidyverse)
library(tidytext)
library(geniusR)

lyrics <- read_csv("tidytext/data/beatles_lyrics.csv")

x <- c("please please me", "with the beatles", "a hard day's night", "beatles for sale",
       "help","rubber soul","revolver","sgt pepper's lonely hearts club band", "magical mystery tour",
       "the beatles (the white album)","yellow submarine", "abbey road", "let it be") 

lyrics <- lyrics %>%
  mutate(album_title = fct_relevel(album_title, !!!x))

lyrics %>%
  anti_join(stop_words) %>%
  count(word,sort = TRUE) %>%
  top_n(20) %>%
  ggplot() +
  geom_col(aes(x=reorder(word,n),y=n)) + 
  coord_flip()

top_words <- lyrics %>%
  filter(!is.na(word)) %>%
  anti_join(stop_words) %>%
  count(album_title,word,sort = TRUE) %>%
  arrange(album_title,desc(n))

top_words

rev(unique(top_words$word))


top_words %>%
  group_by(album_title) %>%
  top_n(10) %>%
  ggplot() +
  geom_col(aes(x=reorder(word,n),y=n)) + 
  facet_wrap(~album_title, scales = "free") + 
  coord_flip()

relevant_words <- top_words %>%
  bind_tf_idf(word,album_title,n) %>%
  arrange(desc(tf_idf))


relevant_words %>%
  group_by(album_title) %>%
  top_n(10) %>%
  ungroup() %>%
  ggplot(aes(reorder(word,tf_idf),tf_idf)) + 
  geom_col(show.legend = FALSE) + 
  facet_wrap(~album_title, scales = "free") + 
  coord_flip()



