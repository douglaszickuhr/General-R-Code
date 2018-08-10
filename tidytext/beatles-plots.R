library(tidyverse)
library(tidytext)
library(wordcloud)
library(ggradar)
library(dplyr)
library(scales)

lyrics <- read_csv("tidytext/data/beatles_lyrics.csv")

x <- c("please please me", "with the beatles", "a hard day's night", "beatles for sale",
       "help","rubber soul","revolver","sgt pepper's lonely hearts club band", "magical mystery tour",
       "the beatles (the white album)","yellow submarine", "abbey road", "let it be") 
x <- str_to_title(x)

lyrics <- lyrics %>%
  filter(!is.na(word)) %>%
  mutate(album_title = str_to_title(album_title)) %>%
  mutate(album_title = fct_relevel(album_title, !!!x))

lyrics_no_sw <- lyrics %>%
  anti_join(stop_words)


lyrics %>%
  anti_join(stop_words) %>%
  count(word) %>%
  with(wordcloud(words = word, 
                 freq = n,
                 max.words = 100))


lyrics %>%
  anti_join(stop_words) %>%
  count(word,sort = TRUE) %>%
  top_n(20) %>%
  ggplot() +
  geom_col(aes(x=reorder(word,n),y=n)) + 
  scale_fill_viridis_d(direction = -1,
                        option = "C") +
  coord_flip() + 
  labs(x= "Word",
       y = "Count")

ordered_lyrics <- lyrics_no_sw %>%
  count(album_title,word) %>%
  group_by(album_title) %>%
  top_n(10,n) %>%
  ungroup() %>%
  arrange(album_title, desc(n)) %>%
  mutate(order = row_number())

ggplot(ordered_lyrics,
       aes(desc(order),n,fill=album_title)) + 
  geom_bar(stat = "identity", show.legend = FALSE) +
  facet_wrap(~album_title, scales = "free") +
  theme_bw() +
  scale_x_continuous(
    breaks = desc(ordered_lyrics$order),
    labels = ordered_lyrics$word,
    expand = c(0,0)
  ) +
  coord_flip() + 
  scale_fill_viridis_d(direction = -1,
                       option = "C") + 
  labs(title = "Word Frequency by Album",
       x = "Word",
       y = "Frequency")



top_words <- lyrics %>%
  filter(!is.na(word)) %>%
  anti_join(stop_words) %>%
  count(album_title,word,sort = TRUE) %>%
  arrange(album_title,desc(n))

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

nrc <- get_sentiments("nrc")

source("tidytext/radar-plot.R")

lyrics %>%
  anti_join(stop_words) %>%
  inner_join(nrc) %>%
  group_by(album_title) %>%
  mutate(album_n = n(),
         sentiment = str_to_title(sentiment)) %>%
  group_by(album_title,sentiment) %>%
  mutate(n = n()) %>%
  mutate(n = n/album_n) %>%
  summarise(n = mean(n)) %>%
  spread(sentiment,n) %>%
  ungroup()  %>%
  rename(group = album_title) %>%
  mutate_if(is.numeric,scale,center=FALSE) %>%
  as.data.frame() %>%
  radar_plot(grid.max = 1.8,
             legend.title = "Album",
             palette = scale_color_viridis_d(direction = -1,
                                             option = "C"),
             labs = labs(title="Sentiment Analaysis of Beatles's Studio Album"),
             label.gridline.min = FALSE,
             label.gridline.mid = FALSE,
             label.gridline.max = FALSE)
