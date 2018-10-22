library(tidyverse)
library(tidytext)
library(geniusR)


artist <- "the beatles"
albums <- c("please please me", "with the beatles", "a hard day's night", "beatles for sale",
            "help","rubber soul","revolver","sgt pepper's lonely hearts club band", "magical mystery tour",
            "the beatles (the white album)","yellow submarine", "abbey road", "let it be") 

search_parameters <- tibble(
  artist = rep(artist,length(albums)),
  albums = albums
)

album_lyrics <- search_parameters %>% 
  mutate(tracks = map2(artist, albums, genius_album))


lyrics <- album_lyrics %>%
  rename(album_title = albums) %>%
  unnest(tracks)

write_csv(lyrics,"tidytext/data/beatles_lyrics.csv")

tidy_lyrics <- lyrics %>%
  unnest_tokens(output = word, input = lyric)

write_csv(tidy_lyrics,"tidytext/data/beatles_tidy_lyrics.csv")
