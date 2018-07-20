library(tidytext)
library(janeaustenr)
library(stringr)

books <- austen_books() %>%
  group_by(book) %>%
  mutate(line = row_number(),
         chapter = cumsum(str_detect(text,"^CHAPTER .*"))) %>%
  ungroup()


tidy_books <- books %>%
  unnest_tokens(word,text)

tidy_books %>%
  anti_join(stop_words) %>%
  count(word, sort = TRUE) %>%
  top_n(15) %>%
  mutate(word = reorder(word,n)) %>%
  ggplot() + 
  geom_col(aes(x = word, y = n)) + 
  coord_flip()

    
