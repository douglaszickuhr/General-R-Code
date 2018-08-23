library(tidyverse)
library(tidytext)
library(lubridate)
library(scales)
library(magrittr)

# Download files from github
walk(1:9,function(x){
  download.file(url = str_glue("https://github.com/fivethirtyeight/russian-troll-tweets/raw/master/IRAhandle_tweets_{x}.csv"),
                destfile = str_glue("tidytuesday/data/IRAhandle_tweets_{x}.csv"))
})

# Reading and returning as a dataframe
tweets <- map_df(1:9,function(x){
  read_csv(file = str_glue("tidytuesday/data/IRAhandle_tweets_{x}.csv")) %>% 
    mutate(publish_date = mdy_hm(publish_date),
           harvested_date = mdy_hm(harvested_date))
})

# Authors Activity by Day of Week and Hour
p1 <- tweets %>%
  mutate(hour = lubridate::hour(publish_date),
         week_day = lubridate::wday(publish_date,label = TRUE)) %>%
  count(hour,week_day) %>%
  ggplot() + 
  geom_tile(aes(x=week_day,
                y=hour,
                fill=n)) + 
  scale_y_continuous(breaks = 1:24) + 
  scale_fill_viridis_c(direction = -1,
                       option = "D") + 
  labs(title = "Tweeting Activity",
       subtitle = "By Day of Week and Hour",
       x = "Week day of Publishing",
       y = "Hour of Publishing",
       fill = "Number of Tweets")


# Looking up for the top 20 authors where the language is English 
top20_authors <- tweets %>%
  filter(retweet == 0 & language == "English") %>%
  count(author, sort = TRUE) %>% 
  top_n(20)

# Top 20 Twitter Trolls Historical Activity
p2 <- tweets %>%
  semi_join(top20_authors) %>%
  mutate(publish_date = floor_date(publish_date,unit = "week")) %>% 
  count(publish_date, account_category) %>%
  ggplot() + 
  geom_area(aes(x = publish_date,
                y = n,
                group = account_category,
                fill = account_category),
            alpha = 0.7) + 
  scale_x_datetime(breaks = date_breaks(width = "1 month"),
                   labels = date_format("%b/%Y")) + 
  theme(axis.text.x = element_text(angle=45,
                                   hjust = 1)) + 
  scale_fill_viridis_d(direction = -1,
                       option = "D") + 
  labs(title = "Top 20 Twitter Trolls Activity",
       subtitle = "By Category",
       x = "Publish Date",
       y = "Number of Tweets",
       fill = "Category")

# Tidying tweets text for top 20 authors
tidy_tweets <- tweets %>%
  semi_join(top20_authors) %>%
  unnest_tokens(word,content,token = "tweets") %>%
  anti_join(stop_words)

# Words frequency
words_frequency <- tidy_tweets %>%
  count(word) %>%
  mutate(freq = n/sum(n)) %>%
  ungroup()

# Top 20 Tweeted Words  
p3 <- tidy_tweets %>%
  count(word) %>%
  mutate(freq = n/sum(n)) %>%
  ungroup() %>%
  filter(!str_detect(word,"^#")) %>%
  arrange(desc(n)) %>%
  head(20) %>%
  ggplot() + 
  geom_bar(aes(x=reorder(word,n),y=n,
               fill = n),
           stat="identity",
           show.legend = FALSE) + 
  coord_flip() + 
  scale_fill_viridis_c(direction = -1,
                       option = "D") + 
  labs(title = "Top 20 Words",
       subtitle = "Not including hashtags and mentions",
       x = "Word",
       y = "Number of Tweets",
       fill = "Category")

# Counting the top 5 words by Author
top_words_by_author <- tidy_tweets %>%
  filter(!str_detect(word,"^#")) %>%
  group_by(author,word) %>%
  summarise(n = n()) %>%
  arrange(author,desc(n)) %>%
  top_n(5) %>%
  ungroup() %>%
  mutate(order = row_number())

# Top 5 words by Author
p4 <- top_words_by_author %>%
  ggplot(aes(x=desc(order),
             y=n,
             fill=n)) +
  geom_bar(stat="identity") + 
  coord_flip() + 
  facet_wrap(~author, 
             scales = "free", 
             ncol = 5) + 
  scale_x_continuous(
    breaks = desc(top_words_by_author$order),
    labels = top_words_by_author$word,
    expand = c(0,0)
  ) + 
  scale_fill_viridis_c(direction = -1,
                       option = "D") + 
  labs(title = "Top 5 Words by Author",
       subtitle = "Not including hashtags and mentions",
       x = "Word",
       y = NULL,
       fill = "Number of Tweets")

# Sentiment Analysis
afinn <- get_sentiments(lexicon = "afinn")  

p5 <- tidy_tweets %>%
  inner_join(afinn,by="word") %>%
  group_by(author) %>%
  summarise(score = sum(score)) %>%
  ggplot(aes(x=reorder(author,score),
             y=score,
             fill = score)) + 
  geom_bar(stat = "identity",
           show.legend = FALSE) + 
  coord_flip()  + 
  scale_fill_viridis_c(direction = 1,
                       option = "D") + 
  labs(title = "Tweets Sentiment Analysis using AFINN Score",
       subtitle = "Top 20 Authors",
       x = "Author",
       y = "AFINN Score")


plots <- list(p1,p2,p3,p4,p5)
numbers <- 1:5

walk2(plots,numbers,function(x,y){
  ggsave(filename = str_glue("tidytuesday/week20-{y}.png"),
         plot = x)
})
