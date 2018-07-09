library(tidyverse)
library(readxl)

filename <- "https://github.com/rfordatascience/tidytuesday/blob/master/data/week15_beers.xlsx?raw=true"
download.file(filename,"data/week15_beers.xlsx")

df <- read_xlsx("data/week15_beers.xlsx")
head(df)


top_beers_styles <- df %>% 
  group_by(style) %>%
  summarise(n = n(),
            abv = (mean(abv, na.rm = TRUE))*100) %>%
  arrange(desc(n)) %>%
  head(10)

beers_ibu_abv <- df %>%
  filter(!is.na(abv) & !is.na(ibu)) %>%
  select(abv,ibu)

ggplot(top_beers_styles %>%
         mutate(style = fct_reorder(style,n))) + 
  geom_bar(aes(style,n,
               fill = desc(abv)),
           stat = "identity") + 
  coord_flip() + 
  labs(y = "No of Beers",
       x = "Style",
       title = "Top 10 Styles of beer",
       fill = "ABV") + 
  scale_fill_gradient(low="black",high="yellow")

ibu_abv_cor <- round(cor(beers_ibu_abv$abv, beers_ibu_abv$ibu),2)

ggplot(beers_ibu_abv) + 
  geom_point(aes(ibu,
                 abv),
             alpha=0.5) + 
  geom_smooth(aes(ibu,abv),
              method = "lm",
              na.rm = TRUE) +
  geom_text(aes(100,0.025,label=paste0("r = ", ibu_abv_cor))) + 
  labs(y = "IBU",
       x = "ABV",
       title = "Correlation analysis on IBU vs ABV") 
  
  
