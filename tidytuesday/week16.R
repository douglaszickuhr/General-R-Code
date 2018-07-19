library(readxl)
library(tidyverse)

url <- "https://github.com/rfordatascience/tidytuesday/blob/master/data/week16_exercise.xlsx?raw=true"
filename <- "week16_exercise.xlsx"
download.file(url,paste("data",filename,sep = "/"))

df <- read_xlsx(paste("data",filename,sep = "/"),
                sheet = "tidy") %>%
  select(-count) %>%
  mutate(exercise = parse_double(exercise))

glimpse(df)

boxplot_by_sex <- df %>%
  filter(state != "state_average" & sex != "both") %>%
  ggplot() + 
  geom_boxplot(aes(x=sex,y=exercise,fill = sex)) + 
  facet_wrap(~work_status) + 
  labs(title = "Distribution of Exercise data in the US",
       y = "% of adults that meet exercises guidelines",
       x = "Sex") + 
  theme_minimal() + 
  theme(
    legend.position = "none"
  )

ggsave(boxplot_by_sex, filename = "week16-1.png")

boxplot_by_work_status <- df %>%
  filter(state != "state_average" & sex != "both" & work_status != "all") %>%
  ggplot() + 
  geom_boxplot(aes(x=work_status,y=exercise,fill = sex)) + 
  facet_wrap(~sex) + 
  labs(title = "Distribution of Exercise data in the US",
       y = "% of adults that meet exercises guidelines",
       x = "Working Status") + 
  theme_minimal()

ggsave(boxplot_by_work_status, filename = "week16-2.png")



histogram_by_sex <- df %>%
  filter(state != "state_average" & sex != "both" & work_status != "all") %>%
  ggplot() + 
  geom_histogram(aes(x=exercise, fill = sex), binwidth = 3) + 
  labs(title = "Distribution of Exercise data in the US",
       y = "Number of observations",
       x = "% of adults that meet exercises guidelines",
       fill = "Sex") + 
  theme_minimal()

ggsave(histogram_by_sex, filename = "week16-3.png")


histogram_by_work_status <- df %>%
  filter(state != "state_average" & sex != "both" & work_status != "all") %>%
  ggplot() + 
  geom_histogram(aes(x=exercise, fill = work_status), binwidth = 5) + 
  labs(title = "Distribution of Exercise data in the US",
       y = "Number of records",
       x = "% of adults that meet exercises guidelines",
       fill = "Work Status") + 
  theme_minimal()
  
ggsave(histogram_by_work_status, filename = "week16-4.png")
