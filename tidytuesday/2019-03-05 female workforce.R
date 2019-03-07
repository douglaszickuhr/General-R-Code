library(readr)
library(dplyr)
library(ggplot2)
library(scales)
library(ggrepel)


jobs_gender <- read_csv("C:/Users/ST9DZ1/Documents/jobs_gender.csv")

labels <- jobs_gender %>% 
  filter(year == 2016,
         major_category %in% c("Computer, Engineering, and Science")) %>% 
  mutate(male_rate = workers_male/total_workers) %>%
  mutate(max_male_rate = max(male_rate),
         min_male_rate = min(male_rate),
         max_salary = max(total_earnings),
         min_salary = min(total_earnings),
         max_workforce = max(total_workers)) %>%
  filter(male_rate == max_male_rate | 
           male_rate == min_male_rate | 
           total_earnings == max_salary |
           total_earnings == min_salary |
           total_workers == max_workforce)

ggplot() + 
  geom_point(data = filter(jobs_gender,year == 2016,!major_category %in% c("Computer, Engineering, and Science")),
             aes(x = workers_male/total_workers,
                 y = total_earnings,
                 size = total_workers),
             alpha = 1/3,
             colour = "grey") + 
  geom_point(data = filter(jobs_gender,year == 2016,major_category %in% c("Computer, Engineering, and Science")),
             aes(x = workers_male/total_workers,
                 y = total_earnings,
                 size = total_workers),
             colour = "red",
             alpha = 1/2) + 
  geom_smooth(data =  filter(jobs_gender,year == 2016),
              aes(x = workers_male/total_workers,
                  y = total_earnings),
              method = "lm",
              se = FALSE,
              colour = "black",
              show.legend = FALSE) +
  geom_text_repel(data = labels,
                  aes(x = workers_male/total_workers,
                      y = total_earnings,
                      label = occupation),
                  vjust = 3) +
  scale_x_continuous(labels = scales::percent_format(accuracy = 1)) + 
  scale_y_continuous(limits = c(25000,150000),
                     breaks = seq(25000, 150000, by = 25000),
                     labels = scales::dollar_format()) + 
  scale_size_continuous(guide = FALSE) +
  scale_color_viridis_d(option = "B") +
  labs(title = "Does Computer, Engineering and Science pays more for majority male roles?",
       subtitle = "Annual median salary vs Male workforce rate in 2016",
       caption = "Source: The US Census Bureau/Bureau of Labor",
       x = "Male workforce rate (%)",
       y = "Median annual earning (US$)") + 
  theme_bw()
