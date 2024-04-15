library(tidyverse)
library(readr)
library(showtext)

# Font in the Plot

font_add_google(name = "Nosifer",
                family = "nosifer", db_cache = FALSE)
showtext_auto()
font_add_google(name = "Sedan",
                family = "sedan", db_cache = FALSE)
showtext_auto()




data_chocolate <- read_csv("chocolate.csv")
data_taste <- read_csv("chocolate_taste_dataset.csv")

taste_data <- data_chocolate %>%
  filter(!is.na(first_taste),
         !is.na(second_taste)) %>%
  group_by(first_taste) %>%
  summarise(n = n()) %>%
  arrange(desc(n)) %>%
  ungroup() # creamy & sandy are the most frequent first tastes

  
taste_data2 <- data_chocolate %>%
  filter(first_taste == "creamy" | 
           first_taste == "sandy") %>%
  group_by(first_taste, second_taste) %>%
  summarise(n = n())%>%
  arrange(first_taste, desc(n)) %>%
  slice_max(order_by = n, n = 8) %>%
  mutate(x_value = seq(0.5, 4.5, by = 0.5)) %>%
    #x_value = row_number()) %>%
ungroup() 
  
  
subtitle_text <- str_wrap("\nThe chocolate is one of the most popular
candies in the world. The most two popular tastes are creamy and 
sandy. Nutty taste is the most frequent second taste among the creamy 
ones; while sweet is the most favorite second taste for the sandy 
ones.", 80)
  
  
p <- ggplot(taste_data2) +
  geom_point(aes(x = x_value, y = -1 * n + 0.2), size = 4, 
             color = "#7B3F00") +
  geom_segment(taste_data2 %>% filter(n <= 7),
               mapping = aes(x = x_value, xend = x_value,
                   y = 0, yend = -1 * n),
               lineend = "round", size = 4, color = "#7B3F00") +
  geom_curve(taste_data2 %>% filter(n <= 7),
             mapping = aes(x = x_value - 0.05, xend = x_value,
                 y = 0, yend = -1 * n),
             curvature = -0.2, size = 4, color = "#7B3F00") +
  geom_curve(taste_data2 %>% filter(n <= 7),
             mapping = aes(x = x_value + 0.05, xend = x_value,
                 y = 0, yend = -1 * n),
             curvature = 0.2, size = 4, color = "#7B3F00") + 
  geom_rect(aes(xmin = 0.25, xmax = 4.75,
                   ymin = -1, ymax = 10),
               size = 2, fill = "#7B3F00") +
  ###
  geom_curve(taste_data2 %>% filter(n > 7),
             mapping = aes(x = x_value - 0.05, xend = x_value,
                 y = 0, yend = -1 * n),
             curvature = -0.1, size = 4, color = "#7B3F00") +
  geom_curve(taste_data2 %>% filter(n > 7),
             mapping = aes(x = x_value + 0.05, xend = x_value,
                           y = 0, yend = -1 * n),
             curvature = 0.1, size = 4, color = "#7B3F00") +
  geom_segment(taste_data2 %>% filter(n > 7),
               mapping = aes(x = x_value, xend = x_value,
                             y = 0, yend = -1 * n),
               lineend = "round", size = 4, color = "#7B3F00") +
  geom_point(taste_data2 %>% filter(n > 7),
             mapping = aes(x = x_value, y = -0.5), size = 14, 
             color = "#7B3F00") +
  geom_text(aes(x = 2.5, y = 4, label = toupper(first_taste)),
            color = "ivory", family = "nosifer", size = 12) +
  geom_text(aes(x = x_value, y = -1 * n - 4, 
                label = toupper(second_taste)),
            color = "#7B3F00", family = "nosifer", size = 6.5) +
  
  scale_x_continuous(limits = c(0.2, 4.85)) +
  facet_wrap(~first_taste, nrow = 2) +
  labs(title = "CHOCOLATE BARS",
       subtitle = subtitle_text,
       caption = "Data Source: Kaggle\n#30DayChartChallenge - Day #9\nPrepared by: C. YAZICI") +
  theme(panel.background = element_rect(fill = "ivory", color = NA),
        plot.background = element_rect(fill = "ivory", color = NA),
        panel.grid = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank(),
        strip.background = element_blank(),
        strip.text = element_blank(),
        plot.title = element_text(family = "nosifer", hjust = 0, 
                                  size = 50, color = "#411900"),
        plot.subtitle = element_text(family = "sedan", hjust = 0, 
                                  size = 30, color = "#411900"),
        plot.caption = element_text(family = "sedan", hjust = 1, 
                                  size = 25, color = "#411900"),
        plot.margin = margin(1.5, 1, 1.5, 1, "cm")) 


# Save the Plot

ggsave("Day9.png", p, width = 22, height = 16, dpi = 72)


  


