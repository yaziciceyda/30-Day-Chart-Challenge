library(readxl)
library(dplyr)
library(gganimate)
library(ggblur)
library(showtext)
library(usefunc)

# Font in the Plot

font_add_google('Happy Monkey', 'hm')
showtext_auto()

# Data Import - Happiness

data_happy <- read.csv("Happiness.csv") %>%
  filter(Year == 2021) %>%
  select(-c(Code, Year)) %>%
  rename(score = Cantril.ladder.score)

# Data Import - Sunshine & Wrangling

data_sun <- read.csv("Sunshine.csv") %>%
  filter(City %in% c("Istanbul", "Athens", "Paris", "Madrid", "London",
                     "Amsterdam")) %>%
  select(-Year) %>%
  mutate(city_num = case_when(
    City == "Istanbul"~ 1,
    City == "Athens"~ 2,
    City == "Paris"~ 3,
    City == "Madrid"~ 4,
    City == "London"~ 5,
    City == "Amsterdam"~ 6,
  )) %>%
  mutate(avg_sun = rowMeans(.[,-1:-2])) %>%
  left_join(data_happy, by = c("Country" = "Entity")) %>%
  mutate(City = fct_reorder(City, score))
  
subtitle_text <- str_wrap("The highest self-reported life satisfaction of the selected 
cities belongs to Amsterdam and the lowest value belongs to Istanbul. London 
and Paris are similar in terms of this value and duration of sunshine. As expected,
Athens, Madrid  and Istanbul have the longest duration of sunshine.", 60) 

# The Plot

p <- ggplot(data_sun) +
  geom_segment(aes(x = City, xend = City,
                   y = 0, yend = score), size = 3,
               color = "blue") +
  geom_point_blur(aes(x = City, y = score, color = avg_sun, fill = avg_sun,
                      blur_size = avg_sun),
                  shape = 21, size = 17, stroke = 2) +
  geom_point(aes(x = City, y = score, color = avg_sun),
             shape = 8, size = 25, stroke = 2) +
  geom_text(aes(x = City, y = 0, label = City), angle = 90, vjust = 0,
            hjust = 0, size = 10, family = "hm") +
  # Text --> happiness
  geom_text(aes(x = 6.2, y = 1, 
                label = "The height shows\nthe average life\nsatisfaction"), 
            hjust = 0, size = 6, color = "#83470B", family = "hm") +
  # Arrow
  geom_curve(aes(x = 7.2, xend = 6.1, y = 1.5, yend = 2.2), color = "#83470B",
             arrow = arrow(length = unit(1, "cm")), size = 1) +
  labs(fill = "A",
       title = "SUNSHINE AND LIFE SATISFACTION",
       x = "",
       y = "LIFE SATISFACTION SCORE",
       subtitle = subtitle_text,
       caption = "Data Source: OWID & Kaggle\n#30DayChartChallenge - Day #11\nPrepared by: C. YAZICI") +
  scale_blur_size_continuous(range = c(10, 15), guide = "none") +
  scale_color_gradient(high = "firebrick", low = "#ffce00", name = " ",
                       guide = "none") +
  scale_fill_gradient(high = "firebrick", low = "#ffce00", 
                      name = "SUNSHINE\n(HOURS)") +
  coord_fixed(clip = "off") +
  theme(panel.background = element_rect(fill = "#92EBA2", color = NA),
        plot.background = element_rect(fill = "#92EBA2", color = NA),
        panel.grid = element_blank(),
        axis.ticks = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_text(size = 20, color = "#83470B", family = "hm"),
        axis.title.y = element_text(size = 22, color = "#83470B", family = "hm"),
        legend.title = element_text(size = 20, color = "#83470B", family = "hm"),
        plot.caption = element_text(family = "hm", hjust = 1, size = 18, 
                                    color = "#83470B"),
        plot.title = element_text(family = "hm", hjust = 0.1, size = 30, 
                                    color = "#83470B"),
        plot.subtitle = element_text(family = "hm", hjust = 0, size = 17, 
                                color = "#83470B"),
        legend.direction = "vertical",
        legend.key.height = unit(2.0, 'cm'),
        legend.text = element_text(size = 18, color = "#83470B", family = "hm"),
        legend.background = element_rect(fill = "#92EBA2"),
        legend.position = "right",
        legend.justification = "top",
        plot.margin = unit(c(1.0, 5.0, 1.0, 5.0), "cm"))

# Save the Plot

ggsave("Day11.png", p, width = 15, height = 15, dpi = 72)



