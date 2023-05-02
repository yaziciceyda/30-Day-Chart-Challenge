library(readxl)
library(janitor)
library(gganimate)
library(showtext)
library(ggtext)

# Font in the Plot

font_add_google('Maiden Orange', 'mo')
showtext_auto()

# Data Import - Apples

apple_data <- read.csv("Apples.csv") %>%
  clean_names() %>%
  select(-product) %>%
  filter(country == "World") %>%
  select(country, production_t, year) %>%
  dplyr::rename("apple" = production_t)

# Data Import - Oranges

orange_data <- read.csv("Oranges.csv") %>%
  clean_names() %>%
  select(-product) %>%
  filter(country == "World") %>%
  select(country, production_t, year) %>%
  dplyr::rename("orange" = production_t)

# The combination of the data sets

all_data <- apple_data %>%
  left_join(orange_data, by = c("country", "year")) %>%
  pivot_longer(-c(country, year), values_to = "values")

# The correlation

cor.test(apple_data$apple, orange_data$orange) # 0.952

subtitle_text <- str_wrap("\nThe total production of apples and oranges since 1960 in the 
whole world is increasing. 93M tonnes of apples and 75M tonnes of oranges were 
produced in 2021. Even though they cannot be compared, their production 
is significantly correlated with a high value of 0.95.", 110)

title_text <- "<span style='color:#e12120'>APPLES 
               <span style='color:black'>AND <span style='color:#FFA500'>ORANGES"

# The Plot

p <- ggplot(all_data) +
  geom_line(aes(x = year, y = values, color = name), linewidth = 3) +
  scale_y_continuous(labels = label_number(suffix = " M", scale = 1e-6)) +
  scale_colour_manual(values = c("apple" = "#e12120",
                                 "orange" = "#FFA500")) +
  labs(title = title_text,
       subtitle = subtitle_text,
       x = "YEAR",
       y = "PRODUCTION (TONNES)",
       colour = "FRUIT",
       caption = "Data Source: OWID | #30DayChartChallenge - Day #20 | Prepared by: C. YAZICI") +
  theme(panel.background = element_rect(fill = "#99F7C1", color = NA),
        plot.background = element_rect(fill = "#99F7C1", color = NA),
        panel.grid = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_text(family = "mo", size = 25),
        axis.text = element_text(family = "mo", size = 22),
        legend.title = element_text(family = "mo", size = 24),
        legend.text = element_text(family = "mo", size = 23),
        legend.background = element_rect(fill = "#99F7C1"),
        legend.key.height = unit(2.0, 'cm'),
        legend.key.width = unit(2.0, 'cm'),
        plot.caption = element_text(family = "mo", size = 25, hjust = 1),
        plot.title = element_markdown(family = "mo", size = 55, hjust = 0),
        plot.subtitle = element_text(family = "mo", size = 32, hjust = 0),
        plot.margin = unit(c(1, 1, 1, 1), "cm"))



# Save the Plot

ggsave("Day20.png", p, width = 25, height = 15, dpi = 72)

