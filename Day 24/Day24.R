library(showtext)

# Font in the Plot

font_add_google("Montserrat", "ms")
showtext_auto()

# Data Import

nobel_winners <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-05-14/nobel_winners.csv")
nobel_winner_all_pubs <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-05-14/nobel_winner_all_pubs.csv")

# Data Wrangling

nobel_data <- nobel_winners %>%
  select(prize_year, category, gender, full_name) %>%
  mutate(color_new = case_when(
    gender == "Female" & category == "Physics" ~ "brown",
    gender == "Female" & category == "Chemistry" ~ "green",
    gender == "Female" & category == "Medicine" ~ "yellow",
    gender == "Female" & category == "Literature" ~ "hotpink",
    gender == "Female" & category == "Peace" ~ "red",
    gender == "Female" & category == "Economics" ~ "black",
    .default = "grey60"
  ))

# The Plot

p <- ggplot(nobel_data) +
  geom_dotplot(aes(x = prize_year, y = 1,
                   fill = color_new, color = color_new), binwidth = 0.8) +
  geom_dotplot(data = nobel_data %>% filter(gender == "Female",
                                            category == "Physics"),
               mapping = aes(x = prize_year, y = 1,
                   fill = "blue", color = "blue"), binwidth = 0.8) +
  scale_fill_identity() +
  scale_colour_identity() +
  scale_x_continuous(breaks = c(1901, 1910, 1920, 1930, 1940, 1950, 1960, 1970,
                                1980, 1990, 2000, 2010, 2016),
                     sec.axis = dup_axis()) +
  facet_wrap(~factor(category, levels = c("Physics", "Chemistry",
                                          "Medicine", "Literature",
                                          "Peace", "Economics")), ncol = 1)  +
  geom_vline(xintercept = 1901, colour = "grey80", size = 2) +
  geom_vline(xintercept = 1910, colour = "grey80", size = 2) +
  geom_vline(xintercept = 1920, colour = "grey80", size = 2) +
  geom_vline(xintercept = 1930, colour = "grey80", size = 2) +
  geom_vline(xintercept = 1940, colour = "grey80", size = 2) +
  geom_vline(xintercept = 1950, colour = "grey80", size = 2) +
  geom_vline(xintercept = 1960, colour = "grey80", size = 2) +
  geom_vline(xintercept = 1970, colour = "grey80", size = 2) +
  geom_vline(xintercept = 1980, colour = "grey80", size = 2) +
  geom_vline(xintercept = 1990, colour = "grey80", size = 2) +
  geom_vline(xintercept = 2000, colour = "grey80", size = 2) +
  geom_vline(xintercept = 2010, colour = "grey80", size = 2) +
  geom_vline(xintercept = 2016, colour = "grey80", size = 2) +
  geom_text(aes(x = 1901, y = 0.7, label = category), family = "ms", hjust = 0,
            size = 5) +
  coord_cartesian() +
  labs(title = "WOMEN NOBEL PRIZE WINNERS",
       subtitle = "\nFrom 1901 to 2016\n",
       caption = "Data Source: Kaggle | #30DayChartChallenge - Day #24 | TidyTuesday 2019 - Week 20 | Prepared by: C. YAZICI") +
  theme(plot.background = element_rect(fill = "#B3F1F7", color = NA),
        panel.background = element_rect(fill = "white", color = NA),
        panel.grid = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank(),
        strip.text = element_blank(),
        axis.text.y = element_blank(),
        axis.text.x = element_text(family = "ms", size = 20),
        strip.background = element_rect(fill = "#B3F1F7", color = NA),
        plot.title = element_text(family = "ms", size = 30, hjust = 0),
        plot.subtitle = element_text(family = "ms", size = 20, hjust = 0),
        plot.caption = element_text(family = "ms", size = 20, hjust = 1),
        plot.margin = margin(0.5, 1.5, 0.5, 1.5, "cm"))

# Save the Plot

ggsave("Day24.png", p, width = 25, height = 12, dpi = 72)


  
 
