library(ggridges)
library(forecast)
library(tidyverse)
library(ggdist)
library(showtext)

# Font in the Plot

font_add_google(name = "Permanent Marker",   
                family = "pm")
showtext_auto()


font_add_google('Shippori Antique', 'sa')
showtext_auto()

# Data Import

tuesdata <- tidytuesdayR::tt_load(2021, week = 38)

billboard <- tuesdata$billboard
audio <- tuesdata$audio_features

# Data Wrangling

plot_data <- audio %>%
  filter(str_detect(song, "good |Good | bad | Bad ")) %>%
  drop_na(danceability, energy, speechiness, acousticness, liveness, valence) %>%
  select(song, danceability, energy, speechiness, acousticness, liveness, 
         valence) %>%
  pivot_longer(-song, values_to = "values") %>%
  mutate(index = ifelse(str_detect(song, "good |Good "), "good", "bad"))

plot_data_summary <- plot_data %>%
  group_by(name, index) %>%
  summarise(avg_val = mean(values),
            se_val = sd(values)) %>%
  ungroup() %>%
  mutate(y = case_when(
    name == "valence"~ 6,
    name == "danceability"~ 5,
    name == "energy"~ 4,
    name == "acousticness"~ 3,
    name == "liveness"~ 2,
    name == "speechiness"~ 1,
    ),
    y = ifelse(index == "bad", y + 0.5, y)) %>%
  group_by(name) %>%
  mutate(avg_y = mean(y),
         avg_x = avg_val - 3 * se_val - 0.3) %>%
  ungroup()
  
subtitle_text <- str_wrap("\nThe Billboard Hot 100 is the music industry standard record
chart in the United States for songs, published weekly by Billboard magazine.
The songs which include GOOD and BAD in their names are investigated according
to several criteria. In terms of energy and liveness, the songs with BAD in 
their title are less variable, while GOOD songs have less spread in valence,
danceability and acousticness. For speechiness, both group of songs have almost
the same variability. On the other hand, speechiness have the less spread, and 
acousticness have more in both groups.", 120)

# The Plot

p <- ggplot(plot_data_summary) +
  geom_rect(mapping = aes(ymin = -Inf, ymax = Inf,
                          xmin = -Inf, xmax = Inf),
            fill = "#F075C5",
            colour = NA,
            size = 0.5) +
  annotation_custom(grid::rasterGrob(paste0("#0F0E0E", as.hexmode(1:240)), 
                                     width = unit(1,"npc"), 
                                     height = unit(1,"npc"), 
                                     interpolate = TRUE), 
                    xmin = -Inf, xmax = Inf, 
                    ymin = -Inf, ymax = Inf) +
  geom_rect(aes(xmin = avg_val - 3 * se_val, 
                              xmax = avg_val + 3 * se_val, 
                              ymin = y - 0.2, ymax = y + 0.2), 
                          fill = "#84F1F6") +
  geom_rect(aes(xmin = avg_val - 2 * se_val, 
                              xmax = avg_val + 2 * se_val, 
                              ymin = y - 0.2, ymax = y + 0.2), 
                          fill = "#339EA4") + 
  geom_rect(aes(xmin = avg_val - 1 * se_val, 
                xmax = avg_val + 1 * se_val, 
                ymin = y - 0.2, ymax = y + 0.2), 
            fill = "#077075") + 
  geom_point(aes(x = avg_val, y = y), color = "#D925DC", size = 5) +
  geom_text(aes(x = avg_val - 3 * se_val - 0.15,
                y = y, label = index), hjust = 0, family = "pm",
            size = 8) +
  geom_text(data = plot_data_summary %>%filter(index == "good"),
    aes(x = avg_x - 0.2, y = avg_y, label = name), hjust = 0, family = "pm",
            size = 8) +
  # How to read
  geom_rect(aes(xmin = 0, xmax = 1, 
                ymin = 8, ymax = 8.5), 
            fill = "#84F1F6") +
  geom_rect(aes(xmin = 0.2, xmax = 0.8, 
                ymin = 8, ymax = 8.5), 
            fill = "#339EA4") +
  geom_rect(aes(xmin = 0.4, xmax = 0.6, 
                ymin = 8, ymax = 8.5), 
            fill = "#077075") +
  geom_point(aes(x = 0.5, y = 8.25), size = 5, color = "#D925DC") +
  geom_segment(aes(x = 0.4, y = 7.8, 
                 xend = 0.6, yend = 7.8),
             arrow = arrow(length = unit(1, "cm")), size = 1) +
  geom_text(aes(x = 0.75, y = 7.8, label = "one std dev away\nfrom the mean"),
            color = "#077075", size = 5, family = "sa") +
  geom_segment(aes(x = 0.2, y = 7.4, 
                   xend = 0.8, yend = 7.4),
               arrow = arrow(length = unit(1, "cm")), size = 1) +
  geom_text(aes(x = 0.95, y = 7.4, label = "two std dev away\nfrom the mean"),
            color = "#339EA4", size = 5, family = "sa") +
  geom_segment(aes(x = 0, y = 7.1, 
                   xend = 1, yend = 7.1),
               arrow = arrow(length = unit(1, "cm")), size = 1) +
  geom_text(aes(x = 1.2, y = 7.0, label = "three std dev away\nfrom the mean"),
            color = "#84F1F6", size = 5, family = "sa") +
  geom_curve(aes(x = 0.5, y = 8.25, 
                 xend = 0.55, yend = 8.8),
             arrow = arrow(length = unit(1, "cm")),
             curvature = -0.5, size = 1) +
  geom_text(aes(x = 0.59, y = 8.9, label = "mean"),
            color = "black", size = 5, family = "sa") +
  geom_text(aes(x = 0.4, y = 9.3, label = "How to read"),
            color = "black", size = 10, family = "sa") +
  coord_cartesian(xlim = c(-1.0, 1.3)) +
  labs(title = "Top 100 Billboard",
       subtitle = subtitle_text,
    caption = "\nData Source: Data.World  | #30DayChartChallenge - Day #27 | #TidyTuesday - 2021 - Week 38 | Prepared by: C. YAZICI") +
  theme(plot.background = element_rect(fill = "ivory", color = NA),
        panel.background = element_rect(fill = "ivory", color = NA),
    panel.grid = element_blank(),
    axis.ticks = element_blank(),
    axis.text = element_blank(),
    axis.title = element_blank(),
    plot.caption = element_text(family = "sa", hjust = 1, size = 15),
    plot.title = element_text(family = "pm", hjust = 0, size = 50),
    plot.subtitle = element_text(family = "sa", hjust = 0, size = 20),
    plot.margin = unit(c(0.5, 1.5, 0.5, 1.5), "cm"))

# Save the Plot

ggsave("Day27.png", p, width = 26, height = 15, dpi = 72)


