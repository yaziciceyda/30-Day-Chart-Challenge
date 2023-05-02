library(lubridate)
library(showtext)

# Font in the Plot

font_add_google('Phudu', 'phudu')
showtext_auto()

# Data Import

tuesdata <- tidytuesdayR::tt_load(2021, week = 38)

billboard <- tuesdata$billboard
audio <- tuesdata$audio_features

# Data Wrangling

billboard_data <- billboard %>%
  mutate(week_id = as.POSIXct(week_id, 
                             format = "%m/%d/%Y"),
         year = year(week_id)) %>%
  group_by(year) %>%
  slice(which.max(weeks_on_chart)) %>%
  ungroup() %>%
  arrange(year) %>%
  mutate(y = 1:64)

tag <- str_wrap("The Billboard Hot 100 is the music industry standard record 
chart in the United States for songs, published weekly by Billboard magazine. 
The song which appeard on the chart 
longest for each year is presented here with the performer's name. The darker 
names stayed longer than the lighter ones.", 35)

# The Plot

p <- ggplot(billboard_data) +
  geom_text(aes(x = 0.9, y = 40, label = tag),
            color = "white", family = "phudu", size = 6, hjust = 0) +
  geom_text(aes(x = 1, y = y, label = song, alpha = weeks_on_chart),
            color = "white", family = "phudu", size = 4, hjust = 0.5) +
  geom_text(aes(x = 1.1, y = y, label = performer, alpha = weeks_on_chart),
            color = "white", family = "phudu", size = 4, hjust = 0.5) +
  geom_text(aes(x = 1.05, y = y, label = year, alpha = weeks_on_chart),
            color = "white", family = "phudu", size = 4, hjust = 0.5) +
  geom_text(aes(x = 1.05, y = 67, label = "YEAR"),
            color = "white", family = "phudu", size = 10, hjust = 0.5) +
  geom_text(aes(x = 1, y = 67, label = "SONG"),
            color = "white", family = "phudu", size = 10, hjust = 0.5) +
  geom_text(aes(x = 1.1, y = 67, label = "PERFORMER"),
            color = "white", family = "phudu", size = 10, hjust = 0.5) +
  scale_x_continuous(limits = c(0.9, 1.2), expand = c(0, 0)) +
  scale_y_continuous(limits = c(0, 69), expand = c(0, 0)) +
  coord_cartesian() +
  labs(title = "TOP 100 BILLBOARD",
       caption = "\nData Source: Data.World  | #30DayChartChallenge - Day #29 | #TidyTuesday - 2021 - Week 38 | Prepared by: C. YAZICI") +
  theme(panel.background = element_rect(fill = "black", color = NA),
        plot.background = element_rect(fill = "black", color = NA),
        panel.grid = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank(),
        axis.text = element_blank(),
        legend.position = "none",
      plot.title = element_text(family = "phudu", hjust = 0, size = 50,
                                color = "white"),
      plot.subtitle = element_text(family = "phudu", hjust = 0, size = 15,
                                color = "white"),
      plot.caption = element_text(family = "phudu", hjust = 1, size = 18,
                                  color = "white"),
      plot.margin = unit(c(1, 1.5, 1, 1.5), "cm"))


# Save the Plot

ggsave("Day29.png", p, width = 25, height = 15, dpi = 72)



