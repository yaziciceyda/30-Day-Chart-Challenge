library(readxl)
library(tidyverse)
library(ggrepel)
library(showtext)
library(ggtext)
library(usefunc)

# Font in the Plot

font_add_google('Roboto Slab', 'rs')
showtext_auto()


# Data Import & Wrangling

data_disasters <- read.csv("Data.csv", sep = ";") %>%
  filter(Entity %in% c("Drought", "Earthquake", "Extreme temperature",
"Extreme weather", "Flood", "Volcanic activity", "Wildfire")) %>%
  select(-Code) %>%
  rename(Frequency = Number.of.reported.natural.disasters) %>%
  mutate(time_period = case_when(
    Year >= 1900 & Year < 1940 ~ "Period 1",
    Year >= 1940 & Year < 1980 ~ "Period 2",
    Year >= 1980 ~ "Period 3"
  )) %>%
  group_by(time_period, Entity) %>%
  summarise(sum_freq = sum(Frequency)) %>%
  ungroup()


data_disasters_summary <- data_disasters %>%
  group_by(time_period) %>%
  summarise(sum_freq = sum(sum_freq)) %>%
  ungroup()  

# Subtitle of the Plot

subtitle_text <- str_wrap("<br>The climate related natural disasters and earthquakes 
are summarized in three consecutive time periods. Total <br>of 408 disasters occured in
the first period (1900 - 1939); while 1850 occured in the second one 
(1940 - 1979).<br>The sum of the number of disasters increased more than 6 times 
to 12449 in the Period 3 (1980 - 2022). <br>When each type of disasters 
investigated, all of them has an increasing path especially in the last period;
<br>however <span style='color:#0A35F7'>Flood <span style='color:black'>and 
<span style='color:#8C950B'>Extreme Weather Conditions 
<span style='color:black'>have the highest increase.", 80)

# The Plot

p <- ggplot(data = data_disasters, aes(x = time_period, y = sum_freq, 
                                  group = Entity)) +
  geom_line(aes(color = Entity), size = 4) +
  geom_point(aes(color = Entity), size = 6) +
  geom_text_repel(data = data_disasters %>%
                    filter(time_period == "Period 3"),
                           aes(x = "Period 3", y = sum_freq, label = Entity,
                               color = Entity),
                  hjust = "right", 
                  fontface = "bold", 
                  size = 6, 
                  nudge_x = .5, 
                  direction = "y",
                  family = "rs") +
  scale_colour_manual(values = c("Drought" = "#BB720E",
                                 "Earthquake" = "#0B0701", 
                                 "Extreme temperature" = "#F71C0A",
                                 "Extreme weather" = "#8C950B", 
                                 "Flood" = "#0A35F7", 
                                 "Volcanic activity" = "#E84C22", 
                                 "Wildfire" = "#963919")) +
  scale_x_discrete(position = "top") +
  coord_cartesian() +
  labs(title = "NATURAL DISASTERS",
       subtitle = subtitle_text,
       x = "",
       y = "Total",
       caption = "Data: OWID |#30DayChartChallenge - Day #5 |Prepared by: C. YAZICI") +
  theme(plot.background = element_rect(fill = "#B6A3B3", color = NA), 
        panel.background = element_rect(fill = "#B6A3B3", color = NA),
        legend.position = "none",
        axis.ticks = element_blank(),
        panel.grid = element_blank(),
        axis.title = element_text(size = 20, family = "rs"),
        axis.text = element_text(size = 18, family = "rs"),
        plot.caption = element_text(size = 18, family = "rs", hjust = 1),
        plot.title = element_text(size = 40, family = "rs", hjust = 0.5),
        plot.subtitle = element_markdown(size = 22, family = "rs", hjust = 0),
        plot.margin = unit(c(1.5, 0.8, 1.5, 1.5), "cm"))

# Save the Plot

ggsave("Day5.png", p, width = 25, height = 12, dpi = 72) 

