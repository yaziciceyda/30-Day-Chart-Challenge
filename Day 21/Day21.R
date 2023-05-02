library(readxl)
library(tidyverse)
library(dplyr)
library(ggforce)
library(showtext)
library(ggtext)

font_add_google("Orbitron", "orb")
showtext_auto()

# Data Import

data_tide_original <- read_xlsx("Tide_London.xlsx") 

# Data Wrangling

data_tide <- data_tide_original %>%
  pivot_longer(-c(Day, Time, Hide_Hr, Low_Hr)) %>%
  mutate(x = 1:24,
         value = ifelse(is.na(value), 0, value),
         Hide_Hr = as.POSIXct(Hide_Hr, format = "%Y-%m-%d %H:%M:%S"),
         time_high = format(as.POSIXct(Hide_Hr),format = "%H:%M:%S"),
         Low_Hr = as.POSIXct(Low_Hr, format = "%Y-%m-%d %H:%M:%S"),
         time_low = format(as.POSIXct(Low_Hr),format = "%H:%M:%S"),
         color_new = factor(case_when(
           Time == "morning" ~ "green",
           Time == "afternoon" ~ "red",
           Time == "night" ~ "blue",
         )), 
         Time = factor(Time, levels = c("morning", "afternoon", "night"))) 
         # time_high = strptime(time_high, format = "%H:%M:%S"),
         # time_low = strptime(time_low, format = "%H:%M:%S"))

data_day <- data_tide %>%
  group_by(Day) %>%
  summarise(avg_x = mean(x)) %>%
  ungroup()

subtitle_text <- str_wrap("London Bridge tide measurements are taken three times 
a day<br> as <span style='color:blue'>morning, <span style='color:green'>afternoon 
<span style='color:black'>and <span style='color:red'>night. 
<span style='color:black'>There are two types
of tide<br> as low and high. Low and high tides are measured in the mornings and<br> 
afternoon, respectively. However, both are measured in the night. Here,<br>
the height and time of the tides for the next 4 days are given.", 80)

# The Plot

p <- ggplot(data_tide) +
  geom_line(aes(x = x, y = value), linewidth = 2) +
  geom_circle(data_tide %>% filter(value != 0),
              mapping = aes(x0 = x, y0 = value, colour = color_new, r = 1,
                            fill = color_new,
                            linewidth = 0.01)) +
  geom_circle(data_tide %>% filter(value != 0),
              mapping = aes(x0 = x, y0 = value, colour = color_new, r = 0.99,
                            linewidth = 0.01), fill = "white") +
  geom_text(data_tide %>% filter(!is.na(time_high),
                                 value != 0,
                                 Time != "night"),
            mapping = aes(x = x, y = value, label = time_high),
            family = "orb", size = 5) +
  geom_text(data_tide %>% filter(!is.na(time_low),
                                 value != 0,
                                 Time != "night"),
            mapping = aes(x = x, y = value, label = time_low),
            family = "orb", size = 5) +
  geom_text(data_tide %>% filter(Time == "night",
                                 value < 5),
            mapping = aes(x = x, y = value, label = time_low),
            family = "orb", size = 5) +
  geom_text(data_tide %>% filter(Time == "night",
                                 value > 5),
            mapping = aes(x = x, y = value, label = time_high),
            family = "orb", size = 5) +
  geom_text(data_day,
            mapping = aes(x = avg_x, y = -1, label = Day),
            family = "orb", size = 8) +
  geom_text(data_tide,
            mapping = aes(x = 26, y = 6.5, label = "High\nTide"),
            family = "orb", size = 7) +
  geom_text(data_tide,
            mapping = aes(x = 26.5, y = 0.5, label = "Low\nTide"),
            family = "orb", size = 7) +
  coord_cartesian() +
  scale_fill_identity(guide = 'legend',
                      labels = levels(data_tide$Time),
                      breaks = levels(data_tide$color_new)) +
  labs(title = "LONDON BRIDGE TIDE TIMES",
       subtitle = subtitle_text,
       y = "Height (m)",
       fill = "Period",
       caption = "Data Source: tide-forecast.com | #30DayChartChallenge - Day #21 | Prepared by: C. YAZICI") +
  theme(panel.background = element_rect(fill = "#F3F799", color = NA),
        plot.background = element_rect(fill = "#F3F799", color = NA),
        panel.grid = element_blank(),
        axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.y = element_blank(),
        axis.title.y = element_text(family = "orb", size = 24),
        axis.text.y = element_text(family = "orb", size = 22),
        legend.background = element_rect(fill = "#F3F799"),
        legend.title = element_text(family = "orb", size = 25),
        legend.text = element_text(family = "orb", size = 23),
        legend.key.size = unit(1.9, 'cm'),
        plot.caption = element_text(family = "orb", size = 21, hjust = 1),
        plot.title = element_text(family = "orb", size = 50, hjust = 0),
        plot.subtitle = element_markdown(family = "orb", size = 28, hjust = 0),
        plot.margin = unit(c(1, 1, 1, 1), "cm")
        ) +
  guides(linewidth = "none",
         colour = "none")

# Save the Plot

ggsave("Day21.png", p, width = 25, height = 15, dpi = 72)






