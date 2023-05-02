library(readxl)
library(janitor)
library(geomtextpath)
library(showtext)
library(ggtext)

# Font in the Plot

font_add_google('Roboto Slab', 'rs')
showtext_auto()

# Data Import

data_disasters <- read.csv("Data.csv", sep = ";") %>%
  clean_names

# Data Wrangling

data_global <- data_disasters %>%
  filter(entity %in% c("Drought", "Earthquake", "Extreme temperature",
                       "Extreme weather", "Flood", "Volcanic activity", 
                       "Wildfire")) %>%
  select(-code) %>%
  mutate(hj = case_when(
    entity == "Drought" ~ 0.6,
    entity == "Earthquake" ~ 0.7,
    entity == "Extreme temperature" ~ 0.75,
    entity == "Extreme weather" ~ 0.9,
    entity == "Flood" ~ 0.8,
    entity == "Volcanic activity" ~ 1.0,
    entity == "Wildfire" ~ 0.88,
  ))


# Subtitle of the Plot

subtitle_text <- str_wrap("<br>The climate related natural disasters and
earthquakes since 1900 are modeled simply. Even <br>though the total number of
disasters stayed constant until 1970, each type of disaster has an<br> increasing
path after this year. <span style='color:#0A35F7'>Flood <span style='color:black'>and 
<span style='color:#8C950B'>Extreme Weather Conditions 
<span style='color:black'>have the highest<br> increase, but the rest of them have
less activity. ", 80)

# The Plot

p <- ggplot(data_global, aes(x = year, y = number_of_reported_natural_disasters,
                        colour = entity, group = entity)) +
  geom_point(alpha = 0.5) +
  geom_labelsmooth(aes(label = entity, hjust = hj), text_smoothing = 30,
                   fill = "#F6F6FF",
                   method = "loess", formula = y ~ x, 
                   size = 8, linewidth = 1, boxlinewidth = 0.3,
                   family = "rs", vjust = 0.3) +
  scale_colour_manual(values = c("Drought" = "#BB720E",
                                 "Earthquake" = "#0B0701", 
                                 "Extreme temperature" = "#F71C0A",
                                 "Extreme weather" = "#8C950B", 
                                 "Flood" = "#0A35F7", 
                                 "Volcanic activity" = "#E84C22", 
                                 "Wildfire" = "#963919")) +
  coord_cartesian(clip = "off") +
  labs(y = "TOTAL",
       x = "",
       title = "NATURAL DISASTERS",
       subtitle = subtitle_text,
       caption = "Data: OWID |#30DayChartChallenge - Day #26 |Prepared by: C. YAZICI") +
  theme(panel.background = element_rect(fill = "#EDE6D0", color = NA),
        plot.background = element_rect(fill = "#EDE6D0", color = NA),
        panel.grid = element_blank(),
        axis.ticks = element_blank(),
        legend.position = "none",
        axis.title = element_text(family = "rs", size = 22),
        axis.text = element_text(family = "rs", size = 20),
        plot.title = element_text(family = "rs", size = 50, hjust = 0),
        plot.caption = element_text(family = "rs", hjust = 1, size = 20),
        plot.subtitle = element_markdown(family = "rs", hjust = 0, size = 25),
        plot.margin = unit(c(1.5, 0.8, 1.5, 1.5), "cm"))

# Save the Plot

ggsave("Day25.png", p, width = 25, height = 12, dpi = 72) 

