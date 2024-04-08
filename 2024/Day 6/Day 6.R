library(tidyverse)
library(readxl)
library(janitor)
library(ggimage)
library(showtext)


# Font in the Plot

font_add_google('Roboto Slab', 'rs')
showtext_auto()

# The droplet image is taken from 
# https://waterdata.usgs.gov/blog/ggplot-jazz/
  
data6 <- readxl::read_xlsx("oecd_data.xlsx") %>%
  clean_names() %>%
  arrange(measles)

subtitle_text <- str_wrap("\nThe percentage of children vaccinated for 
diphtheria, tetanus, pertussis (written in the droplet) and 
measles (written in the injector) for several countries are shown 
here. Poland has the least vaccination rate for measles and 
Brazil has the least rate for diphtheria, tetanus, pertussis. 
On the other hand, China has the highest vaccination rate for both
group of diseases.", 120)

p <- ggplot(data6) +
  geom_rect(aes(xmin = 0, xmax = 1, ymin = 0.2, ymax = 0.5),
            color = "grey80") +
  geom_rect(aes(xmin = 0, xmax = measles / 100, ymin = 0.2, ymax = 0.5,
                fill = measles)) +
  # First  
  geom_rect(aes(xmin = 1.0, xmax = 1.05, ymin = 0.1, ymax = 0.6),
                fill = "#4FD8EB") +
  geom_rect(aes(xmin = 1.05, xmax = 1.15, ymin = 0.4, ymax = 0.3),
            fill = "grey60") +
  geom_rect(aes(xmin = 1.15, xmax = 1.2, ymin = 0.2, ymax = 0.5),
            fill = "#4FD8EB") +
  # Second
  geom_rect(aes(xmin = -0.1, xmax = 0, ymin = 0.25, ymax = 0.45),
            fill = "#4FD8EB") +
  geom_text(aes(x = -0.05, y = 0.35, label = measles),
            family = "rs", hjust = 0.5, size = 5.5) +
  geom_rect(aes(xmin = -0.2, xmax = -0.1, ymin = 0.3, ymax = 0.4),
            fill = "grey60") +
  geom_segment(aes(x = -0.2, xend = -0.5,
                   y = 0.35, yend = 0.35), linewidth = 0.8) +
  # Lines
  geom_segment(aes(x = 0.2, xend = 0.2,
                   y = 0.2, yend = 0.25), linewidth = 1.2) +
  geom_segment(aes(x = 0.25, xend = 0.25,
                   y = 0.2, yend = 0.35), linewidth = 1.2) +
  geom_segment(aes(x = 0.3, xend = 0.3,
                   y = 0.2, yend = 0.25), linewidth = 1.2) +
  geom_segment(aes(x = 0.35, xend = 0.35,
                   y = 0.2, yend = 0.35), linewidth = 1.2) +
  geom_segment(aes(x = 0.4, xend = 0.4,
                   y = 0.2, yend = 0.25), linewidth = 1.2) +
  geom_segment(aes(x = 0.45, xend = 0.45,
                   y = 0.2, yend = 0.35), linewidth = 1.2) +
  geom_segment(aes(x = 0.5, xend = 0.5,
                   y = 0.2, yend = 0.25), linewidth = 1.2) +
  geom_segment(aes(x = 0.55, xend = 0.55,
                   y = 0.2, yend = 0.35), linewidth = 1.2) +
  geom_segment(aes(x = 0.6, xend = 0.6,
                   y = 0.2, yend = 0.25), linewidth = 1.2) +
  geom_segment(aes(x = 0.65, xend = 0.65,
                   y = 0.2, yend = 0.35), linewidth = 1.2) +
  geom_segment(aes(x = 0.7, xend = 0.7,
                   y = 0.2, yend = 0.25), linewidth = 1.2) +
  geom_segment(aes(x = 0.75, xend = 0.75,
                   y = 0.2, yend = 0.35), linewidth = 1.2) +
  scale_fill_gradient(high = "#F93505",
                      low = "#F7BDAF") +
  ggimage::geom_image(aes(x = -0.5, y = 0.25, color = diphtheria,
         image = 'https://labs.waterdata.usgs.gov/visualizations/23_chart_challenge/raindrop.png'),
                      # Aspect ratio
                      asp = 4) +
  geom_text(aes(x = -0.5, y = 0.22, label = diphtheria),
            family = "rs", hjust = 0.5, size = 5.5, color = "ivory") +
  scale_colour_gradient(high = "#0A6A28",
                        low = "#9CF5B8") +
  facet_wrap(~reorder(country, measles), ncol = 2) +
  labs(fill = "Measles",
       color = "Diphtheria, Tetanus, Pertussis",
       title = "Child Vaccination Rates",
       subtitle = subtitle_text,
       caption = "Data Source: OECD | #30DayChartChallenge - Day #6 | Prepared by: C. YAZICI") +
  theme(panel.background = element_rect(fill = "ivory", color = NA),
        plot.background = element_rect(fill = "ivory", color = NA),
        panel.grid = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank(),
        axis.text = element_blank(),
        strip.background = element_rect(fill = "ivory"),
        strip.text = element_text(family = "rs", hjust = 0.5, 
                                  size = 18),
        legend.background = element_rect(fill = "ivory"),
        legend.position = "top",
        legend.title = element_text(family = "rs", hjust = 0.5, 
                                    size = 18),
        legend.text = element_text(family = "rs", hjust = 0.5, 
                                    size = 16),
        legend.key.width = unit(2.2, 'cm'),
        plot.caption = element_text(family = "rs", hjust = 1, 
                                    size = 18),
        plot.title = element_text(family = "rs", hjust = 0, 
                                    size = 40),
        plot.subtitle = element_text(family = "rs", hjust = 0, 
                                  size = 22),
        plot.margin = margin(0.7, 0.7, 0.7, 0.7, "cm")) +
  guides(fill = guide_colorbar(title.position = "top",
                               title.hjust = 0.5),
         color = guide_colorbar(title.position = "top", 
                                title.hjust = 0.5))


# Save the Plot

ggsave("Day6.png", p, width = 24, height = 16, dpi = 72)



