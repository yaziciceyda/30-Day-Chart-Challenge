library(readxl)
library(emojifont)
library(showtext)
library(magick)

# Font in the Plot

font_add_google('Comfortaa', 'com')
showtext_auto()

# Data Import

img <- image_read("globe.png")
img_globe <- image_modulate(img, brightness = 100, saturation = 100, hue = 100)
print(img_globe)
img <- paste0("image_globe.png")

population_data <- read_xlsx("Population.xlsx") %>%
  mutate(img = img)

# The Plot
  
p <-  ggplot(population_data) +
  geom_image(aes(x = Year, y = 0.5, image = img, size = I(Population/70))) +
  geom_text(aes(x = 2000, y = 0.25, label = "Paul Crutuzen and Eugene Stoermer\n
            coin the term the ‘ANTHROPOCENE’. Crutzen suggests the Industrial\n
            Revolution as the starting point, but the Anthropocene Working\n
    Group believe this date only represents regional changes to the Earth."),
            family = "com", size = 6) +
    geom_text(aes(x = Year, y = 0.6, label = paste0(Population, "B")),
              family = "com", size = 10) +
    geom_text(aes(x = 2046, y = 0.4, label = "(Estimated)"), family = "com",
              size = 6) +
    geom_curve(aes(x = 2005, xend = 2000, 
                   y = 0.11, yend = 0.001), color = "black",
               arrow = arrow(length = unit(1, "cm")), size = 1.5) +
    coord_cartesian(ylim = c(0, 0.75),
                    xlim = c(1949, 2050)) +
    scale_x_continuous(breaks = population_data$Year) +
    labs(title = "WORLD POPULATION",
         caption = "\nData Source: geographical.co.uk & eea.europa.eu | #30DayChartChallenge - Day #19 | Prepared by: C. YAZICI") +
  theme(panel.background = element_rect(fill = "grey100", color = NA),
        plot.background = element_rect(fill = "grey100", color = NA),
        panel.grid = element_blank(),
        axis.ticks = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.text.x = element_text(family = "com", size = 17),
        axis.title.x = element_text(family = "com", size = 19),
        plot.caption = element_text(family = "com", hjust = 1, size = 18),
        plot.title = element_text(family = "com", hjust = 0.5, size = 40),
        plot.margin = unit(c(1.0, 0.5, 1.0, 1.0), "cm"))


# Save the Plot

ggsave("Day19.png", p, width = 20, height = 15, dpi = 72)





