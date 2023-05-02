library(readxl)
library(tidyverse)
library(janitor)
library(rnaturalearth)
library(sf)
library(plotly)
library(showtext)
library(usefunc)


# Font in the Plot

font_add_google('Space Grotesk', 'sg')
showtext_auto()

data_quake <- read_tsv("earthquakes.tsv") %>%
  clean_names() %>%
  drop_na(location_name) %>%
  filter(longitude > 25,
         location_name != "TURKEY-CIS")

# Turkey Map 
tr_sf <- ne_states(country = "turkey", returnclass = "sf")

# Spatial Data 

tr_sf_data <- st_as_sf(data_quake,
                        coords = c("longitude", "latitude"),
                        crs = 4326,
                        remove = FALSE)

feb6 <- tr_sf_data %>%
  filter(year == 2023, 
         mo == 2,
         dy == 6)
  
subtitle_text <- str_wrap("\nTotal of 174 earthquakes struck Turkey since 1900. 
The magnitude of them have a range between 4.0 to 7.8 and their deepness change
between 3km to 80km. On February 6, 2023, two earthquakes struck Turkey with 
magnitudes of 7.8 and 7.5 
with the depth of 17km and 10 km.", 80)  
  
# Plot

p <- ggplot() +
  geom_sf(data = tr_sf,
          colour = "black",
          fill = "#F9F0FC",
          size = 0.2) +
  geom_sf(data = tr_sf_data, 
          mapping = aes(size = mag,
                        colour = focal_depth_km)) +
  geom_text(data = feb6, aes(x = longitude, y = latitude + 0.25, 
                             label = mag), size = 7, family = "sg") +
  scale_colour_gradient2(
    low = "#F9E1E1",
    mid = "#D05858",
    high = "#720606",
    midpoint = 30,
    na.value = "grey50") +
  coord_sf() +
  geom_curve(aes(x = 39, y = 36, xend = 37.2, yend = 37.16),
             arrow = arrow(length = unit(1, "cm")),
             curvature = 0.5, size = 1.5) +
  geom_curve(aes(x = 39, y = 36, xend = 37.2, yend = 38.02),
             arrow = arrow(length = unit(1, "cm")),
             curvature = 0.5, size = 1.5) + 
  geom_text(aes(x = 40.3, y = 35.8, label = "These two earthquakes struck 
                Turkey on Feb 6, 2023.\n"), hjust = 0.18, size = 5, family = "sg") +
  labs(title = "EARTHQUAKES IN TURKEY\n",
       caption = "\nData Source: noaa.gov | #30DayChartChallenge - Day #7 | Prepared by: C. YAZICI",
       colour = "Depth",
       size = "Magnitude",
       subtitle = subtitle_text) +
  theme(panel.background = element_rect(fill = "lightblue", color = NA),
        plot.background = element_rect(fill = "lightblue", color = NA),
        panel.grid = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank(),
        axis.text = element_blank(),
        plot.title = element_text(size = 42, hjust = 0.5, family = "sg"),
        plot.subtitle = element_text(size = 28, hjust = 0, family = "sg"),
        plot.caption = element_text(size = 20, hjust = 1, family = "sg"),
        legend.title = element_text(family = "sg", size = 20, colour = "black"),
        legend.text = element_text(family = "sg", size = 18, colour = "black"),
        legend.background = element_rect(fill = "lightblue"),
        legend.key.size = unit(1, 'cm'),
        plot.margin = unit(c(1.5, 1.0, 1.5, 1.5), "cm"))

# Save the Plot

ggsave("Day7.png", p, width = 25, height = 12, dpi = 72) 




