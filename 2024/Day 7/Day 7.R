library(tidyverse)
library(readr)
library(janitor)
library(lubridate)
library(rnaturalearth)
library(gganimate)
library(sf)
library(showtext)
library(usefunc)
library(magick)
library(beepr)
library(knitr)

# Font in the Plot

font_add_google('Space Grotesk', 'sg')
showtext_auto()

# Data Import

data7 <- read_csv("data.csv") %>%
  clean_names() %>%
  mutate(date = dmy_hms(date),
         year = year(date),
         month = month(date),
         day = day(date),
         hour = hour(date)) %>%
  filter(year == 2023,
         month == 2,
         day < 7,
         longitude > 32) 

max_longitude <- max(data7$longitude)
min_longitude <- min(data7$longitude)
max_latitude <- max(data7$latitude)
min_latitude <- min(data7$latitude)


# Turkey Map 
tr_sf <- ne_states(country = "turkey", returnclass = "sf")

# Spatial Data 

tr_sf_data <- st_as_sf(data7,
                       coords = c("longitude", "latitude"),
                       crs = 4326,
                       remove = FALSE)


tag_text <- str_wrap("On February 6, 2023, two earthquakes struck 
Turkey with magnitudes of 7.7 and 7.6 with the depth of 17 km and 
10 km, respectively. The first largest one occured at 4:17 and total 
of 234 earthquakes whose magnitude is greater than 4.0 occured 
on the same day.", 50)

# Turkey Map

p1 <- ggplot() +
  geom_sf(data = tr_sf,
          colour = "brown",
          fill = "#F9F0FC",
          size = 0.2) +
  geom_rect(aes(xmin = min_longitude - 0.5, xmax = max_longitude + 0.5,
                ymin = min_latitude - 0.5, ymax = max_latitude + 0.5),
            fill = NA, color = "black") +
  coord_sf() +
  labs(title = "EARTHQUAKES IN TURKEY\n",
       tag = tag_text,
       caption = "Data Source: AFAD | #30DayChartChallenge - Day #7 | Prepared by: C. YAZICI") +
  theme(panel.background = element_rect(fill = "lightblue", color = NA),
        plot.background = element_rect(fill = "lightblue", color = NA),
        panel.grid = element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_blank(),
        axis.title = element_blank(),
        plot.title = element_text(family = "sg", size = 40, 
                                  hjust = 0, vjust = -10),
        plot.tag = element_text(family = "sg", size = 18, 
                                  hjust = 0),
        plot.caption = element_text(family = "sg", hjust = -0.2, 
                                    vjust = -115, size = 20), 
      #  plot.tag.position = c(0.96, 0.6),
        plot.tag.position = c(0.73, -0.4),
        legend.position = "none",
        plot.margin = margin(0.1, 0.7, 5.5, 0.1, "cm"))

ggsave("p1.png")

# Animation 

p <- ggplot() +
  geom_sf(data = tr_sf,
          colour = "black",
          fill = "#F9F0FC",
          size = 0.2) +
  geom_sf(data = tr_sf_data, 
          mapping = aes(size = magnitude,
                        colour = depth)) +
  coord_sf(xlim = c(34, 42.2),
           ylim = c(34.8, 39.2)) +
  scale_color_gradient(high = "#511B02",
                       low = "#FCB290") +
  labs(color = "Depth",
       size = "Magnitude") +
  transition_time(hour) +
  theme(panel.background = element_rect(fill = "lightblue", 
                                        color = "lightblue", 
                                        linewidth = 7),
        plot.background = element_rect(fill = "lightblue", 
                                       color = "lightblue", 
                                       linewidth = 7),
        panel.grid = element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_blank(),
        axis.line.x = element_blank(),
        legend.background = element_rect(fill = "lightblue"),
        legend.title = element_text(family = "sg", size = 15,
                                    hjust = 0),
        legend.text = element_text(family = "sg", size = 14,
                                   hjust = 0),
        legend.position = "top",
        legend.key.width = unit(1, 'cm'),
        plot.margin = margin(0.5, 0.5, 0.5, 0.5, "cm")) +
  guides(color = guide_colorbar(title.position = "top", 
                                title.hjust = 0.5),
         size = guide_legend(title.position = "top", 
                             title.hjust = 0.5))

map_with_shadow <- p +
  ggtitle(paste0('Hour: {frame_time}', ":00")) +
  shadow_mark()


anim_save("Day7.gif", map_with_shadow, nframes = 18,
          width = 730, height = 605)



  
knitr::include_graphics("p1.png")
knitr::include_graphics("day7.gif")

turkey_plot <- image_read("p1.png")
turkey_gif <- image_read("day7.gif")
frames <- image_composite(turkey_plot, turkey_gif, offset = "+200+600")
animation <- image_animate(frames, fps = 10)
image_write(animation, "Day7_final.gif")



