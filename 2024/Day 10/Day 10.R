library(dplyr)
library(tidyverse)
library(janitor)
library(showtext)


# Font in the Plot

font_add_google('Eagle Lake', 'el')
showtext_auto()


data10 <- read.csv("Lakes.csv") %>%
  clean_names() %>%
  mutate(depth_in_meters = as.numeric(depth_in_meters),
         depth_in_feet = as.numeric(depth_in_feet)) %>%
  filter(!is.na(depth_in_meters)) %>%
  arrange(depth_in_meters)


#  summarise(min_depth = min(depth_in_meters), #406
#           max_depth = max(depth_in_meters), # 1642
#            avg_depth = mean(depth_in_meters)) #578

# Kernal Density Calculation

density_lake <- density(data10$depth_in_meters)
lake_values <- density_lake$x
density_values <- density_lake$y

lake_data <- tibble(lake_values, density_values)

# plot(density(data10$depth_in_meters))

subtitle_text <- str_wrap("The information including the deepest 35 lakes in 
the world show that their depth ranges between 406 and 1642 meters 
and the average depth is 578m.", 80)

p <- ggplot(lake_data) +
  geom_area(aes(lake_values, -1 * density_values),
             fill = "#0921EE") +
  labs(x = "Depth (in meters)",
       title = "Depth of Lakes",
       subtitle = subtitle_text,
       caption = "Data Source: Kaggle\n#30DayChartChallenge - Day #10\nPrepared by: C. YAZICI") +
  theme(panel.background = element_rect(fill = "grey70", color = NA),
        plot.background = element_rect(fill = "grey70", color = NA),
        panel.grid = element_blank(),
        axis.text.y = element_blank(),
        axis.title.y = element_blank(),
        axis.ticks = element_blank(),
        axis.title.x = element_text(family = "el", size = 20),
        axis.text.x = element_text(family = "el", size = 20),
        plot.title = element_text(family = "el", size = 50,
                                  hjust = 0),
        plot.subtitle = element_text(family = "el", size = 25,
                                                 hjust = 0),
        plot.caption = element_text(family = "el", size = 20,
                                  hjust = 1),
        plot.margin = margin(1, 1.5, 1, 1.5, "cm"))

# Save the Plot

ggsave("Day10.png", p, width = 22, height = 16, dpi = 72)


