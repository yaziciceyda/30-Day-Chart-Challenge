library(maps)
library(janitor)
library(showtext)
library(ggrepel)
library(cowplot)


# Font icolorspaces# Font in the Plot

font_add_google('Ubuntu', 'ubuntu')
showtext_auto()

city_data <- world.cities %>%
  clean_names() %>%
  filter(str_detect(name, "new ") | str_detect(name, "New ")) %>%
  mutate(continent = case_when(
    country_etc == "UK" ~ "Europe",
    country_etc == "Ireland" ~ "Europe",
    country_etc == "USA" ~ "North America",
    country_etc == "Canada" ~ "North America",
    country_etc == "Guyana" ~ "South America",
    country_etc == "Philippines" ~ "Asia",
    country_etc == "Mauritius" ~ "Africa",
    country_etc == "Ghana" ~ "Africa",
    country_etc == "Australia" ~ "Oceania",
    country_etc == "New Zealand" ~ "Oceania",
  )) %>%
  group_by(continent) %>%
  mutate(n = n()) %>%
  ungroup() %>%
  arrange(n, continent)

worldmap <- ne_countries(scale = 'medium', type = 'map_units',
                         returnclass = 'sf')
# Africa

africa <- worldmap[worldmap$continent == "Africa", ]

p_africa <- ggplot() +
  geom_sf(data = africa, fill = "#DBAC54", colour = NA) +
  geom_point(data = city_data %>% filter(continent == "Africa"), 
             aes(x = long, y = lat), color = "brown", size = 5) +
  geom_text(data = city_data %>% filter(continent == "Africa"), 
            aes(x = long, y = lat + 3, label = name), 
            family = "ubuntu", hjust = 0.5, vjust = 0.5,
            size = 4) +
  coord_sf(xlim = c(-20, 68), expand = FALSE) +
  labs(title = "AFRICA") +
  theme(panel.background = element_rect(fill = "ivory", color = NA),
        plot.background = element_rect(fill = "ivory", color = NA),
        panel.grid = element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_blank(),
        axis.title = element_blank(),
        plot.title = element_text(family = "ubuntu", size = 18,
                                  hjust = 0.5),
        plot.margin = unit(c(0.6, 0, 0, 0.5), "cm"))
  
  
# Asia

asia <- worldmap[worldmap$continent == "Asia", ]

p_asia <- ggplot() +
  geom_sf(data = asia, fill = "#DBAC54", colour = NA) +
  geom_point(data = city_data %>% filter(continent == "Asia"), 
             aes(x = long, y = lat), color = "brown", size = 5) +
  geom_text_repel(data = city_data %>% filter(continent == "Asia"), 
            aes(x = long, y = lat + 5, label = name), 
            family = "ubuntu", hjust = 0.5, vjust = 0.5,
            size = 4) +
  coord_sf(expand = FALSE) +
  labs(title = "ASIA") +
  theme(panel.background = element_rect(fill = "ivory", color = NA),
        plot.background = element_rect(fill = "ivory", color = NA),
        panel.grid = element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_blank(),
        axis.title = element_blank(),
        plot.title = element_text(family = "ubuntu", size = 18,
                                  hjust = 0.5),
        plot.margin = unit(c(0.6, 0.5, 0, 0.5), "cm"))


# Europe

europe <- worldmap[worldmap$continent == "Europe", ]

p_europe <- ggplot() +
 geom_sf(data = europe, fill = "#C88535", colour = NA) +
  geom_point(data = city_data %>% filter(continent == "Europe"), 
             aes(x = long, y = lat), color = "brown", size = 5) +
  geom_text_repel(data = city_data %>% filter(continent == "Europe"), 
            aes(x = long, y = lat + 2, label = name), 
            family = "ubuntu", hjust = 0.5, vjust = 0.5,
            size = 4) +
  coord_sf(xlim = c(-60, 185),
           ylim = c(35, 85), expand = FALSE) +
  labs(title = "EUROPE") +
  theme(panel.background = element_rect(fill = "ivory", color = NA),
        plot.background = element_rect(fill = "ivory", color = NA),
        panel.grid = element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_blank(),
        axis.title = element_blank(),
        plot.title = element_text(family = "ubuntu", size = 18,
                                  hjust = 0.5),
        plot.margin = unit(c(0.6, 0.5, 0, 0.5), "cm"))

# South America

s_america <- worldmap[worldmap$continent == "South America", ]

p_s_america <- ggplot() +
    geom_sf(data = s_america , fill = "#edd273", colour = NA) +
  geom_point(data = city_data %>% filter(continent == "South America"), 
             aes(x = long, y = lat), color = "brown", size = 5) +
  geom_text_repel(data = city_data %>% filter(continent == "South America"), 
                  aes(x = long, y = lat + 2, label = name), 
                  family = "ubuntu", hjust = 0.5, vjust = 0.5,
                  size = 4) +
  coord_sf(xlim = c(-90, -35), expand = FALSE) +
  labs(title = "SOUTH AMERICA") +
  theme(panel.background = element_rect(fill = "ivory", color = NA),
        plot.background = element_rect(fill = "ivory", color = NA),
        panel.grid = element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_blank(),
        axis.title = element_blank(),
        plot.title = element_text(family = "ubuntu", size = 18,
                                  hjust = 0.5),
        plot.margin = unit(c(0.6, 0.5, 0, 0.5), "cm"))

# North America

n_america <- worldmap[worldmap$continent == "North America", ]

options(ggrepel.max.overlaps = Inf)

p_n_america <- ggplot() +
  geom_sf(data = n_america , fill = "#BF7226", colour = NA) +
  geom_point(data = city_data %>% 
              filter(continent == "North America"), 
             aes(x = long, y = lat), color = "brown", size = 3) +
  geom_text_repel(data = city_data %>% filter(continent == "North America"), 
                  aes(x = long, y = lat + 2, label = name), 
                  family = "ubuntu", hjust = 0.5, vjust = 0.5,
                  size = 4) +
  coord_sf(xlim = c(-160, 0), expand = FALSE) +
  labs(title = "NORTH AMERICA") +
  theme(panel.background = element_rect(fill = "ivory", color = NA),
        plot.background = element_rect(fill = "ivory", color = NA),
        panel.grid = element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_blank(),
        axis.title = element_blank(),
        plot.title = element_text(family = "ubuntu", size = 18,
                                  hjust = 0.5),
        plot.margin = unit(c(0.6, 0.5, 0, 0.5), "cm"))

# Oceania

oceania <- worldmap[worldmap$continent == "Oceania", ]

p_oceania <- ggplot() +
  geom_sf(data = oceania , fill = "#DBAC54", colour = NA) +
  geom_point(data = city_data %>% 
               filter(continent == "Oceania"), 
             aes(x = long, y = lat), color = "brown", size = 5) +
  geom_text_repel(data = city_data %>% filter(continent == "Oceania"), 
                  aes(x = long, y = lat + 3, label = name), 
                  family = "ubuntu", hjust = 0.5, vjust = 0.5,
                  size = 4) +
  coord_sf(xlim = c(110, 180),
           ylim = c(-45, -10), expand = FALSE) +
  labs(title = "OCEANIA") +
  theme(panel.background = element_rect(fill = "ivory", color = NA),
        plot.background = element_rect(fill = "ivory", color = NA),
        panel.grid = element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_blank(),
        axis.title = element_blank(),
        plot.title = element_text(family = "ubuntu", size = 18,
                                  hjust = 0.5),
        plot.margin = unit(c(0.6, 0.5, 0, 0.5), "cm"))

subtitle_text <- str_wrap("\nThere are cities with New in their title in six continents.
There is only one city in South America, but North America has fourteen 
cities.", 130)


# The Final Plot

plots <- align_plots(p_s_america, p_africa, p_asia, 
                     p_oceania, p_europe, p_n_america,
                     align = 'r', axis = 'r')

title1 <- ggdraw() + 
  draw_label(
    bquote('Cities with ' ~ bold('New') ~ 'in their Title'),
    fontfamily = "ubuntu",
    hjust = 0,
    x = 0.01,
    size = 40,
    color = "black"
  )  
title2 <- ggdraw() + 
  draw_label(
    subtitle_text,
    fontfamily = "ubuntu",
    hjust = 0,
    x = 0.01,
    y = 0.4, 
    size = 18,
    color = "black"
  ) 
caption <- ggdraw() + 
  draw_label(
    "Data Source: {maps} | #30DayChartChallenge\n2024 - Day #2 | Prepared by: C. YAZICI", 
    fontfamily = "ubuntu",
    hjust = 1,
    x = 0.95,
    y = 0.5, 
    size = 18,
    color = "black"
  ) 
top_row <-  plot_grid(
  plots[[1]], plots[[2]], plots[[3]], 
  labels = "",
  rel_widths = c(1.0, 1.0, 1.0), 
  nrow = 1
)

bottom_row <- plot_grid(
  plots[[4]], plots[[5]], plots[[6]],
  labels = "",
  rel_widths = c(1.0, 1.0), 
  nrow = 1
)

final_plot <- plot_grid(title1, title2, top_row, bottom_row, caption, 
                        labels = "", ncol = 1,
                        rel_heights = c(0.1, 0.15, 0.9, 0.9, 0.1),
                        align = "hv") +
  theme(plot.background = element_rect(fill = "ivory", colour = NA),
        plot.margin = margin(0.5, 0.5, 0.5, 0.5, "cm"))


# Save the Plot

ggsave("Day2.png", final_plot, width = 20, height = 15, dpi = 72)



