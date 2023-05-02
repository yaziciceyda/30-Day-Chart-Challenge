library(readxl)
library(janitor)
library(tidyverse)
library(scales)
library(ggnewscale)
library(showtext)
library(usefunc)

# Font in the Plot

font_add_google('Poppins', 'pp')
showtext_auto()

# Data Import


data_ag_land_use <- data.frame(country = c("Greece", "Italy", "Portugal",
                                       "Spain", "Turkey"),
                           ag_land_use = c(5867200, 12999000, 3893900, 
                                           26142600, 37762000)) %>%
                             mutate(y = rescale(ag_land_use, to = c(0, 10)))


data_apple <- read_excel("Apple_m.xlsx")%>%
  clean_names() %>%
  select(country, production_t, land_use_ha) %>%
  right_join(data_ag_land_use, by = "country") %>%
  mutate(land_use_percent = (land_use_ha/ag_land_use * 100),
         prod = "apple")


data_blueberry <- read_excel("Blueberry_m.xlsx")%>%
  clean_names() %>%
  select(country, production_t, land_use_ha) %>%
  right_join(data_ag_land_use, by = "country") %>%
  mutate(land_use_percent = (land_use_ha/ag_land_use * 100),
         prod = "blueberry")

data_lemon <- read_excel("Lemon_m.xlsx")%>%
  clean_names() %>%
  select(country, production_t, land_use_ha) %>%
  right_join(data_ag_land_use, by = "country") %>%
  mutate(land_use_percent = (land_use_ha/ag_land_use * 100),
         prod = "lemon")

data_lettuce <- read_excel("Lettuce_m.xlsx")%>%
  clean_names() %>%
  select(country, production_t, land_use_ha) %>%
  right_join(data_ag_land_use, by = "country") %>%
  mutate(land_use_percent = (land_use_ha/ag_land_use * 100),
         prod = "lettuce")

data_walnut <- read_excel("Walnut_m.xlsx")%>%
  clean_names() %>%
  select(country, production_t, land_use_ha) %>%
  right_join(data_ag_land_use, by = "country") %>%
  mutate(land_use_percent = (land_use_ha/ag_land_use * 100),
         prod = "walnut")

data_potatoes <- read_excel("Potatoes_m.xlsx")%>%
  clean_names() %>%
  select(country, production_t, land_use_ha) %>%
  right_join(data_ag_land_use, by = "country") %>%
  mutate(land_use_percent = (land_use_ha/ag_land_use * 100),
         prod = "potatoes")


data_all <- data_apple %>%
  rbind(data_lemon) %>%
  rbind(data_potatoes) %>%
  rbind(data_lettuce) %>%
  rbind(data_walnut) %>%
  rbind(data_blueberry) %>%
  mutate(country_num = case_when(
    country == "Greece" ~ 1,
    country == "Italy" ~ 2,
    country == "Portugal" ~ 3,
    country == "Spain" ~ 4,
    country == "Turkey" ~ 5,
  )) 

subtitle_text <- str_wrap("\nThe land usage for six different agricultural products for Greece, Italy,
Portugal, Spain and Turkey is given as petals of the flowers. Among these five countries, 
Turkey has the largest land usage for agriculture. 
Moreover, Italy has the largest land usage for lemon and lettuce, while Portugal has the 
same one for blueberry and Turkey has the largest usage for the rest of the products.", 115)

p <- ggplot(data_all) +
  geom_hline(aes(yintercept = 0), colour = "tan4", size = 5) +
  geom_curve(aes(x = country_num * 10, xend = country_num * 10, 
                 y = y + 2, yend = 0),
             colour = "forestgreen", size = 4,
             curvature = -0.05) +
  geom_regon(aes(x0 = (country_num * 10), y0 = y + 2, 
                 sides = 6, angle = pi/2, r = 1.5),
             fill = "orange", colour = "black", size = 0.3) +
  geom_text(aes(x = country_num * 10, y = y + 2, label = toupper(country)),
            family = "pp", size = 4) +
  # Apples
  geom_regon(data_all %>%
               filter(prod == "apple"), 
             mapping = aes(x0 = (country_num * 10) + 2.6, 
                 y0 = 2.05 + y, sides = 6, angle = pi/2, r = 1.5,
                 fill = land_use_percent), colour = "red", size = 0.3) +
  scale_fill_gradient2(
    low = "#F1AC9F",
    mid = "#E85B6A",
    high = "#E71504",
    midpoint = 0.22,
    na.value = "grey50") +
  labs(fill = "Apple") +
  new_scale_fill() +
  geom_text(aes(x = country_num * 10 + 2.6, y = 2.05 + y), label = "Apple",
            family = "pp", size = 5) +
  # Lemon
  geom_regon(data_all %>%
               filter(prod == "lemon"), 
             mapping = aes(x0 = (country_num * 10) + 1.299, 
                 y0 = y - 0.23, sides = 6, angle = pi/2, r = 1.5,
                 fill = land_use_percent), colour = "yellow", size = 0.3) +
  scale_fill_gradient2(
    low = "#E9EDBD",
    mid = "#E3DD5F",
    high = "#E1D707",
    midpoint = 0.10,
    na.value = "grey50") +
  labs(fill = "Lemon") +
  new_scale_fill() +
  geom_text(aes(x = country_num * 10 + 1.299, y = y - 0.23), label = "Lemon",
            family = "pp", size = 5) +
  # Walnut
  geom_regon(data_all %>%
                filter(prod == "walnut"), 
              mapping = aes(x0 = (country_num * 10) + 1.299,
                 y0 = y + 4.25, sides = 6, angle = pi/2, r = 1.5,
                 fill = land_use_percent), colour = "#F79533", size = 0.3) +
  scale_fill_gradient2(
    low = "#F1C39F",
    mid = "#A26634",
    high = "#924606",
    midpoint = 0.15,
    na.value = "grey50") +
  labs(fill = "Walnut") +
  new_scale_fill() +
  geom_text(aes(x = country_num * 10 + 1.299, y = y + 4.25), label = "Walnut",
            family = "pp", size = 5) +
  # Lettuce
  geom_regon(data_all %>%
               filter(prod == "lettuce"), 
             mapping = aes(x0 = (country_num * 10) - 1.299,
                y0 = y + 4.2, sides = 6, angle = pi/2, r = 1.5,
                  fill = land_use_percent), colour = "green", size = 0.3) +
  scale_fill_gradient2(
    low = "#9EE0B0",
    mid = "#61CE7F",
    high = "#0AB93A",
    midpoint = 0.12,
    na.value = "grey50") +
  labs(fill = "Lettuce") +
  new_scale_fill() +
  geom_text(aes(x = country_num * 10 - 1.299, y = y + 4.2), label = "Lettuce",
            family = "pp", size = 5) +
  # Blueberry
  geom_regon(data_all %>%
               filter(prod == "blueberry"), 
             mapping = aes(x0 = (country_num * 10) - 1.299*2, 
                y0 = y + 2, sides = 6, angle = pi/2, r = 1.5,
                 fill = land_use_percent), colour = "blue", size = 0.3) +
  scale_fill_gradient2(
    low = "#80B7DD",
    mid = "#457A9E",
    high = "#0C5C93",
    midpoint = 0.08,
    na.value = "grey50") +
  labs(fill = "Blueberry") +
  new_scale_fill() +
  geom_text(aes(x = country_num * 10 - 2 * 1.299, y = y + 2), 
            label = "Blueberry", family = "pp", size = 5) +
  # Potatoes
  geom_regon(data_all %>%
               filter(prod == "potatoes"), 
             mapping = aes(x0 = (country_num * 10) - 1.299, 
                y0 = y - 0.25, sides = 6, angle = pi/2, r = 1.5,
                fill = land_use_percent), colour = "brown", size = 0.3) +
  scale_fill_gradient2(
    low = "#F1C39F",
    mid = "#DCAF60",
    high = "#ECA730",
    midpoint = 0.30,
    na.value = "grey50") +
  labs(fill = "Potatoe") +
  new_scale_fill() +
  geom_text(aes(x = country_num * 10 - 1.299, y = y - 0.25), 
            label = "Potatoe", family = "pp", size = 5) +
  # Arrow
  geom_text(aes(x = 45, y = 3.5, label = "The (rescaled) agricultural 
  land use\n of the countries 
                is represented with the height\n to the center of the flower"),
            family = "pp", size = 5) +
  geom_curve(aes(x = 45, y = 5.5, xend = 50, yend = 7.5),
             arrow = arrow(length = unit(1, "cm")),
             curvature = -0.5, size = 1.5) +
  labs(title = "AGRICULTURAL LAND USAGE",
       subtitle = subtitle_text,
       y = "Rescaled Agricultural Land Use",
       caption = "Data Source: OWID & World Bank | #30DayChartChallenge - Day #3 | Prepared by: C. YAZICI") +
  theme(panel.background = element_rect(fill = "#BDEDE1", color = NA),
        plot.background = element_rect(fill = "#BDEDE1", color = NA),
        panel.grid = element_blank(),
    plot.title = element_text(family = "pp", size = 40, hjust = 0.5),
    plot.subtitle = element_text(family = "pp", size = 25, hjust = 0),
    plot.caption = element_text(family = "pp", size = 17, hjust = 1),
    axis.ticks = element_blank(),
    axis.title.x = element_blank(),
    axis.text.x = element_blank(),
    axis.text.y = element_text(family = "pp", size = 16),
    axis.title.y = element_text(family = "pp", size = 19),
    legend.background = element_rect(fill = "#BDEDE1"),
    legend.title = element_text(family = "pp", size = 15),
    legend.text = element_text(family = "pp", size = 13),
    plot.margin = unit(c(1.0, 1.0, 1.0, 1.0), "cm")) 

# Save the Plot

ggsave("Day3.png", p, width = 30, height = 15, dpi = 72)








