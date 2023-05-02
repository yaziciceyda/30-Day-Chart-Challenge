library(readxl)
library(janitor)
library(lubridate)
library(showtext)
library(ggtext)
library(ggnewscale)
library(usefunc)


# Font in the Plot

font_add_google('Merriweather', 'mw')
showtext_auto()

# library to use fontawesome icons
font_add('fa-reg', 'Font Awesome 6 Free-Regular-400.otf')
font_add('fa-brands', 'Font Awesome 6 Brands-Regular-400.otf')
font_add('fa-solid', 'Font Awesome 6 Free-Solid-900.otf')
showtext_auto()


# Data Import

data_weather <- read_xlsx("Weather.xlsx") %>%
  clean_names() 

# Labels for the temperature 

label <- c("<span style='font-family:fa-solid'>&#xf76b;</span>",
          "<span style='font-family:fa-solid'>&#xf2ca;</span>",
          "<span style='font-family:fa-solid'>&#xf2ca;</span>",
          "<span style='font-family:fa-solid'>&#xf2c9;</span>",
          "<span style='font-family:fa-solid'>&#xf2c8;</span>",
          "<span style='font-family:fa-solid'>&#xf2c7;</span>",
          "<span style='font-family:fa-solid'>&#xf769;</span>",
          "<span style='font-family:fa-solid'>&#xf769;</span>",
          "<span style='font-family:fa-solid'>&#xf2c7;</span>",
          "<span style='font-family:fa-solid'>&#xf2c8;</span>",
          "<span style='font-family:fa-solid'>&#xf2ca;</span>",
          "<span style='font-family:fa-solid'>&#xf2ca;</span>")
        
# Labels for the rainfall 

label_rain <- c("<span style='font-family:fa-solid'>&#xf740;</span>",
                "<span style='font-family:fa-solid'>&#xf740;</span>",
                "<span style='font-family:fa-solid'>&#xf740;</span>",
                "<span style='font-family:fa-solid'>&#xf740;</span>",
                "<span style='font-family:fa-solid'>&#xf740;</span>",
                "<span style='font-family:fa-solid'>&#xf740;</span>",
                "<span style='font-family:fa-solid'>&#xf043;</span>",
                "<span style='font-family:fa-solid'>&#xf043;</span>",
                "<span style='font-family:fa-solid'>&#xf043;</span>",
                "<span style='font-family:fa-solid'>&#xf73d;</span>",
                "<span style='font-family:fa-solid'>&#xf73d;</span>",
                "<span style='font-family:fa-solid'>&#xf740;</span>")
  
  
 # Data Wrangling

weather_temp <- data_weather %>%
  mutate(month.nr = 1:12,
         month_name = month.abb[month.nr],
         label = label) %>%
  arrange(month.nr)

weather_rain <- data_weather %>%
  mutate(month.nr = 1:12,
         month_name = month.abb[month.nr],
         label = label_rain,
         rainfall = ifelse(rainfall > 52, 50, rainfall)) %>%
  arrange(month.nr)


subtitle_text <- str_wrap("The average rainfall and temperature for each month of Turkey
is given. There is a negative relationship between these two. When the 
temperature takes high values, there is little rainfall. However, when the 
weather is cold, it gets more rainfall.", 140)

# The Plot

p <- ggplot() +
  geom_line(data = weather_rain,
            mapping = aes(x = reorder(month_name, month.nr), 
                y = rainfall, group = 1), size = 2, colour = "#4454B1",
            linetype = "dashed") +
  geom_richtext(data = weather_rain,
                mapping = aes(x = reorder(month_name, month.nr),
                    y = rainfall,
                    label = label,
                    colour = rainfall),
                family = 'fontawesome-webfont', size = 16,
                fill = NA, label.colour = NA) +
  scale_colour_gradient2(
    low = "#B8BFE7",
    mid = "#4454B1",
    high = "#05177E",
    midpoint = 27,
    na.value = "grey50") +
  labs(colour = "Average\nRainfall (mm)") +
  new_scale_colour() +
  geom_line(data = weather_temp,
            mapping = aes(x = reorder(month_name, month.nr), 
                y = mean_t, group = 1),
            colour = "#EADB2A", size = 2, linetype = "dashed") +
  geom_richtext(data = weather_temp,
                mapping = aes(x = reorder(month_name, month.nr),
                    y = mean_t,
                    label = label,
                    colour = mean_t),
                family = 'fontawesome-webfont', size = 16,
                fill = NA, label.colour = NA) +
  scale_colour_gradient2(
    low = "#1079EF",
    mid = "#EADB2A",
    high = "#9A0B07",
    midpoint = 11,
    na.value = "grey50") +
  labs(colour = "Average\nTemperature (°C)") +
  new_scale_colour() +
  # Text
  geom_text(aes(x = 0.8, y = 16, 
  label = "The average temperature\ntakes negative value\nonly in January"),
  family = "mw", size = 8, hjust = 0) +
  # Arrow
  geom_curve(aes(x = 1.2, xend = 0.9, 
                 y = 10, yend = 6), color = "black",
             arrow = arrow(length = unit(1, "cm")), size = 1.5) +
  labs(x = "Months",
       y = "",
       title = "AVERAGE TEMPERATURE AND RAINFALL IN TURKEY",
       subtitle = subtitle_text,
       caption = "\nData Source: Worldbank | #30DayChartChallenge - Day #15 | Prepared by: C. YAZICI") +
  theme(panel.background = element_rect(fill = "#CDF6D0", color = NA),
        plot.background = element_rect(fill = "#CDF6D0", color = NA),
        panel.grid = element_blank(),
        axis.ticks = element_blank(),
        axis.text.y = element_blank(),
        legend.background = element_rect(fill = "#CDF6D0"),
        axis.text = element_text(family = "mw", size = 18),
        axis.title = element_text(family = "mw", size = 21),
        legend.title = element_text(family = "mw", size = 15),
        legend.text = element_text(family = "mw", size = 13),
        legend.key.height = unit(2.0, 'cm'),
        plot.title = element_text(family = "mw", hjust = 0.5, size = 35,
                                  margin = margin(0, 0, 0, 0)),
        plot.subtitle = element_text(family = "mw", hjust = 0, size = 20,
                                     margin = margin(0, 20, 0, 0)),
        plot.caption = element_text(family = "mw", hjust = 1, size = 18),
        plot.margin = unit(c(0.5, 1.0, 0.5, 1.0), "cm"))

# Save the Plot

ggsave("Day15.png", p, width = 30, height = 15, dpi = 72)



