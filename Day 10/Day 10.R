library(ggnewscale)
library(showtext)
library(usefunc)

# Font in the Plot

font_add_google('Neucha', 'neucha', db_cache = FALSE)
showtext_auto()

# Data Import

tuesdata <- tidytuesdayR::tt_load(2020, week = 22)


cocktails <- tuesdata$cocktails 
boston_cocktails <- tuesdata$boston_cocktails

# Data Wrangling

ingredient_data <- cocktails %>%
  filter(category %in% c("Cocktail", "Ordinary Drink", "Coffee / Tea",
                         "Homemade Liqueur")) %>%
  group_by(category, drink) %>%
  slice(which.max(ingredient_number)) %>%
  ungroup() %>%
  group_by(category, ingredient_number) %>%
  summarise(n = n()) %>%
  ungroup() %>%
  mutate(label = case_when(
    category == "Cocktail"~ emoji("cocktail"),
    category == "Ordinary Drink"~ emoji("tropical_drink"),
    category == "Coffee / Tea"~ emoji("coffee"),
    category == "Homemade Liqueur"~ emoji("tumbler_glass")
  ))

# Legend

legend_cocktail <- ingredient_data %>%
  filter(category == "Cocktail",
         ingredient_number == 1 |ingredient_number == 5 |
           ingredient_number == 9) %>%
  mutate(y = c(0, 1.2, 1.9),
         label_text = c(60, 10, 5))

subtitle_text <- str_wrap("Cocktails generally include more than one ingredient.
Here, the distribution of the number of ingredients in cocktails are given. There 
is a total of 64 cocktails and 22 of them have 3 and 21 of them have 4 different
ingredients.", 120)
 
# The Plot

p <- ggplot(ingredient_data %>% filter(category == "Cocktail")) +
  geom_text(aes(x = ingredient_number, y = 0, label = label,
                size = n, colour = n, vjust = 0), 
            family = "EmojiOne") + 
  scale_colour_gradient2(
              low = "#CBE392",
              mid = "#d5ff26",
              high = "#8aab00",
              midpoint = 15,
              na.value = "grey50") +
  #n = 9 --> Arizona Twister
  geom_text(aes(x = 9, y = 1.0, label = "Arizona\nTwister"), color = "#FF007F",
            family = "neucha", size = 10) +
  # Arrow
  geom_curve(aes(x = 9, xend = 9, y = 0.8, yend = 0.25), color = "#FF007F",
             arrow = arrow(length = unit(1, "cm")), size = 1.5) +
  # n = 1 --> Tequila Sunrise
  geom_text(aes(x = 1, y = 1.0, label = "Tequila\nSunrise"), color = "#FF007F",
            family = "neucha", size = 10) +
  # Arrow
  geom_curve(aes(x = 1, xend = 1, y = 0.8, yend = 0.25), color = "#FF007F",
             arrow = arrow(length = unit(1, "cm")), size = 1.5) +
  scale_x_continuous(breaks = c(1:9)) +
  scale_size_area(max_size = 70, guide = "none") +
  coord_fixed(xlim = c(0.5, 9), ylim = c(-0.1, 2.5)) +
  labs(x = "NUMBER OF INGREDIENTS",
       colour = "FREQUENCY", 
       title = "COCKTAILS",
       subtitle = subtitle_text, 
       caption = "\nData Source: Kaggle | #30DayChartChallenge - Day #10 | #TidyTuesday - 2020 - Week 22 | Prepared by: C. YAZICI") +
  theme(panel.background = element_rect(fill = "grey20", color = NA),
        plot.background = element_rect(fill = "grey20", color = NA),
        panel.grid = element_blank(),
        axis.ticks = element_blank(),
        axis.text.y = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_text(family = "neucha", size = 25,
                                   color = "#04d9ff"),
        axis.title.x = element_text(family = "neucha", size = 30,
                                    color = "#04d9ff"),
        plot.title = element_text(family = "neucha", size = 50, hjust = 0.5,
                                  color = "#04d9ff"),
        plot.subtitle = element_text(family = "neucha", size = 30, hjust = 0.5,
                                  color = "#04d9ff"),
        plot.caption = element_text(family = "neucha", size = 29, hjust = 1,
                                  color = "#04d9ff"),
        legend.title = element_text(family = "neucha", size = 28,
                                    color = "#04d9ff"),
        legend.text = element_text(family = "neucha", size = 22,
                                    color = "#04d9ff"),
        legend.background = element_rect(fill = "grey20"),
        legend.key.size = unit(2.5, 'cm'),
        plot.margin = unit(c(1.0, 1.0, 1.0, 1.0), "cm"))

 # Save the Plot
 
 ggsave("Day10.png", p, width = 30, height = 15, dpi = 72)
 
 
