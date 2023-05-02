library(showtext)
library(tidyverse)
library(readxl)
library(ggtext)
library(ggrepel)
library(usefunc)


# library to use fontawesome icons
font_add('fa-reg', 'Font Awesome 6 Free-Regular-400.otf')
font_add('fa-brands', 'Font Awesome 6 Brands-Regular-400.otf')
font_add('fa-solid', 'Font Awesome 6 Free-Solid-900.otf')
showtext_auto()

# Font in the Plot

font_add_google('Ubuntu', 'ubuntu')
showtext_auto()

# Data Import & Wrangling 

set.seed(1234)
label_code <- sort(sample(192, 96))
label = rep("<span style='font-family:fa-solid'>&#xf182;</span>",192)
label[label_code] = "<span style='font-family:fa-solid'>&#xf183;</span>"

data_age <- read_excel("Age_data.xlsx") %>%
  select(-Code) %>%
  filter(Year < 2022 & Year >=1990) %>%
  mutate(label = label,
         Entity = ifelse(Entity == "United Kingdom", "United\nKingdom", Entity))

subtitle_text <- str_wrap("\nThe median age of several European countries show 
that we are getting older. The Mediterranean countries have similar age in this 
time period (1990 - 2020). Among them, Italy is the oldest country; while Turkey 
is the youngest. Only UK seems to have constant median age after 2010.", 100)

# Plot

p <- ggplot(data_age) +
  geom_richtext(data = data_age,
                mapping = aes(x = Year,
                              y = Age,
                              label = label,
                              color = Age,
                              group = Entity),
                family = 'fontawesome-webfont', size = 12,
                fill = NA, label.colour = NA) +
  scale_colour_gradient2(
    low = "#F5E2BA",
    mid = "#E7BA59",
    high = "#B17C09",
    midpoint = 35,
    na.value = "grey50") +
  geom_text_repel(data_age %>%filter(Year == 2020),
            mapping = aes(x = 2023, y = Age, label = Entity),
            color = "ivory", size = 8, hjust = "right",
            nudge_x = 0.8, 
            direction = "y") +
  labs(y = "Median Age",
       x = "Year",
       subtitle = subtitle_text,
       title = "WE ARE GETTING OLDER\n",
       caption = "Data: OWID |#30DayChartChallenge - Day #6 |Prepared by: C. YAZICI") +
  theme(panel.background = element_rect(fill = "grey20", color = NA),
        plot.background = element_rect(fill = "grey20", color = NA),
        panel.grid = element_blank(),
        axis.ticks = element_blank(),
        plot.title = element_text(family = "ubuntu",
                                size = 40, hjust = 0.5, color = "ivory"),
        plot.subtitle = element_text(family = "ubuntu",
                                  size = 25, hjust = 0, color = "ivory"),
       axis.text = element_text(family = "ubuntu",
                                  size = 20, color = "ivory"),
       axis.title = element_text(family = "ubuntu",
                                size = 25, color = "ivory"),
       plot.caption = element_text(family = "ubuntu",
                                 size = 25, hjust = 1, color = "ivory"),
       legend.text = element_text(family = "ubuntu", size = 18,
                                  color = "ivory"),
       legend.title = element_text(family = "ubuntu", size = 18,
                                  color = "ivory"),
       legend.background = element_rect(fill = "grey20"),
       legend.key.size = unit(1, 'cm'),
       plot.margin = unit(c(1.5, 0.8, 1.5, 1.5), "cm"))

# Save the Plot

ggsave("Day6.png", p, width = 25, height = 12, dpi = 72) 

