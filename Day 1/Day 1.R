library(magick)
library(readxl)
library(tidyverse)
library(usefunc)
library(ggtext)
library(ggimage)
library(png)
library(showtext)
library(ggpubr)

# Font in the Plot

font_add_google('Shippori Antique', 'sa')
showtext_auto()

# Olive Image

olive <- image_read('olive.jpg')
print(olive)

# Changing colour for the image to represent different countries

img_italy <- image_modulate(olive, brightness = 100, saturation = 100, hue = 90)
image_write(img_italy, path = "italy.png", format = "png")

img_portugal <- image_modulate(olive, brightness = 100, saturation = 100, hue = 80)
image_write(img_portugal, path = "portugal.png", format = "png")

img_spain <- image_modulate(olive, brightness = 100, saturation = 100, hue = 120)
image_write(img_spain, path = "spain.png", format = "png")

img_turkey <- image_modulate(olive, brightness = 100, saturation = 100, hue = 110)
image_write(img_turkey, path = "turkey.png", format = "png")

img_background <- paste0(here::here(), "/background_img.png")
img_italy <- paste0(here::here(), "/italy.png")
img_portugal <- paste0(here::here(), "/portugal.png")
img_spain <- paste0(here::here(), "/spain.png")
img_turkey <- paste0(here::here(), "/turkey.png")

# Data Import & Wrangling

data_olive <- read_excel("Data.xlsx") %>%
   # Production proportion for European Countries
  mutate(prod_prop = round(Production / sum(Production) * 100),
         prod_prop = ifelse(Country == "Spain", 65, prod_prop)) %>%
  arrange(prod_prop)


# Data Preparation for plot

olive_plot <- rep_df(expand.grid(x = rep(1:10), y = rep(1:10))) %>%
  mutate(type = rep(data_olive$Country, times = data_olive$prod_prop),
         img = case_when(
           type == "Italy" ~ img_italy,
           type == "Portugal" ~ img_portugal,
           type == "Spain" ~ img_spain,
           type == "Turkey" ~ img_turkey,
           ))

# Legend Data

legend_data <- tibble(x = c(rep(12, times = 4)),
                         y = c(6, 5, 4, 3),
                         images = c(img_spain, img_italy, img_turkey,
                                    img_portugal),
                         country = c("Spain", "Italy", "Turkey", "Portugal"))
  
# Subtitle of the Plot

subtitle_text <- str_wrap("\nSpain, Italy, Turkey and Portugal are the largest 
                          olive producers in Europe in 2020. ", 100)
# The Plot

p <- ggplot() +
  geom_richtext(data = olive_plot,
                mapping = aes(x = x,
                             y = y,
                             label = type)) +
  geom_image(data = olive_plot,
             mapping = aes(x = x,
                           y = y,
                           image = img), size = 0.1) +
  geom_image(data = legend_data,
             mapping = aes(x = x,
                           y = y,
                           image = images), size = 0.1) +
  geom_text(data = legend_data,
            mapping = aes(x = 13.05, y = y, label = country),
            family = "sa", size = 7, color = "#808000") +
  labs(title = "OLIVE PRODUCTION (in tonnes)",
       subtitle = subtitle_text,
       caption = "Data: OWID |#30DayChartChallenge - Day #1 |Prepared by: @Cyd_yzc") +
  theme(panel.background = element_rect("ivory", color = NA),
        plot.background = element_rect("ivory", color = NA),
        panel.grid = element_blank(),
        axis.text = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        plot.title = element_text(size = 30, family = "sa", hjust = 0.5,
                                  color = "#808000"),
        plot.subtitle = element_text(size = 20, family = "sa", hjust = 0.5,
                                  color = "#808000"),
        plot.caption = element_text(size = 15, family = "sa", hjust = 1,
                                     color = "#808000"),
        plot.margin = unit(c(1.5, 1.5, 1.5, 1.5), "cm"))

# Save the Plot

ggsave("Day1_2023.png", p, width = 25, height = 12, dpi = 72)


                
