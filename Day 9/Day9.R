library(tidyverse)
library(png)
library(magick)
library(ggimage)
library(ggchicklet)
library(cowplot)
library(showtext)


# Font in the Plot

font_add_google('Varela Round', 'vr', db_cache = TRUE)
showtext_auto()


pumpkin <- image_read('Pumpkin.jpg')
print(pumpkin)
pumpkin_img <- paste0("Pumpkin.jpg")

# Data Import

tuesdata <- tidytuesdayR::tt_load(2021, week = 43)

pumpkins <- tuesdata$pumpkins

# Data Wrangling

min_data <- pumpkins %>%
  separate(id, into = c("year", "type")) %>%
  mutate(place = as.numeric(place),
         weight_lbs = as.numeric(weight_lbs)) %>%
  group_by(year, type) %>%
  slice_min(order_by = place, n = 3) %>%
  ungroup() %>%
  filter(!type %in% c("P", "S"),
         year == 2021) %>%
  mutate(img = pumpkin_img)

  

  
field_data <- pumpkins %>%
  separate(id, into = c("year", "type")) %>%
  mutate(place = as.numeric(place),
         weight_lbs = as.numeric(weight_lbs)) %>%
  filter(type == "W",
         year == 2021) %>%
  mutate(img = pumpkin_img)

# The first plot

field_ranking <- ggplot(min_data) +
  geom_image(aes(x = 1.5, y = 1.0, image = img), size = 0.5) +
  geom_image(aes(x = 3.7, y = 0.77, image = img), size = 0.3) +
  geom_image(aes(x = 4.8, y = 0.54, image = img), size = 0.1) +
  coord_cartesian(xlim = c(0, 5), ylim = c(-1, 2)) +
  # 1st Rank
  ggchicklet:::geom_rrect(aes(xmin = 0.1, xmax = 2.9, 
                              ymin = -0.4, ymax = 0.4), 
                          color = "#703701",
                          fill = "#fee3ca",
                          radius = unit(0.5, units = "cm")) +
  geom_text(aes(x = 1.5, y = 0.0, label = "Weight: 329lbs\nGrown in: North Carolina\n(US)"),
            size = 6, color = "#FF7518", family = "vr") +
  # 2nd Rank
  ggchicklet:::geom_rrect(aes(xmin = 2.9, xmax = 4.5, 
                              ymin = -0.4, ymax = 0.4), 
                          color = "#703701",
                          fill = "#fee3ca",
                          radius = unit(0.5, units = "cm")) +
  geom_text(aes(x = 3.7, y = 0.0, label = "Weight: 301.5lbs\nGrown in: Kentucky\n(US)"),
            size = 5, color = "#FF7518", family = "vr") +
  # 3rd Rank
  ggchicklet:::geom_rrect(aes(xmin = 4.5, xmax = 5.1, 
                              ymin = -0.4, ymax = 0.4), 
                          color = "#703701",
                          fill = "#fee3ca",
                          radius = unit(0.5, units = "cm")) +
  geom_text(aes(x = 4.8, y = 0.0, label = "Weight: 298lbs\nGrown in: Kentucky\n(US)"),
            size = 4, color = "#FF7518", family = "vr") + 
  labs(title = "GIANT WATERMELON PUMPKIN") +
  theme(panel.background = element_rect(fill = "#fee7d2", color = NA),
        plot.background = element_rect(fill = "#fee7d2", color = NA),
        panel.grid = element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_blank(),
        axis.title = element_blank(),
        plot.title = element_text(family = "vr", size = 20, hjust = 0.5, 
                                  color = "black"))

# The second plot

field_density <- ggplot(field_data) +
  geom_density(aes(weight_lbs), fill = "#FF7518") +
  labs(x = "WEIGHT") +
  theme(panel.background = element_rect(fill = "#fee7d2", color = NA),
        plot.background = element_rect(fill = "#fee7d2", color = NA),
        panel.grid = element_blank(),
        axis.ticks = element_blank(),
        axis.text.y = element_blank(),
        axis.title.y = element_blank(),
        axis.title.x = element_text(family = "vr", size = 20),
        axis.text.x = element_text(family = "vr", size = 17))


# The third plot

tomato_data <- pumpkins %>%
  separate(id, into = c("year", "type")) %>%
  mutate(place = as.numeric(place),
         weight_lbs = as.numeric(weight_lbs)) %>%
  filter(type == "T",
         year == 2021) %>%
  mutate(img = pumpkin_img)
      


tomato_density <- ggplot(tomato_data) +
  geom_density(aes(weight_lbs), fill = "#FF7518") +
  labs(x = "WEIGHT") +
  theme(panel.background = element_rect(fill = "#fee7d2", color = NA),
        plot.background = element_rect(fill = "#fee7d2", color = NA),
        panel.grid = element_blank(),
        axis.ticks = element_blank(),
        axis.text.y = element_blank(),
        axis.title.y = element_blank(),
        axis.title.x = element_text(family = "vr", size = 20),
        axis.text.x = element_text(family = "vr", size = 17))


# The fourth plot

tomato_ranking <- ggplot(tomato_data) +
  geom_image(aes(x = 1.5, y = 1.0, image = img), size = 0.5) +
  geom_image(aes(x = 3.7, y = 0.77, image = img), size = 0.3) +
  geom_image(aes(x = 4.8, y = 0.54, image = img), size = 0.1) +
  coord_cartesian(xlim = c(0, 5), ylim = c(-1, 2)) +
  # 1st Rank
  ggchicklet:::geom_rrect(aes(xmin = 0.1, xmax = 2.9, 
                              ymin = -0.4, ymax = 0.4), 
                          color = "#703701",
                          fill = "#fee3ca",
                          radius = unit(0.5, units = "cm")) +
  geom_text(aes(x = 1.5, y = 0.0, label = "Weight: 9.09lbs\nGrown in: Washington\n(US)"),
            size = 6, color = "#FF7518", family = "vr") +
  # 2nd Rank
  ggchicklet:::geom_rrect(aes(xmin = 2.9, xmax = 4.5, 
                              ymin = -0.4, ymax = 0.4), 
                          color = "#703701",
                          fill = "#fee3ca",
                          radius = unit(0.5, units = "cm")) +
  geom_text(aes(x = 3.7, y = 0.0, label = "Weight: 9.06lbs\nGrown in: Minnesota\n(US)"),
            size = 5, color = "#FF7518", family = "vr") +
  # 3rd Rank
  ggchicklet:::geom_rrect(aes(xmin = 4.5, xmax = 5.1, 
                              ymin = -0.4, ymax = 0.4), 
                          color = "#703701",
                          fill = "#fee3ca",
                          radius = unit(0.5, units = "cm")) +
  geom_text(aes(x = 4.8, y = 0.0, label = "Weight: 8.36lbs\nGrown in: Indiana\n(US)"),
            size = 4, color = "#FF7518", family = "vr") + 
  labs(title = "TOMATO PUMPKIN") +
  theme(panel.background = element_rect(fill = "#fee7d2", color = NA),
        plot.background = element_rect(fill = "#fee7d2", color = NA),
        panel.grid = element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_blank(),
        axis.title = element_blank(),
        plot.title = element_text(family = "vr", size = 20, hjust = 0.5, 
                                  color = "black"))



# The Final Plot

plots <- align_plots(tomato_ranking, tomato_density, 
                     field_ranking, field_density,  align = 'r', axis = 'r')

title1 <- ggdraw() + 
  draw_label(
    "GIANT PUMPKINS",
    fontface = 'bold',
    fontfamily = "vr",
    hjust = 0.5,
    x = 0.5,
    size = 50,
    color = "#FF7518"
  )  

subtitle_text <- str_wrap("\nThe The Great Pumpkin Commonwealth's (GPC) mission 
  cultivates the hobby of 
    growing giant pumpkins. The types of tomato pumpkin
    and giant watermelon pumpkin are the ones with the lowest and highest 
    weights. The weight and location of the pumpkins in the first three 
    ranking  in 2021 are
    given with the distribution of weights.\n", 135)

title2 <- ggdraw() + 
  draw_label(
    subtitle_text ,
    fontface = 'bold',
    fontfamily = "vr",
    hjust = 0.5,
    x = 0.5,
    size = 17,
    color = "#b05602"
  ) 
caption <- ggdraw() + 
  draw_label(
    "Data Source: BigPumpkins.com | #30DayChartChallenge - Day #9 | #TidyTuesday - 2021 - Week 43 | Prepared by: C. YAZICI", 
    fontface = 'bold',
    fontfamily = "vr",
    hjust = 0.5,
    x = 0.5,
    y = 0.5, 
    size = 18,
    color = "black"
  ) 

top_row <- plot_grid(
  plots[[1]], plots[[2]],
  labels = "",
  rel_widths = c(1.5, 0.5), 
  nrow = 1
)

bottom_row <- plot_grid(
  plots[[3]], plots[[4]],
  labels = "",
  rel_widths = c(1.5, 0.5), 
  nrow = 1
)

final_plot <- plot_grid(title1, title2, top_row, bottom_row, caption, 
                        labels = "", ncol = 1,
                        rel_heights = c(0.22, 0.18, 0.9, 0.9, 0.08),
                        align = "hv") +
  theme(plot.background = element_rect(fill = "#fee7d2", colour = NA),
        plot.margin = margin(0.5, 0.5, 0.5, 1.0, "cm"))


final_plot

# Save the Plot

ggsave("Day9.png", final_plot, width = 25, height = 12, dpi = 72)








