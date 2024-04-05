library(readxl)
library(tidyverse)
library(dplyr)
library(ggforce)
library(geomtextpath)
library(cowplot)
library(showtext)


# Font in the Plot

font_add_google('Shippori Antique', 'sa')
showtext_auto()

# Data Ishowtext# Data Import

data_olive <- read_excel("Data.xlsx") 

# Data Wrangling
data_oil <- data_olive %>%
  mutate(Continent = as.factor(Continent)) %>%
  dplyr::group_by(Continent) %>%
  # Production proportion for Countries
  mutate(prod_prop = round(Production / sum(Production) * 100)) %>%
  arrange(desc(prod_prop), .by_group = TRUE) %>%
  ungroup() %>%
  mutate(color_code = rep(c("#5f720f", "#65880f", "#8eb027", "#c1d64d", "#d1ef71"),
                          2))


# Data for European Countries

oil_europe <- data_oil %>%
  filter(Continent == "Europe") %>%
  mutate(x_circle = row_number())


data_circle <- tibble(x_spain = 1 + 53 * cos(seq(0, 2 * pi, 
                                              length.out = 100)),
                       y_spain = 1 + 53 * sin(seq(0, 2 * pi, 
                                              length.out = 100)),
                       x_italy = 2 + 16 * cos(seq(0, 2 * pi, 
                                                  length.out = 100)),
                       y_italy = 1 + 16 * sin(seq(0, 2 * pi, 
                                                  length.out = 100)),
                       x_greece = 3 + 14 * cos(seq(0, 2 * pi, 
                                                 length.out = 100)),
                       y_greece = 1 + 14 * sin(seq(0, 2 * pi, 
                                                 length.out = 100)),
                       x_turkey = 4 + 10 * cos(seq(0, 2 * pi, 
                                                  length.out = 100)),
                       y_turkey = 1 + 10 * sin(seq(0, 2 * pi, 
                                                  length.out = 100)),
                       x_portugal = 5 + 7 * cos(seq(0, 2 * pi, 
                                                  length.out = 100)),
                       y_portugal = 1 + 7 * sin(seq(0, 2 * pi, 
                                                  length.out = 100)),
                      )




p_europe <- ggplot(oil_europe) +
  geom_circle(aes(x0 = x_circle, y0 = 1, r = prod_prop,
                  color = color_code, fill = color_code)) +
  geom_labelpath(data = data_circle, aes(x = x_portugal, y = y_portugal,
                                         label = "PORTUGAL"), 
                 hjust = 0.7, vjust = 0,
                 fill = "#e7d6c5", size = 6, family = "sa") + 
  geom_labelpath(data = data_circle, aes(x = x_turkey, y = y_turkey,
                                         label = "TURKEY"), 
                 hjust = 0.12, vjust = 0,
                 fill = "#e7d6c5", size = 6, family = "sa") + 
  geom_labelpath(data = data_circle, aes(x = x_greece, y = y_greece,
                                         label = "GREECE"), 
                 hjust = 0.25, vjust = 0,
                 fill = "#e7d6c5", size = 6, family = "sa") + 
  geom_labelpath(data = data_circle, aes(x = x_italy, y = y_italy,
                                         label = "ITALY"), 
                 hjust = 0.35, vjust = 0,
                 fill = "#e7d6c5", size = 6, family = "sa") + 
  geom_labelpath(data = data_circle, aes(x = x_spain, y = y_spain,
                                         label = "SPAIN"), 
                 hjust = 0.35, vjust = 0,
                 fill = "#e7d6c5", size = 6, family = "sa") +
  scale_fill_identity() +
  scale_color_identity() +
  coord_cartesian() +
  labs(title = "EUROPE") +
  theme(panel.background = element_rect(fill = "#DFD855", color = NA),
        plot.background = element_rect(fill = "#DFD855", color = NA),
        panel.grid = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank(),
        axis.text = element_blank(),
        plot.title = element_text(hjust = 0.5, size = 20, 
                                  family = "sa", color = "#723608"))




# Data for African Countries

oil_africa <- data_oil %>%
  filter(Continent == "Africa") %>%
  mutate(x_circle = row_number())



data_circle2 <- tibble(x_tunisia = 1 + 41 * cos(seq(0, 2 * pi, 
                                                 length.out = 100)),
                      y_tunisia = 1 + 41 * sin(seq(0, 2 * pi, 
                                                 length.out = 100)),
                      x_morocco = 2 + 35 * cos(seq(0, 2 * pi, 
                                                 length.out = 100)),
                      y_morocco = 1 + 35 * sin(seq(0, 2 * pi, 
                                                 length.out = 100)),
                      x_algeria = 3 + 16 * cos(seq(0, 2 * pi, 
                                                  length.out = 100)),
                      y_algeria = 1 + 16 * sin(seq(0, 2 * pi, 
                                                  length.out = 100)),
                      x_egypt = 4 + 7 * cos(seq(0, 2 * pi, 
                                                  length.out = 100)),
                      y_egypt = 1 + 7 * sin(seq(0, 2 * pi, 
                                                  length.out = 100)),
                      x_libya = 5 + 1 * cos(seq(0, 2 * pi, 
                                                   length.out = 100)),
                      y_libya = 1 + 1 * sin(seq(0, 2 * pi, 
                                                   length.out = 100)),
)



p_africa <- ggplot(oil_africa) +
  geom_circle(aes(x0 = x_circle, y0 = 1, r = prod_prop,
                  color = color_code, fill = color_code)) +
  geom_labelpath(data = data_circle2, aes(x = x_tunisia, y = y_tunisia,
                                         label = "TUNISIA"), 
                 hjust = 0.35, vjust = 0,
                 fill = "#e7d6c5", size = 6, family = "sa") + 
  geom_labelpath(data = data_circle2, aes(x = x_morocco, y = y_morocco,
                                         label = "MOROCCO"), 
                 hjust = 0.25, vjust = 0,
                 fill = "#e7d6c5", size = 6, family = "sa") + 
  geom_labelpath(data = data_circle2, aes(x = x_algeria, y = y_algeria,
                                         label = "ALGERIA"), 
                 hjust = 0.25, vjust = 0,
                 fill = "#e7d6c5", size = 6, family = "sa") + 
  geom_labelpath(data = data_circle2, aes(x = x_egypt, y = y_egypt,
                                         label = "EGYPT"), 
                 hjust = 0.12, vjust = 0,
                 fill = "#e7d6c5", size = 6, family = "sa") + 
  geom_labelpath(data = data_circle2, aes(x = x_libya, y = y_libya,
                                         label = "LIBYA"), 
                 hjust = 0.8, vjust = 0,
                 fill = "#e7d6c5", size = 3, family = "sa") + 
  scale_fill_identity() +
  scale_color_identity() +
  coord_cartesian() +
  labs(title = "AFRICA") +
  theme(panel.background = element_rect(fill = "#DFD855", color = NA),
        plot.background = element_rect(fill = "#DFD855", color = NA),
        panel.grid = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank(),
        axis.text = element_blank(),
        plot.title = element_text(hjust = 0.5, size = 20,
                                  family = "sa", color = "#723608"))


# The Combination of Plots

plots <- align_plots(p_europe, p_africa,  align = 'r', axis = 'r')

title1 <- ggdraw() + 
  draw_label(
    "OLIVE OIL PRODUCTION",
    fontface = 'bold',
    fontfamily = "sa",
    hjust = 0.5,
    x = 0.5,
    size = 40,
    color = "#723608"
  )  

subt <- str_wrap("The largest five olive oil producers in Europe and Africa in 2019 are presented. 
    Spain produced 53% of the oil in Europe; while the second country was Italy with 16%.
    On the other hand, Tunisia was the largest producer in Africa with 41% and then came 
    Morocco with 35%.\n", 100)

title2 <- ggdraw() + 
  draw_label(
    subt,
    fontface = 'bold',
    fontfamily = "sa",
    hjust = 0,
    x = 0.05,
    y = 0.5, 
    size = 24,
    color = "#723608"
  ) 
caption <- ggdraw() + 
  draw_label(
    "Data Source: OWID | #30DayChartChallenge - Day #3 | Prepared by: C. YAZICI", 
    fontface = 'bold',
    fontfamily = "sa",
    hjust = 1,
    x = 0.9,
    y = 0.5, 
    size = 20,
    color = "#723608"
  ) 

bottom_row <- plot_grid(
  plots[[1]], plots[[2]],
  labels = "",
  rel_widths = c(0.5, 0.5), 
  nrow = 1
)

final_plot <- plot_grid(title1, title2, bottom_row, caption, labels = "", ncol = 1,
                        rel_heights = c(0.1, 0.1, 0.9, 0.1)) +
  theme(plot.background = element_rect(fill = "#DFD855", colour = "#DFD855"),
        plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm"))



# Save the Plot

ggsave("Day3.png", final_plot, width = 25, height = 19, dpi = 72)


