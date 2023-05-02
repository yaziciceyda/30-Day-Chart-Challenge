library(readxl)
library(tidyverse)
library(dplyr)
install.packages("waffle", repos = "https://cinc.rud.is")
library(waffle)
library(showtext)
library(cowplot)

# Font in the Plot

font_add_google('Shippori Antique', 'sa')
showtext_auto()

# Data Import

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
   filter(Continent == "Europe") 
 
 # Data for African Countries
 
 oil_africa <- data_oil %>%
   filter(Continent == "Africa")
 
 # Plot for Europe
 
europe_plot <- ggplot() +
  geom_waffle(oil_europe, mapping = aes(fill = color_code, values = prod_prop),
                                        make_proportional = TRUE,
              radius = unit(4, "pt"),
              n_rows = 10, size = 0.33, colour = "white") +
  scale_fill_identity(guide = "legend",
                      labels = oil_europe$Country) +
  labs(fill = "Country",
       title = "EUROPE") +
  coord_equal() +
  theme(legend.position = "right",
        legend.title = element_text(family = "sa", size = 19),
        legend.text = element_text(family = "sa", size = 16),
        axis.ticks = element_blank(),
        axis.title = element_blank(),
        axis.text = element_blank(),
        panel.grid = element_blank(),
        panel.background = element_rect(fill = "#DFD855", color = NA),
        plot.background = element_rect(fill = "#DFD855", color = NA),
        legend.background = element_rect(fill = "#DFD855"),
        plot.title = element_text(color = "#723608", family = "sa",
                                  hjust = 0.5, size = 25))

# Plot for Africa

africa_plot <- ggplot() +
  geom_waffle(oil_africa, mapping = aes(fill = color_code, values = prod_prop),
              make_proportional = TRUE,
              radius = unit(4, "pt"),
              n_rows = 10, size = 0.33, colour = "white") +
  scale_fill_identity(guide = "legend",
                      labels = oil_africa$Country) +
  labs(fill = "Country",
       title = "AFRICA") +
  coord_equal() +
  theme(legend.position = "right",
        legend.title = element_text(family = "sa", size = 19),
        legend.text = element_text(family = "sa", size = 16),
        axis.ticks = element_blank(),
        axis.title = element_blank(),
        axis.text = element_blank(),
        panel.grid = element_blank(),
        panel.background = element_rect(fill = "#DFD855", color = NA),
        plot.background = element_rect(fill = "#DFD855", color = NA),
        legend.background = element_rect(fill = "#DFD855"),
        plot.title = element_text(color = "#723608", family = "sa",
                                  hjust = 0.5, size = 25)) 

# The Combination of Plots

plots <- align_plots(europe_plot, africa_plot,  align = 'r', axis = 'r')

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
    hjust = 0.5,
    x = 0.5,
    y = 0.5, 
    size = 18,
    color = "#723608"
  ) 
caption <- ggdraw() + 
  draw_label(
    "Data Source: OWID | #30DayChartChallenge - Day #2 | Prepared by: C. YAZICI", 
    fontface = 'bold',
    fontfamily = "sa",
    hjust = 0.5,
    x = 0.5,
    y = 0.5, 
    size = 18,
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

final_plot

# Save the Plot

ggsave("Day2.png", final_plot, width = 30, height = 15, dpi = 72)


