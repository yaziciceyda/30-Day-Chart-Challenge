remotes::install_github("jespermaag/gganatogram")
library(gganatogram)
library(tidyverse)
library(PrettyCols)
library(showtext)
library(cowplot)

# Font in the Plot

font_add_google('Stick No Bills', 'snb')
showtext_auto()

# Male 

male <- gganatogram(data = hgMale_key %>% filter(type == "nervous_system"),
            organism = "human", sex = "male",
            fill = "value", fillOutline = "ivory") +
  scale_fill_pretty_c("Teals", 
                      direction = -1) +
  labs(title = "MALE",
       fill = "VALUE") +
  coord_fixed() +
  theme_void() +
  theme(panel.background = element_rect(fill = "black", color = NA),
        plot.background = element_rect(fill = "black", color = NA),
        legend.text = element_text(family = "snb", color = "ivory",
                                   size = 20),
        legend.title = element_text(family = "snb", color = "ivory",
                                   size = 22),
        plot.title = element_text(family = "snb", color = "ivory",
                                    size = 28, hjust = 0.5),
        plot.margin = margin(0.5, 2, 0.5, 2, "cm"),
        legend.key.size = unit(3, 'cm'),) 

# Female

female <- gganatogram(data = hgFemale_key %>% filter(type == "nervous_system"),
            organism = "human", sex = "female",
            fill = "value", fillOutline = "ivory") +
  scale_fill_pretty_c("Teals", 
                      direction = -1) +
  labs(title = "FEMALE",
       fill = "VALUE") +
  coord_fixed() +
  theme_void() +
  theme(panel.background = element_rect(fill = "black", color = NA),
        plot.background = element_rect(fill = "black", color = NA),
        legend.position = "none",
        plot.title = element_text(family = "snb", color = "ivory",
                                  size = 28, hjust = 0.5),
        plot.margin = margin(0.5, 2, 0.5, 2, "cm")) 

# The Final Plot

plots <- align_plots(female, male, align = 'r', axis = 'r')

title1 <- ggdraw() + 
  draw_label(
    "NERVOUS SYSTEM IN HUMANS",
    fontface = 'bold',
    fontfamily = "snb",
    hjust = 0.5,
    x = 0.5,
    size = 50,
    color = "#008080"
  )  
 
caption <- ggdraw() + 
  draw_label(
    "Data Source: {gganatogram} | #30DayChartChallenge - Day #8 | Prepared by: C. YAZICI", 
    fontface = 'bold',
    fontfamily = "snb",
    hjust = 0.5,
    x = 0.5,
    y = 0.5, 
    size = 24,
    color = "#008080"
  ) 

bottom_row <- plot_grid(
  plots[[1]], plots[[2]],
  labels = "",
  rel_widths = c(1.0, 1.0), 
  nrow = 1
)

final_plot <- plot_grid(title1, bottom_row, caption, 
                        labels = "", ncol = 1,
                        rel_heights = c(0.1, 1.3, 0.1),
                        align = "hv") +
  theme(plot.background = element_rect(fill = "black", colour = NA),
        plot.margin = margin(0.5, 2, 0.5, 2, "cm"))

final_plot

# Save the Plot

ggsave("Day8.png", final_plot, width = 25, height = 12, dpi = 72)







