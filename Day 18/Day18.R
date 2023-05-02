library(readxl)
library(maps)
library(biscale)
library(showtext)
library(cowplot)


#import fonts for plot

sysfonts::font_add_google("Salsa", "salsa")
showtext::showtext_auto()

# Data Import

tax_data <- read_xlsx("Tax.xlsx") %>%
  mutate(Tax = ifelse(Tax == ":", NA, Tax))

gas_data <- read_xlsx("Greenhouse_gas.xlsx") %>%
  mutate(Gas = ifelse(Gas == ":", NA, Gas))

# Data Wrangling

all_data <- tax_data %>%
  right_join(gas_data, by = "Country") %>%
  mutate(Tax = as.numeric(Tax),
         Gas = as.numeric(Gas),
         Country = ifelse(Country == 
        "Germany (until 1990 former territory of the FRG)", "Germany", Country),
        Country = ifelse(Country == "Türkiye", "Turkey", Country)) %>%
  filter(!str_detect(Country, "European"))


# Map data & preparation

europe_data <- map_data("world") %>%
  filter(region %in% all_data$Country)

map_data <- europe_data %>%
  inner_join(all_data, by = c("region" = "Country")) %>%
  select(-subregion)

eu_classes <- bi_class(map_data, x = Tax, y = Gas, 
                       style="quantile", dim = 3) %>%
  mutate(bi_class = ifelse(str_detect(bi_class, "NA"), NA, bi_class))

# The Plot

map_plot <- ggplot() +
  geom_polygon(data = eu_classes,
               mapping = aes(x = long, y = lat, group = group, fill = bi_class),
               color= "ivory", size = 0.5, show.legend = FALSE) +
  bi_scale_fill(pal = "DkCyan2", dim = 3, na.value = "grey50") +
  scale_x_continuous(limits = c(-10, 45)) +
  scale_y_continuous(limits = c(35, 67)) +
  coord_map() +
  theme(plot.background = element_rect(fill = "ivory", color = NA),
        panel.background = element_rect(fill = "ivory", color = NA),
        panel.grid = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank(),
        axis.text = element_blank(),
        plot.margin = margin(0.5, 0.5, 0.5, 0.5, "cm"))


p_legend <- bi_legend(pal = "DkCyan2",
                      dim = 3,
                      xlab = "Tax (M Euro)",
                      ylab = "Greenhouse Gas",
                      size = 12) +
  theme(plot.background = element_rect(fill = "ivory", colour = "ivory"),
        panel.background = element_rect(fill = "ivory", colour = "ivory"),
        axis.title = element_text(family = "salsa", hjust = 0, size = 15, 
                                  colour = "#0A550C"))
p_legend


# The Final Plot

plots <- align_plots(map_plot,  p_legend,  align = 'r', axis = 'r')

title1 <- ggdraw() + 
  draw_label(
    "ENVIRONMENTAL TAXES AND GREENHOUSE GASES",
    fontface = 'bold',
    fontfamily = "salsa",
    hjust = 0.5,
    x = 0.5,
    size = 30,
    color = "#0A550C"
  )  

subtitle_text <- str_wrap("\nEnvironmental taxes by economic activity (in million
                          Euro) and Net Greenhouse Gas emissions in Europe
                          are given for each country. They are positively
                          correlated in most countries. \n", 90)

title2 <- ggdraw() + 
  draw_label(
    subtitle_text ,
    fontface = 'bold',
    fontfamily = "salsa",
    hjust = 0.5,
    x = 0.5,
    size = 18,
    color = "#0A550C"
  ) 
caption <- ggdraw() + 
  draw_label(
    "Data Source: Eurostat | #30DayChartChallenge - Day #18 | Prepared by: C. YAZICI", 
    fontface = 'bold',
    fontfamily = "salsa",
    hjust = 0.5,
    x = 0.5,
    y = 0.5, 
    size = 18,
    color = "#0A550C"
  ) 

top_row <- plot_grid(
  plots[[1]], NULL, plots[[2]], 
  labels = "",
  rel_widths = c(1.9, 0.0, 0.5), 
  nrow = 1
)


final_plot <- plot_grid(title1, title2, top_row, caption, 
                        labels = "", ncol = 1,
                        rel_heights = c(0.22, 0.18, 0.9,  0.08),
                        align = "hv") +
  theme(plot.background = element_rect(fill = "ivory", colour = NA),
        plot.margin = margin(0.5, 4.5, 0.5, 4.0, "cm"))


final_plot

# Save the Plot

ggsave("Day18.png", final_plot, width = 15, height = 12, dpi = 72)



