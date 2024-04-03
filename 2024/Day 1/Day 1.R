devtools::install_github("doehm/ggbrick")
library(ggbrick)
library(tidyverse)
library(showtext)


# Font in the Plot

font_add_google(name = "Lacquer",
                family = "lacquer", db_cache = FALSE)
showtext_auto()

total <- 9 * 60 + 8
part1 <- 3 * 60 + 21
part2 <- 3 * 60 + 59
part3 <- 1 * 60 + 48

part1_percent <- part1 / total * 100
part2_percent <- part2 / total * 100
part3_percent <- part3 / total * 100

data1 <- tibble(part = c("Part 1", "Part 2", "Part 3"),
                percent = c(round(part1_percent),
                            round(part2_percent),
                            round(part3_percent)))

subtitle_text <- str_wrap("The famous song of Pink Floyd which was relased in 1979 was
divided into three parts. Part 1 is a song about the sadness of a child
and Part 2 is a rock opera protest song. 
The loudest and shortest part is the last part which is 
about the rockstar's anger. The bar chart here shows the percentage of
the length of each part.", 85)

p <- ggplot(data1) +
  geom_brick(aes(part, percent, fill = part)) +
  coord_brick() +
  scale_fill_manual(values = c("#f28859",
                               "#f46c3e",
                               "#812a0e")) +
  labs(title = "Another Brick in the Wall",
       x = "",
       y = "Percentage of Length",
       fill = "Parts of the Song",
       subtitle = subtitle_text,
       caption = "\nData Source: Wikipedia  | #30DayChartChallenge\n2024 - Day #1 | Prepared by: C. YAZICI") +
  theme(panel.background = element_rect(fill = "ivory", color = NA),
        plot.background = element_rect(fill = "ivory", color = NA),
        panel.grid = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_text(family = "lacquer", size = 26,
                                  hjust = 0.5, vjust = 0.5),
        axis.text = element_text(family = "lacquer", size = 24,
                                  hjust = 0.5, vjust = 0.5),
        plot.caption = element_text(family = "lacquer", size = 25,
                                   hjust = 1, vjust = 0.5),
        legend.position = "none",
        plot.title = element_text(family = "lacquer", size = 54,
                                  hjust = 0, vjust = 0.5),
        plot.subtitle = element_text(family = "lacquer", size = 26,
                                  hjust = 0, vjust = 0.5),
        plot.title.position = "plot",
        plot.margin = unit(c(0.3, 0.8, 0.3, 0.8), "cm"),
        aspect.ratio = 0.5)

# Save the plot

ggsave("Day1.png", p, width = 22, height = 20, dpi = 72) 


