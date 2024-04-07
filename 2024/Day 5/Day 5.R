library(tidyverse)
library(readr)
library(showtext)
library(ggpubr)
library(patchwork)

# Font in the Plot

font_add_google('Indie Flower', 'if')
showtext_auto()

# Data Import

data5 <- read_csv("chocolate.csv")


# Data Wrangling

chocolate_data <- data5 %>%
  mutate(rating_class = factor(case_when(
    rating >= 4 ~ "Outstanding",
    between(rating, 3.5, 3.9) ~ "Highly Recommended",
    between(rating, 3.0, 3.49) ~ "Recommended",
    between(rating, 2.0, 2.9) ~ "Disappointing",
    between(rating, 1.0, 1.9) ~ "Unpleasant",
  ),
  levels = c("Outstanding", "Highly Recommended", "Recommended", 
             "Disappointing", "Unpleasant"))) %>%
  select(sugar, rating_class) %>%
  group_by(sugar) %>%
  mutate(total_type = n()) %>%
  ungroup() %>%
  group_by(sugar, rating_class, keep = total_type) %>%
  summarise(n = n(),
            percent = round((n / keep) * 100)) %>%
  ungroup() %>%
  select(sugar, rating_class, percent) %>%
  group_by(sugar, rating_class, keep = percent) %>%
  summarise(n = n()) %>%
  ungroup() %>%
  mutate(sugar = ifelse(sugar == "have_not_sugar",
                        "No Sugar", "With Sugar")) %>%
  add_row(sugar = "No Sugar", 
            rating_class = "Outstanding", 
            keep = 0, 
            n = 0) %>%
  mutate(rating_class = ordered(rating_class, 
                                levels = c("Unpleasant",
                                           "Highly Recommended",
                                           "Recommended",
                                           "Disappointing",
                                           "Outstanding"))) %>%
                                  arrange(sugar, rating_class)



axis_margin <- 5.5

p1 <- ggplot(chocolate_data %>% filter(sugar == "With Sugar"),
       aes(keep, rating_class, fill = rating_class)) +
  geom_col(width = 0.6) +
  scale_x_reverse(limits = c(65, 0),
                  breaks = c(0, 10, 20, 30, 40)) +
  scale_y_discrete(position = "right") +
  scale_fill_manual(values = c("#391500", "#823200",
                               "#bb4800", "#d2691e", "#fee3d2")) +
  labs(x = "",
       title = "WITH SUGAR") +
  theme(
    axis.text.y = element_blank(),
    axis.text.x = element_text(family = "if", size = 24),
    axis.title.y = element_blank(),
    axis.ticks.y = element_blank(),
    legend.position = "none",
    plot.margin = margin(axis_margin, 5, axis_margin, axis_margin + 3),
    panel.background = element_rect(fil = "#fafafa", color = NA),
    plot.background = element_rect(fil = "#fafafa", color = NA),
    panel.grid = element_blank(),
    plot.title = element_text(family = "if", color = "#823200",
                              size = 32, hjust = 0.5)
  )


p2 <- ggplot(chocolate_data %>% filter(sugar == "No Sugar"),
             aes(keep, rating_class, fill = rating_class)) +
  geom_col(width = 0.6) +
  scale_y_discrete() +
  scale_x_continuous(limits = c(0, 46)) +
  scale_fill_manual(values = c("#391500", "#823200",
                               "#bb4800", "#d2691e", "#fee3d2")) +
  labs(x = "",
       title = "NO SUGAR") +
  theme(
    axis.title.y = element_blank(),
    plot.margin = margin(axis_margin, axis_margin - 3, axis_margin, 0),
    axis.text.y.left = element_text(margin = margin(0, axis_margin, 
                                                    0, axis_margin),
                            size = 24, hjust = 0.5, family = "if",
                            face = "bold"),
    axis.text.x = element_text(family = "if", size = 24),
    axis.ticks.y = element_blank(),
    panel.background = element_rect(fil = "#fafafa", color = NA),
    plot.background = element_rect(fil = "#fafafa", color = NA),
    panel.grid = element_blank(),
    legend.position = "none",
    plot.title = element_text(family = "if", color = "#823200",
                              size = 32, hjust = 0.5))


subtitle_text <- str_wrap("The chocolate is one of the most popular candies in 
the world. Here, the chocolate bars with and without sugar are rated
and the percentage of each rating is shown.", 90)



final_plot <- ggarrange(p1, p2) +
  plot_annotation(title = "CHOCOLATE BARS",
                  subtitle = subtitle_text,
caption = "Data Source: Kaggle | #30DayChartChallenge - Day #5 | Prepared by: C. YAZICI") &
  theme(plot.caption = element_text(family = "if", size = 25,
                                    hjust = 1),
        plot.title = element_text(family = "if", size = 52,
                                  hjust = 0),
        plot.subtitle = element_text(family = "if", size = 28,
                                     hjust = 0),
        panel.background = element_rect(fil = "#fafafa", color = NA),
        plot.background = element_rect(fil = "#fafafa", color = NA),
        panel.grid = element_blank(),
        plot.margin = margin(1, 3, 1, 1, "cm"))

# Save the Plot

ggsave("Day5.png", final_plot, width = 20, height = 16, dpi = 72)




