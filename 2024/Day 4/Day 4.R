library(tidyverse)
library(readr)
install.packages("waffle", repos = "https://cinc.rud.is")
library(waffle)
library(showtext)

# Font in the Plot

font_add_google('Indie Flower', 'if')
showtext_auto()

# Data Import

data4 <- read_csv("chocolate.csv")

# Data Wrangling

chocolate_data <- data4 %>%
  mutate(rating_class = factor(case_when(
    rating >= 4 ~ "Outstanding",
    between(rating, 3.5, 3.9) ~ "Highly Recommended",
    between(rating, 3.0, 3.49) ~ "Recommended",
    between(rating, 2.0, 2.9) ~ "Disappointing",
    between(rating, 1.0, 1.9) ~ "Unpleasant",
  ),
  levels = c("Outstanding", "Highly Recommended", "Recommended", 
             "Disappointing", "Unpleasant")),
  rating_class = ordered(rating_class, 
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
                        "No Sugar", "With Sugar"))
  
# Subtitle of the Text

subtitle_text <- str_wrap("The chocolate is one of the most popular candies in 
the world. Here, the chocolate bars with and without sugar are rated
and the percentage of each rating are shown.", 90)

# The Plot

p <- ggplot(chocolate_data) +
  geom_waffle(aes(fill = rating_class, values = keep),
              na.rm = TRUE,
              make_proportional = TRUE,
              radius = unit(2, "pt"),
              n_rows = 10, size = 0.33, colour = "white") +
  scale_fill_manual(values = c("#391500", "#823200",
                               "#bb4800", "#d2691e", "#fee3d2")) +
  facet_wrap(~sugar, ncol = 1) +
  labs(fill = "Rating",
       title = "CHOCOLATE BARS",
       subtitle = subtitle_text,
       caption = "Data Source: Kaggle | #30DayChartChallenge - Day #4 | Prepared by: C. YAZICI") +
  theme(panel.background = element_rect(fil = "#fafafa", color = NA),
        plot.background = element_rect(fil = "#fafafa", color = NA),
        panel.grid = element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_blank(),
        axis.title = element_blank(),
        legend.title = element_blank(),
        legend.text = element_text(family = "if", size = 20),
        legend.background = element_rect(fill = "#fafafa"),
        legend.position = "top",
        legend.key.width = unit(2.5,"cm"),
        strip.background = element_rect(fill = "#fafafa"),
        strip.text = element_text(family = "if", size = 23, vjust = 1),
        plot.caption = element_text(family = "if", size = 25,
                                    hjust = 1),
        plot.title = element_text(family = "if", size = 42,
                                    hjust = 0),
        plot.subtitle = element_text(family = "if", size = 28,
                                  hjust = 0),
        plot.margin = margin(1, 1, 1, 1, "cm")) 


# Save the Plot

ggsave("Day4.png", p, width = 20, height = 16, dpi = 72)




             



