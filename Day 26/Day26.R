library(readxl)
library(showtext)

# Font in the Plot

font_add_google('Ubuntu', 'ubuntu')
showtext_auto()

# Data Import

data_age_future <- read_csv("Age_predict.csv") %>%
  clean_names()

# Data Wrangling

data_future <- data_age_future %>%
  filter(entity %in% c("Turkey", "Spain", "Italy", "Portugal", "Greece",
                       "United Kingdom")) %>%
  rename(predictions = median_age_sex_all_age_all_variant_medium,
         actual =  median_age_sex_all_age_all_variant_estimates) %>%
  filter(year >= 2023 & year <= 2100) %>%
  group_by(entity) %>%
  mutate(avg_pred = mean(predictions)) %>%
  ungroup() %>%
  arrange(desc(avg_pred)) %>%
  mutate(entity = reorder(entity, -avg_pred))

data_future_summary <- data_age_future %>%
  filter(entity %in% c("Turkey", "Spain", "Italy", "Portugal", "Greece",
                       "United Kingdom")) %>%
  rename(predictions = median_age_sex_all_age_all_variant_medium,
         actual =  median_age_sex_all_age_all_variant_estimates) %>%
  select(-c(code, predictions)) %>%
  filter(year < 2023) %>%
  select(-year) %>%
  mutate(actual = as.numeric(actual)) %>%
  group_by(entity) %>%
  summarise(avg_age = mean(actual, na.rm = TRUE),
            median_age = median(actual, na.rm = TRUE)) %>%
  ungroup() 
  

subtitle_text <- str_wrap("The median age of several European countries until 2022 show 
that we are getting older. The predictions of median ages between 2023 and 2100
are calculated and their distributions are obtained. The Mediterranean countries
are forecasted to have similar age, while Turkey and UK are predicted to be younger
countries. Among all, Turkey has more spread than the other countries.", 70)

# The Plot

p <-  ggplot(data_future, aes(y = entity, x = predictions,
                          fill = entity)) +
  stat_gradientinterval() +
    scale_fill_manual(values = c("#FF008C", "#74ee15", 
                                 "#ffe700", "#f000ff",
                                 "#00FFE1", "blue")) +
    coord_cartesian(clip = "off") +
    labs(title = "WE WILL GET OLDER",
         subtitle = subtitle_text,
      x = "Predictions",
         caption = "Data: OWID |#30DayChartChallenge - Day #26 |Prepared by: C. YAZICI") +
    theme(panel.background = element_rect(fill = "ivory", color = NA),
          plot.background = element_rect(fill = "ivory", color = NA),
          panel.grid = element_blank(),
          legend.position = "none",
          axis.ticks = element_blank(),
          axis.text.y = element_text(family = "ubuntu", size = 25, hjust = 0),
          axis.text.x = element_text(family = "ubuntu", size = 22),
          axis.title.y = element_blank(),
          axis.title.x = element_text(family = "ubuntu", size = 24),
          plot.caption = element_text(family = "ubuntu", hjust = 1, size = 24),
          plot.title = element_text(family = "ubuntu", hjust = 0, size = 50),
          plot.subtitle = element_text(family = "ubuntu", size = 27, hjust = 0),
          plot.margin = unit(c(0.8, 0.8, 0.8, 1.5), "cm"))
  
  # Save the Plot
  
  ggsave("Day26.png", p, width = 22, height = 12, dpi = 72) 
  

  

