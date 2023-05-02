library(readxl)
library(janitor)
library(PrettyCols)
library(showtext)

# Font in the Plot

font_add_google("Bruno Ace", "ba", db_cache = FALSE)
showtext_auto()

# Data Import

work_hours <- read.csv("annual-working-hours-per-worker.csv")

# Data Wrangling

country_list <- work_hours %>%
  clean_names() %>%
  filter(year >= 1995 & year <= 2017) %>%
  count(entity) %>%
  filter(n == 23)
  
work_hours2 <- work_hours %>%
  clean_names() %>%
  filter(entity %in% country_list$entity,
         year >= 1995 & year <= 2017) %>%
  rename(hours = average_annual_working_hours_per_worker)

work_hours_summary <- work_hours2 %>%
  filter(year %in% c(1995, 2017)) %>%
  select(-code) %>%
  pivot_wider(names_from = c(year), values_from = hours) %>%
  group_by(entity) %>%
  mutate(rel_change = (`2017` - `1995`) / `1995` * 100,
         rel_change = round(rel_change, 2)) %>%
  ungroup() %>%
  arrange(rel_change) %>%
  filter(rel_change <= -10.24 | rel_change >= 0.28) %>%
  select(c(entity, rel_change))


work_hours2 <- work_hours2 %>%
  clean_names() %>%
  right_join(work_hours_summary, by = "entity") %>%
  filter(entity %in% work_hours_summary$entity,
         year >= 1995 & year <= 2017) %>%
  arrange(desc(rel_change)) %>%
    mutate(entity = reorder(entity, -rel_change))

  
subtitle_text <- str_wrap("Average annual working hours per worker between 1995 
and 2017 are presented. The countries are selected according to relative change 
between these two years. The top 10 countries (South Korea - Iceland) have 
decreasing working hours and the other ones (Cambodia - Peru) have increasing 
working hours.", 70)
                          

p1 <- ggplot(work_hours2, aes(x = year, y = reorder(entity, -rel_change), fill = hours)) +
  geom_tile() +
  scale_fill_pretty_c("Teals",
                      direction = -1) +
  coord_fixed() +
  labs(fill = "Working\nHours",
       x = "",
       y = "",
       title = "AVERAGE ANNUAL WORKING HOURS",
       subtitle = subtitle_text,
       caption = "Data Source: OWID | #30DayChartChallenge - Day #23 | Prepared by: C. YAZICI") +
  theme(panel.background = element_rect(fill = "ivory", color = NA),
        plot.background = element_rect(fill = "ivory", color = NA),
        legend.title = element_text(family = "ba", size = 20),
        legend.text = element_text(family = "ba", size = 20),
        legend.background = element_rect(fill = "ivory"),
        legend.key.height = unit(1.3, 'cm'),
        panel.grid = element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_text(family = "ba", size = 18),
        plot.caption = element_text(family = "ba", hjust = 1, size = 17),
        plot.title = element_text(family = "ba", hjust = 0, size = 34),
        plot.subtitle = element_text(family = "ba", hjust = 0, size = 18),
        plot.margin = margin(0.5, 2.5, 0.5, 2.5, "cm")
        )
  
# Save the Plot

ggsave("Day23_v1.png", p1, width = 20, height = 12, dpi = 72)



subtitle_text2 <- str_wrap("Average annual working hours per worker between 1995 
and 2017 are presented. The countries are selected according to relative change 
between these two years. The top 10 countries (Cambodia - Peru) have 
increasing working hours and the other ones (South Korea - Iceland) have 
decreasing working hours.", 85)

p2 <- ggplot(work_hours2, aes(x = year, y = reorder(entity, -rel_change), fill = hours)) + 
  geom_tile(color = "grey15", size = 0.15) +
  facet_wrap(~entity, scales = "free", nrow = 5) +
  scale_fill_pretty_c("Teals",
                     direction = -1) +
  labs(x = "",
        y = "",
       fill = "Working\nHours",
       title = "AVERAGE ANNUAL WORKING HOURS",
       subtitle = subtitle_text2,
       caption = "Data Source: OWID | #30DayChartChallenge - Day #23 | Prepared by: C. YAZICI") +
  theme(panel.background = element_rect(fill = "ivory", color = NA),
        plot.background = element_rect(fill = "ivory", color = NA),
        panel.grid = element_blank(),
        axis.ticks = element_blank(),
        strip.text = element_text(family = "ba", hjust = 0.5, size = 18),
        strip.background = element_rect(fill = "ivory"),
        axis.text = element_text(family = "ba", size = 12),
        axis.text.y = element_blank(),
        legend.title = element_text(family = "ba", size = 18),
        legend.text = element_text(family = "ba", size = 18),
        legend.background = element_rect(fill = "ivory"),
        legend.key.height = unit(1.3, 'cm'),
        plot.caption = element_text(family = "ba", hjust = 1, size = 17),
        plot.title = element_text(family = "ba", hjust = 0, size = 38),
        plot.subtitle = element_text(family = "ba", hjust = 0, size = 18),
        plot.margin = margin(0.5, 1.5, 0.5, 1.5, "cm")
        )
# Save the Plot

ggsave("Day23_v2.png", p2, width = 25, height = 12, dpi = 72)

