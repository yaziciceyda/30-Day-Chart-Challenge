library(tidyverse)
library(readr)
library(showtext)
library(janitor)
library(ggh4x)

font_add_google('PT Serif', 'pts')
showtext_auto()


data12 <- read.csv("sub-energy-fossil-renewables-nuclear.csv") %>%
  clean_names()


countries <- c("European Union (27)", "France", "Sweden", "Slovakia",
               "Slovenia", "Bulgaria", "Finland", "Belgium",
               "Czechia", "Hungary", "Spain", "Romania",
               "United Kingdom", "Germany", "Netherlands")

data_all <- data12 %>%
filter(entity %in% countries) %>%
  select(-code) %>%
  rename(nuclear = nuclear_equivalent_primary_energy,
         renewables = renewables_equivalent_primary_energy,
         fossil = fossil_fuels_equivalent_primary_energy) %>%
  mutate(entity = ifelse(entity == "European Union (27)",
                         "European Union", entity)) %>%
  pivot_longer(-c(entity, year),
               names_to = "type", values_to = "amount") %>%
  group_by(entity, year) %>%
  mutate(total_amount = sum(amount),
         percentage = round(amount / total_amount * 100)) %>%
  ungroup() %>%
  mutate(type = factor(type,
                       levels = c("fossil", "renewables",
                                  "nuclear"),
                       ordered = TRUE),
         entity = ifelse(entity == "Czechia", "Czech\nRep.",
                         entity),
         entity = ifelse(entity == "United Kingdom", "United\nKingdom",
                         entity),
         entity = ifelse(entity == "European Union", "European\nUnion",
                         entity),
         entity = factor(entity,
         levels = c("European\nUnion", "France", "Sweden", "Slovakia",
               "Slovenia", "Bulgaria", "Finland", "Belgium",
               "Czech\nRep.", "Hungary", "Spain", "Romania",
               "United\nKingdom", "Germany", "Netherlands")))


design12 <- "
##A##  
BCDEF  
GHIJK
LMNO#
"

x_labels <- data.frame(entity = c("European\nUnion", "France", "Sweden", "Slovakia",
                                  "Slovenia", "Bulgaria", "Finland", "Belgium",
                                  "Czech\nRep.", "Hungary", "Spain", "Romania",
                                  "United\nKingdom", "Germany", "Netherlands"), 
                       label1 = c(1965, rep(NA, 14)),
                       label2 = c(2022, rep(NA, 14))) %>%
  mutate(entity = factor(entity,
                          levels = c("European\nUnion", "France", "Sweden", "Slovakia",
                                     "Slovenia", "Bulgaria", "Finland", "Belgium",
                                     "Czech\nRep.", "Hungary", "Spain", "Romania",
                                     "United\nKingdom", "Germany", "Netherlands")))

p <- ggplot(data_all) +
  geom_area(aes(year, percentage, fill = type)) +
  # Percentage for fossil 
  geom_text(data_all %>%
              filter(year == 2022, type == "fossil"),
            mapping = aes(x = 2026.5, y = 95, 
                          label = paste(percentage, "%", sep = "")),
            color = "#343B34", family = "pts", fontface = "bold",
            size = 6, hjust = 0.35) +
  # Percentage for renewables
  geom_text(data_all %>%
              filter(year == 2022, type == "renewables"),
            mapping = aes(x = 2026.5, y = 55, 
                          label = paste(percentage, "%", sep = "")),
            color = "#6BA46B", size = 6, hjust = 0.35,
            family = "pts", fontface = "bold") +
  # Percentage for nuclear
  geom_text(data_all %>%
              filter(year == 2022, type == "nuclear"),
            mapping = aes(x = 2027.5, y = 5, 
                          label = paste(percentage, "%", sep = "")),
            color = "#D2B77A", size = 8, hjust = 0.35,
            family = "pts", fontface = "bold") +
  geom_text(data_all %>% 
              filter(entity == "Netherlands"),
            mapping = aes(x = 1985, y = 87, 
                label = entity),
            color = "ivory", family = "pts", size = 6,
            hjust = 0,
            fontface = "bold") +
  geom_text(data_all %>% 
              filter(entity != "Netherlands"),
            mapping = aes(x = 1994, y = 87, 
                label = entity),
            color = "ivory", family = "pts", size = 6,
            hjust = 0,
            fontface = "bold") +
  # 1965
  geom_text(x_labels %>% 
              filter(entity == "European\nUnion"),
            mapping = aes(x = 1974, y = 106, label = label1),
            color = "black", hjust = 1, family = "pts", size = 6) +
  # 2022
  geom_text(x_labels %>% 
              filter(entity == "European\nUnion"),
            mapping = aes(x = 2011, y = 106, label = label2),
            color = "black", hjust = 0, family = "pts", size = 6) +
     facet_manual(vars(entity), design = design12) +
  scale_fill_manual(values = c("#343B34",
                               "#6BA46B",
                               "#D2B77A")) +
   labs(x = "",
       y = "",
       title = "Nuclear reliance in Europe",
       subtitle = "Source of energy by supply of total energy demand.",
       caption = "Data Source: OWID\n#30DayChartChallenge - Day #12\nPrepared by: C. YAZICI") +
  coord_fixed(xlim = c(1965, 2035)) +
  theme(legend.position = "none",
        axis.text = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        panel.background = element_rect(fill = "#fafafa", 
                                        color = "#fafafa"),
        plot.background = element_rect(fill = "#fafafa", 
                                       color = "#fafafa"),
        panel.grid = element_blank(),
        strip.text = element_blank(),
        panel.spacing = unit(2.5, "lines"),
        strip.background = element_rect(fill = "#fafafa"),
        plot.title = element_text(family = "pts", hjust = 0,
                                  size = 40),
        plot.subtitle = element_text(family = "pts", hjust = 0,
                                     size = 30),
        plot.caption = element_text(family = "pts", hjust = 1,
                                    size = 20),
        plot.margin = margin(0.5, 1.0, 0.5, 1.0, "cm")) 


# Save the Plot

ggsave("Day12.png", p, width = 26, height = 17, dpi = 72)






