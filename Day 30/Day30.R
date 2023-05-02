library(readxl)
library(janitor)
library(tidyverse)
library(gghighlight)
library(showtext)


# Font in the Plot

font_add_google('Arimo', 'arimo')
showtext_auto()

# Data Import

data_imm <- read_xls("Data_final.xls") %>%
  clean_names()

# Data Wrangling

plot_data <- data_imm %>%
  select(-c(indicator_name, indicator_code, x1960:x1980)) 

colnames(plot_data) = gsub("x","",colnames(plot_data))
  
plot_data2 <- plot_data %>%
  select(-country_code) %>%
  filter(country_name %in% c("Africa Eastern and Southern",
                             "Africa Western and Central",
                             "Sub-Saharan Africa",
                             "Central Europe and the Baltics",
                             "East Asia & Pacific",
                             "European Union",
                             "Latin America & Caribbean",
                             "Least developed countries: UN classification",
                             "Low income",
                             "Middle East & North Africa",
                             "World")) %>%
  pivot_longer(-c(country_name),
                  values_to = "values") %>%
  rename(year = name) %>%
  mutate(year = as.numeric(year))
 
   
subtitle_text <- str_wrap("Measles can be prevented with vaccine. After 1990, most of the
countries are vaccinated with high percentages of population. Here, several
classifications as Africa Eastern and Southern, Africa Western and Central,
Sub-Saharan Africa, Central Europe and the Baltics, East Asia & Pacific,
European Union, Latin America & Caribbean, Least developed countries: UN 
classification, Low income and Middle East & North Africa are shown in addition 
to the overall. Among these, Central Europe and Baltics is the one which 
vaccinated 100% of the population in a short time and Africa Western and 
Central is the group with the lowest percentage. In 1990, all of them had a 
higher rate of vaccination which can be clearly seen in the plot in the 
                          bottom.", 115)
# The Plot

p <- ggplot(plot_data2) +
  geom_line(aes(x = year, y = values, colour = country_name),
            linewidth = 2) +
  gghighlight(country_name %in% c("Africa Western and Central",
                                 "Central Europe and the Baltics")) +
  coord_cartesian() +
  labs(title = "IMMUNIZATION",
       subtitle = subtitle_text,
       y = "% of Population",
       caption = "\nData Source: Worldbank  | #30DayChartChallenge - Day #30 | Prepared by: C. YAZICI") +
  theme(panel.background = element_rect(fill = "ivory", color = NA),
        plot.background = element_rect(fill = "ivory", color = NA),
        axis.ticks = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_text(family = "arimo", size = 20),
        panel.grid = element_blank(),
        axis.text = element_text(family = "arimo", size = 15),
        plot.title = element_text(family = "arimo", hjust = 0, size = 50),
        plot.subtitle = element_text(family = "arimo", hjust = 0, size = 20),
        plot.caption = element_text(family = "arimo", hjust = 1, size = 20),
        plot.margin = unit(c(1, 1, 1, 1), "cm"))


ggm <- ggmagnify(p,
          xlim = c(1983, 1992), ylim = c(40, 96),
          inset_xlim = c(2000, 2020), inset_ylim = c(10, 38),
          proj = "single",
          colour = "black", proj_linetype = 1, linewidth = 0.8,
          shadow = TRUE)

# Save the plot

ggsave("Day30.png", width = 25, height = 15, dpi = 72, compose(ggm)) 

