library(lubridate)
library(showtext)
library(usefunc)


# Font in the Plot

font_add_google('Kanit', 'kanit')
showtext_auto()

# Data Import

tuesdata <- tidytuesdayR::tt_load(2020, week = 4)
spotify_songs <- tuesdata$spotify_songs

# Data Wrangling

pop_data <- spotify_songs %>%
  filter(playlist_genre == "pop") %>%
  mutate(track_album_release_date = as.Date(track_album_release_date),
         month = month(track_album_release_date),
         season = factor(case_when(
           month %in% c(12, 1, 2) ~ "Winter",
           month %in% c(3, 4, 5) ~ "Spring",
           month %in% c(6, 7, 8) ~ "Summer",
           month %in% c(9, 10, 11) ~ "Fall",
         ), levels = c("Winter", "Fall", "Spring", "Summer")),
         track_popularity = as.numeric(track_popularity),
         energy = as.numeric(energy),
         danceability = as.numeric(danceability),
         millenium = case_when(
           track_album_release_date < '2000-01-01'~ "Before Millenium",
           track_album_release_date > '2000-01-01'~ "After Millenium",
         )) %>%
  filter(!is.na(season),
         !is.na(millenium),
         track_name != "Fake Plastic Trees",
         track_artist != "Radiohead")


# Correlation 

corr_data <- pop_data %>%
  group_by(season, playlist_subgenre, millenium) %>%
  dplyr::summarise(p_value = cor.test(energy, danceability)$p.value,
                   cor_value = cor.test(energy, danceability)$estimate) %>%
  ungroup() %>%
  mutate(sig = ifelse(p_value < 0.05, 1, 0),
         y = case_when(
           playlist_subgenre == "dance pop" ~ 1,
           playlist_subgenre == "electropop" ~ 2,
           playlist_subgenre == "indie poptimism" ~ 3,
           playlist_subgenre == "post-teen pop" ~ 4,
         )) %>%
  dplyr::rename("Subgenre" = "playlist_subgenre")

subtitle_text <- str_wrap("\nEnergy measures a perceptual measure of intensity and activity;
while danceability describes how suitable a track is for dancing based on a
combination of musical elements. The correlation of these two measures for four 
different subgenre and two time periods as Before and After Millenium (2000) 
is presented. The high correlation between energy and danceability decreases
after 2000 for all subgenre in summer. These two are weakly correlated
for these two time periods in spring. Even though they were negatively
correlated for dance pop in fall and post-teen pop in winter before millenium, 
they become positively correlated after 2000.", 130)

# The Plot

p <- ggplot(corr_data) +
  geom_vline(xintercept = 0) +
  geom_segment(aes(x = 0, xend = cor_value,
                   y = y, yend = y, color = millenium,
                   linetype = Subgenre),
               linewidth = 2) +
  geom_point(aes(x = cor_value, y = y), size = 20, colour = "#C9F3F4") +
  geom_text(aes(x = cor_value, y = y, label = round(cor_value, 2),
                color = millenium), size = 7, family = "kanit",
             hjust = 0.5, show.legend = FALSE) +
  facet_wrap(~factor(season, levels = c('Spring', 'Summer', 'Fall', 'Winter'))) +
  scale_linetype_manual(values = c("twodash", "dotted", "dashed", "solid"))+
  scale_colour_manual(values = c("Before Millenium" = "#F1252A",
                                 "After Millenium" = "#76B213")) +
  coord_cartesian() +
  labs(title = "ENERGY AND DANCEABILITY", 
       subtitle = subtitle_text,
       x = "CORRELATION",
       color = "Millenium",
       caption = "\nData Source: {spotifyr} | #30DayChartChallenge - Day #13 | Prepared by: C. YAZICI") +
  theme(legend.key.width = unit(2.5, 'cm'),
        legend.key.height = unit(2.0, 'cm'),
        panel.grid = element_blank(),
        axis.ticks = element_blank(),
        axis.text.y = element_blank(),
        axis.title.y = element_blank(),
        axis.title.x = element_text(family = "kanit", size = 18),
        axis.text.x = element_text(family = "kanit", size = 18),
        panel.background = element_rect(fill = "ivory", color = NA),
        plot.background = element_rect(fill = "ivory", color = NA),
        strip.text = element_text(family = "kanit", size = 25),
        strip.background = element_rect(fill = "#f9cf95"),
        legend.background = element_rect(fill = "ivory"),
        legend.title = element_text(family = "kanit", size = 18),
        legend.text = element_text(family = "kanit", size = 18),
        plot.title = element_text(family = "kanit", size = 40),
        plot.caption = element_text(family = "kanit", size = 20, hjust = 1),
        plot.subtitle = element_text(family = "kanit", size = 20, hjust = 0),
        plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm"))


# Save the Plot

ggsave("Day13.png", p, width = 25, height = 15, dpi = 72)





