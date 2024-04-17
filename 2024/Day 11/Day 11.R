library(readr)
library(tidyverse)
library(ggtext)
library(showtext)
library(ggchicklet)
library(ggnewscale)

# library to use fontawesome icons
font_add('fa-reg', 'Font Awesome 6 Free-Regular-400.otf')
font_add('fa-solid', 'Font Awesome 6 Free-Solid-900.otf')
showtext_auto()

font_add_google('IBM Plex Sans', 'ips')
showtext_auto()


data11 <- read.csv("AppleStore.csv") 

app_data <- data11 %>%
  filter(price == 0) %>%
  group_by(prime_genre) %>%
  summarise(n = n(),
            avg_rating = mean(user_rating)) %>%
  arrange(desc(n)) %>%
  ungroup() %>%
  mutate(category_lab = case_when(
    prime_genre == "Games" ~ "<span style='font-family:fa-solid'>&#xf11b;</span>",
    prime_genre == "Entertainment" ~ "<span style='font-family:fa-solid'>&#xf12e;</span>",
    prime_genre == "Photo & Video" ~ "<span style='font-family:fa-solid'>&#xf008;</span>",
    prime_genre == "Social Networking" ~ "<span style='font-family:fa-solid'>&#xf164;</span>",
    prime_genre == "Education" ~ "<span style='font-family:fa-solid'>&#xf19d;</span>",
    prime_genre == "Shopping" ~ "<span style='font-family:fa-solid'>&#xf07a;</span>",
    prime_genre == "Utilities" ~ "<span style='font-family:fa-solid'>&#xf013;</span>",
    prime_genre == "Lifestyle" ~ "<span style='font-family:fa-solid'>&#xf21e;</span>",
    prime_genre == "Finance" ~ "<span style='font-family:fa-solid'>&#xf53d;</span>",
    prime_genre == "Sports" ~ "<span style='font-family:fa-solid'>&#xf206;</span>",
    prime_genre == "Health & Fitness" ~ "<span style='font-family:fa-solid'>&#xf21e;</span>",
    prime_genre == "Music" ~ "<span style='font-family:fa-solid'>&#xf001;</span>",
    prime_genre == "Book" ~ "<span style='font-family:fa-solid'>&#xf02d;</span>",
    prime_genre == "Productivity" ~ "<span style='font-family:fa-solid'>&#xf275;</span>",
    prime_genre == "News" ~ "<span style='font-family:fa-solid'>&#xf1ea;</span>",
    prime_genre == "Travel" ~ "<span style='font-family:fa-solid'>&#xf3c5;</span>",
    prime_genre == "Food & Drink" ~ "<span style='font-family:fa-solid'>&#xf2e7;</span>",
    prime_genre == "Weather" ~ "<span style='font-family:fa-solid'>&#xf743;</span>",
    prime_genre == "Business" ~ "<span style='font-family:fa-solid'>&#xf0b1;</span>",
    prime_genre == "Navigation" ~ "<span style='font-family:fa-solid'>&#xf276;</span>",
    prime_genre == "Reference" ~ "<span style='font-family:fa-solid'>&#x2a;</span>",
    prime_genre == "Catalogs" ~ "<span style='font-family:fa-solid'>&#xf0ca;</span>",
    prime_genre == "Medical" ~ "<span style='font-family:fa-solid'>&#xf479;</span>"
    ),
    index = row_number(),
    y = rep(c(7, 5.8, 4.6, 3.4, 2.2, 1.0), each = 4)[-24],
    x = rep(c(0.4, 1.8, 3.1, 4.5), 6)[-24])


subtitle_text <- str_wrap("More than 7000 Apple iOS mobile applications
show that the most popular category is Games with a total of 2257 apps. 
Although the average ratings are similar, Productivity and Music 
categories have the highest ratings.", 50)

p <- ggplot(app_data) +
  ggchicklet:::geom_rrect(mapping = aes(xmin = -0.6, 
                                        xmax = 6, 
                              ymin = -1, ymax = 9), 
                          color = "black",
                          fill = "black",
                          radius = unit(0.5, units = "cm")) +
  ggchicklet:::geom_rrect(mapping = aes(xmin = -0.4, 
                                        xmax = 5.8, 
                                        ymin = -0.8, ymax = 8.8), 
                          color = "#F8E881",
                          fill = "#F8E881",
                          radius = unit(0.5, units = "cm")) +
  ggchicklet:::geom_rrect(mapping = aes(xmin = 1.5, 
                                       xmax = 4, 
                                       ymin = 8.3, ymax = 9), 
                         color = "black",
                         fill = "black",
                         radius = unit(0.5, units = "cm")) +
  geom_point(mapping = aes(x = 1.8, y = 8.7), color = "grey60",
             size = 5) +
  # Signal Sign
  geom_richtext(mapping = aes(x = 4.25, y = 8.6,
                label = "<span style='font-family:fa-solid'>&#xf012;</span>"),
                family = 'fontawesome-webfont', size = 6,
                colour = "white",
                label.colour = NA, fill = NA,
                hjust = 0, vjust = 0.5) +
  # Wifi Sign
  geom_richtext(mapping = aes(x = 4.65, y = 8.6,
                              label = "<span style='font-family:fa-solid'>&#xf1eb;</span>"),
                family = 'fontawesome-webfont', size = 6,
                colour = "white",
                label.colour = NA, fill = NA,
                hjust = 0, vjust = 0.5) +
  # Battery Sign
  geom_richtext(mapping = aes(x = 5.1, y = 8.6,
                              label = "<span style='font-family:fa-solid'>&#xf243;</span>"),
                family = 'fontawesome-webfont', size = 6,
                colour = "white",
                label.colour = NA, fill = NA,
                hjust = 0, vjust = 0.5) +
  ggchicklet:::geom_rrect(aes(xmin = x - 0.05, xmax = x + 0.85, 
                              ymin = y - 0.4, ymax = y + 0.4), 
                          color = "#FB928E",
                          fill = "#FBC4BF",
                          radius = unit(0.5, units = "cm")) +
  geom_richtext(aes(x = x, y = y,
                    label = category_lab, colour = n),
                family = 'fontawesome-webfont', size = 14,
                label.colour = NA, fill = NA,
                hjust = 0, vjust = 0.5) +
  scale_colour_gradient(
    low = "#6745F0",
   # mid = "#7357E2",
    high = "#1E0878") +
  labs(colour = "") +
  new_scale_colour() +
  geom_text(aes(x = x + 0.35, y = y - 0.5, label = prime_genre),
            hjust = 0.5, family = "ips", size = 4, fontface = "bold") +
  geom_point(aes(x = x + 0.85, y = y + 0.4, color = avg_rating), 
             size = 17, stroke = 1) +
  scale_colour_gradient(
    low = "#81F981",
    # mid = "#7357E2",
    high = "#023002", guide = "none") +
  geom_text(aes(x = x + 0.85, y = y + 0.4, 
                label = round(avg_rating, 2)), 
            hjust = 0.5, color = "ivory", 
            family = "ips", size = 4, fontface = "bold") +
  # Legend Title
  geom_text(mapping = aes(x = 2.7, y = 0.15, 
                label = "Number of Apps"), 
            hjust = 0.5, color = "black", 
            family = "ips", size = 6, fontface = "bold") +
  geom_text(mapping = aes(x = 6.8, y = 6.5, label = "Average\nRating"),
            hjust = 0.5, color = "black", 
            family = "ips", size = 6, fontface = "bold") +
  geom_curve(mapping = aes(x = 5.5, xend = 6.8, 
                           y = 7.5, yend = 6.9), 
             arrow = arrow(length = unit(0.07, "inch")), 
             linewidth = 0.5,
             curvature = -0.2,
             color = "black")+
  coord_equal(xlim = c(-0.4, 7.5)) +
  labs(title = "Mobile Applications",
       subtitle = subtitle_text,
       caption = "Data Source: Kaggle\n#30DayChartChallenge - Day #11\nPrepared by: C. YAZICI") +
  theme(legend.position = c(0.40, 0.1),
        legend.key.width = unit(2, "cm"),
        legend.direction = "horizontal",
        legend.text = element_text(family = "ips", size = 14),
        legend.background = element_rect(fill = "#F8E881"),
        plot.title = element_text(family = "ips",
                                  size = 45, hjust = 0),
        plot.subtitle = element_text(family = "ips",
                                  size = 20, hjust = 0),
        plot.caption = element_text(family = "ips",
                                  size = 18, hjust = 1),
        plot.background = element_rect(fill = "ivory", color = NA),
        panel.background = element_rect(fill = "ivory", color = NA),
        panel.grid = element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_blank(),
        axis.title = element_blank(),
        plot.margin = margin(1.0, 0, 0.2, 0, "cm")) 


# Save the Plot

ggsave("Day11.png", p, width = 17, height = 17, dpi = 72)


