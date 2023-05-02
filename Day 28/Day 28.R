library(colorspace)
library(showtext)
library(ggtext)

# Font in the Plot

font_add_google('Maiden Orange', 'mo')
showtext_auto()

# Data Import - Apples

apple_data <- read.csv("Apples.csv") %>%
  clean_names() %>%
  select(-product) %>%
  filter(country == "World") %>%
  select(country, production_t, year) %>%
  dplyr::rename("apple" = production_t)

# Data Import - Oranges

orange_data <- read.csv("Oranges.csv") %>%
  clean_names() %>%
  select(-product) %>%
  filter(country == "World") %>%
  select(country, production_t, year) %>%
  dplyr::rename("orange" = production_t)


# Time Series Model

train_ts.clean<- ts(orange_data$orange, frequency = 1)
fit2.clean<-ets(train_ts.clean)  
ets.f.clean <- forecast(fit2.clean,h=6)
plot(ets.f.clean)

ets.f.clean_orange <- tibble(country = "World",
                             orange = ets.f.clean$mean,
                             year = c(2022:2027),
                             high80_orange = ets.f.clean$upper[,1],
                             high95_orange = ets.f.clean$upper[,2],
                             low80_orange = ets.f.clean$lower[,1],
                             low95_orange = ets.f.clean$lower[,2]) 


orange_data <- orange_data %>%
  mutate(high80_orange = NA,
         high95_orange = NA,
         low80_orange = NA,
         low95_orange = NA) %>%
  rbind(ets.f.clean_orange)


# Time Series Model

train_ts.clean_apple <- ts(apple_data$apple, frequency = 1)
fit2.clean_apple <-ets(train_ts.clean_apple)  
ets.f.clean_apple <- forecast(fit2.clean_apple,h=6)
plot(ets.f.clean_apple)

ets.f.clean_apple <- tibble(country = "World",
                             apple = ets.f.clean_apple$mean,
                             year = c(2022:2027),
                             high80 = ets.f.clean_apple$upper[,1],
                             high95 = ets.f.clean_apple$upper[,2],
                             low80 = ets.f.clean_apple$lower[,1],
                             low95 = ets.f.clean_apple$lower[,2]) 



apple_data <- apple_data %>%
  mutate(high80 = NA,
         high95 = NA,
         low80 = NA,
         low95 = NA) %>%
  rbind(ets.f.clean_apple)


all_data <- apple_data %>%
  left_join(orange_data, by = c("country", "year")) 
all_data$orange[62:67] = ets.f.clean$mean

subtitle_text <- str_wrap("\nThe total production of apples and oranges since 
1960 in the whole world is increasing. 93M tonnes of apples and 75M tonnes of 
oranges were produced in 2021. Since the data do not include 2022 value, 
forecasts for the next six years are made. In addition to them, 80% and 95% 
confidence intervals (C. I.) are constructed.", 110)

title_text <- "<span style='color:#e12120'>APPLES 
               <span style='color:black'>AND <span style='color:#FFA500'>ORANGES"

# The Plot

p <- ggplot(all_data, aes(year)) +
  geom_ribbon(aes(xmin = 1961, xmax = 2027,
                  ymin = low95, ymax = high95), fill = "#AC2929") +
    geom_ribbon(aes(xmin = 1961, xmax = 2027,
               ymin = low80, ymax = high80), fill = "#FF4B4A") +
    geom_line(aes(y = apple), color = "#e12120", size = 3) +
  #Orange
  geom_ribbon(aes(xmin = 1961, xmax = 2027,
                  ymin = low95_orange, ymax = high95_orange), fill = "#C98102") +
  geom_ribbon(aes(xmin = 1961, xmax = 2027,
                  ymin = low80_orange, ymax = high80_orange), fill = "#FFB86F") +
  geom_line(aes(y = orange), color = "#FFA500", size = 3) +
  geom_text(aes(x = 2030, y = 109350000, label = "0.95 C.I."),
            color = "#AC2929",
            size = 10, family = "mo") +
  geom_text(aes(x = 2030, y = 101310000, label = "0.80 C.I."), 
            color = "#FF4B4A",
            size = 10, family = "mo") +
  geom_vline(xintercept = 2022, linetype = "dashed") +
  scale_y_continuous(labels = label_number(suffix = " M", scale = 1e-6)) +
  scale_x_continuous(breaks = c(1960, 1980, 2000, 2022, 2027)) +
  coord_cartesian() +
  labs(title = title_text,
       subtitle = subtitle_text,
       x = "YEAR",
       y = "PRODUCTION (TONNES)",
       colour = "FRUIT",
       caption = "Data Source: OWID | #30DayChartChallenge - Day #28 | Prepared by: C. YAZICI") +
  theme(panel.background = element_rect(fill = "#99F7C1", color = NA),
        plot.background = element_rect(fill = "#99F7C1", color = NA),
        panel.grid = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_text(family = "mo", size = 25),
        axis.text = element_text(family = "mo", size = 22),
        plot.caption = element_text(family = "mo", size = 25, hjust = 1),
        plot.title = element_markdown(family = "mo", size = 55, hjust = 0),
        plot.subtitle = element_text(family = "mo", size = 32, hjust = 0),
        plot.margin = unit(c(1, 1, 1, 1), "cm"))



# Save the Plot

ggsave("Day28.png", p, width = 25, height = 15, dpi = 72)



  
  

