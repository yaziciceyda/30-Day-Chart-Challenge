library(ggimage)
library(cropcircles)
library(scales)
library(showtext)



# Font in the Plot

font_add_google('Open Sans', 'os')
showtext_auto()

# Data Import

tuesdata <- tidytuesdayR::tt_load(2020, week = 50)
women <- tuesdata$women

# Preparation for the map

world <- map_data("world")
# create data for world coordinates using map_data() function
world_coordinates <- map_data("world")

world_coord <- world %>%
  group_by(region) %>%
  summarise(avg_long = mean(long),
            avg_lat = mean(lat)) %>%
  ungroup() 

new_coord <- tibble(region = c("Hong Kong", "Exiled Uighur from Ghulja (in Chinese, Yining)"),
                    avg_long = c(114.177, 87.62),
                    avg_lat = c(22.302, 43.793))

world_coord <- world_coord %>%
  rbind(new_coord)

# Data Wrangling

women_data <- women %>%
  filter(category != "All") %>%
  mutate(images_cropped = circle_crop(img),
         country = ifelse(country == "US", "USA", country),
         country = ifelse(country == "UAE", "United Arab Emirates", country),
         country = ifelse(country == "Somaliland", "Somalia", country),
         country = ifelse(country == "Iraq/UK", "UK", country),
         country = ifelse(country == "UK ", "UK", country),
         country = ifelse(country == "US", "USA", country),
         country = ifelse(country == "India ", "India", country),
         country = ifelse(country == "Republic of Ireland", "Ireland", country),
         country = ifelse(country == "Republic of Ireland", "Ireland", country),
         country = ifelse(country == "Northern Ireland", "Ireland", country),
         country = ifelse(country == "DR Congo", "Democratic Republic of the Congo", country),
         country = ifelse(country == "Wales, UK", "UK", country)) %>%
  left_join(world_coord, by = c("country" = "region")) %>%
  mutate(x_img = ifelse(avg_long < -10, -120, 220)) %>%
# -120 --> n = 21
  arrange(x_img, avg_lat) %>%
  mutate(y_img = c(seq(-37, 49, by = 4.09)[1:21], rep(0, 78)),
         x_img = ifelse(x_img == -120, c(rep(c(-120, -140, -160, -180), 
                                             times = 5), -120), x_img))
  
  
women2 <- women_data %>%
  select(name, avg_long, avg_lat, x_img) %>%
  arrange(x_img, avg_lat) %>%
  mutate(y_img = c(seq(-37, 49, by = 4.09)[1:21], rep(0, 78)))
  
# The Plot

p <- ggplot() +
  geom_map(
    data = world, map = world,
    aes(long, lat, map_id = region),
    color = "#222222", fill = "#FBB3B5"
  ) +
  geom_point(
    data = women_data,
    aes(avg_long, avg_lat, color = category), size = 22) +
  geom_image(data = women_data,
             aes(x = avg_long, y = avg_lat, image = images_cropped),
                 size = 0.03) +
  coord_fixed(clip = "off") + 
  labs(title = "Women of 2020",
       color = "Category for award",
       caption = "Data Source: BBC | #30DayChartChallenge - Day #12 | Prepared by: C. YAZICI") +
  bbc_style() +
  theme(plot.caption = element_text(hjust = 1, family = "os", size = 20),
        plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm"))

# Save the Plot

ggsave("Day12.png", p, width = 25, height = 15, dpi = 72)





