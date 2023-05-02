#### set up ####
#### Libraries (Base) ####
library(here)
library(tidyverse, quietly = TRUE)
library(scales)
library(ggtext)
library(systemfonts)


#### Libraries (This Plot) ####
library(igraph)
library(ggraph)
library(tidygraph)
library(ggforce)

#import fonts for plot
sysfonts::font_add_google("Cabin Sketch", "cs")
showtext::showtext_auto()

#### Directory ####
# Create directory to save images and plots
dir.create(here("2022/2022_Week_033/Plots"), recursive = TRUE, mode = "0755")


#### Read data ####
characters <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-08-16/characters.csv')
psychometrics <-  readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-08-16/psych_stats.csv')


#### Data wrangling #### 
##### Network to see similarities between Sherlock Holmes characters?
###### set seed
set.seed(122)

#### Intersection #### 
psycho_df <- psychometrics %>%
  dplyr::filter(uni_name == "Sherlock") %>% 
  dplyr::group_by(char_name, question) %>%
  dplyr::slice_max(order_by = avg_rating, n = 1, with_ties = FALSE) %>% 
  dplyr::ungroup() %>% 
  dplyr::group_by(question) %>%
  dplyr::mutate(question_id = dplyr::cur_group_id(),
                personality = stringr::str_c(question_id, personality, sep = "_")) %>% 
  dplyr::ungroup() %>%
  dplyr::select(char_name, question, personality) %>% 
  dplyr::group_by(char_name) %>%
  dplyr::summarise(personality = list(as.character(personality))) %>%
  dplyr::rename("from" = "char_name") %>% 
  tidyr::crossing(dplyr::rename(., "to" = "from", "personality_to" = "personality")) %>% 
  dplyr::mutate(frequency = purrr::map2_dbl(personality, personality_to, ~length(base::intersect(.x, .y)))) %>% 
  dplyr::select(from, to, frequency) %>% 
  dplyr::filter(from != to) %>% 
  dplyr::mutate(similarity = case_when(frequency <= quantile(frequency, probs = 0.80) ~ "q80",
                                       frequency <= quantile(frequency, probs = 0.90) ~ "q90",
                                       TRUE ~ "Top1")) %>% 
  dplyr::arrange(frequency)


#### Network #### 
##### Network vertices #####

psycho_ve <- psycho_df %>%
  dplyr::group_by(to) %>%
  dplyr::summarise(links = sum(frequency)) %>%
  dplyr::select(to, links)


##### Network object #####

psycho_ig <- igraph::graph_from_data_frame(d = psycho_df, 
                                           vertices = psycho_ve, 
                                           directed = FALSE)

##### Network tidy #####

psycho_tg <- tidygraph::as_tbl_graph(psycho_ig) %>% 
  tidygraph::activate(nodes) %>% 
  dplyr::mutate(label = name)

size <- igraph::V(psycho_tg)$links

#### Legend Annotation ####

data_label <- tibble(x = seq(1.25, 2.75, length.out = 6),
                     y = seq(-1.35, -1.35, length.out = 6),
                     color  = psycho_ve %>% pull(links) %>% sort(),
                     alpha  = psycho_ve %>% pull(links) %>% sort(),    
                     size   = psycho_ve %>% pull(links) %>% sort())

#### Plot aesthetics ####
background     <- "#d0c4b4"
lines_color    <- "#A6444C"
title_color    <- "#F2C230"
subtitle_color <- "black"
text_color     <- "#F2F2F2"
caption_color  <- "black"


#### Annotation ####
annotation_title_text <- c("SHERLOCK HOLMES")
annotation_text <- c("Open-Source Psychometrics Project conducts a survey and
                     collects data based on personality tests. The idea of
                     this test is to match takers to a fictional character
                     based on similarity of personality. It is assumed that a 
                     character's assumed personality is reflected in the average 
                     ratings of individuals. Here, the characters of the 
                     Sherlock Holmes are investigated. ")
annotation_text <- stringr::str_wrap(annotation_text, 80) %>% 
  stringr::str_replace_all("\n","<br>")

set.seed(122)
#### Plot ####
Plot <- psycho_tg %>%
  ggraph::ggraph(layout = "igraph", algorithm = 'fr') +
  ggraph::geom_edge_arc(aes(edge_width = similarity, 
                            colour = similarity), lineend = "round", 
                        strength = 0.1) +
  ggraph::geom_node_point(aes(colour = links), fill = background, 
                          size = log(size) * 1.5, alpha = 1.0, shape = 21) +
  ggraph::geom_node_point(aes(colour = links), size = log(size) * 1.0, 
                          alpha = 1.0) +
  ### Annotations ###
  ggraph::geom_node_label(aes(label = label), colour = "black",
                          size = log(size) * 0.9, 
                          family = "cs", 
                          fontface = "bold", repel = FALSE, nudge_y = -0.04,
                          nudge_x = 0.00, alpha = 0.6, fill = background, 
                          label.size = NA) +
  ggtext::geom_richtext(aes(x = 0.55, y = 0.53), 
              label = annotation_text, color = subtitle_color, size = 6,
              family = "cs", face = "plain", fill = "transparent",
              label.size = NA, hjust = 0,  vjust = 0.5) +
  ### Scales ###
  ggraph::scale_edge_width_manual(values = c(seq(0.2, 1.0, length.out = 2), 2.0), 
                                  breaks = c("q80","q90","Top1")) +
  ggraph::scale_edge_alpha_manual(values = c(seq(0.2, 0.5, length.out = 2), 0.8),
                                  breaks = c("q80","q90","Top1")) +
  ggraph::scale_edge_colour_manual(values = viridis::mako(n = 
  length(unique(psycho_df$similarity)), begin = 0.5, end = 0.9, direction = 1)) +
  scale_colour_gradientn(colors = viridis::turbo(n = 
      length(unique(psycho_ve$links)), begin = 0.1, end = 0.5, direction = 1)) +
  guides(edge_width = "none", edge_alpha = "none", colour = "none", 
         edge_colour = "none")   +
  coord_cartesian(clip = "off") +
  ### Theme ###
  theme_void() +
  theme(
    ## Plot Aesthetic ##
    panel.background = element_rect(fill = background, color = NA),
    plot.background  = element_rect(fill = background, color = NA),
    ## Titles & Caption ##
    plot.title.position = "panel",
    plot.title = ggtext::element_markdown(color = "black", family = "cs", 
                        face = "plain", size = 80, hjust = 0.5, halign = 0.5,
                        vjust = 0.5, valign = 0.5,
                        margin = unit(c(0.1, 0.1, 0.1, 0.1), "cm")),
    plot.caption.position = "panel",
    plot.caption = ggtext::element_markdown(color = caption_color, 
                  family = "cs", size = 10, hjust = 1, vjust = 0, 
                  margin = margin(t = -0.5, r = 0.1, b = 0.1, l = 0.0, 
                                  unit = "cm")),
    ## Margin ##
    plot.margin = margin(t = 0.5, r = 1.5, b = 0.5, l = 1.5, unit = "cm")) +
  ### Labels ###
  ggplot2::labs(x = "",
                y = "",
                title = annotation_title_text,
  caption = "<span style='font-size: 18pt'>Data Source: Open-Source Psychometrics Project | #30DayChartChallenge - Day #17 | TidyTuesday 2022 - Week 33 | Prepared by: C. YAZICI</span>") 

Final_Plot <- Plot +
  ### Legend Annotation ###
  ggforce::geom_link2(data = data_label, aes(x = x, y = y + 1, colour = color, 
                  alpha = alpha, size = size * 10000, group = 1), lineend = 'round') +
  geom_point(data = data_label[c(1, 6),], aes(x = x, y = y + 1, colour = color), 
             fill = background, alpha = 1.0, size = range(log(size) * 1.5), 
             shape = 21) +
  geom_point(data = data_label[c(1, 6),], aes(x = x, y = y + 1, colour = color),
             alpha = 1.0, size = range(log(size))) +
  geom_text(data = data_label[c(1, 6),], aes(x = x, y = y + 1), 
            label = c("-","+"), colour = "black", alpha = 1.0,
            size = sort(log(size))[c(1,6)], family = "cs", 
            fontface = "bold", hjust = 0.5, vjust = 0.5) +
  geom_text(data = data_label[3,], aes(x = x, y = y + 1.05), 
            label = "Connections", colour = "black", 
            alpha = 1.0, size = sort(log(size) * 0.9)[1], 
            family = "cs", fontface = "bold") +
  geom_text(data = data_label[3,], aes(x = x, y = y + 0.95), 
            label = "Similarity", colour = "black", 
            alpha = 1.0, size = sort(log(size) * 0.9)[1], 
            family = "cs", fontface = "bold") +
  scale_alpha_continuous(range = c(0.0, 1.0)) +
  scale_size_continuous(range = c(0.2, 2.0)) +
  guides(colour = "none", alpha = "none", size = "none")

Final_Plot

#### Progress ####

ggsave("Day17.png", Final_Plot, width = 25, height = 15, dpi = 72)
