library(tidyverse)
library(janitor)
library(ggtext)
library(grid)

### Data downloaded from: https://baseballsavant.mlb.com/leaderboard/bat-tracking
bat_tracking_left <- read_csv("bat-tracking-left.csv") %>%
  clean_names() %>%
  select(id, name, swings_competitive, avg_bat_speed) %>%
  arrange(desc(swings_competitive))
bat_tracking_right <- read_csv("bat-tracking-right.csv") %>%
  clean_names() %>%
  select(id, name, swings_competitive, avg_bat_speed) %>%
  arrange(desc(swings_competitive))

switch_hitters <- full_join(bat_tracking_left, bat_tracking_right, by = c("id", "name"), suffix = c("_left", "_right")) %>%
  filter(swings_competitive_left >= 50 & swings_competitive_right >= 50) %>%
  mutate(diff = avg_bat_speed_left - avg_bat_speed_right,
         category = case_when(diff > 1 & id == 682829 ~ "Better left (Elly)",
                              diff > 1 ~ "Better left",
                              diff < -1 ~ "Better right",
                              TRUE ~ "Same"))

elly_colour <- "#bf0d3e"
other_left_colour <- "#bf0d3e"
mid_colour <- "#c5c5c5"
right_colour <- "#041e42"

rotation <- 45

rough_scatter <- ggplot(switch_hitters, aes(y = avg_bat_speed_left, x = avg_bat_speed_right, colour = category)) +
  geom_point(size = 4.5,
             color = case_when(switch_hitters$category == "Better left (Elly)" ~ elly_colour,
                               switch_hitters$category == "Better left" ~ other_left_colour,
                               switch_hitters$category == "Better right" ~ right_colour,
                               TRUE ~ mid_colour),
             alpha = case_when(switch_hitters$category == "Better left (Elly)" ~ 1,
                               switch_hitters$category == "Better left" ~ 0.5,
                               switch_hitters$category == "Better right" ~ 0.5,
                               TRUE ~ 0.4)) +
  scale_x_continuous(limits = c(59, 81),
                     breaks = seq(60, 80, 5)) +
  scale_y_continuous(limits = c(59, 81),
                     breaks = seq(60, 80, 5))
rough_scatter

themed_scatter <- rough_scatter +
  theme_ptb() +
  theme(axis.title = element_blank(),
        axis.ticks = element_blank(),
        axis.line = element_blank(),
        axis.text.x = element_text(angle = (-1 * rotation),
                                   hjust = 0.5,
                                   vjust = 0.5),
        axis.text.y = element_text(angle = (-1 * rotation),
                                   hjust = 0.5,
                                   vjust = 0),
        plot.margin = margin(0.9, 0.9, 0.9, 0.9, unit = "in")) +
  coord_equal(clip = "off")
themed_scatter

### FLIPPED USING OWEN PHILLIPS' TUTORIAL AT THE F5: https://thef5.substack.com/p/how-to-diamond-plots-in-r
