library(tidyverse)
library(googlesheets4)
library(janitor)
library(RcppRoll)
library(ggtext)

# Results for all teams imported via Google Sheets from Wikipedia article series: 'Head-to-head records of Tier 1 rugby union national teams'
# e.g. https://en.wikipedia.org/wiki/History_of_rugby_union_matches_between_Argentina_and_Australia

hn_results <- read_sheet(ss = "https://docs.google.com/spreadsheets/d/17bEWMIJhsPKAO0OvvtyIUAeJOOsKLi-lFZi3uVlFmAc/",
                              sheet = "Tidy (1996-)") %>%
  clean_names()

hn_results_tidy <- hn_results %>%
  select(1:4, 11:12)

roll_no_hn <- 60
hn_roll_results <- hn_results_tidy
hn_roll_results$rolling_points_for <- roll_sum(hn_roll_results$tri_points, n = roll_no_hn, align = "right", fill = NA) / roll_no_hn
hn_roll_results$rolling_points_against <- roll_sum(hn_roll_results$opponent_points, n = roll_no_hn, align = "right", fill = NA) / roll_no_hn

hn_roll_results <- hn_roll_results %>%
  filter(!is.na(rolling_points_for))

tri_blue <- "#023064"
hn_red <- "#af001d"
tri_grey <- "#e0e4ef"

hn_rough_line <- ggplot(hn_roll_results, aes(x = index)) +
  geom_ribbon(aes(ymin = rolling_points_against, ymax = rolling_points_for), fill = tri_grey) +
  geom_path(aes(y = rolling_points_for),
            color = tri_blue,
            linewidth = 1.25) +
  geom_path(aes(y = rolling_points_against),
            color = hn_red,
            linewidth = 1.25) +
  scale_y_continuous(name = NULL,
                     limits = c(1, 49),
                     expand = c(0,0),
                     breaks = seq(10, 40, 10)) +
  scale_x_continuous(limits = c((min(hn_roll_results$index) - 70), 30),
                     expand = c(0,0)) +
  theme(axis.line.x = element_blank(),
        axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        axis.ticks.x = element_blank(),
        panel.grid.major.x = element_blank())
hn_rough_line

hn_themed_line <- hn_rough_line + theme_ptb()
hn_themed_line

hn_annotated_line <- hn_themed_line +
  geom_vline(xintercept = -287,
             size = 0.5,
             color = tri_grey,
             linetype = "dashed") +
  geom_vline(xintercept = -248,
             size = 0.5,
             color = tri_grey,
             linetype = "dashed") +
  geom_vline(xintercept = -211,
             size = 0.5,
             color = tri_grey,
             linetype = "dashed") +
  geom_vline(xintercept = -162,
             size = 0.5,
             color = tri_grey,
             linetype = "dashed") +
  geom_vline(xintercept = -119,
             size = 0.5,
             color = tri_grey,
             linetype = "dashed") +
  geom_vline(xintercept = -71,
             size = 0.5,
             color = tri_grey,
             linetype = "dashed") +
  geom_vline(xintercept = -25,
             size = 0.5,
             color = tri_grey,
             linetype = "dashed")
hn_annotated_line

hn_labelled_line <- hn_annotated_line +
  labs(title = "The <b style='color:#af001d'>Home Nations</b> closed the gap",
       subtitle = "Average scoreline in games between the <b style='color:#af001d'>Home<br>Nations</b> and the <b style='color:#023064'>Tri Nations</b> since 1996",
       caption = "<i>Averages calculated on a 60-game rolling basis</i><br><br>Data: Wikipedia | Chart: Plot the Ball")
hn_labelled_line
