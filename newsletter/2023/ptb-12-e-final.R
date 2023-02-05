library(tidyverse)
library(googlesheets4)
library(janitor)
library(RcppRoll)
library(ggtext)

# Results for all teams imported via Google Sheets from Wikipedia article series: 'Head-to-head records of Tier 1 rugby union national teams'
# e.g. https://en.wikipedia.org/wiki/History_of_rugby_union_matches_between_Argentina_and_Australia

wal_results_tidy <- wal_results %>%
  select(1:4, 11:12)

roll_no <- 30
wal_roll_results <- wal_results_tidy
wal_roll_results$rolling_points_for <- roll_sum(wal_roll_results$wal_points, n = roll_no, align = "right", fill = NA) / roll_no
wal_roll_results$rolling_points_against <- roll_sum(wal_roll_results$opponent_points, n = roll_no, align = "right", fill = NA) / roll_no

wal_roll_results <- wal_roll_results %>%
  filter(!is.na(rolling_points_for))

wal_red <- "#d8252e"
wal_gold <- "#c9b771"
wal_grey <- "#e0e4ef"

wal_rough_line <- ggplot(wal_roll_results, aes(x = index)) +
  geom_ribbon(aes(ymin = rolling_points_against, ymax = rolling_points_for), fill = wal_grey, alpha = 0.6) +
  geom_path(aes(y = rolling_points_for),
            color = wal_red,
            size = 1) +
  geom_path(aes(y = rolling_points_against),
            color = wal_gold,
            size = 1) +
  scale_y_continuous(name = NULL,
                     limits = c(1, 49),
                     expand = c(0,0),
                     breaks = seq(10, 40, 10)) +
  scale_x_continuous(limits = c((min(wal_roll_results$index)- 40), 20),
                     expand = c(0,0)) +
  theme(axis.line.x = element_blank(),
        axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        axis.ticks.x = element_blank(),
        panel.grid.major.x = element_blank())
wal_rough_line

wal_themed_line <- wal_rough_line + theme_ptb()
wal_themed_line

wal_annotated_line <- wal_themed_line +
  geom_vline(xintercept = -263,
             size = 0.5,
             color = wal_grey,
             linetype = "dashed") +
  geom_vline(xintercept = -228,
             size = 0.5,
             color = wal_grey,
             linetype = "dashed") +
  geom_vline(xintercept = -194,
             size = 0.5,
             color = wal_grey,
             linetype = "dashed") +
  geom_vline(xintercept = -156,
             size = 0.5,
             color = wal_grey,
             linetype = "dashed") +
  geom_vline(xintercept = -114,
             size = 0.5,
             color = wal_grey,
             linetype = "dashed") +
  geom_vline(xintercept = -73,
             size = 0.5,
             color = wal_grey,
             linetype = "dashed") +
  geom_vline(xintercept = -29,
             size = 0.5,
             color = wal_grey,
             linetype = "dashed")
wal_annotated_line

wal_labelled_line <- wal_annotated_line +
  labs(title = "<b style='color:#d8252e'>Wales</b> in the professional era",
       subtitle = "Rolling average no. of points <b style='color:#d8252e'>scored</b> and <b style='color:#c9b771'>conceded</b> per<br>game by <b style='color:#d8252e'>Wales</b> against Tier 1 <b style='color:#c9b771'>opponents</b> since 1996",
       caption = "<i>Averages calculated on a 30-game rolling basis</i><br><br>Data: Wikipedia | Chart: Plot the Ball")
wal_labelled_line
