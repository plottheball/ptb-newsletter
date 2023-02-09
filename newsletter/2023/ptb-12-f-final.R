library(tidyverse)
library(googlesheets4)
library(janitor)
library(RcppRoll)
library(ggtext)

# Results for all teams imported via Google Sheets from Wikipedia article series: 'Head-to-head records of Tier 1 rugby union national teams'
# e.g. https://en.wikipedia.org/wiki/History_of_rugby_union_matches_between_Argentina_and_Australia

ita_results_tidy <- ita_results %>%
  select(1:4, 11:12)

roll_no <- 30
ita_roll_results <- ita_results_tidy
ita_roll_results$rolling_points_for <- roll_sum(ita_roll_results$ita_points, n = roll_no, align = "right", fill = NA) / roll_no
ita_roll_results$rolling_points_against <- roll_sum(ita_roll_results$opponent_points, n = roll_no, align = "right", fill = NA) / roll_no

ita_roll_results <- ita_roll_results %>%
  filter(!is.na(rolling_points_for))

ita_blue <- "#005cb9"
ita_gold <- "#9b8a45"
ita_grey <- "#e0e4ef"

ita_rough_line <- ggplot(ita_roll_results, aes(x = index)) +
  geom_ribbon(aes(ymin = rolling_points_against, ymax = rolling_points_for), fill = ita_grey, alpha = 0.6) +
  geom_path(aes(y = rolling_points_for),
            color = ita_blue,
            size = 1) +
  geom_path(aes(y = rolling_points_against),
            color = ita_gold,
            size = 1) +
  scale_y_continuous(name = NULL,
                     limits = c(1, 49),
                     expand = c(0,0),
                     breaks = seq(10, 40, 10)) +
  scale_x_continuous(limits = c((min(ita_roll_results$index)- 40), 20),
                     expand = c(0,0)) +
  theme(axis.line.x = element_blank(),
        axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        axis.ticks.x = element_blank(),
        panel.grid.major.x = element_blank())
ita_rough_line

ita_themed_line <- ita_rough_line + theme_ptb()
ita_themed_line

ita_annotated_line <- ita_themed_line +
  geom_vline(xintercept = -210,
             size = 0.5,
             color = ita_grey,
             linetype = "dashed") +
  geom_vline(xintercept = -188,
             size = 0.5,
             color = ita_grey,
             linetype = "dashed") +
  geom_vline(xintercept = -157,
             size = 0.5,
             color = ita_grey,
             linetype = "dashed") +
  geom_vline(xintercept = -126,
             size = 0.5,
             color = ita_grey,
             linetype = "dashed") +
  geom_vline(xintercept = -90,
             size = 0.5,
             color = ita_grey,
             linetype = "dashed") +
  geom_vline(xintercept = -56,
             size = 0.5,
             color = ita_grey,
             linetype = "dashed") +
  geom_vline(xintercept = -21,
             size = 0.5,
             color = ita_grey,
             linetype = "dashed")
ita_annotated_line

ita_labelled_line <- ita_annotated_line +
  labs(title = "<b style='color:#005cb9'>Italy</b> in the professional era",
       subtitle = "Average no. of points <b style='color:#005cb9'>scored</b> and <b style='color:#9b8a45'>conceded</b> per game<br>by <b style='color:#005cb9'>Italy</b> against Tier 1 <b style='color:#9b8a45'>opponents</b> since 1996",
       caption = "<i>Averages calculated on a 30-game rolling basis</i><br><br>Data: Wikipedia | Chart: Plot the Ball")
ita_labelled_line
