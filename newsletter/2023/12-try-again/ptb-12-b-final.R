library(tidyverse)
library(googlesheets4)
library(janitor)
library(RcppRoll)
library(ggtext)

# Results for all teams imported via Google Sheets from Wikipedia article series: 'Head-to-head records of Tier 1 rugby union national teams'
# e.g. https://en.wikipedia.org/wiki/History_of_rugby_union_matches_between_Argentina_and_Australia

fra_results_tidy <- fra_results %>%
  select(1:4, 11:12)

roll_no <- 30
fra_roll_results <- fra_results_tidy
fra_roll_results$rolling_points_for <- roll_sum(fra_roll_results$france_points, n = roll_no, align = "right", fill = NA) / roll_no
fra_roll_results$rolling_points_against <- roll_sum(fra_roll_results$opponent_points, n = roll_no, align = "right", fill = NA) / roll_no

fra_roll_results <- fra_roll_results %>%
  filter(!is.na(rolling_points_for))

france_blue <- "#2a4d7b"
france_red <- "#bc0a19"
france_grey <- "#e0e4ef"

fra_rough_line <- ggplot(fra_roll_results, aes(x = index)) +
  geom_ribbon(aes(ymin = rolling_points_against, ymax = rolling_points_for), fill = france_grey, alpha = 0.6) +
  geom_path(aes(y = rolling_points_for),
            color = france_blue,
            size = 1) +
  geom_path(aes(y = rolling_points_against),
            color = france_red,
            size = 1) +
  scale_y_continuous(name = NULL,
                     limits = c(1, 49),
                     expand = c(0,0),
                     breaks = seq(10, 40, 10)) +
  scale_x_continuous(limits = c((min(fra_roll_results$index) - 40), 20),
                     expand = c(0,0)) +
  theme(axis.line.x = element_blank(),
        axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        axis.ticks.x = element_blank(),
        panel.grid.major.x = element_blank())
fra_rough_line

fra_themed_line <- fra_rough_line + theme_ptb()
fra_themed_line

fra_annotated_line <- fra_themed_line +
  geom_vline(xintercept = -264,
             size = 0.5,
             color = france_grey,
             linetype = "dashed") +
  geom_vline(xintercept = -227,
             size = 0.5,
             color = france_grey,
             linetype = "dashed") +
  geom_vline(xintercept = -185,
             size = 0.5,
             color = france_grey,
             linetype = "dashed") +
  geom_vline(xintercept = -143,
             size = 0.5,
             color = france_grey,
             linetype = "dashed") +
  geom_vline(xintercept = -104,
             size = 0.5,
             color = france_grey,
             linetype = "dashed") +
  geom_vline(xintercept = -64,
             size = 0.5,
             color = france_grey,
             linetype = "dashed") +
  geom_vline(xintercept = -25,
             size = 0.5,
             color = france_grey,
             linetype = "dashed")
fra_annotated_line

fra_labelled_line <- fra_annotated_line +
  labs(title = "<b style='color:#2a4d7b'>France</b> in the professional era",
       subtitle = "Rolling average no. of points <b style='color:#2a4d7b'>scored</b> and <b style='color:#bc0a19'>conceded</b> per<br>game by <b style='color:#2a4d7b'>France</b> against Tier 1 <b style='color:#bc0a19'>opponents</b> since 1996",
       caption = "<i>Averages calculated on a 30-game rolling basis</i><br><br>Data: Wikipedia | Chart: Plot the Ball")
fra_labelled_line
