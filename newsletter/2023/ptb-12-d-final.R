library(tidyverse)
library(googlesheets4)
library(janitor)
library(RcppRoll)
library(ggtext)

# Results for all teams imported via Google Sheets from Wikipedia article series: 'Head-to-head records of Tier 1 rugby union national teams'
# e.g. https://en.wikipedia.org/wiki/History_of_rugby_union_matches_between_Argentina_and_Australia

sco_results_tidy <- sco_results %>%
  select(1:4, 11:12)

roll_no <- 30
sco_roll_results <- sco_results_tidy
sco_roll_results$rolling_points_for <- roll_sum(sco_roll_results$sco_points, n = roll_no, align = "right", fill = NA) / roll_no
sco_roll_results$rolling_points_against <- roll_sum(sco_roll_results$opponent_points, n = roll_no, align = "right", fill = NA) / roll_no

sco_roll_results <- sco_roll_results %>%
  filter(!is.na(rolling_points_for))

sco_navy <- "#073c65"
sco_purple <- "#eabafb"
sco_grey <- "#e0e4ef"

sco_rough_line <- ggplot(sco_roll_results, aes(x = index)) +
  geom_ribbon(aes(ymin = rolling_points_against, ymax = rolling_points_for), fill = sco_grey, alpha = 0.6) +
  geom_path(aes(y = rolling_points_for),
            color = sco_navy,
            size = 1) +
  geom_path(aes(y = rolling_points_against),
            color = sco_purple,
            size = 1) +
  scale_y_continuous(name = NULL,
                     limits = c(1, 49),
                     expand = c(0,0),
                     breaks = seq(10, 40, 10)) +
  scale_x_continuous(limits = c((min(sco_roll_results$index)- 40), 20),
                     expand = c(0,0)) +
  theme(axis.line.x = element_blank(),
        axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        axis.ticks.x = element_blank(),
        panel.grid.major.x = element_blank())
sco_rough_line

sco_themed_line <- sco_rough_line + theme_ptb()
sco_themed_line

sco_annotated_line <- sco_themed_line +
  geom_vline(xintercept = -227,
             size = 0.5,
             color = sco_grey,
             linetype = "dashed") +
  geom_vline(xintercept = -197,
             size = 0.5,
             color = sco_grey,
             linetype = "dashed") +
  geom_vline(xintercept = -164,
             size = 0.5,
             color = sco_grey,
             linetype = "dashed") +
  geom_vline(xintercept = -129,
             size = 0.5,
             color = sco_grey,
             linetype = "dashed") +
  geom_vline(xintercept = -95,
             size = 0.5,
             color = sco_grey,
             linetype = "dashed") +
  geom_vline(xintercept = -58,
             size = 0.5,
             color = sco_grey,
             linetype = "dashed") +
  geom_vline(xintercept = -25,
             size = 0.5,
             color = sco_grey,
             linetype = "dashed")
sco_annotated_line

sco_labelled_line <- sco_annotated_line +
  labs(title = "<b style='color:#073c65'>Scotland</b> in the professional era",
       subtitle = "Rolling average no. of points <b style='color:#073c65'>scored</b> and <b style='color:#eabafb'>conceded</b> per<br>game by <b style='color:#073c65'>Scotland</b> against Tier 1 <b style='color:#eabafb'>opponents</b> since 1996",
       caption = "<i>Averages calculated on a 30-game rolling basis</i><br><br>Data: Wikipedia | Chart: Plot the Ball")
sco_labelled_line
