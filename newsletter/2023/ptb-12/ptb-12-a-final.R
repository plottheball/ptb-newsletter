library(tidyverse)
library(googlesheets4)
library(janitor)
library(RcppRoll)
library(ggtext)

# Results for all teams imported via Google Sheets from Wikipedia article series: 'Head-to-head records of Tier 1 rugby union national teams'
# e.g. https://en.wikipedia.org/wiki/History_of_rugby_union_matches_between_Argentina_and_Australia

ire_results_tidy <- ire_results %>%
  select(1:4, 11:12)

roll_no <- 30
ire_roll_results <- ire_results_tidy
ire_roll_results$rolling_points_for <- roll_sum(ire_roll_results$ireland_points, n = roll_no, align = "right", fill = NA) / roll_no
ire_roll_results$rolling_points_against <- roll_sum(ire_roll_results$opponent_points, n = roll_no, align = "right", fill = NA) / roll_no

ire_roll_results <- ire_roll_results %>%
  filter(!is.na(rolling_points_for))

ireland_green <- "#197149"
ireland_navy <- "#1f253f"
ireland_grey <- "#e0e4ef"

ire_rough_line <- ggplot(ire_roll_results, aes(x = index)) +
  geom_ribbon(aes(ymin = rolling_points_against, ymax = rolling_points_for), fill = ireland_grey, alpha = 0.6) +
  geom_path(aes(y = rolling_points_for),
            color = ireland_green,
            size = 1) +
  geom_path(aes(y = rolling_points_against),
            color = ireland_navy,
            size = 1) +
  scale_y_continuous(name = NULL,
                     limits = c(1, 49),
                     expand = c(0,0),
                     breaks = seq(10, 40, 10)) +
  scale_x_continuous(limits = c((min(ire_roll_results$index) - 40), 20),
                     expand = c(0,0)) +
  theme(axis.line.x = element_blank(),
        axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        axis.ticks.x = element_blank(),
        panel.grid.major.x = element_blank())
ire_rough_line

ire_themed_line <- ire_rough_line + theme_ptb()
ire_themed_line

ire_annotated_line <- ire_themed_line +
  geom_vline(xintercept = -240,
             size = 0.5,
             color = ireland_grey,
             linetype = "dashed") +
  geom_vline(xintercept = -211,
             size = 0.5,
             color = ireland_grey,
             linetype = "dashed") +
  geom_vline(xintercept = -177,
             size = 0.5,
             color = ireland_grey,
             linetype = "dashed") +
  geom_vline(xintercept = -140,
             size = 0.5,
             color = ireland_grey,
             linetype = "dashed") +
  geom_vline(xintercept = -102,
             size = 0.5,
             color = ireland_grey,
             linetype = "dashed") +
  geom_vline(xintercept = -64,
             size = 0.5,
             color = ireland_grey,
             linetype = "dashed") +
  geom_vline(xintercept = -24,
             size = 0.5,
             color = ireland_grey,
             linetype = "dashed")
ire_annotated_line

ire_labelled_line <- ire_annotated_line +
  labs(title = "<b style='color:#197149'>Ireland</b> in the professional era",
       subtitle = "Rolling average no. of points <b style='color:#197149'>scored</b> and <b style='color:#1f253f'>conceded</b> per<br>game by <b style='color:#197149'>Ireland</b> against Tier 1 <b style='color:#1f253f'>opponents</b> since 1996",
       caption = "<i>Averages calculated on a 30-game rolling basis</i><br><br>Data: Wikipedia | Chart: Plot the Ball")
ire_labelled_line
