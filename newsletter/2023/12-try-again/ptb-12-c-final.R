library(tidyverse)
library(googlesheets4)
library(janitor)
library(RcppRoll)
library(ggtext)

# Results for all teams imported via Google Sheets from Wikipedia article series: 'Head-to-head records of Tier 1 rugby union national teams'
# e.g. https://en.wikipedia.org/wiki/History_of_rugby_union_matches_between_Argentina_and_Australia

eng_results_tidy <- eng_results %>%
  select(1:4, 11:12)

roll_no <- 30
eng_roll_results <- eng_results_tidy
eng_roll_results$rolling_points_for <- roll_sum(eng_roll_results$england_points, n = roll_no, align = "right", fill = NA) / roll_no
eng_roll_results$rolling_points_against <- roll_sum(eng_roll_results$opponent_points, n = roll_no, align = "right", fill = NA) / roll_no

eng_roll_results <- eng_roll_results %>%
  filter(!is.na(rolling_points_for))

england_red <- "#e72649"
england_navy <- "#181c1f"
england_grey <- "#e0e4ef"

eng_rough_line <- ggplot(eng_roll_results, aes(x = index)) +
  geom_ribbon(aes(ymin = rolling_points_against, ymax = rolling_points_for), fill = england_grey, alpha = 0.6) +
  geom_path(aes(y = rolling_points_for),
            color = england_red,
            size = 1) +
  geom_path(aes(y = rolling_points_against),
            color = england_navy,
            size = 1) +
  scale_y_continuous(name = NULL,
                     limits = c(1, 49),
                     expand = c(0,0),
                     breaks = seq(10, 40, 10)) +
  scale_x_continuous(limits = c((min(eng_roll_results$index) - 40), 20),
                     expand = c(0,0)) +
  theme(axis.line.x = element_blank(),
        axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        axis.ticks.x = element_blank(),
        panel.grid.major.x = element_blank())
eng_rough_line

eng_themed_line <- eng_rough_line + theme_ptb()
eng_themed_line

eng_annotated_line <- eng_themed_line +
  geom_vline(xintercept = -271,
             size = 0.5,
             color = england_grey,
             linetype = "dashed") +
  geom_vline(xintercept = -235,
             size = 0.5,
             color = england_grey,
             linetype = "dashed") +
  geom_vline(xintercept = -195,
             size = 0.5,
             color = england_grey,
             linetype = "dashed") +
  geom_vline(xintercept = -153,
             size = 0.5,
             color = england_grey,
             linetype = "dashed") +
  geom_vline(xintercept = -112,
             size = 0.5,
             color = england_grey,
             linetype = "dashed") +
  geom_vline(xintercept = -70,
             size = 0.5,
             color = england_grey,
             linetype = "dashed") +
  geom_vline(xintercept = -25,
             size = 0.5,
             color = england_grey,
             linetype = "dashed")
eng_annotated_line


eng_labelled_line <- eng_annotated_line +
  labs(title = "<b style='color:#e72649'>England</b> in the professional era",
       subtitle = "Rolling average no. of points <b style='color:#e72649'>scored</b> and <b style='color:#181c1f'>conceded</b> per<br>game by <b style='color:#e72649'>England</b> against Tier 1 <b style='color:#181c1f'>opponents</b> since 1996",
       caption = "<i>Averages calculated on a 30-game rolling basis</i><br><br>Data: Wikipedia | Chart: Plot the Ball")
eng_labelled_line
