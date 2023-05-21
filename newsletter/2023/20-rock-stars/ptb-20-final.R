library(tidyverse)
library(googlesheets4)
library(janitor)
library(RcppRoll)
library(ggtext)
library(scales)

# Results imported via Google Sheets from https://www.leinsterrugby.ie/fixtures-and-results/combined-results/
leinster_results <- read_sheet(leinster_sheets) %>%
  clean_names()

results_tidy <- leinster_results %>%
  select(1:4, 9:12)

roll_no <- 30
roll_results <- results_tidy
roll_results$rolling_points_for <- roll_sum(roll_results$leinster_score, n = roll_no, align = "right", fill = NA) / roll_no
roll_results$rolling_points_against <- roll_sum(roll_results$opponent_score, n = roll_no, align = "right", fill = NA) / roll_no

roll_results_final <- roll_results %>%
  filter(season_ending != 2002 & season_ending != 2003 )

leinster_blue <- "#0c4190"
leinster_yellow <- "#eeb111"
leinster_green <- "#6BDBCC"
leinster_white <- "#e0e4ef"

rough_line <- ggplot(roll_results_final, aes(x = overall_index)) +
  geom_ribbon(aes(ymin = rolling_points_against, ymax = rolling_points_for), fill = leinster_white) +
  geom_path(aes(y = rolling_points_for),
            color = leinster_blue,
            size = 1) +
  geom_path(aes(y = rolling_points_against),
            color = leinster_yellow,
            size = 1) +
  scale_y_continuous(name = NULL,
                     limits = c(1, 49),
                     expand = c(0,0),
                     breaks = seq(10, 40, 10)) +
  scale_x_continuous(limits = c(-40, 650)) +
  theme(axis.line.x = element_blank(),
        axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        axis.ticks.x = element_blank(),
        panel.grid.major.x = element_blank())
rough_line

themed_line <- rough_line + theme_ptb()
themed_line

annotated_line <- themed_line +
  geom_vline(xintercept = 34,
             size = 0.5,
             color = leinster_white,
             linetype = "dashed") +
  geom_vline(xintercept = 148,
             size = 0.5,
             color = leinster_white,
             linetype = "dashed") +
  geom_vline(xintercept = 260,
             size = 0.5,
             color = leinster_white,
             linetype = "dashed") +
  geom_vline(xintercept = 387,
             size = 0.5,
             color = leinster_white,
             linetype = "dashed") +
  geom_vline(xintercept = 387,
             size = 0.5,
             color = leinster_white,
             linetype = "dashed") +
  geom_vline(xintercept = 418,
             size = 1,
             color = leinster_green,
             linetype = "dotted") +
  geom_vline(xintercept = 512,
             size = 0.5,
             color = leinster_white,
             linetype = "dashed")
  
annotated_line

labelled_line <- annotated_line +
  labs(title = "<b style='color:#0c4190'>Leinster's attack</b> went to another level after<br> Stuart Lancaster took over as 'senior coach'",
       subtitle = "Rolling average no. of points <b style='color:#0c4190'>scored</b> and <b style='color:#eeb111'>conceded</b> per <br>game by Leinster in all competitions since 2003-04",
       caption = "<i>Averages calculated on a 30-game rolling basis</i><br><br>Data: Leinster Rugby | Chart: Plot the Ball")
labelled_line
