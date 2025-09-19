library(tidyverse)
library(rvest)
library(janitor)
library(googlesheets4)
library(RcppRoll)
library(lemon)
library(ggtext)
library(scales)

### data imported from https://www.tennisabstract.com/cgi-bin/player-classic.cgi?p=CarlosAlcaraz&f=ACareerqq
alcaraz_data <- read_sheet(ss = "https://docs.google.com/spreadsheets/d/1AqNUG4xvQ_SY_XJeLOAC0KbQgI2Uqw1wCb1pPsIXrBk/",
                           sheet = "Alcaraz") %>%
  clean_names()

alcaraz_clean <- alcaraz_data %>%
  select(date, tournament, round, match, x1st_in_percent, x1st_percent, x2nd_percent) %>%
  mutate(date = dmy(date)) %>%
  arrange(date, round) %>%
  mutate(match_index = 0 - n() + row_number(),
         .before = "date")

### data imported from https://www.tennisabstract.com/cgi-bin/player-classic.cgi?p=JannikSinner&f=ACareerqq
sinner_data <- read_sheet(ss = "https://docs.google.com/spreadsheets/d/1AqNUG4xvQ_SY_XJeLOAC0KbQgI2Uqw1wCb1pPsIXrBk/",
                           sheet = "Sinner") %>%
  clean_names()

sinner_clean <- sinner_data %>%
  select(date, tournament, round, match, x1st_in_percent, x1st_percent, x2nd_percent) %>%
  mutate(date = dmy(date)) %>%
  arrange(date, round) %>%
  mutate(match_index = 0 - n() + row_number(),
         .before = "date")

roll_no <- 50

alcaraz_rolling <- alcaraz_clean %>%
  mutate(roll_in_sum = roll_sum(x1st_in_percent, n = roll_no, align = "right", fill = NA),
         a_roll_in_ave = roll_in_sum / roll_no,
         roll_1st_sum = roll_sum(x1st_percent, n = roll_no, align = "right", fill = NA),
         b_roll_1st_ave = roll_1st_sum / roll_no,
         roll_2nd_sum = roll_sum(x2nd_percent, n = roll_no, align = "right", fill = NA),
         c_roll_2nd_ave = roll_2nd_sum / roll_no) %>%
  select(match_index, a_roll_in_ave, b_roll_1st_ave, c_roll_2nd_ave) %>%
  mutate(player = "Alcaraz",
         .after = match_index) %>%
  filter(!is.na(b_roll_1st_ave))

sinner_rolling <- sinner_clean %>%
  mutate(roll_in_sum = roll_sum(x1st_in_percent, n = roll_no, align = "right", fill = NA),
         a_roll_in_ave = roll_in_sum / roll_no,
         roll_1st_sum = roll_sum(x1st_percent, n = roll_no, align = "right", fill = NA),
         b_roll_1st_ave = roll_1st_sum / roll_no,
         roll_2nd_sum = roll_sum(x2nd_percent, n = roll_no, align = "right", fill = NA),
         c_roll_2nd_ave = roll_2nd_sum / roll_no) %>%
  select(match_index, a_roll_in_ave, b_roll_1st_ave, c_roll_2nd_ave) %>%
  mutate(player = "Sinner",
         .after = match_index) %>%
  filter(!is.na(b_roll_1st_ave))

combined_rolling <- bind_rows(alcaraz_rolling, sinner_rolling) %>%
  pivot_longer(cols = c("a_roll_in_ave", "b_roll_1st_ave", "c_roll_2nd_ave"),
               names_to = "metric",
               values_to = "value") %>%
  clean_names() %>%
  filter(match_index > -300 & metric != "c_roll_2nd_ave")

facet_labels <- c("a_roll_in_ave" = "% of first serves in",
                  "b_roll_1st_ave" = "% of first-serve points won")

sinner_colour <- "#A3CCC3"
alcaraz_colour <- "#CC4B14"

rough_line <- ggplot(combined_rolling, aes(x = match_index, y = value, colour = player)) +
  geom_line(linewidth = 1.5) +
  geom_vline(xintercept = seq(-299, 0, 50),
             linewidth = 0.75,
             color = ptb_light_grey,
             linetype = "dashed") +
  facet_rep_wrap(~metric, ncol = 1, labeller = labeller(metric = facet_labels)) + 
  scale_colour_manual(values = c("Alcaraz" = alcaraz_colour,
                                 "Sinner" = sinner_colour)) +
  scale_y_continuous(limits = c(0.51, 0.89),
                     breaks = seq(0.6, 0.8, 0.1),
                     labels = percent_format()) +
  scale_x_continuous(limits = c(-310, 30)) +
  theme(legend.position = "none",
        axis.line.x = element_blank(),
        axis.line.y = element_blank(),
        axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        axis.ticks = element_blank(),
        panel.grid.major.x = element_blank())
rough_line

themed_line <- rough_line +
  theme_ptb()
themed_line

labelled_line <- themed_line +
  labs(title = "<b style='color:#CC4B14'>Alcaraz</b>'s first serve is less potent than<br><b style='color:#8FB3AB'>Sinner</b>'s — but more accurate",
       subtitle = "Serving performance of <b style='color:#8FB3AB'>Jannik Sinner</b> and <b style='color:#CC4B14'>Carlos<br>Alcaraz</b> over their last 300 singles matches",
       caption = "<i>Averages calculated on a 50-match rolling basis</i><br><br>Data: Tennis Abstract | Chart: Plot the Ball")
labelled_line