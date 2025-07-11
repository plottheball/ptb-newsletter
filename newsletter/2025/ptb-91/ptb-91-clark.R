library(tidyverse)
library(rvest)
library(janitor)
library(RcppRoll)
library(ggtext)
library(scales)
library(googlesheets4)

### game logs imported from Basketball Reference via Google Sheets:
college_data <- read_sheet(ss = "https://docs.google.com/spreadsheets/d/1x26TFG9gmi1HsDGvyNTdlZfiXME6uVwEuIJ5ND-hXRI/edit?gid=0#gid=0",
                           sheet = "College") %>%
  clean_names() %>%
  select(date, team, opp, mp, ast, tov)
wnba_data <- read_sheet(ss = "https://docs.google.com/spreadsheets/d/1x26TFG9gmi1HsDGvyNTdlZfiXME6uVwEuIJ5ND-hXRI/edit?gid=0#gid=0",
                           sheet = "WNBA") %>%
  clean_names() %>%
  select(date, tm, opp, mp, ast, tov) %>%
  rename("team" = "tm")

all_data <- bind_rows(college_data, wnba_data)

clean_data <- all_data %>%
  arrange(date) %>%
  mutate(game_index = row_number(),
         .before = date)

roll_no <- 20

roll_data <- clean_data %>%
  mutate(roll_mp_tally = roll_sum(mp, n = roll_no, fill = NA, align = "right"),
         roll_ast_tally = roll_sum(ast, n = roll_no, fill = NA, align = "right"),
         roll_tov_tally = roll_sum(tov, n = roll_no, fill = NA, align = "right"))

final_ast_tov <- roll_data %>%
  mutate(ast_36 = roll_ast_tally / roll_mp_tally * 36,
         tov_36 = roll_tov_tally / roll_mp_tally * 36) %>%
  select(game_index, team, ast_36, tov_36) %>%
  filter(!is.na(ast_36)) %>%
  mutate(col_ast = if_else(game_index <= 140, ast_36, NA),
         wnba_ast = if_else(game_index >= 139, ast_36, NA),
         col_tov = if_else(game_index <= 140, tov_36, NA),
         wnba_tov = if_else(game_index >= 139, tov_36, NA))

iowa_colour <- "#D9CC98"
iowa_colour_alt <- "#B3AAAA"
focus_colour_one_light <- "#F5F5F5"
fever_colour <- "#E6B800"
fever_colour_alt <- "#CC6677"
focus_colour_two_light <- "#E6E6E6"

rough_line <- ggplot(final_ast_tov, aes(x = game_index)) +
  geom_hline(yintercept = 0,
             colour = ptb_dark_grey) +
  geom_vline(xintercept = 139,
             linewidth = 1,
             color = ptb_light_grey,
             linetype = "dashed") +
  scale_y_continuous(limits = c(0, 14),
                     breaks = seq(0, 12, 3),
                     expand = c(0, 0)) +
  scale_x_continuous(limits = c(-10, 225),
                     expand = c(0, 0)) +
  geom_ribbon(aes(ymin = col_tov, ymax = col_ast), fill = focus_colour_one_light) +
  geom_line(aes(y = final_ast_tov$col_ast), colour = iowa_colour, linewidth = 1.75, alpha = 1) +
  geom_line(aes(y = final_ast_tov$col_tov), colour = iowa_colour_alt, linewidth = 1.75, alpha = 1) +
  geom_ribbon(aes(ymin = wnba_tov, ymax = wnba_ast), fill = focus_colour_two_light) +
  geom_line(aes(y = final_ast_tov$wnba_ast), colour = fever_colour, linewidth = 1.75, alpha = 1) +
  geom_line(aes(y = final_ast_tov$wnba_tov), colour = fever_colour_alt, linewidth = 1.75, alpha = 1) +
  theme(axis.line.x = element_blank(),
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
  labs(title = "Caitlin Clark hasn't yet cut down on<br>her <b style='color:#CC6677'>turnovers</b> at the WNBA level",
       subtitle = "Number of <b style='color:#E6B800'>assists</b> and <b style='color:#CC6677'>turnovers</b> per 36 minutes<br>recorded by <b>Caitlin Clark</b> in all games since 2020",
       caption = "<i>Averages calculated on a 20-game rolling basis</i><br><br>Data: Basketball Reference | Chart: Plot the Ball")
labelled_line
