library(tidyverse)
library(janitor)
library(baseballr)
library(RcppRoll)
library(scales)
library(ggtext)

ohtani_fg_id <- 19755

batter_2018 <- fg_batter_game_logs(ohtani_fg_id, year = 2018) %>%
  arrange(Date)
batter_2019 <- fg_batter_game_logs(ohtani_fg_id, year = 2019) %>%
  arrange(Date)
batter_2020 <- fg_batter_game_logs(ohtani_fg_id, year = 2020) %>%
  arrange(Date)
batter_2021 <- fg_batter_game_logs(ohtani_fg_id, year = 2021) %>%
  arrange(Date)
batter_2022 <- fg_batter_game_logs(ohtani_fg_id, year = 2022) %>%
  arrange(Date)
batter_2023 <- fg_batter_game_logs(ohtani_fg_id, year = 2023) %>%
  arrange(Date)

all_games_batter <- bind_rows(batter_2018, batter_2019, batter_2020, batter_2021, batter_2022, batter_2023) %>%
  clean_names() %>%
  select(1:28)

batter_clean <- all_games_batter %>%
  mutate(obp_numerator = h + bb + hbp,
         obp_denominator = ab + bb + hbp + sf,
         slg_numerator = (x1b * 1) + (x2b * 2) + (x3b * 3) + (hr * 4),
         slg_denominator = ab) %>%
  select(1:6, 29:32) %>%
  filter(obp_denominator != 0)

batter_clean <- batter_clean %>%
  mutate(game_index = row_number(),
         .before = date)

roll_no_batter <- 81

batter_clean$roll_obp_num <- roll_sum(batter_clean$obp_numerator, n = roll_no_batter, align = "right", fill = NA)
batter_clean$roll_obp_den <- roll_sum(batter_clean$obp_denominator, n = roll_no_batter, align = "right", fill = NA)
batter_clean$roll_slg_num <- roll_sum(batter_clean$slg_numerator, n = roll_no_batter, align = "right", fill = NA)
batter_clean$roll_slg_den <- roll_sum(batter_clean$slg_denominator, n = roll_no_batter, align = "right", fill = NA)
batter_clean$roll_obp <- batter_clean$roll_obp_num / batter_clean$roll_obp_den
batter_clean$roll_slg <- batter_clean$roll_slg_num / batter_clean$roll_slg_den
batter_clean$roll_ops <- batter_clean$roll_obp + batter_clean$roll_slg
batter_clean$date <- ymd(batter_clean$date)

batter_final <- batter_clean %>%
  filter(season > 2020)

# Average for 2021-23 as at Sun 25 Jun: https://www.baseball-reference.com/leagues/majors/bat.shtml
mlb_waverage_ops <- sum((2306 * 0.729), (4860 * 0.706), (4858 * 0.728)) / sum(2306, 4860, 4858)

line_colour <- "#e0e4ef"
angels_red <- "#BA0021"
mlb_blue <- "#09479E"

roll_ops_line <- ggplot(batter_final, aes(x = game_index, y = roll_ops)) +
  geom_path(color = angels_red,
            size = 1.25) +
  scale_y_continuous(limits = c(0.1, 1.25),
                     breaks = seq(0.2,1.2,0.2),
                     labels = number_format(accuracy = 0.001),
                     expand = c(0,0)) +
  scale_x_continuous(limits = c(200, 700),
                     expand = c(0,0)) +
  theme(axis.title.x = element_blank(),
        axis.ticks.x = element_blank(),
        panel.grid.major.x = element_blank(),
        axis.text.x = element_blank(),
        axis.title.y = element_blank(),
        axis.line.x = element_blank()) +
  geom_hline(yintercept = mlb_waverage_ops,
             size = 1,
             color = mlb_blue,
             linetype = "dotted") +
  geom_vline(xintercept = 253,
             size = 0.5,
             color = line_colour,
             linetype = "dashed") +
  geom_vline(xintercept = 408,
             size = 0.5,
             color = line_colour,
             linetype = "dashed") +
  geom_vline(xintercept = 565,
             size = 0.5,
             color = line_colour,
             linetype = "dashed")
roll_ops_line

themed_ops_line <- roll_ops_line +
  theme_ptb()
themed_ops_line

labelled_ops_line <- themed_ops_line +
  labs(title = "<b style='color:#BA0021'>Ohtani</b> is on another hot streak at the plate",
       subtitle = "Rolling <b>On-Base Plus Slugging % (OPS)</b> recorded by Shohei<br>Ohtani since the start of the 2021 MLB regular season",
       caption = "<i>OPS calculated on a 81-game rolling basis</i><br><br>Data: FanGraphs | Chart: Plot the Ball")
labelled_ops_line

pitcher_2018 <- fg_pitcher_game_logs(ohtani_fg_id, year = 2018) %>%
  arrange(Date)
pitcher_2020 <- fg_pitcher_game_logs(ohtani_fg_id, year = 2020) %>%
  arrange(Date)
pitcher_2021 <- fg_pitcher_game_logs(ohtani_fg_id, year = 2021) %>%
  arrange(Date)
pitcher_2022 <- fg_pitcher_game_logs(ohtani_fg_id, year = 2022) %>%
  arrange(Date)
pitcher_2023 <- fg_pitcher_game_logs(ohtani_fg_id, year = 2023) %>%
  arrange(Date)

all_games_pitcher <- bind_rows(pitcher_2018, pitcher_2020, pitcher_2021, pitcher_2022, pitcher_2023) %>%
  clean_names() %>%
  select(1:31)

pitcher_clean <- all_games_pitcher %>%
  select(1:4, 6:7, 20, 24) %>%
  mutate(ip_complete = floor(ip),
         ip_incomplete = (ip - ip_complete) / 0.3,
         ip_amended = ip_complete + ip_incomplete,
         .after = ip) %>%
  mutate(game_index = row_number(),
         .before = date)

roll_no_pitcher <- 13

pitcher_clean$roll_ip <- roll_sum(pitcher_clean$ip_amended, n = roll_no_pitcher, align = "right", fill = NA)
pitcher_clean$roll_er <- roll_sum(pitcher_clean$er, n = roll_no_pitcher, align = "right", fill = NA)
pitcher_clean$roll_era <- pitcher_clean$roll_er / pitcher_clean$roll_ip * 9
pitcher_clean$date <- ymd(pitcher_clean$date)

pitcher_final <- pitcher_clean %>%
  filter(season > 2020)

# Average for 2021-23 as at Sun 25 Jun: https://www.baseball-reference.com/leagues/majors/pitch.shtml
mlb_waverage_era <- sum((2306 * 4.27), (4860 * 3.96), (4858 * 4.26)) / sum(2306, 4860, 4858)

roll_era_line <- ggplot(pitcher_final, aes(x = game_index, y = roll_era)) +
  geom_path(color = angels_red,
            size = 1.5) +
  scale_y_continuous(limits = c(0.5, 6.4),
                     breaks = seq(1,6,1),
                     labels = number_format(accuracy = 0.01),
                     expand = c(0,0)) +
  scale_x_continuous(limits = c(0, 90),
                     expand = c(0,0)) +
  theme(axis.title.x = element_blank(),
        axis.ticks.x = element_blank(),
        panel.grid.major.x = element_blank(),
        axis.text.x = element_blank(),
        axis.title.y = element_blank(),
        axis.line.x = element_blank()) +
  geom_hline(yintercept = mlb_waverage_era,
             size = 1,
             color = mlb_blue,
             linetype = "dotted") +
  geom_vline(xintercept = 13,
             size = 0.5,
             color = line_colour,
             linetype = "dashed") +
  geom_vline(xintercept = 35,
             size = 0.5,
             color = line_colour,
             linetype = "dashed") +
  geom_vline(xintercept = 63,
             size = 0.5,
             color = line_colour,
             linetype = "dashed")
roll_era_line

themed_era_line <- roll_era_line +
  theme_ptb()
themed_era_line

labelled_era_line <- themed_era_line +
  labs(title = "<b style='color:#BA0021'>Ohtani</b>'s pitching has regressed this year",
       subtitle = "Rolling <b>Earned Run Average (ERA)</b> recorded by Shohei<br>Ohtani since the start of the 2021 MLB regular season",
       caption = "<i>ERA calculated on a 13-start rolling basis</i><br><br>Data: FanGraphs | Chart: Plot the Ball")
labelled_era_line
