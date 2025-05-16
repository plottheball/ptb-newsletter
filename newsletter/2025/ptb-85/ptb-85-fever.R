library(tidyverse)
library(rvest)
library(janitor)
library(RcppRoll)
library(ggtext)
library(scales)

seasons <- seq(2019, 2024, 1)

url_base <- "https://www.basketball-reference.com/wnba/years/"
url_end <- "_games.html"

regular_season_all <- tibble()

for (i in seasons) {
  
  Sys.sleep(3)
  
  url_complete <- str_c(url_base, i, url_end)
  
  html <- read_html(url_complete)
  
  tables <- html %>%
    html_nodes("table") %>%
    html_table()
  
  key_table <- tables[[1]] %>%
    clean_names() %>%
    mutate(pts = as.integer(pts),
           pts_2 = as.integer(pts_2))
  
  playoff_ref <- grep("Playoffs", key_table$date)
  
  regular_season <- key_table[1:(playoff_ref - 1), 1:6]
  playoffs <- key_table[(playoff_ref + 1):nrow(key_table), 1:6]
  
  regular_season_all <- bind_rows(regular_season_all, regular_season)

}

regular_season_final <- regular_season_all %>%
  select(-x) %>%
  mutate(date = mdy(date)) %>%
  filter(!is.na(pts) & !is.na(pts_2))

all_teams <- unique(c(unlist(regular_season_final$visitor_neutral), unlist(regular_season_final$home_neutral)))

folded_teams <- c("Charlotte Sting", "Cleveland Rockers", "Houston Comets", "Miami Sol", "Portland Fire", "Sacramento Monarchs")

regular_season_final$visitor_neutral <- str_replace_all(regular_season_final$visitor_neutral, "Detroit Shock", "Dallas Wings")
regular_season_final$home_neutral <- str_replace_all(regular_season_final$home_neutral, "Detroit Shock", "Dallas Wings")
regular_season_final$visitor_neutral <- str_replace_all(regular_season_final$visitor_neutral, "Tulsa Shock", "Dallas Wings")
regular_season_final$home_neutral <- str_replace_all(regular_season_final$home_neutral, "Tulsa Shock", "Dallas Wings")

regular_season_final$visitor_neutral <- str_replace_all(regular_season_final$visitor_neutral, "Orlando Miracle", "Connecticut Sun")
regular_season_final$home_neutral <- str_replace_all(regular_season_final$home_neutral, "Orlando Miracle", "Connecticut Sun")

regular_season_final$visitor_neutral <- str_replace_all(regular_season_final$visitor_neutral, "Utah Starzz", "Las Vegas Aces")
regular_season_final$home_neutral <- str_replace_all(regular_season_final$home_neutral, "Utah Starzz", "Las Vegas Aces")
regular_season_final$visitor_neutral <- str_replace_all(regular_season_final$visitor_neutral, "San Antonio Stars", "Las Vegas Aces")
regular_season_final$home_neutral <- str_replace_all(regular_season_final$home_neutral, "San Antonio Stars", "Las Vegas Aces")
regular_season_final$visitor_neutral <- str_replace_all(regular_season_final$visitor_neutral, "San Antonio Silver Stars", "Las Vegas Aces")
regular_season_final$home_neutral <- str_replace_all(regular_season_final$home_neutral, "San Antonio Silver Stars", "Las Vegas Aces")

final_teams <- unique(c(unlist(regular_season_final$visitor_neutral), unlist(regular_season_final$home_neutral)))
active_teams <- final_teams[!final_teams %in% folded_teams]

final_data <- tibble()

for (i in active_teams) {
  
  road_games <- regular_season_final %>%
    filter(visitor_neutral == i) %>%
    mutate(location = "Road/Neutral",
           team_score = pts,
           opp_score = pts_2,
           margin = team_score - opp_score)
  
  home_games <- regular_season_final %>%
    filter(home_neutral == i) %>%
    mutate(location = "Home/Neutral",
           team_score = pts_2,
           opp_score = pts,
           margin = team_score - opp_score)
  
  all_games <- bind_rows(road_games, home_games) %>%
    arrange(date) %>%
    mutate(game_index = 0 - nrow(road_games) - nrow(home_games) + row_number(),
           season = year(date),
           team = i,
           .after = date) %>%
    select(-5, -6, -7, -8)
  
  final_data <- bind_rows(final_data, all_games)
  
}

current_era <- final_data %>%
  filter(season >= 2019) %>%
  select(2, 4, 8)

rolling_average_margin <- current_era %>%
  pivot_wider(names_from = team,
              values_from = margin) %>%
  clean_names() %>%
  filter(game_index != min(game_index))

roll_no <- 40

margin_final <- rolling_average_margin %>%
  mutate(liberty_rolling = (roll_sum(x = new_york_liberty, n = roll_no, align = "right", fill = NA) / roll_no),
         sparks_rolling = (roll_sum(x = los_angeles_sparks, n = roll_no, align = "right", fill = NA) / roll_no),
         mercury_rolling = (roll_sum(x = phoenix_mercury, n = roll_no, align = "right", fill = NA) / roll_no),
         aces_rolling = (roll_sum(x = las_vegas_aces, n = roll_no, align = "right", fill = NA) / roll_no),
         mystics_rolling = (roll_sum(x = washington_mystics, n = roll_no, align = "right", fill = NA) / roll_no),
         wings_rolling = (roll_sum(x = dallas_wings, n = roll_no, align = "right", fill = NA) / roll_no),
         sun_rolling = (roll_sum(x = connecticut_sun, n = roll_no, align = "right", fill = NA) / roll_no),
         lynx_rolling = (roll_sum(x = minnesota_lynx, n = roll_no, align = "right", fill = NA) / roll_no),
         storm_rolling = (roll_sum(x = seattle_storm, n = roll_no, align = "right", fill = NA) / roll_no),
         fever_rolling = (roll_sum(x = indiana_fever, n = roll_no, align = "right", fill = NA) / roll_no),
         sky_rolling = (roll_sum(x = chicago_sky, n = roll_no, align = "right", fill = NA) / roll_no),
         dream_rolling = (roll_sum(x = atlanta_dream, n = roll_no, align = "right", fill = NA) / roll_no)) %>%
  filter(game_index > -170)

fever_colour <- "#E6B800"
other_colour <- "#D9D1CE"

rough_line <- ggplot(margin_final, aes(x = game_index)) +
  geom_hline(yintercept = 0,
             colour = ptb_dark_grey) +
  geom_vline(xintercept = -79,
             linewidth = 0.7,
             color = other_colour,
             linetype = "dashed") +
  geom_vline(xintercept = -115,
             linewidth = 0.7,
             color = other_colour,
             linetype = "dashed") +
  geom_vline(xintercept = -147,
             linewidth = 0.7,
             color = other_colour,
             linetype = "dashed") +
  scale_y_continuous(limits = c(-19, 19),
                     breaks = seq(-15, 15, 5),
                     labels = label_number(style_positive = "plus"),
                     expand = c(0, 0)) +
  scale_x_continuous(limits = c(-170, 0),
                     expand = c(0.2, 0.2)) +
  geom_line(aes(y = margin_final$sparks_rolling), colour = other_colour, linewidth = 1, alpha = 0.6) +
  geom_line(aes(y = margin_final$mercury_rolling), colour = other_colour, linewidth = 1, alpha = 0.6) +
  geom_line(aes(y = margin_final$mystics_rolling), colour = other_colour, linewidth = 1, alpha = 0.6) +
  geom_line(aes(y = margin_final$wings_rolling), colour = other_colour, linewidth = 1, alpha = 0.6) +
  geom_line(aes(y = margin_final$sun_rolling), colour = other_colour, linewidth = 1, alpha = 0.6) +
  geom_line(aes(y = margin_final$lynx_rolling), colour = other_colour, linewidth = 1, alpha = 0.6) +
  geom_line(aes(y = margin_final$storm_rolling), colour = other_colour, linewidth = 1, alpha = 0.6) +
  geom_line(aes(y = margin_final$sky_rolling), colour = other_colour, linewidth = 1, alpha = 0.6) +
  geom_line(aes(y = margin_final$dream_rolling), colour = other_colour, linewidth = 1, alpha = 0.6) +
  geom_line(aes(y = margin_final$aces_rolling), colour = other_colour, linewidth = 1, alpha = 0.6) +
  geom_line(aes(y = margin_final$liberty_rolling), colour = other_colour, linewidth = 1, alpha = 0.6) +
  geom_vline(xintercept = -39,
             linewidth = 0.7,
             color = "#CC6677",
             linetype = "dashed") +
  geom_line(aes(y = margin_final$fever_rolling), colour = fever_colour, linewidth = 1.75, alpha = 1)
rough_line

themed_line <- rough_line +
  theme_ptb() +
  theme(axis.line = element_blank(),
        axis.ticks = element_blank(),
        axis.text.x = element_blank(),
        panel.grid.major.x = element_blank(),
        axis.title.x = element_blank())
themed_line

labelled_line <- themed_line +
  labs(title = "After <b style='color:#CC6677'>adding Caitlin Clark</b>, the <b style='color:#E6B800'>Fever</b><br> got worse before they got better",
       subtitle = "Rolling average per-game <b>points margin</b> of all<br>current WNBA teams since the start of 2020",
       caption = "<i>Calculated on a 40-game rolling basis</i><br><br>Data: Basketball Reference | Chart: Plot the Ball")
labelled_line
