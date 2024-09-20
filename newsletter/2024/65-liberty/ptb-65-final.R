library(tidyverse)
library(rvest)
library(janitor)
library(RcppRoll)
library(ggtext)
library(scales)
library(googlesheets4)

seasons <- seq(1997, 2024, 1)

url_base <- "https://www.basketball-reference.com/wnba/years/"
url_end <- "_games.html"

regular_season_all <- tibble()
playoffs_all <- tibble()

for (i in seasons) {
  
  Sys.sleep(1)
  
  url_complete <- str_c(url_base, i, url_end)
  
  html <- read_html(url_complete)
  
  tables <- html %>%
    html_nodes("table") %>%
    html_table()
  
  key_table <- tables[[1]] %>%
    clean_names() %>%
    mutate(pts = as.integer(pts),
           pts_2 = as.integer(pts_2))
  
  if(i == 2024) {
    
    regular_season_all <- bind_rows(regular_season_all, key_table)
    next
  }
  
  playoff_ref <- grep("Playoffs", key_table$date)
  
  regular_season <- key_table[1:(playoff_ref - 1), 1:6]
  playoffs <- key_table[(playoff_ref + 1):nrow(key_table), 1:6]
  
  regular_season_all <- bind_rows(regular_season_all, regular_season)
  playoffs_all <- bind_rows(playoffs_all, playoffs)

}

regular_season_final <- regular_season_all %>%
  select(-x, -notes) %>%
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
  filter(season >= 2008) %>%
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
  filter(game_index > -340)

liberty_colour <- "#74B3A1"
aces_colour <- "#BA1334"
other_colour <- "#D9D1CE"

rough_line <- ggplot(margin_final, aes(x = game_index)) +
  geom_hline(yintercept = 0,
             colour = ptb_dark_grey) +
  geom_vline(xintercept = -39,
             linewidth = 0.7,
             color = "#e0e4ef",
             linetype = "dashed") +
  geom_vline(xintercept = -79,
             linewidth = 0.7,
             color = "#e0e4ef",
             linetype = "dashed") +
  geom_vline(xintercept = -115,
             linewidth = 0.7,
             color = "#e0e4ef",
             linetype = "dashed") +
  geom_vline(xintercept = -147,
             linewidth = 0.7,
             color = "#e0e4ef",
             linetype = "dashed") +
  geom_vline(xintercept = -169,
             linewidth = 0.7,
             color = "#e0e4ef",
             linetype = "dashed") +
  geom_vline(xintercept = -203,
             linewidth = 0.7,
             color = "#e0e4ef",
             linetype = "dashed") +
  geom_vline(xintercept = -239,
             linewidth = 0.7,
             color = "#e0e4ef",
             linetype = "dashed") +
  geom_vline(xintercept = -271,
             linewidth = 0.7,
             color = "#e0e4ef",
             linetype = "dashed") +
  geom_vline(xintercept = -305,
             linewidth = 0.7,
             color = "#e0e4ef",
             linetype = "dashed") +
  geom_vline(xintercept = -339,
             linewidth = 0.7,
             color = "#e0e4ef",
             linetype = "dashed") +
  scale_y_continuous(limits = c(-19, 19),
                     breaks = seq(-15, 15, 5),
                     labels = label_number(style_positive = "plus"),
                     expand = c(0, 0)) +
  scale_x_continuous(limits = c(-340, 0),
                     expand = c(0.2, 0.2)) +
  geom_line(aes(y = margin_final$sparks_rolling), colour = other_colour, linewidth = 1, alpha = 0.8) +
  geom_line(aes(y = margin_final$mercury_rolling), colour = other_colour, linewidth = 1, alpha = 0.8) +
  geom_line(aes(y = margin_final$mystics_rolling), colour = other_colour, linewidth = 1, alpha = 0.8) +
  geom_line(aes(y = margin_final$wings_rolling), colour = other_colour, linewidth = 1, alpha = 0.8) +
  geom_line(aes(y = margin_final$sun_rolling), colour = other_colour, linewidth = 1, alpha = 0.8) +
  geom_line(aes(y = margin_final$lynx_rolling), colour = other_colour, linewidth = 1, alpha = 0.8) +
  geom_line(aes(y = margin_final$storm_rolling), colour = other_colour, linewidth = 1, alpha = 0.8) +
  geom_line(aes(y = margin_final$fever_rolling), colour = other_colour, linewidth = 1, alpha = 0.8) +
  geom_line(aes(y = margin_final$sky_rolling), colour = other_colour, linewidth = 1, alpha = 0.8) +
  geom_line(aes(y = margin_final$dream_rolling), colour = other_colour, linewidth = 1, alpha = 0.8) +
  geom_line(aes(y = margin_final$aces_rolling), colour = aces_colour, linewidth = 1.5, alpha = 0.95) +
  geom_line(aes(y = margin_final$liberty_rolling), colour = liberty_colour, linewidth = 1.5, alpha = 0.95)
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
  labs(title = "The <b style='color:#74B3A1'>New York Liberty</b> are clearly the<br>best team in the league",
       subtitle = "Rolling average per-game <b>points margin</b> of all<br>current WNBA teams since the start of 2015",
       caption = "<i>Calculated on a 40-game rolling basis; data as at 19 Sep 2024</i><br><br>Data: Basketball Reference | Chart: Plot the Ball")
labelled_line

### game logs imported from Basketball Reference via Google Sheets:
stewart_2016 <- read_sheet(ss = "https://docs.google.com/spreadsheets/d/1rLw8Lb0l_nxYAxF9FY10Osvlg6vyUKs_DkVdM693f3Q/edit?usp=sharing",
                          sheet = "stewart_2016") %>%
  clean_names() %>%
  select(date, tm, mp, x3p, x3pa)
stewart_2017 <- read_sheet(ss = "https://docs.google.com/spreadsheets/d/1rLw8Lb0l_nxYAxF9FY10Osvlg6vyUKs_DkVdM693f3Q/edit?usp=sharing",
                           sheet = "stewart_2017") %>%
  clean_names() %>%
  select(date, tm, mp, x3p, x3pa)
stewart_2018 <- read_sheet(ss = "https://docs.google.com/spreadsheets/d/1rLw8Lb0l_nxYAxF9FY10Osvlg6vyUKs_DkVdM693f3Q/edit?usp=sharing",
                           sheet = "stewart_2018") %>%
  clean_names() %>%
  select(date, tm, mp, x3p, x3pa)
stewart_2020 <- read_sheet(ss = "https://docs.google.com/spreadsheets/d/1rLw8Lb0l_nxYAxF9FY10Osvlg6vyUKs_DkVdM693f3Q/edit?usp=sharing",
                           sheet = "stewart_2020") %>%
  clean_names() %>%
  select(date, tm, mp, x3p, x3pa)
stewart_2021 <- read_sheet(ss = "https://docs.google.com/spreadsheets/d/1rLw8Lb0l_nxYAxF9FY10Osvlg6vyUKs_DkVdM693f3Q/edit?usp=sharing",
                           sheet = "stewart_2021") %>%
  clean_names() %>%
  select(date, tm, mp, x3p, x3pa)
stewart_2022 <- read_sheet(ss = "https://docs.google.com/spreadsheets/d/1rLw8Lb0l_nxYAxF9FY10Osvlg6vyUKs_DkVdM693f3Q/edit?usp=sharing",
                           sheet = "stewart_2022") %>%
  clean_names() %>%
  select(date, tm, mp, x3p, x3pa)
stewart_2023 <- read_sheet(ss = "https://docs.google.com/spreadsheets/d/1rLw8Lb0l_nxYAxF9FY10Osvlg6vyUKs_DkVdM693f3Q/edit?usp=sharing",
                           sheet = "stewart_2023") %>%
  clean_names() %>%
  select(date, tm, mp, x3p, x3pa)
stewart_2024 <- read_sheet(ss = "https://docs.google.com/spreadsheets/d/1rLw8Lb0l_nxYAxF9FY10Osvlg6vyUKs_DkVdM693f3Q/edit?usp=sharing",
                           sheet = "stewart_2024") %>%
  clean_names() %>%
  select(date, tm, mp, x3p, x3pa)
stewart_playoffs <- read_sheet(ss = "https://docs.google.com/spreadsheets/d/1rLw8Lb0l_nxYAxF9FY10Osvlg6vyUKs_DkVdM693f3Q/edit?usp=sharing",
                           sheet = "stewart_playoffs") %>%
  clean_names() %>%
  select(x2016_playoffs, tm, mp, x3p, x3pa) %>%
  rename("date" = "x2016_playoffs")

all_games <- bind_rows(stewart_2016, stewart_2017, stewart_2018, stewart_2020, stewart_2021, stewart_2022, stewart_2023, stewart_2024, stewart_playoffs) %>%
  arrange(date) %>%
  separate(col = mp, into = c("mp_mins", "mp_secs"), sep = ":") %>%
  mutate(game_index = row_number(),
         .before = date)

all_games$mp_mins <- as.integer(all_games$mp_mins)
all_games$mp_secs <- as.integer(all_games$mp_secs)

all_games <- all_games %>%
  mutate(roll_3p_tally = roll_sum(x3p, n = roll_no, fill = NA, align = "right"),
         roll_3pa_tally = roll_sum(x3pa, n = roll_no, fill = NA, align = "right"),
         roll_3p_rate = roll_3p_tally / roll_3pa_tally * 100)

final_3p <- all_games %>%
  select(game_index, tm, roll_3p_rate) %>%
  mutate(storm_3p_rate = if_else(game_index <= 206, roll_3p_rate, NA),
         liberty_3p_rate = if_else(game_index >= 205, roll_3p_rate, NA))

storm_colour <- "#26402C"
liberty_colour_alt <- "#7CBFAC"
league_colour <- "#fa4d00"

rough_line_3p <- ggplot(final_3p, aes(x = game_index)) +
  geom_hline(yintercept = 34.22,
             colour = league_colour,
             alpha = 0.6,
             linewidth = 1,
             linetype = "dotted") +
  geom_vline(xintercept = 205,
             linewidth = 0.7,
             color = "#e0e4ef",
             linetype = "dashed") +
  scale_y_continuous(limits = c(16, 54),
                     breaks = seq(20, 50, 10),
                     labels = percent_format(scale = 1),
                     expand = c(0, 0)) +
  scale_x_continuous(limits = c(0, 342),
                     expand = c(0, 0)) +
  geom_line(aes(y = final_3p$storm_3p_rate), colour = storm_colour, linewidth = 1.5, alpha = 1) +
  geom_line(aes(y = final_3p$liberty_3p_rate), colour = liberty_colour_alt, linewidth = 1.5, alpha = 1)
rough_line_3p

themed_line_3p <- rough_line_3p +
  theme_ptb() +
  theme(axis.line = element_blank(),
        axis.ticks = element_blank(),
        axis.text.x = element_blank(),
        panel.grid.major.x = element_blank(),
        axis.title.x = element_blank())
themed_line_3p

labelled_line_3p <- themed_line_3p +
  labs(title = "Breanna Stewart has improved her<br>shooting from range in recent weeks",
       subtitle = "Rolling average <b>three-point FG %</b> recorded by<br>Breanna Stewart in all WNBA games since 2016",
       caption = "<i>Calculated on a 40-game rolling basis; data as at 19 Sep 2024</i><br><br>Data: Basketball Reference | Chart: Plot the Ball")
labelled_line_3p
