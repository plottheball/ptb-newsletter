library(tidyverse)
library(janitor)
library(worldfootballR)
library(ggtext)

spain_prior <- fb_season_team_stats(country = "ESP", gender = "F", season_end_year = 2023, tier = "1st", stat_type = "league_table") %>%
  clean_names() %>%
  select(competition_name, season_end_year, squad, mp, x_g, x_ga)
spain_current <- fb_season_team_stats(country = "ESP", gender = "F", season_end_year = 2024, tier = "1st", stat_type = "league_table") %>%
  clean_names() %>%
  select(competition_name, season_end_year, squad, mp, x_g, x_ga)

england_prior <- fb_season_team_stats(country = "ENG", gender = "F", season_end_year = 2023, tier = "1st", stat_type = "league_table") %>%
  clean_names() %>%
  select(competition_name, season_end_year, squad, mp, x_g, x_ga)
england_current <- fb_season_team_stats(country = "ENG", gender = "F", season_end_year = 2024, tier = "1st", stat_type = "league_table") %>%
  clean_names() %>%
  select(competition_name, season_end_year, squad, mp, x_g, x_ga)

germany_prior <- fb_season_team_stats(country = "GER", gender = "F", season_end_year = 2023, tier = "1st", stat_type = "league_table") %>%
  clean_names() %>%
  select(competition_name, season_end_year, squad, mp, x_g, x_ga)
germany_current <- fb_season_team_stats(country = "GER", gender = "F", season_end_year = 2024, tier = "1st", stat_type = "league_table") %>%
  clean_names() %>%
  select(competition_name, season_end_year, squad, mp, x_g, x_ga)

france_prior <- fb_season_team_stats(country = "FRA", gender = "F", season_end_year = 2023, tier = "1st", stat_type = "league_table") %>%
  clean_names() %>%
  select(competition_name, season_end_year, squad, mp, x_g, x_ga)
france_current <- fb_season_team_stats(country = "FRA", gender = "F", season_end_year = 2024, tier = "1st", stat_type = "league_table") %>%
  clean_names() %>%
  select(competition_name, season_end_year, squad, mp, x_g, x_ga)

italy_prior <- fb_season_team_stats(country = "ITA", gender = "F", season_end_year = 2023, tier = "1st", stat_type = "league_table") %>%
  clean_names() %>%
  select(competition_name, season_end_year, squad, mp, x_g, x_ga)
italy_current <- fb_season_team_stats(country = "ITA", gender = "F", season_end_year = 2024, tier = "1st", stat_type = "league_table") %>%
  clean_names() %>%
  select(competition_name, season_end_year, squad, mp, x_g, x_ga)

all_league_data <- bind_rows(spain_prior, spain_current,
                             england_prior, england_current,
                             germany_prior, germany_current,
                             france_prior, france_current,
                             italy_prior, italy_current) %>%
  filter(!is.na(mp))

team_summary <- all_league_data %>%
  group_by(squad) %>%
  summarise(no_seasons = n(),
            tot_mp = sum(mp),
            tot_xgf = sum(x_g),
            tot_xga = sum(x_ga)) %>%
  filter(no_seasons == 2) %>%
  mutate(xgf_per_game = tot_xgf / tot_mp,
         xga_per_game = tot_xga / tot_mp,
         xgd_per_game = xgf_per_game - xga_per_game) %>%
  arrange(desc(xgd_per_game))

total_summary <- all_league_data %>%
  summarise(tot_mp = sum(mp),
            tot_xgf = sum(x_g),
            tot_xga = sum(x_ga)) %>%
  mutate(xgf_per_game = tot_xgf / tot_mp,
         xga_per_game = tot_xga / tot_mp,
         xgd_per_game = xgf_per_game - xga_per_game)

barca_colour <- "#cd122d"
background_colour <- "#8DA2B8"

rough_scatter <- ggplot(team_summary, aes(x = xga_per_game, y = xgf_per_game)) +
  geom_hline(yintercept = 0,
             linewidth = 1,
             color = ptb_dark_grey) +
  geom_vline(xintercept = 0,
             linewidth = 1,
             color = ptb_dark_grey) +
  geom_point(color = case_when(team_summary$squad == "Barcelona" ~ barca_colour,
                               TRUE ~ background_colour),
             alpha = case_when(team_summary$squad == "Barcelona" ~ 1,
                               TRUE ~ 0.35),
             size = 5) +
  scale_y_continuous(name = NULL,
                     limits = c(0, 3.59),
                     breaks = seq(0, 3, 1),
                     expand = c(0,0)) +
  scale_x_continuous(name = NULL,
                     limits = c(0, 3.59),
                     breaks = seq(0, 3, 1),
                     expand = c(0,0)) +
  theme(legend.position = "none",
        axis.line.x = element_blank(),
        axis.line.y = element_blank()) +
  annotate(geom = "text",
           x = 0.05,
           y = 3.25,
           label = "xG created per game",
           family = ptb_font,
           colour = ptb_dark_grey,
           fontface = "bold",
           hjust = 0,
           vjust = 1) +
  annotate(geom = "text",
           x = 3.5,
           y = 0.25,
           label = "xG conceded per game",
           family = ptb_font,
           colour = ptb_dark_grey,
           fontface = "bold",
           hjust = 1,
           vjust = 1)
rough_scatter

themed_scatter <- rough_scatter +
  theme_ptb()
themed_scatter

labelled_scatter <- themed_scatter +
  labs(title = "<span style='color:#cd122d;'>Barça Femení</span> are — by far — the most<br>dominant club in European football",
       subtitle = "Average <b>xG created</b> and <b>conceded</b> per game in<br>league matches by clubs in England, France,<br>Germany, Italy and Spain since 2022-23",
       caption = "<i>Data as at 13 Mar 2024</i><br><br>Data: FBref | Chart: Plot the Ball")
labelled_scatter

usa_touches <- fb_season_team_stats(country = "USA", gender = "F", season_end_year = 2023, tier = "1st", stat_type = "possession") %>%
  clean_names() %>%
  select(competition_name, season_end_year, squad, team_or_opponent, mins_per_90, att_pen_touches)

usa_clean <- usa_touches %>%
  mutate(att_pen_per_game = att_pen_touches / mins_per_90)

usa_clean$squad <- str_remove(usa_clean$squad, fixed("vs "))

usa_final <- usa_clean %>%
  select(3, 4, 7) %>%
  pivot_wider(names_from = "team_or_opponent", values_from = "att_pen_per_game")

nwsl_summary <- usa_clean %>%
  summarise(tot_mp = sum(mins_per_90),
            tot_touches = sum(att_pen_touches)) %>%
  mutate(touches_per_game = tot_touches / tot_mp)

spain_touches <- fb_season_team_stats(country = "ESP", gender = "F", season_end_year = 2024, tier = "1st", stat_type = "possession") %>%
  clean_names() %>%
  select(competition_name, season_end_year, squad, team_or_opponent, mins_per_90, att_pen_touches)

spain_clean <- spain_touches %>%
  mutate(att_pen_per_game = att_pen_touches / mins_per_90)

spain_clean$squad <- str_remove(spain_clean$squad, fixed("vs "))

spain_final <- spain_clean %>%
  select(3, 4, 7) %>%
  pivot_wider(names_from = "team_or_opponent", values_from = "att_pen_per_game")

barca_touches <- spain_final %>%
  filter(squad == "Barcelona")

touch_comp <- bind_rows(usa_final, barca_touches)

nwsl_colour <- "#1e2d54"

rough_scatter_ii <- ggplot(touch_comp, aes(x = opponent, y = team)) +
  geom_hline(yintercept = 0,
             linewidth = 1,
             color = ptb_dark_grey) +
  geom_vline(xintercept = 0,
             linewidth = 1,
             color = ptb_dark_grey) +
  geom_point(color = case_when(touch_comp$squad == "Barcelona" ~ barca_colour,
                               TRUE ~ nwsl_colour),
             alpha = case_when(touch_comp$squad == "Barcelona" ~ 1,
                               TRUE ~ 0.25),
             size = 5) +
  scale_y_continuous(name = NULL,
                     limits = c(0, 59),
                     breaks = seq(0, 50, 10),
                     expand = c(0,0)) +
  scale_x_continuous(name = NULL,
                     limits = c(0, 59),
                     breaks = seq(0, 50, 10),
                     expand = c(0,0)) +
  theme(legend.position = "none",
        axis.line.x = element_blank(),
        axis.line.y = element_blank()) +
  annotate(geom = "text",
           x = 2,
           y = 51.5,
           label = "Penalty-area\ntouches per\ngame",
           family = ptb_font,
           colour = ptb_dark_grey,
           fontface = "bold",
           hjust = 0,
           vjust = 1) +
  annotate(geom = "text",
           x = 57,
           y = 13,
           label = "Penalty-area\ntouches conceded\nper game",
           family = ptb_font,
           colour = ptb_dark_grey,
           fontface = "bold",
           hjust = 1,
           vjust = 1)
rough_scatter_ii

themed_scatter_ii <- rough_scatter_ii +
  theme_ptb()
themed_scatter_ii

labelled_scatter_ii <- themed_scatter_ii +
  labs(title = "<span style='color:#cd122d;'>Barça</span>'s dominance of the ball is unlike<br>anything you see in the NWSL",
       subtitle = "Number of <b>penalty-area touches</b> recorded for<br>and against per game by each <b>NWSL</b> team in<br>2023, compared to <b style='color:#cd122d;'>Barça Femení</b> in 2023-24",
       caption = "<i>Data as at 13 Mar 2024</i><br><br>Data: FBref | Chart: Plot the Ball")
labelled_scatter_ii
