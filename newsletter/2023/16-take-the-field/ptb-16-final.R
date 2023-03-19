library(tidyverse)
library(worldfootballR)
library(janitor)
library(RcppRoll)
library(ggtext)
library(scales)
library(forcats)

barcelona_url <- "https://fbref.com/en/squads/15f49df1/Barcelona-Women-Stats"
bayern_url <- "https://fbref.com/en/squads/51ec22be/Bayern-Munich-Women-Stats"
arsenal_url <- "https://fbref.com/en/squads/411b1108/Arsenal-Women-Stats"
lyon_url <- "https://fbref.com/en/squads/7f2012ad/Lyon-Women-Stats"
wolfsburg_url <- "https://fbref.com/en/squads/a1393014/Wolfsburg-Women-Stats"
roma_url <- "https://fbref.com/en/squads/02f8d026/AS-Roma-Stats"
chelsea_url <- "https://fbref.com/en/squads/a6a4e67d/Chelsea-Women-Stats"
psg_url <- "https://fbref.com/en/squads/80595417/Paris-Saint-Germain-Women-Stats"

barcelona_results <- fb_team_match_results(barcelona_url) %>%
  clean_names() %>%
  filter(!is.na(gf) & comp != "Champions Lg") %>%
  mutate(index = row_number()) %>%
  select(2, 3, 5, 10, 11, 12, 13, 14, 21) %>%
  mutate(match_xgd = x_g - x_ga,
         cum_xgd = cumsum(match_xgd))
bayern_results <- fb_team_match_results(bayern_url) %>%
  clean_names() %>%
  filter(!is.na(gf) & comp != "Champions Lg" & comp != "DFB-Pokal Frauen") %>%
  mutate(index = row_number()) %>%
  select(2, 3, 5, 10, 11, 12, 13, 14, 21) %>%
  mutate(match_xgd = x_g - x_ga,
         cum_xgd = cumsum(match_xgd))
arsenal_results <- fb_team_match_results(arsenal_url) %>%
  clean_names() %>%
  filter(!is.na(gf) & comp != "Champions Lg") %>%
  mutate(index = row_number()) %>%
  select(2, 3, 5, 10, 11, 12, 13, 14, 21) %>%
  mutate(match_xgd = x_g - x_ga,
         cum_xgd = cumsum(match_xgd))
lyon_results <- fb_team_match_results(lyon_url) %>%
  clean_names() %>%
  filter(!is.na(gf) & comp != "Champions Lg") %>%
  mutate(index = row_number()) %>%
  select(2, 3, 5, 10, 11, 12, 13, 14, 21) %>%
  mutate(match_xgd = x_g - x_ga,
         cum_xgd = cumsum(match_xgd))
wolfsburg_results <- fb_team_match_results(wolfsburg_url) %>%
  clean_names() %>%
  filter(!is.na(gf) & comp != "Champions Lg" & comp != "DFB-Pokal Frauen") %>%
  mutate(index = row_number()) %>%
  select(2, 3, 5, 10, 11, 12, 13, 14, 21) %>%
  mutate(match_xgd = x_g - x_ga,
         cum_xgd = cumsum(match_xgd))
roma_results <- fb_team_match_results(roma_url) %>%
  clean_names() %>%
  filter(!is.na(gf) & comp != "Champions Lg") %>%
  mutate(index = row_number()) %>%
  select(2, 3, 5, 10, 11, 12, 13, 14, 21) %>%
  mutate(match_xgd = x_g - x_ga,
         cum_xgd = cumsum(match_xgd))
chelsea_results <- fb_team_match_results(chelsea_url) %>%
  clean_names() %>%
  filter(!is.na(gf) & !is.na(x_g) & comp != "Champions Lg") %>%
  mutate(index = row_number()) %>%
  select(2, 3, 5, 10, 11, 12, 13, 14, 21) %>%
  mutate(match_xgd = x_g - x_ga,
         cum_xgd = cumsum(match_xgd))
psg_results <- fb_team_match_results(psg_url) %>%
  clean_names() %>%
  filter(!is.na(gf) & comp != "Champions Lg") %>%
  mutate(index = row_number()) %>%
  select(2, 3, 5, 10, 11, 12, 13, 14, 21) %>%
  mutate(match_xgd = x_g - x_ga,
         cum_xgd = cumsum(match_xgd))

league_results <- bind_rows(arsenal_results, barcelona_results, bayern_results, chelsea_results, lyon_results, psg_results, roma_results, wolfsburg_results) %>%
  select(1, 9, 11)

uwcl_matches <- fb_match_results(country = "NA", gender = "F", season_end_year = c("2015", "2016", "2017", "2018", "2019", "2020", "2021", "2022", "2023"), non_dom_league_url = "https://fbref.com/en/comps/181/history/Champions-League-Seasons")

matches_xg <- uwcl_matches %>%
  filter(Season_End_Year == "2022" | Season_End_Year == "2023") %>%
  select(4, 5, 8, 10, 11, 12, 13, 18, 19, 20) %>%
  clean_names()

qf_fixtures <- matches_xg %>%
  filter(is.na(home_goals))

qf_teams_home <- unlist(qf_fixtures$home)
qf_teams_away <- unlist(qf_fixtures$away)

uwcl_historical <- matches_xg %>%
  filter(!is.na(home_goals)) %>%
  select(3, 4, 6, 8, 9, 10)

home_aggregates <- uwcl_historical %>%
  group_by(home) %>%
  summarise(matches = n(),
            xg_for = sum(home_x_g),
            xg_against = sum(away_x_g),
            xg_difference = xg_for - xg_against,
            xgd_per_match = xg_difference / matches) %>%
  filter(home %in% qf_teams_home) %>%
  mutate(category = "home") %>%
  rename("team" = "home")

away_aggregates <- uwcl_historical %>%
  group_by(away) %>%
  summarise(matches = n(),
            xg_for = sum(away_x_g),
            xg_against = sum(home_x_g),
            xg_difference = xg_for - xg_against,
            xgd_per_match = xg_difference / matches) %>%
  filter(away %in% qf_teams_away) %>%
  mutate(category = "away") %>%
  rename("team" = "away")

combined_aggregates <- bind_rows(home_aggregates, away_aggregates)

combined_aggregates$team <- combined_aggregates$team %>%
  str_remove_all(pattern = " eng|eng | it|it | es|es | de|de | fr|fr ") %>%
  str_squish()

uwcl_by_team <- combined_aggregates %>%
  group_by(team) %>%
  summarise(matches_total = sum(matches),
            xg_for_total = sum(xg_for),
            xg_against_total = sum(xg_against),
            xg_difference_total = xg_for_total - xg_against_total,
            xgd_per_match_total = xg_difference_total / matches_total) %>%
  arrange(-xgd_per_match_total)

uwcl_colour <- "#8DA2B8"
uwcl_colour_v2 <- "#8DA2B875"
    
rough_line <- ggplot(league_results, aes(x = index, y = cum_xgd, colour = team)) +
  geom_line(size = 1.5) +
  scale_colour_manual(values = c("Barcelona Women" = "#cd122d",
                                 "Arsenal Women" = uwcl_colour_v2,
                                 "Bayern Munich Women" = uwcl_colour_v2,
                                 "Chelsea Women" = uwcl_colour_v2,
                                 "Lyon Women" = uwcl_colour_v2,
                                 "Paris Saint Germain Women" = uwcl_colour_v2,
                                 "AS Roma" = uwcl_colour_v2,
                                 "Wolfsburg Women" = uwcl_colour_v2)) +
  theme(legend.position = "none") +
  scale_y_continuous(limits = c(-4, 74),
                     breaks = seq(0, 70, 10),
                     expand = c(0,0),
                     labels = label_number(style_positive = "plus")) +
  scale_x_continuous(limits = c(0, 23),
                     breaks = seq(0, 20, 5),
                     expand = c(0,0))
rough_line
  
themed_line <- rough_line +
  theme_ptb() +
  theme(axis.line.x = element_blank()) +
  geom_hline(yintercept = 0,
             colour = ptb_dark_grey)
themed_line
  
labelled_line <- themed_line +
  labs(x = "Match no.",
       title = "<b style='color:#cd122d'>Barça</b> are dominant in Liga F this year",
       subtitle = "Cumulative <b>xG difference</b> recorded by each of the<br>UWCL quarter-finalists in league play in <b>2022-23</b>",
       caption = "Data: FBref | Chart: Plot the Ball")
labelled_line
  
rough_bar <- ggplot(uwcl_by_team, aes(x = fct_reorder(team, xgd_per_match_total), y = xgd_per_match_total, fill = team)) +
  geom_bar(stat = "identity", width = 0.7) +
  scale_fill_manual(values = c("Barcelona" = "#cd122d",
                               "Arsenal" = uwcl_colour,
                               "Bayern Munich" = uwcl_colour,
                               "Chelsea" = uwcl_colour,
                               "Lyon" = uwcl_colour,
                               "Paris S-G" = uwcl_colour,
                               "AS Roma" = uwcl_colour,
                               "Wolfsburg" = uwcl_colour)) +
  scale_y_continuous(limits = c(-0.4, 3.4),
                     breaks = seq(0, 3, 1),
                     expand = c(0,0),
                     labels = label_number(style_positive = "plus")) +
  geom_hline(yintercept = 0,
             colour = ptb_dark_grey) +
  coord_flip() +
  theme(legend.position = "none")
rough_bar

themed_bar <- rough_bar +
  theme_ptb() +
  theme(axis.line.y = element_blank(),
        axis.ticks.y = element_blank(),
        panel.grid.major.y = element_blank())
themed_bar

labelled_bar <- themed_bar +
  labs(y = " Net xG difference per match", 
       title = "<b style='color:#cd122d'>Barça</b> have dominated the UWCL too",
       subtitle = "Net <b>xG difference</b> per match recorded by each UWCL<br>quarter-finalist in the UWCL in <b>2021-22</b> and <b>2022-23</b>",
       caption = "Data: FBref | Chart: Plot the Ball")
labelled_bar
