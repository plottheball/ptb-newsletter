library(tidyverse)
library(janitor)
library(worldfootballR)
library(gt)
library(gtExtras)
library(gtUtils)

uwcl_results <- fb_match_results(country = "", gender = "F", season_end_year = seq(2022, 2025, 1), tier = "", non_dom_league_url = "https://fbref.com/en/comps/181/history/Champions-League-Seasons")

uwcl_clean <- uwcl_results %>%
  clean_names() %>%
  select(date, season_end_year, round, home, home_goals, home_x_g, away, away_goals, away_x_g, match_url) %>%
  filter(!is.na(home_goals))

### semi-finalists since 2021-22: Lyon, PSG, Barcelona, Wolfsburg, Arsenal, Chelsea
home_list <- c("Arsenal eng", "Barcelona es", "Chelsea eng", "Lyon fr", "Paris S-G fr", "Wolfsburg de")
away_list <- c("eng Arsenal", "es Barcelona", "eng Chelsea", "fr Lyon", "fr Paris S-G", "de Wolfsburg")

uwcl_top <- uwcl_clean %>%
  filter(home %in% home_list & away %in% away_list)

uwcl_top$home <- str_remove_all(uwcl_top$home, fixed(" eng"))
uwcl_top$away <- str_remove_all(uwcl_top$away, fixed("eng "))

uwcl_top$home <- str_remove_all(uwcl_top$home, fixed(" es"))
uwcl_top$away <- str_remove_all(uwcl_top$away, fixed("es "))

uwcl_top$home <- str_remove_all(uwcl_top$home, fixed(" fr"))
uwcl_top$away <- str_remove_all(uwcl_top$away, fixed("fr "))

uwcl_top$home <- str_remove_all(uwcl_top$home, fixed(" de"))
uwcl_top$away <- str_remove_all(uwcl_top$away, fixed("de "))

home_summary <- uwcl_top %>%
  group_by(home) %>%
  summarise(no_games = n(),
            tot_gf = sum(home_goals),
            tot_xgf = sum(home_x_g),
            tot_ga = sum(away_goals),
            tot_xga = sum(away_x_g)) %>%
  rename("team" = "home") %>%
  mutate(category = "Home",
         .after = "team")

away_summary <- uwcl_top %>%
  group_by(away) %>%
  summarise(no_games = n(),
            tot_gf = sum(away_goals),
            tot_xgf = sum(away_x_g),
            tot_ga = sum(home_goals),
            tot_xga = sum(home_x_g)) %>%
  rename("team" = "away") %>%
  mutate(category = "Away",
         .after = "team")

all_summary <- bind_rows(home_summary, away_summary)

final_table <- all_summary %>%
  group_by(team) %>%
  summarise(games = sum(no_games),
            gf = sum(tot_gf),
            xgf = sum(tot_xgf),
            ga = sum(tot_ga),
            xga = sum(tot_xga)) %>%
  mutate(gf_per_game = gf / games,
         xgf_per_game = xgf / games,
         ga_per_game = ga / games,
         xga_per_game = xga / games,
         gd_per_game = (gf - ga) / games,
         xgd_per_game = (xgf - xga) / games) %>%
  arrange(desc(xgd_per_game)) %>%
  select(-gf, -xgf, -ga, -xga)

display_tbl <- final_table %>%
  gt(rowname_col = "team") %>%
  cols_width(team ~ px(120),
             games ~ px(50),
             gf_per_game ~ px(80),
             xgf_per_game  ~ px(80),
             ga_per_game  ~ px(80),
             xga_per_game ~ px(80),
             gd_per_game ~ px(80),
             xgd_per_game ~ px(80))
display_tbl

header_colour <- "#79807E"
arsenal_colour <- "#CC141B"
highlight_arsenal <- str_c(arsenal_colour, "10")

tbl_formatted <- display_tbl %>%
  tab_header(title = html("<b style='color:#CC141B'>Arsenal Women</b> have struggled against Europe's other top clubs in recent seasons"),
             subtitle = html("Average performance of the six clubs to have qualified for the <b>UEFA Women's Champions League semi-finals</b> since 2021-22 in <b>head-to-head UWCL match-ups</b> over that period<br><br>")) %>%
  tab_source_note(source_note = html("<br>'MP' = matches played | 'G' = goals | 'xG' = expected goals<br><br>Data: FBref | Table: Plot the Ball<br>")) %>%
  tab_stubhead(label = "") %>%
  tab_spanner(label = md("— FOR —"),
              columns = c(gf_per_game, xgf_per_game)) %>%
  tab_spanner(label = md("— AGAINST —"),
              columns = c(ga_per_game, xga_per_game)) %>%
  tab_spanner(label = md("— +/- —"),
              columns = c(gd_per_game, xgd_per_game)) %>%
  fmt_number(columns = c(gf_per_game, xgf_per_game, ga_per_game, xga_per_game), use_seps = TRUE, decimals = 1, force_sign = FALSE) %>%
  fmt_number(columns = c(gd_per_game, xgd_per_game), use_seps = TRUE, decimals = 1, force_sign = TRUE) %>%
  cols_label(games = "MP",
             gf_per_game= "GF",
             xgf_per_game = "xGF",
             ga_per_game = "GA",
             xga_per_game= "xGA",
             gd_per_game= "GD",
             xgd_per_game= "▼ xGD") %>%
  cols_align(align = "right",
             columns = everything()) %>%
  opt_table_font(font = ptb_font) %>%
  tab_options(table.border.top.width = px(0),
              table.border.bottom.width = px(0),
              heading.border.bottom.width = px(0),
              column_labels.border.bottom.width = px(0),
              column_labels.border.top.width = px(0),
              table_body.border.top.width = px(0),
              table_body.border.bottom.width = px(0),
              stub.border.width = px(2),
              stub.border.color = ptb_light_grey) %>%
  tab_style(locations = cells_title(groups = "title"),
            style = list(cell_text(align = "left",
                                   color = ptb_dark_grey,
                                   weight = "bold",
                                   size = px(30)))) %>%
  tab_style(locations = cells_title(groups = "subtitle"),
            style = list(cell_text(align = "left",
                                   color = ptb_dark_grey,
                                   size = px(20)))) %>%
  tab_style(locations = list(cells_column_spanners(),
                             cells_column_labels(),
                             cells_stubhead()),
            style = list(cell_text(color = "#FFFFFF",
                                   weight = "bold",
                                   size = "large"),
                         cell_fill(color = header_colour))) %>%
  tab_style(locations = cells_stub(rows = TRUE),
            style =  list(cell_text(align = "left",
                                    color = ptb_mid_grey,
                                    size = "large"),
                          cell_borders(sides = c("top", "bottom"),
                                       weight = px(1),
                                       color = ptb_light_grey))) %>%
  tab_style(locations = cells_body(),
            style = list(cell_text(color = ptb_mid_grey,
                                   size = "large"),
                         cell_borders(sides = c("top", "bottom"),
                                      weight = px(1),
                                      color = ptb_light_grey))) %>%
  tab_style(locations = cells_source_notes(),
            style = list(cell_text(align = "right",
                                   color = ptb_mid_grey,
                                   size = "medium"))) %>%
  tab_style(locations = cells_body(columns = c(gf_per_game, ga_per_game, gd_per_game)),
            style = list(cell_borders(sides = c("left"),
                                      weight = px(2),
                                      color = ptb_light_grey))) %>%
  tab_style(locations = cells_stub(rows = team == "Arsenal"),
            style = list(cell_text(color = arsenal_colour),
                         cell_fill(color = highlight_arsenal))) %>%
  tab_style(locations = cells_body(rows = team == "Arsenal"),
            style = list(cell_text(color = arsenal_colour),
                         cell_fill(color = highlight_arsenal))) %>%
  tab_style(locations = cells_body(rows = team == "Barcelona"),
            style = list(cell_text(weight = "bold"))) %>%
  tab_style(locations = cells_body(columns = games,
                                   rows = team == "Wolfsburg"),
            style = list(cell_text(weight = "bold")))
tbl_formatted
