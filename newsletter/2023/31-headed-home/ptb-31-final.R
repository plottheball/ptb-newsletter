library(tidyverse)
library(worldfootballR)
library(janitor)
library(gt)
library(gtExtras)
library(svglite)
library(scales)
library(ggtext)

wsl_2023_urls <- fb_match_urls(country = "ENG", gender = "F", season_end_year = 2023, tier = "1st")
wsl_2022_urls <- fb_match_urls(country = "ENG", gender = "F", season_end_year = 2022, tier = "1st")
wsl_2021_urls <- fb_match_urls(country = "ENG", gender = "F", season_end_year = 2021, tier = "1st")
wsl_2020_urls <- fb_match_urls(country = "ENG", gender = "F", season_end_year = 2020, tier = "1st")
nwsl_2022_urls <- fb_match_urls(country = "USA", gender = "F", season_end_year = 2019, tier = "1st")
al_2019_urls <- fb_match_urls(country = "AUS", gender = "F", season_end_year = 2019, tier = "1st")
ucl_2023_urls <- fb_match_urls(country = NA, gender = "F", season_end_year = 2023, tier = "1st", non_dom_league_url = "https://fbref.com/en/comps/181/history/Champions-League-Seasons")
ucl_2022_urls <- fb_match_urls(country = NA, gender = "F", season_end_year = 2022, tier = "1st", non_dom_league_url = "https://fbref.com/en/comps/181/history/Champions-League-Seasons")

chelsea_seasons <- c(wsl_2023_urls, wsl_2022_urls, wsl_2021_urls, wsl_2020_urls, ucl_2023_urls, ucl_2022_urls)
chicago_seasons <- c(nwsl_2022_urls)
perth_seasons <- c(al_2019_urls)

teams <- c("Chelsea", "Red-Stars", "Perth")

chelsea_urls <- chelsea_seasons[grepl(fixed(teams[1]), chelsea_seasons)]
chicago_urls <- chicago_seasons[grepl(fixed(teams[2]), chicago_seasons)]
perth_urls <- perth_seasons[grepl(fixed(teams[3]), perth_seasons)]
all_urls <- c(chelsea_urls, chicago_urls, perth_urls)

team_shots <- fb_match_shooting(all_urls)

np_shots <- team_shots %>%
  clean_names() %>%
  filter(player == "Sam Kerr") %>%
  arrange(date)

shots_tidy <- np_shots %>%
  select(1, 2, 6, 7, 9, 10, 11, 12, 14)

shots_tidy$date <- ymd(shots_tidy$date)
shots_tidy$x_g <- as.numeric(shots_tidy$x_g)
shots_tidy$distance <- as.numeric(shots_tidy$distance)
shots_tidy$goal_y_n <- if_else(shots_tidy$outcome == "Goal", 1, 0)
shots_tidy$xg_bucket <- case_when(shots_tidy$x_g <= 0.1 ~ "0.00-0.10 xG shots",
                                  shots_tidy$x_g <= 0.2 ~ "0.11-0.20 xG shots",
                                  shots_tidy$x_g <= 0.3 ~ "0.21-0.30 xG shots",
                                  shots_tidy$x_g <= 0.4 ~ "0.31-0.40 xG shots",
                                  shots_tidy$x_g <= 0.5 ~ "0.41-0.50 xG shots",
                                  TRUE ~ "0.50+ xG shots")

shots_by_xg_bucket <- shots_tidy %>%
  group_by(xg_bucket) %>%
  summarise(no_shots = n(),
            total_xg = sum(x_g),
            total_goals = sum(goal_y_n),
            xg_performance = (total_goals / total_xg) - 1)

aus_green <- "#143e34"
aus_gold <- "#fdbc1d"

display_tbl <- shots_by_xg_bucket %>%
  gt(rowname_col = "xg_bucket")
display_tbl

display_tbl <- display_tbl %>%
  gt_plt_bar(column = xg_performance, keep_column = TRUE, width = 25, color = aus_gold)
display_tbl

tbl_formatted <- display_tbl %>%
  tab_header(title = html("<b>Sam Kerr excels at taking difficult shots</b>"),
             subtitle = html("Non-penalty shots taken by Sam Kerr in club football since 2018-19,<br>grouped into buckets based on the probability of a given shot being scored<br><br>")) %>%
  tab_source_note(source_note = html("<br><br>Data: FBref | Table: Plot the Ball<br>")) %>%
  tab_stubhead(label = "Bucket") %>%
  tab_spanner(label = md("— AGGREGATE —"),
              columns = c(no_shots, total_xg, total_goals)) %>%
  fmt_number(columns = c(no_shots, total_goals), decimals = 0) %>%
  fmt_number(columns = c(total_xg), decimals = 1) %>%
  fmt_percent(columns = xg_performance, decimals = 1, force_sign = TRUE) %>%
  cols_label(no_shots = "Shots",
             total_xg = "xG",
             total_goals = "Goals",
             xg_performance = "+/-",
             DUPE_COLUMN_PLT = "") %>%
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
                         cell_fill(color = aus_green))) %>%
  tab_style(locations = cells_stub(rows = TRUE),
            style =  list(cell_text(align = "left",
                                    color = ptb_dark_grey,
                                    size = "large"),
                          cell_borders(sides = c("top", "bottom"),
                                       weight = px(1),
                                       color = ptb_light_grey))) %>%
  tab_style(locations = cells_body(),
            style = list(cell_text(color = ptb_dark_grey,
                                   size = "large"),
                         cell_borders(sides = c("top", "bottom"),
                                      weight = px(1),
                                      color = ptb_light_grey))) %>%
  tab_style(locations = cells_source_notes(),
            style = list(cell_text(align = "right",
                                   color = ptb_mid_grey,
                                   size = "medium")))  %>%
  tab_style(locations =  list(cells_stub(rows = everything())),
            style = list(cell_text(weight = "bold"))) %>%
  tab_style(locations = list(cells_body(columns = xg_performance)),
            style = list(cell_text(weight = "bold",
                                   color = aus_gold))) %>%
  tab_style(locations = list(cells_body(columns = c(total_xg, total_goals))),
            style = list(cell_text(color = ptb_mid_grey)))
tbl_formatted
