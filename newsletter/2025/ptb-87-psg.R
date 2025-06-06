library(tidyverse)
library(rvest)
library(worldfootballR)
library(janitor)
library(gt)
library(gtUtils)
library(gtExtras)

season_match_logs <- c("https://fbref.com/en/squads/e2d8892c/2024-2025/matchlogs/c13/schedule/Paris-Saint-Germain-Scores-and-Fixtures-Ligue-1",
                       "https://fbref.com/en/squads/e2d8892c/2023-2024/matchlogs/c13/schedule/Paris-Saint-Germain-Scores-and-Fixtures-Ligue-1")

all_matches <- c()

for (i in season_match_logs) {
  
  Sys.sleep(6)
  
  html <- read_html(i)
  
  links <- html %>%
    html_nodes("a") %>%
    html_attr("href")
  
  match_urls <- unique(links[grepl("/matches/", links)])
  
  all_matches <- c(all_matches, match_urls) %>%
    unique()
  
}

final_urls <- tibble(complete = all_matches, links = all_matches) %>%
  separate(col = links, sep = "/", into = c("col1", "col2", "col3", "col4", "col5")) %>%
  filter(!is.na(col5)) %>%
  mutate(final = str_c("https://fbref.com", complete),
         .before = complete) %>%
  select(final) %>%
  unique() %>%
  unlist()

final_urls <- final_urls[!grepl(fixed("Champions-League"), x = final_urls)]

all_shots <- fb_match_shooting(match_url = final_urls,
                               time_pause = 10)

psg_shots <- all_shots %>%
  clean_names() %>%
  filter(squad == "Paris S-G") %>%
  select(-squad, -home_away, -match_half, -minute, -p_sx_g) %>%
  mutate(date = ymd(date),
         x_g = as.numeric(x_g),
         distance = as.integer(distance)) %>%
  mutate(shot_no =row_number(),
         season = case_when(date < ymd("2024-07-01") ~ "2023-24",
                            date < ymd("2025-07-01") ~ "2024-25",
                            TRUE ~ "!"),
         .before = "date")

summary_by_sca <- psg_shots %>%
  group_by(season, event_sca_1, event_sca_2) %>%
  summarise(count = n(),
            tot_x_g = sum(x_g)) %>%
  arrange(desc(count))

no_games <- psg_shots %>%
  select(season, date) %>%
  unique() %>%
  group_by(season) %>%
  summarise(matches = n())

open_play_clean <- summary_by_sca %>%
  filter(event_sca_1 != "") %>%
  filter(event_sca_1 != "Fouled" & event_sca_2 != "Fouled") %>%
  filter(event_sca_1 != "Pass (Dead)" & event_sca_2 != "Pass (Dead)") %>%
  mutate(category = case_when(event_sca_1 == "Pass (Live)" & event_sca_2 == "Pass (Live)" ~ "at least two passes",
                              event_sca_1 == "Take-On" | event_sca_2 == "Take-On" ~ "a successful take-on",
                              event_sca_1 == "Interception" | event_sca_1 == "Tackle" | event_sca_2 == "Interception" | event_sca_2 == "Tackle" ~ "an interception or a tackle",
                              TRUE ~ "other on-ball actions"),
         group = "OPEN-PLAY SHOTS FOLLOWING:")

open_play_final <- open_play_clean %>%
  group_by(season, group, category) %>%
  summarise(total = sum(count),
            sum_xg = sum(tot_x_g))

open_play_final <- left_join(open_play_final, no_games, by = "season")

for_table <- open_play_final %>%
  mutate(shots_per_game = total / matches,
         xg_per_shot = sum_xg / total) %>%
  select(-total, -sum_xg, -matches) %>%
  pivot_wider(names_from = "season",
              values_from = c("shots_per_game", "xg_per_shot")) %>%
  clean_names() %>%
  mutate(shot_diff = round(shots_per_game_2024_25, digits = 1) - round(shots_per_game_2023_24, digits = 1),
         .after = shots_per_game_2024_25) %>%
  mutate(xgps_diff = round(xg_per_shot_2024_25, digits = 2) - round(xg_per_shot_2023_24, digits = 2),
         .after = "xg_per_shot_2024_25") %>%
  arrange(category)

display_table <- for_table %>%
  gt(rowname_col = "category",
     groupname_col = "group") %>%
  cols_width(category ~ px(230),
             shots_per_game_2023_24 ~ px(70),
             shots_per_game_2024_25 ~ px(70),
             shot_diff ~ px(70),
             xg_per_shot_2023_24 ~ px(70),
             xg_per_shot_2024_25 ~ px(70),
             xgps_diff ~ px(70))
display_table

header_colour <- "#79807E"
text_positive <- "#CC2947"
text_negative <- "#0D5080"
fill_positive <- str_c(text_positive, "10")
fill_negative <- str_c(text_negative, "10")
text_positive_other <- str_c(text_positive, "70")
text_negative_other <- str_c(text_negative, "70")

tbl_formatted <- display_table %>%
  tab_header(title = html("This season's PSG created <b style='color:#CC2947'>more</b> shots in settled possession, and <b style='color:#0D5080'>fewer</b> after dribbles"),
             subtitle = html("<b>Shots per game</b> recorded in open play by <b>Paris Saint-Germain</b> in each of the last two <b>Ligue 1</b> seasons<br><br>")) %>%
  tab_source_note(source_note = html("<br>'Other' includes shots after rebounds and shots following a single pass<br><br><br>Data: FBref | Table: Plot the Ball<br>")) %>%
  tab_stubhead(label = "") %>%
  tab_spanner(label = md("— SHOTS PER GAME —"),
              columns = c(shots_per_game_2023_24, shots_per_game_2024_25, shot_diff)) %>%
  tab_spanner(label = md("— XG PER SHOT —"),
              columns = c(xg_per_shot_2023_24, xg_per_shot_2024_25, xgps_diff)) %>%
  fmt_number(columns = c(shots_per_game_2024_25, shots_per_game_2023_24, shot_diff), decimals = 1, force_sign = FALSE) %>%
  fmt_number(columns = c(shot_diff), decimals = 1, force_sign = TRUE) %>%
  fmt_number(columns = c(xg_per_shot_2024_25, xg_per_shot_2023_24), use_seps = TRUE, decimals = 2, force_sign = FALSE) %>%
  fmt_number(columns = c(xgps_diff), use_seps = TRUE, decimals = 2, force_sign = TRUE) %>%
  cols_label(shots_per_game_2023_24 = "'23-24",
             shots_per_game_2024_25 = "'24-25",
             shot_diff = "+/-",
             xg_per_shot_2023_24 = "'23-24",
             xg_per_shot_2024_25 = "'24-25",
             xgps_diff = "+/-") %>%
  cols_align(align = "right",
             columns = everything()) %>%
  cols_align(align = "left",
             columns = category) %>%
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
                                    color = ptb_mid_grey_v2,
                                    size = "large"),
                          cell_borders(sides = c("top", "bottom"),
                                       weight = px(1),
                                       color = ptb_light_grey))) %>%
  tab_style(locations = cells_body(),
            style = list(cell_text(color = ptb_mid_grey_v2,
                                   size = "large"),
                         cell_borders(sides = c("top", "bottom"),
                                      weight = px(1),
                                      color = ptb_light_grey))) %>%
  tab_style(locations = cells_source_notes(),
            style = list(cell_text(align = "right",
                                   color = ptb_mid_grey,
                                   size = "medium"))) %>%
  tab_style(locations = cells_row_groups(groups = c(1)),
            style = list(cell_text(color = ptb_mid_grey_v2,
                                   weight = "bold",
                                   size = "large"),
                         cell_borders(color = ptb_light_grey,
                                      weight = px(0)))) %>%
  tab_style(locations = cells_body(columns = c(shots_per_game_2023_24, xg_per_shot_2023_24)),
            style = list(cell_borders(sides = c("left"),
                                      weight = px(2),
                                      color = ptb_light_grey))) %>%
  tab_style(locations = cells_body(columns = c(shot_diff, xgps_diff)),
            style = list(cell_borders(sides = c("left"),
                                      weight = px(1.5),
                                      color = ptb_light_grey))) %>%
  tab_style(locations = cells_body(columns = c(shot_diff),
                                   rows = shot_diff > 0),
            style = cell_text(color = text_positive)) %>%
  tab_style(locations = cells_body(columns = c(shot_diff),
                                   rows = shot_diff < 0),
            style = cell_text(color = text_negative)) %>%
  tab_style(locations = cells_body(columns = c(xgps_diff),
                                   rows = xgps_diff > 0),
            style = cell_text(color = text_positive)) %>%
  tab_style(locations = cells_body(columns = c(xgps_diff),
                                   rows = xgps_diff < 0),
            style = cell_text(color = text_negative)) %>%
  tab_style(locations = cells_stub(rows = c("a successful take-on", "at least two passes")),
            style = list(cell_text(weight = "bold"),
                         cell_fill(color = ptb_light_grey_v5))) %>%
  tab_style(locations = cells_body(rows = c("a successful take-on", "at least two passes")),
            style = list(cell_text(weight = "bold"),
                         cell_fill(color = ptb_light_grey_v5))) %>%
  tab_style(locations = cells_body(columns = c(shot_diff, xgps_diff),
                                   rows = c("an interception or a tackle", "other on-ball actions")),
            style = cell_text(color = text_positive_other)) %>%
  tab_style(locations = cells_body(columns = c(shot_diff, xgps_diff),
                                   rows = "at least two passes"),
            style = list(cell_fill(color = fill_positive))) %>%
  tab_style(locations = cells_body(columns = c(shot_diff, xgps_diff),
                                   rows = "a successful take-on"),
            style = list(cell_fill(color = fill_negative)))
  tbl_formatted
