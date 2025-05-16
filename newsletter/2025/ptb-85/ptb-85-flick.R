library(tidyverse)
library(janitor)
library(worldfootballR)
library(ggtext)
library(gt)
library(gtExtras)
library(gtUtils)

seasons <- seq(2018, 2025, 1)

leagues <- c("ENG", "ESP", "FRA", "GER", "ITA")

all_tables <- tibble()

for (i in seasons) {
  
  Sys.sleep(6)
  
  season_tables <- tibble()
  
  for (j in leagues) {
    
    table <- fb_season_team_stats(country = j, season_end_year = i, gender = "M", tier = "1st", stat_type = "league_table")
    
    season_tables <- bind_rows(season_tables, table)
    
  }
  
  all_tables <- bind_rows(all_tables, season_tables)
  
}

clean_tables <- all_tables %>%
  clean_names() %>%
  select(1:8, 12, 13, 17, 18) %>%
  filter(rk == 1)

data_final <- clean_tables %>%
  select(-2, -3, -6) %>%
  mutate(xgf_per_game = x_g / mp,
         xga_per_game = x_ga / mp,
         xgd_per_game = xgf_per_game - xga_per_game,
         tot_xg_per_game = round(xgf_per_game, digits = 1) + round(xga_per_game, digits = 1)) %>%
  arrange(desc(tot_xg_per_game))

for_table <- data_final %>%
  select(-1, -4, -5, -6, -7, -8, -9) %>%
  head(11) %>%
  mutate(manager = case_when(squad == "Barcelona" & season_end_year == 2025 ~ "Hansi Flick",
                             squad == "Barcelona" & season_end_year == 2018 ~ "Ernesto Valverde",
                             squad == "Paris S-G" & season_end_year == 2025 ~ "Luis Enrique",
                             squad == "Paris S-G" & season_end_year == 2023 ~ "Christophe Galtier",
                             squad == "Paris S-G" & season_end_year == 2020 ~ "Thomas Tuchel",
                             squad == "Paris S-G" & season_end_year == 2019 ~ "Thomas Tuchel",
                             squad == "Bayern Munich" & season_end_year == 2023 ~ "Thomas Tuchel*",
                             squad == "Bayern Munich" & season_end_year == 2022 ~ "Julian Nagelsmann",
                             squad == "Bayern Munich" & season_end_year == 2021 ~ "Hansi Flick",
                             squad == "Bayern Munich" & season_end_year == 2020 ~ "Hansi Flick*",
                             squad == "Bayern Munich" & season_end_year == 2019 ~ "Niko Kovač"),
         .after = squad) %>%
  relocate(season_end_year, .before = manager) %>%
  select(-xgd_per_game)

display_table <- for_table %>%
  gt(rowname_col = "squad") %>%
  cols_width(squad ~ px(150),
             season_end_year ~ px(75),
             manager ~ px(175),
             xgf_per_game ~ px(60),
             xga_per_game ~ px(60),
             tot_xg_per_game ~ px(80))
display_table

header_colour <- "#79807E"
bayern_colour <- "#CC2947"
barca_colour <- "#0D4680"
highlight_bayern <- str_c(bayern_colour, "10")
highlight_barca <- str_c(barca_colour, "10")

tbl_formatted <- display_table %>%
  tab_header(title = html("Flick's title-winning teams at both <b style='color:#004D98'>Barça</b> and <b style='color:#CC2947'>Bayern</b> played an end-to-end style"),
             subtitle = html("Winners of the 'Big Five' leagues since 2017-18, ranked by the sum total of the expected goals created and conceded in their games<br><br>")) %>%
  tab_source_note(source_note = html("<br>'xG' = expected goals | 'F' = for | 'A' = against<br><br><i>* Appointed midseason</i><br><br>Data: FBref | Table: Plot the Ball<br>")) %>%
  tab_stubhead(label = "") %>%
  tab_spanner(label = md("— XG PER GAME —"),
              columns = c(xgf_per_game, xga_per_game, tot_xg_per_game)) %>%
  fmt_number(columns = c(xgf_per_game, xga_per_game, tot_xg_per_game), use_seps = TRUE, decimals = 1, force_sign = FALSE) %>%
  cols_label(season_end_year = "Season",
             manager = "Coach",
             xgf_per_game = "F",
             xga_per_game= "A",
             tot_xg_per_game= "▼ Total") %>%
  cols_align(align = "right",
             columns = everything()) %>%
  cols_align(align = "left",
             columns = manager) %>%
  cols_align(align = "center",
             columns = season_end_year) %>%
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
  tab_style(locations = cells_body(columns = c(season_end_year, manager, xgf_per_game, tot_xg_per_game)),
            style = list(cell_borders(sides = c("left"),
                                      weight = px(2),
                                      color = ptb_light_grey))) %>%
  tab_style(locations = cells_stub(rows = squad == "Bayern Munich" & (season_end_year == 2020 | season_end_year == 2021)),
            style = list(cell_text(color = bayern_colour,
                                   weight = "bold"),
                         cell_fill(color = highlight_bayern))) %>%
  tab_style(locations = cells_body(rows = squad == "Bayern Munich" & (season_end_year == 2020 | season_end_year == 2021)),
            style = list(cell_text(color = bayern_colour,
                                   weight = "bold"),
                         cell_fill(color = highlight_bayern))) %>%
  tab_style(locations = cells_stub(rows = squad == "Barcelona" & (season_end_year == 2025)),
            style = list(cell_text(color = barca_colour,
                                   weight = "bold"),
                         cell_fill(color = highlight_barca))) %>%
  tab_style(locations = cells_body(rows = squad == "Barcelona" & (season_end_year == 2025)),
            style = list(cell_text(color = barca_colour,
                                   weight = "bold"),
                         cell_fill(color = highlight_barca)))
tbl_formatted
