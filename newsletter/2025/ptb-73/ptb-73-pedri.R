library(tidyverse)
library(janitor)
library(worldfootballR)
library(gt)
library(gtExtras)
library(gtUtils)

focus_year <- seq(2024, 2025, 1)
focus_league <- "La Liga"
focus_team <- "Barcelona"

standard_stats_player <- fb_big5_advanced_season_stats(season_end_year = focus_year,
                                                       team_or_player = "player",
                                                       stat_type = "standard") %>%
  clean_names()

passing_stats_player <- fb_big5_advanced_season_stats(season_end_year = focus_year,
                                                      team_or_player = "player",
                                                      stat_type = "passing") %>%
  clean_names()

passing_types_stats_player <- fb_big5_advanced_season_stats(season_end_year = focus_year,
                                                            team_or_player = "player",
                                                            stat_type = "passing_types") %>%
  clean_names()

possession_stats_player <- fb_big5_advanced_season_stats(season_end_year = focus_year,
                                                         team_or_player = "player",
                                                         stat_type = "possession") %>%
  clean_names()

shooting_stats_player <- fb_big5_advanced_season_stats(season_end_year = focus_year,
                                                       team_or_player = "player",
                                                       stat_type = "shooting") %>%
  clean_names()

player_all <- full_join(x = standard_stats_player, y = passing_stats_player) %>%
  full_join(., y = possession_stats_player) %>%
  full_join(., y = shooting_stats_player)

player_clean <- player_all %>%
  filter(squad == focus_team) %>%
  select(season_end_year,
         squad,
         comp,
         player,
         pos,
         min_playing,
         cmp_total,
         att_total,
         prg_p_progression,
         ppa) %>%
  filter(squad == focus_team & pos != "GK") %>%
  select(-squad, - comp, -pos)

passing_focus <- player_clean %>%
  mutate(available_mins = case_when(season_end_year == 2024 ~ 38 * 90,
                                    season_end_year == 2025 ~ 19 * 90),
         mins_share = min_playing / available_mins * 100,
         .after = min_playing) %>%
  filter(mins_share > (1 / 3 * 100)) %>%
  select(-available_mins) %>%
  mutate(prog_p_p90 = prg_p_progression / min_playing * 90) %>%
  arrange(desc(prog_p_p90))

top_five_final <- tibble()

for (i in unique(unlist(passing_focus$season_end_year))) {
  
  filtered <- passing_focus %>%
    filter(season_end_year == i)
  
  top_five <- filtered %>%
    head(5)
  
  top_five_final <- bind_rows(top_five_final, top_five)
  
}

for_table <- top_five_final %>%
  mutate(season = case_when(season_end_year == 2024 ~ "LA LIGA 2023-24",
                            season_end_year == 2025 ~ "LA LIGA 2024-25"),
         player = player,
         mins_share = mins_share,
         ppa_p90 = ppa / min_playing * 90,
         other_prog_p_p90 = (prg_p_progression - ppa) / min_playing * 90,
         prog_p_p90 = prg_p_progression / min_playing * 90,
         other_comp_p_p90 = (cmp_total - prg_p_progression) / min_playing * 90,
         total_comp_p_p90 = cmp_total / min_playing * 90,
         comp_percent = cmp_total / att_total,
         .keep = "none")

display_tbl <- for_table %>%
  select(-comp_percent, -total_comp_p_p90, -other_comp_p_p90, -mins_share) %>%
  gt(rowname_col = "player",
     groupname_col = "season") %>%
  cols_width(player ~ px(200),
             ppa_p90 ~ px(125),
             other_prog_p_p90 ~ px(125),
             prog_p_p90 ~ px(100))
display_tbl

tbl_formatted <- display_tbl %>%
  tab_header(title = html("<b style='color:#CC2941'>Pedri</b> is doing his ball progression work further from goal this season"),
             subtitle = html("<b>Barcelona</b>'s five <b>most frequent progressive passers</b> per 90 minutes in each of the last two <b>La Liga</b> seasons<br><br>")) %>%
  tab_source_note(source_note = html("<br>'PA' = penalty area<br><br><i>Players with < 1/3 of available minutes played excluded</i><br><br>Data: FBref | Table: Plot the Ball<br>")) %>%
  tab_stubhead(label = "Player") %>%
  tab_spanner(label = md("— PROGRESSIVE PASSES PER 90 —"),
              columns = c(ppa_p90, other_prog_p_p90, prog_p_p90)) %>%
  fmt_number(columns = c(ppa_p90, other_prog_p_p90, prog_p_p90), use_seps = TRUE, decimals = 1) %>%
  cols_label(ppa_p90 = "Into PA",
             other_prog_p_p90 = "Outside PA",
             prog_p_p90 = "Total") %>%
  cols_align(align = "right",
             columns = everything()) %>%
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
                         cell_fill(color = "#79807E"))) %>%
  tab_style(locations = cells_stub(rows = TRUE),
            style =  list(cell_text(align = "left",
                                    color = ptb_mid_grey_v2,
                                    size = "large"),
                          cell_borders(sides = c("top", "bottom"),
                                       weight = px(1),
                                       color = ptb_light_grey))) %>%
  tab_style(locations = cells_row_groups(groups = c(1,2)),
            style = list(cell_text(weight = "bold",
                                   color = "#79807E"),
                         cell_borders(weight = px(0)))) %>%
  tab_style(locations = cells_body(),
            style = list(cell_text(color = ptb_mid_grey_v2,
                                   size = "large"),
                         cell_borders(sides = c("top", "bottom"),
                                      weight = px(1),
                                      color = ptb_light_grey))) %>%
  tab_style(locations = cells_source_notes(),
            style = list(cell_text(align = "right",
                                   color = ptb_mid_grey,
                                   size = "medium")))  %>%
  tab_style(locations =  list(cells_stub(rows = player == "Pedri")),
            style = list(cell_text(weight = "bold",
                                   color = "#CC2941"),
                         cell_fill(color = "#CC294110"))) %>%
  tab_style(locations =  list(cells_body(rows = player == "Pedri")),
            style = list(cell_text(color = "#CC2941"),
                         cell_fill(color = "#CC294110"))) %>%
  tab_style(locations = list(cells_body(columns = prog_p_p90)),
            style = list(cell_borders(sides = c("left"),
                                      weight = px(3),
                                      color = ptb_light_grey))) %>%
  tab_style(locations = list(cells_body(columns = prog_p_p90)),
            style = list(cell_borders(sides = c("left"),
                                      weight = px(2),
                                      color = ptb_light_grey),
                         cell_text(weight = "bold")))
tbl_formatted
