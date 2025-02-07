library(tidyverse)
library(janitor)
library(rvest)
library(gt)
library(gtExtras)
library(gtUtils)

pace_test <- "https://stats.espncricinfo.com/ci/engine/stats/index.html?bowling_pacespin=1;class=8;filter=advanced;host=2;orderby=balls;spanmin1=1+Jan+2025;spanval1=span;template=results;type=bowling"
spin_test <- "https://stats.espncricinfo.com/ci/engine/stats/index.html?bowling_pacespin=2;class=8;filter=advanced;host=2;orderby=balls;spanmin1=1+Jan+2025;spanval1=span;template=results;type=bowling"

pace_test_tables <- read_html(pace_test) %>%
  html_nodes("table") %>%
  html_table()
pace_test_final <- pace_test_tables [[3]] %>%
  clean_names() %>%
  mutate(format = "Test (Jan 30-Feb 1)",
         type = "PACE BOWLERS")

spin_test_tables <- read_html(spin_test) %>%
  html_nodes("table") %>%
  html_table()
spin_test_final <- spin_test_tables [[3]] %>%
  clean_names() %>%
  mutate(format = "Test (Jan 30-Feb 1)",
         type = "SPIN BOWLERS")

test_final <- bind_rows(pace_test_final, spin_test_final)

pace_odi <- "https://stats.espncricinfo.com/ci/engine/stats/index.html?bowling_pacespin=1;class=9;filter=advanced;host=2;orderby=balls;spanmin1=1+Jan+2025;spanval1=span;template=results;type=bowling"
spin_odi <- "https://stats.espncricinfo.com/ci/engine/stats/index.html?bowling_pacespin=2;class=9;filter=advanced;host=2;orderby=balls;spanmin1=1+Jan+2025;spanval1=span;template=results;type=bowling"

pace_odi_tables <- read_html(pace_odi) %>%
  html_nodes("table") %>%
  html_table()
pace_odi_final <- pace_odi_tables [[3]] %>%
  clean_names() %>%
  mutate(format = "ODIs (Jan 11, 13, 16)",
         type = "PACE BOWLERS")

spin_odi_tables <- read_html(spin_odi) %>%
  html_nodes("table") %>%
  html_table()
spin_odi_final <- spin_odi_tables [[3]] %>%
  clean_names() %>%
  mutate(format = "ODIs (Jan 11, 13, 16)",
         type = "SPIN BOWLERS")

odi_final <- bind_rows(pace_odi_final, spin_odi_final)

pace_t20i <- "https://stats.espncricinfo.com/ci/engine/stats/index.html?bowling_pacespin=1;class=10;filter=advanced;host=2;orderby=balls;spanmin1=1+Jan+2025;spanval1=span;template=results;type=bowling"
spin_t20i <- "https://stats.espncricinfo.com/ci/engine/stats/index.html?bowling_pacespin=2;class=10;filter=advanced;host=2;orderby=balls;spanmin1=1+Jan+2025;spanval1=span;template=results;type=bowling"

pace_t20i_tables <- read_html(pace_t20i) %>%
  html_nodes("table") %>%
  html_table()
pace_t20i_final <- pace_t20i_tables [[3]] %>%
  clean_names() %>%
  mutate(format = "T20Is (Jan 20, 23, 25)",
         type = "PACE BOWLERS")

spin_t20i_tables <- read_html(spin_t20i) %>%
  html_nodes("table") %>%
  html_table()
spin_t20i_final <- spin_t20i_tables [[3]] %>%
  clean_names() %>%
  mutate(format = "T20Is (Jan 20, 23, 25)",
         type = "SPIN BOWLERS")

t20i_final <- bind_rows(pace_t20i_final, spin_t20i_final)

all_formats <- bind_rows(test_final, odi_final, t20i_final) %>%
  select(format, player, type, balls, runs, wkts) %>%
  mutate(balls = as.integer(balls),
         runs = as.integer(runs),
         wkts = as.integer(wkts)) %>%
  filter(!is.na(balls)) %>%
  separate(player, into = c("name", "team"), sep = "[(]") %>%
  mutate(name = str_squish(name),
         team = str_remove_all(team, fixed(")")))

player_summary <- all_formats %>%
  group_by(name) %>%
  summarise(tot_balls = sum(balls),
            tot_wkts = sum(wkts)) %>%
  arrange(desc(tot_wkts)) %>%
  mutate(bpw = tot_balls / tot_wkts) %>%
  filter(tot_balls >= 120)

team_summary <- all_formats %>%
  group_by(format, team) %>%
  summarise(tot_balls = sum(balls))

team_format_summary <- all_formats %>%
  group_by(format, team, type) %>%
  summarise(n_balls = sum(balls),
            n_runs = sum(runs),
            n_wkts = sum(wkts)) %>%
  mutate(rpb = n_runs / n_balls,
         rpw = n_runs / n_wkts,
         bpw = n_balls / n_wkts)

team_format_summary <- left_join(team_format_summary, team_summary, by = c("format", "team")) %>%
  mutate(type_share = n_balls / tot_balls) %>%
  arrange(desc(team))

eng_share <- team_format_summary %>%
  filter(team == "ENG-W") %>%
  select(format, type, type_share) %>%
  rename(share_eng = type_share)

manual_col_1 <- unlist(eng_share$format)
manual_col_2 <- unlist(eng_share$type)
manual_col_3 <- unlist(eng_share$share_eng)

eng_share <- tibble(format = manual_col_1,
                    type = manual_col_2,
                    eng_share = manual_col_3)

aus_share <- team_format_summary %>%
  filter(team == "AUS-W") %>%
  select(format, type, type_share) %>%
  rename(share_aus = type_share)

manual_col_4 <- unlist(aus_share$format)
manual_col_5 <- unlist(aus_share$type)
manual_col_6 <- unlist(aus_share$share_aus)

aus_share <- tibble(format = manual_col_4,
                    type = manual_col_5,
                    aus_share = manual_col_6)

final_for_table <- team_format_summary %>%
  select(format, team, type, rpw) %>%
  pivot_wider(names_from = "team",
              values_from = "rpw") %>%
  clean_names() %>%
  arrange(type, format) %>%
  left_join(., eng_share, by = c("format", "type")) %>%
  left_join(., aus_share, by = c("format", "type"))

display_tbl <- final_for_table %>%
  gt(rowname_col = "format",
     groupname_col = "type") %>%
  cols_width(format ~ px(200),
             eng_share ~ px(100),
             aus_share ~ px(100),
             eng_w ~ px(115),
             aus_w ~ px(115))
display_tbl

tbl_formatted <- display_tbl %>%
  tab_header(title = html("<b style='color:#208068'>Australia</b>'s spinners took their wickets more cheaply than <b style='color:#205680'>England</b>'s in all formats"),
             subtitle = html("<b>Share of balls bowled</b> and <b>runs conceded per wicket</b> by each <b>bowler type</b> for <b style='color:#205680'>England</b> and <b style='color:#208068'>Australia</b> in the 2025 Women's Ashes<br><br>")) %>%
  tab_source_note(source_note = html("<br><br><br>Data: ESPNCricinfo | Table: Plot the Ball<br>")) %>%
  tab_stubhead(label = "Format") %>%
  tab_spanner(label = md("— ENGLAND —"),
              columns = c(eng_share, eng_w)) %>%
  tab_spanner(label = md("— AUSTRALIA —"),
              columns = c(aus_share, aus_w)) %>%
  fmt_percent(columns = c(eng_share, aus_share), decimals = 0) %>%
  fmt_number(columns = c(eng_w, aus_w), use_seps = TRUE, decimals = 1) %>%
  cols_label(eng_share = "% of balls",
             aus_share = "% of balls",
             eng_w = "Runs per W",
             aus_w = "Runs per W") %>%
  cols_align(align = "center",
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
                         cell_fill(color = "#999696"))) %>%
  tab_style(locations = cells_stub(rows = TRUE),
            style =  list(cell_text(align = "left",
                                    color = ptb_mid_grey_v2,
                                    size = "large"),
                          cell_borders(sides = c("top", "bottom"),
                                       weight = px(1),
                                       color = ptb_light_grey))) %>%
  tab_style(locations = cells_row_groups(groups = c(1,2)),
            style = list(cell_text(weight = "bold",
                                   color = "#999696"),
                         cell_borders(weight = px(0)))) %>%
  tab_style(locations = cells_body(),
            style = list(cell_text(color = ptb_mid_grey_v2,
                                   size = "large"),
                         cell_borders(sides = c("top", "bottom"),
                                      weight = px(1),
                                      color = ptb_light_grey))) %>%
  tab_style(locations = cells_body(columns = c(eng_w, aus_w)),
            cell_borders(sides = c("left"),
                         weight = px(1.5),
                         color = ptb_light_grey)) %>%
  tab_style(locations = cells_body(columns = c(eng_share, aus_share)),
            cell_fill(color = "#99969610")) %>%
  tab_style(locations = cells_body(columns = c(aus_share)),
            cell_borders(sides = c("left"),
                         weight = px(2.5),
                         color = ptb_light_grey)) %>%
  tab_style(locations = cells_source_notes(),
            style = list(cell_text(align = "right",
                                   color = ptb_mid_grey,
                                   size = "medium"))) %>%
  gt_color_pills(columns = aus_w,
                 digits = 1,
                 pill_height = 25,
                 palette = c("#1F5447", "#D2DCDA"),
                 domain = c(min(team_format_summary$rpw), max(team_format_summary$rpw))) %>%
  gt_color_pills(columns = eng_w,
                 digits = 1,
                 pill_height = 25,
                 palette = c("#1F3D54", "#D2D8DC"),
                 domain = c(min(team_format_summary$rpw), max(team_format_summary$rpw)))
tbl_formatted
