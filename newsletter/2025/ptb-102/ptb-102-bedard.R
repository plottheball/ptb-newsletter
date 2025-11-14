library(tidyverse)
library(janitor)
library(gt)
library(gtUtils)
library(gtExtras)

### https://evolving-hockey.com/stats/skater_standard/?_inputs_&std_sk_table=%22Box%20Score%22&std_sk_team=%22All%22&std_sk_pos=%22F%22&std_sk_str=%22All%22&std_sk_adj=%22No%20Adjustment%22&std_sk_type=%22Totals%22&std_sk_range=%22Seasons%22&std_sk_span=%22Regular%22&std_sk_group=%22Team%2C%20Season%22&std_sk_age1=%2217%22&std_sk_age2=%2250%22&std_sk_dft_yr=%22All%22&std_sk_status=%22All%22&std_sk_info=%22No%22&std_sk_players=%5B%22Alexis%20Lafreni%C3%A8re%22%2C%22Auston%20Matthews%22%2C%22Connor%20Bedard%22%2C%22Connor%20McDavid%22%2C%22Jack%20Hughes%22%2C%22John%20Tavares%22%2C%22Juraj%20Slafkovsky%22%2C%22Macklin%20Celebrini%22%2C%22Nail%20Yakupov%22%2C%22Nathan%20MacKinnon%22%2C%22Nico%20Hischier%22%2C%22Patrick%20Kane%22%2C%22Ryan%20Nugent-Hopkins%22%2C%22Steven%20Stamkos%22%2C%22Taylor%20Hall%22%5D&std_sk_toi=%220%22&std_sk_season=%5B%2220252026%22%2C%2220242025%22%2C%2220232024%22%2C%2220222023%22%2C%2220212022%22%2C%2220202021%22%2C%2220192020%22%2C%2220182019%22%2C%2220172018%22%2C%2220162017%22%2C%2220152016%22%2C%2220142015%22%2C%2220132014%22%2C%2220122013%22%2C%2220112012%22%2C%2220102011%22%2C%2220092010%22%2C%2220082009%22%2C%2220072008%22%5D
# data downloaded from: https://evolving-hockey.com/stats/skater_standard/
top_picks <- read_csv("top-picks-shooting.csv") %>%
  clean_names() %>%
  arrange(season)

top_active <- top_picks %>%
  filter(season == "25-26") %>%
  select(player) %>%
  unique() %>%
  unlist()

first_three_seasons <- tibble()
later_seasons <- tibble()

for (i in top_active) {
  
  player_filtered <- top_picks %>%
    filter(player == i)
  
  player_first_three <- player_filtered %>%
    head(3) %>%
    mutate(season_index = row_number())
  
  first_three_seasons <- bind_rows(first_three_seasons, player_first_three)
  
  if(nrow(player_filtered) < 4) {
    
    next
    
  }
  
  player_later <- player_filtered[4:nrow(player_filtered), ]
  
  later_seasons <- bind_rows(later_seasons, player_later)
  
}

first_three_summary <- first_three_seasons %>%
  group_by(player) %>%
  summarise(no_seasons = n(),
            tot_g = sum(g),
            tot_xg = sum(ix_g)) %>%
  mutate(g_vs_xg = (tot_g / tot_xg) - 1)

later_summary <- later_seasons %>%
  group_by(player) %>%
  summarise(no_seasons = n(),
            tot_g = sum(g),
            tot_xg = sum(ix_g)) %>% 
  mutate(g_vs_xg = (tot_g / tot_xg) - 1)

shooting_comparison <- first_three_summary %>%
  left_join(., y = later_summary,
            by = "player",
            suffix = c("_first_three", "_later")) %>%
  arrange(desc(no_seasons_later)) %>%
  filter(no_seasons_later > 3) %>%
  select(-no_seasons_first_three, -no_seasons_later)

display_tbl <- shooting_comparison %>%
  arrange(desc(g_vs_xg_later)) %>%
  gt(rowname_col = "player",
     groupname_col = NA) %>%
  cols_width(player ~ px(200),
             tot_g_first_three ~ px(60),
             tot_xg_first_three ~ px(60),
             g_vs_xg_first_three ~ px(80),
             tot_g_later ~ px(60),
             tot_xg_later ~ px(60),
             g_vs_xg_later ~ px(80))
display_tbl

header_colour <- "#79807E"

tbl_formatted <- display_tbl %>%
  tab_header(title = html("For top picks, early-career finishing is a pretty good guide to future performance"),
             subtitle = html("Finishing performance of active NHL forwards selected first overall in the Draft between 2007 and 2019, by career period<br><br>")) %>%
  tab_source_note(source_note = html("<br>'G' = goals scored | 'xG' = expected goals (excluding blocked attempts)<br><br>Data: Evolving Hockey | Table: Plot the Ball<br>")) %>%
  tab_spanner(label = md("— YEARS 1 TO 3 —"),
              columns = c(tot_g_first_three, tot_xg_first_three, g_vs_xg_first_three)) %>%
  tab_spanner(label = md("— LATER YEARS —"),
              columns = c(tot_g_later, tot_xg_later, g_vs_xg_later)) %>%
  fmt_number(columns = c(tot_g_first_three, tot_g_later), use_seps = TRUE, decimals = 0) %>%
  fmt_number(columns = c(tot_xg_first_three, tot_xg_later), use_seps = TRUE, decimals = 1) %>%
  fmt_percent(columns = c(g_vs_xg_first_three, g_vs_xg_later), force_sign = TRUE, decimals = 0) %>%
  cols_label(tot_g_first_three = "G",
             tot_xg_first_three = "xG",
             g_vs_xg_first_three = "+/-",
             tot_g_later = "G",
             tot_xg_later = "xG",
             g_vs_xg_later = "▼ +/-") %>%
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
  tab_style(locations = cells_body(columns = c(g_vs_xg_first_three, g_vs_xg_later)),
            style = list(cell_borders(sides = c("left"),
                                      weight = px(1.5),
                                      color = ptb_light_grey),
                         cell_text(color = "#fcfcfc"))) %>%
  tab_style(locations = cells_body(columns = c(tot_g_first_three, tot_g_later)),
            style = list(cell_borders(sides = c("left"),
                                      weight = px(2),
                                      color = ptb_light_grey))) %>%
  data_color(columns = c(g_vs_xg_first_three, g_vs_xg_later),
             palette = c("#3663B3", "#d5d5d5", "#B34A36"),
             domain = c(-0.5, 0.5),
             autocolor_text = FALSE)
tbl_formatted
