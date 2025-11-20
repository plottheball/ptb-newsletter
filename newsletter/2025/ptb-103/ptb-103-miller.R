library(tidyverse)
library(janitor)
library(googlesheets4)
library(ggtext)
library(gt)
library(gtUtils)
library(gtExtras)

# data downloaded from: https://theanalyst.com/womens-rugby-world-cup-2025-stats
all_players <- read_sheet(ss = "https://docs.google.com/spreadsheets/d/1BSHeo7ZwioFinn2mqtfp1FgxvoHtXChKNFXN1Brufgw/edit",
                        sheet = "All") %>%
  clean_names()

filtered_players <- all_players %>%
  filter(minutes >= 240) %>%
  arrange(desc(minutes))

attack_effectiveness <- filtered_players %>%
  select(name, team, minutes, carries, metres_gained, defenders_beaten, line_breaks, passes, line_break_assists, offloads) %>%
  mutate(c_p80 = carries / minutes * 80,
         m_per_c = metres_gained / carries,
         lb_rate = carries / line_breaks,
         c_per_db = carries / defenders_beaten,
         offload_rate = carries / offloads) %>%
  arrange(c_per_db) %>%
  head(5) %>%
  select(name, team, c_per_db, lb_rate, offload_rate)

display_tbl <- attack_effectiveness %>%
  gt(rowname_col = "name",
     groupname_col = NA) %>%
  cols_width(name ~ px(240),
             team ~ px(65),
             c_per_db ~ px(105),
             lb_rate ~ px(100),
             offload_rate ~ px(105))
display_tbl

######
# 'JM MADE DEFENDERS MISS LIKE ONE OF THE WORLD'S BEST OUTSIDE BACKS'
# USE COLOUR FROM BFS KICKING PIECE

header_colour <- "#79807E"

tbl_formatted <- display_tbl %>%
  tab_header(title = html("<b style='color:#598074'>Jorja Miller</b> made RWC defenders miss like one of the world's best outside backs"),
             subtitle = html("Most effective ball-carriers among the four teams which qualified for the semi-finals of the 2025 Rugby World Cup<br><br>")) %>%
  tab_source_note(source_note = html("<br><i>Players with <240 tournament minutes played excluded</i><br><br>Data: Opta Analyst | Table: Plot the Ball<br>")) %>%
  tab_spanner(label = md("— CARRIES MADE PER: —"),
              columns = c(c_per_db, lb_rate, offload_rate)) %>%
  fmt_number(columns = c(c_per_db, lb_rate, offload_rate), use_seps = TRUE, decimals = 1) %>%
  cols_label(team = "",
             c_per_db = "Defender beaten ▲",
             lb_rate = "Line break",
             offload_rate = "Completed offload") %>%
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
  tab_style(locations = cells_stub(rows = "Jorja Miller"),
            style =  list(cell_text(color = "#598074",
                                    weight = "bold"),
                          cell_fill(color = "#59807410"))) %>%
  tab_style(locations = cells_body(rows = "Jorja Miller"),
            style =  list(cell_text(color = "#598074",
                                    weight = "bold"),
                          cell_fill(color = "#59807410"))) %>%
  tab_style(locations = cells_body(columns = c(c_per_db)),
            style =  list(cell_borders(sides = c("left"),
                                       weight = px(1.5),
                                       color = ptb_light_grey))) %>%
  tab_style(locations = cells_body(columns = c(lb_rate, offload_rate)),
            style =  list(cell_borders(sides = c("left"),
                                       weight = px(1),
                                       color = ptb_light_grey)))
tbl_formatted
