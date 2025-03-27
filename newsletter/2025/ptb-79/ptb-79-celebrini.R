library(tidyverse)
library(janitor)
library(readxl)
library(rvest)
library(ggtext)
library(lubridate)
library(scales)
library(gt)

### DOWNLOADED FROM: https://www.allthreezones.com/
microstats_raw <- read_xlsx("season-totals-sheet.xlsx") %>%
  clean_names()

forwards_since_2023 <- microstats_raw %>%
  filter(pos != "G" & pos != "D" &  (year == "2023-24" | year == "2024-25"))

forwards_summary_5v5 <- forwards_since_2023 %>%
  group_by(player) %>%
  summarise(no_games = n(),
            tot_mins = sum(x5v5_toi),
            tot_zone_entries = sum(zone_entries_29),
            tot_carries = sum(carries_30),
            tot_exits_w_poss = sum(exits_w_possession),
            tot_carried_exits = sum(carried_exits)) %>%
  arrange(desc(tot_mins))

mins_threshold <- 100

forwards_filtered_5v5 <- forwards_summary_5v5 %>%
  filter(tot_mins >= mins_threshold)

forwards_final <- forwards_filtered_5v5 %>%
  mutate(oz_controlled_entries_per_60 = tot_carries / tot_mins * 60,
         dz_successful_exits_per_60 = tot_exits_w_poss / tot_mins * 60)

forwards_final$player <- str_squish(forwards_final$player)

recently_drafted <- c("Connor Bedard",
                      "Leo Carlsson",
                      "Adam Fantilli",
                      "Will Smith",
                      "Macklin Celebrini",
                      "Juraj Slafkovsky",
                      "Logan Cooley",
                      "Shane Wright",
                      "Cutter Gauthier")

recent_data <- forwards_final %>%
  filter(player %in% recently_drafted) %>%
  select(player, no_games, tot_mins, oz_controlled_entries_per_60, dz_successful_exits_per_60) %>%
  mutate(combined_entries_exits = oz_controlled_entries_per_60 + dz_successful_exits_per_60) %>%
  arrange(desc(combined_entries_exits)) %>%
  relocate(dz_successful_exits_per_60, .after = "tot_mins")

display_tbl <- recent_data %>%
  gt(rowname_col = "player",
     groupname_col = NA) %>%
  cols_width(player ~ px(170),
             no_games ~ px(80),
             tot_mins ~ px(80),
             dz_successful_exits_per_60 ~ px(110),
             oz_controlled_entries_per_60 ~ px(110),
             combined_entries_exits ~ px(80))
display_tbl

header_colour <- "#79807E"
chi_colour <- "#B3364A"
san_colour <-  "#007780"
highlight_chi <- str_c(chi_colour, "16")
highlight_san <- str_c(san_colour, "08")

tbl_formatted <- display_tbl %>%
  tab_header(title = html("In a small sample of minutes as a rookie, <b style='color:#007780'>Celebrini</b> looks like a great puck-mover"),
             subtitle = html("Recently drafted NHL forwards' <b>contribution to puck progression</b> <b>at 5v5</b> in a sample of minutes since the start of the 2023-24 season<br><br>")) %>%
  tab_source_note(source_note = html("<br>'DZ exits' = successful exits from the defensive zone with possession<br>'OZ entries' = controlled entries into the offensive zone with possession<br><br><i>Players selected in the top five of the NHL Draft since 2022 only; excludes those with <100 five-on-five minutes tracked over the last two seasons</i><br><br>Data: All Three Zones | Table: Plot the Ball<br>")) %>%
  tab_stubhead(label = "") %>%
  tab_spanner(label = md("— TRACKED —"),
              columns = c(no_games, tot_mins)) %>%
  tab_spanner(label = md("— PER 60 MINS —"),
              columns = c(dz_successful_exits_per_60, oz_controlled_entries_per_60, combined_entries_exits)) %>%
  fmt_number(columns = c(no_games), use_seps = TRUE, decimals = 0) %>%
  fmt_number(columns = c(tot_mins, oz_controlled_entries_per_60, dz_successful_exits_per_60, combined_entries_exits), use_seps = TRUE, decimals = 1) %>%
  cols_label(no_games = "Games",
             tot_mins = "Mins",
             dz_successful_exits_per_60 = "DZ exits",
             oz_controlled_entries_per_60 = "OZ entries",
             combined_entries_exits = "▼ Total") %>%
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
  tab_style(locations = cells_body(columns = c(dz_successful_exits_per_60)),
            style = list(cell_borders(sides = c("left"),
                                      weight = px(2),
                                      color = ptb_light_grey))) %>%
  tab_style(locations = cells_body(columns = c(combined_entries_exits)),
            style = list(cell_borders(sides = c("left"),
                                      weight = px(1.5),
                                      color = ptb_light_grey))) %>%
  tab_style(locations = cells_body(rows = player == "Macklin Celebrini"),
            style = list(cell_text(color = san_colour,
                                   weight = "bold"),
                         cell_fill(color = highlight_san))) %>%
  tab_style(locations = cells_stub(rows = player == "Macklin Celebrini"),
            style = list(cell_text(color = san_colour,
                                   weight = "bold"),
                         cell_fill(color = highlight_san))) %>%
  tab_style(locations = cells_body(rows = player == "Connor Bedard"),
            style = list(cell_text(color = chi_colour,
                                   weight = "bold"),
                         cell_fill(color = highlight_chi))) %>%
  tab_style(locations = cells_stub(rows = player == "Connor Bedard"),
            style = list(cell_text(color = chi_colour,
                                   weight = "bold"),
                         cell_fill(color = highlight_chi)))
tbl_formatted
