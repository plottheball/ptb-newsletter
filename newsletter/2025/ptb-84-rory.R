library(tidyverse)
library(janitor)
library(googlesheets4)
library(gt)
library(gtExtras)
library(gtUtils)

### data downloaded from: https://datagolf.com/player-profiles?dg_id=10091
data_url <- "https://docs.google.com/spreadsheets/d/12x51TGjZtiyte8lTcUlrD0x7NqeP2jjlzJutcAnznBE/edit?usp=sharing"

major_data <- read_sheet(data_url,
                         sheet = "Clean") %>%
  clean_names()

major_clean <- major_data %>%
  separate(tournament,
           sep = "[—]",
           into = c("tournament", "date")) %>%
  mutate(tournament = str_remove(tournament, fixed("MAJ ")),
         date = mdy(date))

major_clean$tournament <- str_remove_all(major_clean$tournament, fixed(" (Covid-Delayed)"))
major_clean$tournament <- str_replace_all(major_clean$tournament, fixed("Masters Tournament"), "Masters")
major_clean$tournament <- str_replace_all(major_clean$tournament, fixed("The Masters"), "Masters")
major_clean$tournament <- str_replace_all(major_clean$tournament, fixed("The Open Championship"), "Open Champ.")
major_clean$tournament <- str_replace_all(major_clean$tournament, fixed("PGA Championship"), "PGA Champ.")
major_clean$tournament <- str_replace_all(major_clean$tournament, fixed("U.S. Open"), "US Open")

for_table <- major_clean %>%
  arrange(desc(total_true_sg_round)) %>%
  head(10) %>%
  select(-date, -winner_name) %>%
  select(tournament, winner_total_true_sg_round, total_true_sg_round, finishing_position)

for_table$finishing_position <- str_replace_all(for_table$finishing_position, fixed("2"), "2nd")
for_table$finishing_position <- str_replace_all(for_table$finishing_position, fixed("1"), "1st")
for_table$finishing_position <- str_replace_all(for_table$finishing_position, fixed("3"), "3rd")
for_table$finishing_position <- str_replace_all(for_table$finishing_position, fixed("4"), "4th")
for_table$finishing_position <- str_replace_all(for_table$finishing_position, fixed("T5"), "5th (tied)")
for_table$finishing_position <- str_replace_all(for_table$finishing_position, fixed("T6"), "6th (tied)")

display_tbl <- for_table %>%
  gt(rowname_col = "tournament") %>%
  cols_width(tournament ~ px(175),
             winner_total_true_sg_round ~ px(125),
             total_true_sg_round ~ px(125),
             finishing_position ~ px(125))
display_tbl

header_colour <- "#79807E"
augusta_colour <- "#378e7a"
highlight_augusta <- str_c(augusta_colour, "20")

tbl_formatted <- display_tbl %>%
  tab_header(title = html("Rory's <b style='color:#378e7a'>Masters win</b> looks a lot like his performances in other recent majors"),
             subtitle = html("Rory McIlroy's 10 best performances by True Strokes-Gained per round in major championships since 2015<br><br>")) %>%
  tab_source_note(source_note = html("<br>'True SG' = strokes gained vs. the field, adjusted for field strength<br><br>Data: Data Golf | Table: Plot the Ball<br>")) %>%
  tab_stubhead(label = "") %>%
  tab_spanner(label = md("— TRUE SG PER ROUND —"),
              columns = c(winner_total_true_sg_round, total_true_sg_round)) %>%
  tab_spanner(label = md("— FINISH —"),
              columns = c(finishing_position)) %>%
  fmt_number(columns = c(winner_total_true_sg_round, total_true_sg_round), use_seps = TRUE, decimals = 1, force_sign = TRUE) %>%
  cols_label(winner_total_true_sg_round = "Winner",
             total_true_sg_round= "▼ McIlroy",
             finishing_position = "McIlroy") %>%
  cols_align(align = "right",
             columns = everything()) %>%
  cols_align(align = "center",
             columns = c(finishing_position)) %>%
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
  tab_style(locations = cells_body(columns = c(finishing_position)),
            style = list(cell_borders(sides = c("left"),
                                      weight = px(2),
                                      color = ptb_light_grey))) %>%
  tab_style(locations = cells_body(columns = c(total_true_sg_round)),
            style = list(cell_borders(sides = c("left"),
                                      weight = px(1),
                                      color = ptb_light_grey))) %>%
  tab_style(locations = cells_stub(rows = tournament == "2025 Masters"),
            style = list(cell_text(color = augusta_colour,
                                   weight = "bold"),
                         cell_fill(color = highlight_augusta))) %>%
  tab_style(locations = cells_body(rows = tournament == "2025 Masters"),
            style = list(cell_text(color = augusta_colour,
                                   weight = "bold"),
                         cell_fill(color = highlight_augusta)))
tbl_formatted