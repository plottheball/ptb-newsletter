library(tidyverse)
library(janitor)
library(googlesheets4)
library(gt)
library(gtUtils)
library(gtExtras)


### box scores downloaded from fiba.basketball and analysed in Google Sheets
eurobasket_data <- read_sheet(ss = "https://docs.google.com/spreadsheets/d/1uhiC592_bZCZ0-Q7quriVG6M7xbxol-nOfWNKuojnYQ/edit?usp=sharing",
                              sheet = "Summary") %>%
  clean_names()

display_table <- eurobasket_data %>%
  gt(rowname_col = "metric") %>%
  cols_width(metric ~ px(275),
             x2017 ~ px(90),
             x2022 ~ px(90),
             x2025 ~ px(90))
display_table

header_colour <- "#79807E"

tbl_formatted <- display_table %>%
  tab_header(title = html("Dončić has scored over a third of Slovenia's points at EuroBasket 2025"),
             subtitle = html("<b>Luka Dončić</b>'s performances for <b>Slovenia</b> in each of the last three editions of <b>EuroBasket</b><br><br>")) %>%
  tab_source_note(source_note = html("<br><br>Data: FIBA | Table: Plot the Ball<br>")) %>%
  tab_stubhead(label = "") %>%
  tab_spanner(label = md("— EUROBASKET —"),
              columns = c(x2017, x2022, x2025)) %>%
  fmt_percent(columns = c(x2017, x2022, x2025), decimals = 1, force_sign = FALSE) %>%
  cols_label(x2017 = "2017",
             x2022 = "2022",
             x2025 = "2025") %>%
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
  tab_style(locations = cells_body(columns = c(x2017)),
            style = list(cell_borders(sides = c("left"),
                                      weight = px(2),
                                      color = ptb_light_grey))) %>%
  tab_style(locations = cells_body(columns = c(x2022, x2025)),
            style = list(cell_borders(sides = c("left"),
                                      weight = px(1.5),
                                      color = ptb_light_grey))) %>%
  data_color(palette = c("#FAFBFF", "#1A2B66"),
           domain = c(0.12, 0.48))
tbl_formatted
