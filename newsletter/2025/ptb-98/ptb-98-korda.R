library(tidyverse)
library(janitor)
library(googlesheets4)
library(gt)
library(gtUtils)
library(gtExtras)


### box scores downloaded from lpga.com and analysed in Google Sheets
lpga_data <- read_sheet(ss = "https://docs.google.com/spreadsheets/d/126cXNqiIzaT__AOs__Sv5xB1Txq2VuXXJF89HTdR2lc/edit?usp=sharing",
                              sheet = "Table") %>%
  clean_names()

display_table <- lpga_data %>%
  gt(rowname_col = "year") %>%
  cols_width(year ~ px(75),
             starts ~ px(75),
             win ~ px(90),
             top_10 ~ px(90),
             top_20 ~ px(90),
             x21 ~ px(90),
             cut ~ px(90))
display_table

header_colour <- "#79807E"

tbl_formatted <- display_table %>%
  tab_header(title = html("In 17 starts on the LPGA Tour this year, Korda has neither won nor missed a cut"),
             subtitle = html("<b>Nelly Korda</b>'s finishes in tournaments on the <b>LPGA Tour</b> in each year since 2017<br><br>")) %>%
  tab_source_note(source_note = html("<br>Wins excluded from 'Top 10' | Wins and Top 10s excluded from 'Top 20'<br><br>Data: LPGA.com | Table: Plot the Ball<br>")) %>%
  tab_stubhead(label = "") %>%
  tab_spanner(label = md("— FINISHES AS % OF STARTS —"),
              columns = c(win, top_10, top_20, x21, cut)) %>%
  fmt_percent(columns = c(win, top_10, top_20, x21, cut), decimals = 0, force_sign = FALSE) %>%
  cols_label(starts = "Starts",
             win = "Win",
             top_10 = "Top 10",
             top_20 = "Top 20",
             x21 = "21st +",
             cut = "Cut") %>%
  cols_align(align = "center",
             columns = everything()) %>%
  cols_align(align = "center",
             columns = c(year, starts)) %>%
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
  tab_style(locations = cells_body(columns = c(starts)),
            style = list(cell_borders(sides = c("left"),
                                      weight = px(2),
                                      color = ptb_light_grey))) %>%
  tab_style(locations = cells_body(columns = c(win)),
            style = list(cell_borders(sides = c("left"),
                                      weight = px(1.5),
                                      color = ptb_light_grey))) %>%
  data_color(columns = c(win, top_10, top_20, x21, cut),
             palette = c("#FAFFFD", "#064029"),
             domain = c(0.00, 0.8))
tbl_formatted
