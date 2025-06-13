library(tidyverse)
library(rvest)
library(janitor)
library(gt)
library(gtExtras)
library(gtUtils)

seasons <- seq(1997, 2025, 1)

all_scoring <- tibble()

for (i in seasons) {
  
  Sys.sleep(5)
  
  url <- str_c("https://www.basketball-reference.com/wnba/years/", i, ".html")
  
  html <- read_html(url)
  
  tables <- html %>%
    html_nodes("table") %>%
    html_table()
  
  scoring_table <- tables[[4]] %>%
    clean_names() %>%
    mutate(year = i)
  
  all_scoring <- bind_rows(all_scoring, scoring_table)
  
}

scoring_clean <- all_scoring %>%
  select(year, team, g, pts, x3pa, fga, x3p_percent, fg) %>%
  filter(!is.na(pts)) %>%
  mutate(team = str_remove_all(team, fixed("*")),
         x3p_share = x3pa / fga) %>%
  arrange(desc(x3p_share)) %>%
  head(10) %>%
  select(-g, -x3pa, -fga, -fg) %>%
  relocate(x3p_percent,
           pts,
           .after = "x3p_share")

display_table <- scoring_clean %>%
  gt(rowname_col = "team") %>%
  cols_width(team ~ px(200),
             year ~ px(80),
             x3p_share ~ px(110),
             pts ~ px(85),
             x3p_percent ~ px(85))
display_table

header_colour <- "#79807E"
text_liberty <- "#5C9988"
text_liberty_other <- "#74B3A1"
fill_liberty <- str_c(text_liberty, "20")
fill_liberty_other <- str_c(text_liberty_other, "10")

tbl_formatted <- display_table %>%
  tab_header(title = html("The <b style='color:#5C9988'>Liberty</b> are the modern WNBA's leading three-point shooting team"),
             subtitle = html("Teams with the highest proportion of their field-goal attempts from <b>three-point range</b> in WNBA history<br><br>")) %>%
  tab_source_note(source_note = html("<br>'3PA/FGA' = three-point shot attempts as % of all field-goal attempts<br>'3P%' = three-point shots made as % of attempts | 'PPG' = points per game<br><br><br>Data: Basketball Reference | Table: Plot the Ball<br>")) %>%
  tab_stubhead(label = "") %>%
  fmt_number(columns = c(pts), decimals = 1, force_sign = FALSE) %>%
  fmt_percent(columns = c(x3p_share, x3p_percent), force_sign = FALSE, use_seps = TRUE, decimals = 1, scale_values = TRUE) %>%
  cols_label(year = "Season",
             pts = "PPG",
             x3p_share = "▼ 3PA/FGA",
             x3p_percent = "3P %") %>%
  cols_align(align = "right",
             columns = everything()) %>%
  cols_align(align = "center",
             columns = "year") %>%
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
  tab_style(locations = cells_body(columns = c(x3p_share)),
            style = list(cell_borders(sides = c("left"),
                                      weight = px(2),
                                      color = ptb_light_grey))) %>%
  tab_style(locations = cells_body(columns = c(pts, x3p_percent)),
            style = list(cell_borders(sides = c("left"),
                                      weight = px(1.5),
                                      color = ptb_light_grey))) %>%
  tab_style(locations = cells_stub(rows = team == "New York Liberty" & year == 2025),
            style = list(cell_text(color = text_liberty,
                                   weight = "bold"),
                         cell_fill(color = fill_liberty_other))) %>%
  tab_style(locations = cells_body(rows = team == "New York Liberty" & year == 2025),
            style = list(cell_text(color = text_liberty,
                                   weight = "bold"),
                         cell_fill(color = fill_liberty_other))) %>%
  tab_style(locations = cells_stub(rows = team == "New York Liberty" & year != 2025),
            style = list(cell_text(color = text_liberty_other),
                         cell_fill(color = fill_liberty_other))) %>%
  tab_style(locations = cells_body(rows = team == "New York Liberty" & year != 2025),
            style = list(cell_text(color = text_liberty_other),
                         cell_fill(color = fill_liberty_other)))
tbl_formatted
