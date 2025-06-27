library(tidyverse)
library(rvest)
library(janitor)
library(gt)
library(gtExtras)
library(gtUtils)

url_base <- "https://stats.espncricinfo.com/ci/engine/stats/index.html?class=1;filter=advanced;host=1;orderby=start;page="
pages <- seq(1, 4, 1)
url_end <- ";size=200;spanmin1=1+Jan+2000;spanval1=span;template=results;type=team;view=innings;wrappertype=print"

urls <- str_c(url_base, pages, url_end)

all_data <- tibble()

for (i in urls) {
  
  Sys.sleep(3)
  
  html <- read_html(i)
  
  tables <- html %>%
    html_nodes("table") %>%
    html_table()
  
  key_table <- tables[[3]]
  
  all_data <- bind_rows(all_data, key_table)
  
}

clean_data <- all_data %>%
  clean_names() %>%
  select(1, 2, 6, 7, 9, 10, 11) %>%
  mutate(match_id = str_c(ground, " (", start_date, ")"),
         start_date = dmy(start_date),
         year = year(start_date)) %>%
  separate(score, sep = "[/]", into = c("runs", "wkts")) %>%
  mutate(runs = as.integer(runs),
         wkts = as.integer(str_remove_all(wkts, fixed("d"))),
         category = case_when(year > 2021 ~ "2022 to 2025",
                              year > 2017 ~ "2018 to 2021",
                              year > 2013 ~ "2014 to 2017",
                              year > 2009 ~ "2010 to 2013",
                              year > 2005 ~ "2006 to 2009",
                              year > 2001 ~ "2002 to 2005",
                              TRUE ~ "Other")) %>%
  filter(category != "Other") %>%
  filter(team != "Bangladesh" & opposition != "v Bangladesh") %>%
  filter(team != "Ireland" & opposition != "v Ireland") %>%
  filter(team != "Zimbabwe" & opposition != "v Zimbabwe")

clean_data$wkts <- clean_data$wkts %>%
  replace_na(10)

category_summary <- clean_data %>%
  group_by(category) %>%
  summarise(tot_runs = sum(runs),
            tot_wkts = sum(wkts)) %>%
  mutate(rpw_overall = tot_runs / tot_wkts) %>%
  select(-tot_runs, -tot_wkts)

category_inns_summary <- clean_data %>%
  group_by(category, inns) %>%
  summarise(tot_runs = sum(runs),
            tot_wkts = sum(wkts)) %>%
  mutate(rpw_inns = tot_runs / tot_wkts) %>%
  select(-tot_runs, -tot_wkts) %>%
  pivot_wider(names_from = "inns", values_from = rpw_inns, names_prefix = "rpw_inns_") %>%
  full_join(., category_summary, by = "category") %>%
  mutate(rel_1st = rpw_inns_1 / rpw_overall - 1,
         rel_2nd = rpw_inns_2 / rpw_overall - 1,
         rel_3rd = rpw_inns_3 / rpw_overall - 1,
         rel_4th = rpw_inns_4 / rpw_overall - 1)

match_count <- clean_data %>%
  select(match_id, category) %>%
  unique() %>%
  group_by(category) %>%
  summarise(count = n())

recent_4th_inns <- clean_data %>%
  filter(year > 2021 & inns == 4)

final_for_table <- category_inns_summary %>%
  select(-2, -3, -4, -5) %>%
  left_join(., y = match_count, by = "category") %>%
  relocate(count, .after = "category")
### PERIOD | TESTS | RPW | RPW VS. OVERALL AVE. BY INNS. > 1ST | 2ND | 3RD | 4TH
### Excludes games involving Bangladesh, Ireland and Zimbabwe

display_tbl <- final_for_table %>%
  gt(rowname_col = "category", groupname_col = NA) %>%
  cols_width(category ~ px(150),
             count ~ px(75),
             rpw_overall ~ px(75),
             rel_1st ~ px(75),
             rel_2nd ~ px(75),
             rel_3rd ~ px(75),
             rel_4th ~ px(75))
display_tbl

header_colour <- "#002d5a"
text_positive <- "#096F52"
fill_positive <- str_c(text_positive, "10")
text_negative <- "#8A2243"
fill_negative <- str_c(text_negative, "10")
fill_mid <- "#79807E05"

tbl_formatted <- display_tbl %>%
  tab_header(title = html("Fourth-innings run-scoring has taken off in English tests in the last four years"),
             subtitle = html("<b>Runs scored per wicket</b> in each innings of <b>men's test matches in England</b> since 2002<br><br>")) %>%
  tab_source_note(source_note = html("<br>'RPW' = runs scored per wicket | Excludes games involving BAN, IRE and ZIM<br><br><br>Data: ESPNCricinfo | Table: Plot the Ball<br>")) %>%
  tab_stubhead(label = "") %>%
  tab_spanner(label = md("— RPW BY INNS. VS. AVE. —"),
              columns = c(rel_1st, rel_2nd, rel_3rd, rel_4th)) %>%
  fmt_number(columns = c(rpw_overall), decimals = 1) %>%
  fmt_percent(columns = c(rel_1st, rel_2nd, rel_3rd, rel_4th), force_sign = TRUE, use_seps = TRUE, decimals = 0, scale_values = TRUE) %>%
  cols_label(count = "Tests",
             rpw_overall = "RPW",
             rel_1st = "1st",
             rel_2nd = "2nd",
             rel_3rd = "3rd",
             rel_4th = "4th") %>%
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
            style =  list(cell_text(align = "center",
                                    color = header_colour,
                                    weight = "normal",
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
  tab_style(locations = cells_body(columns = c(rpw_overall, rel_2nd, rel_3rd, rel_4th)),
            style = list(cell_borders(sides = c("left"),
                                      weight = px(1),
                                      color = ptb_light_grey))) %>%
  tab_style(locations = cells_body(columns = c(rel_1st)),
            style = list(cell_borders(sides = c("left"),
                                      weight = px(2),
                                      color = ptb_light_grey))) %>%
  tab_style(locations = cells_body(columns = c(rel_1st, rel_2nd, rel_3rd, rel_4th)),
            style = list(cell_fill(color = fill_mid))) %>%
  tab_style(locations = cells_body(columns =  rpw_overall),
            style =  list(cell_text(color = header_colour,
                                    weight = "bold"))) %>%
  ### 1ST INNS
  tab_style(locations = cells_body(columns = rel_1st,
                                   rows = rel_1st >= 0.05),
            style = list(cell_text(color = text_positive,
                                   weight = "bold",
                                   size = "medium"),
                         cell_fill(color = fill_positive))) %>%
  tab_style(locations = cells_body(columns = rel_1st,
                                   rows = rel_1st <= -0.05),
            style = list(cell_text(color = text_negative,
                                   weight = "bold",
                                   size = "medium"),
                         cell_fill(color = fill_negative))) %>%
  ### 2ND INNS
  tab_style(locations = cells_body(columns = rel_2nd,
                                   rows = rel_2nd >= 0.05),
            style = list(cell_text(color = text_positive,
                                   weight = "bold",
                                   size = "medium"),
                         cell_fill(color = fill_positive))) %>%
  tab_style(locations = cells_body(columns = rel_2nd,
                                   rows = rel_2nd <= -0.05),
            style = list(cell_text(color = text_negative,
                                   weight = "bold",
                                   size = "medium"),
                         cell_fill(color = fill_negative))) %>%
  ### 3RD INNS
  tab_style(locations = cells_body(columns = rel_3rd,
                                   rows = rel_3rd >= 0.05),
            style = list(cell_text(color = text_positive,
                                   weight = "bold",
                                   size = "medium"),
                         cell_fill(color = fill_positive))) %>%
  tab_style(locations = cells_body(columns = rel_3rd,
                                   rows = rel_3rd <= -0.05),
            style = list(cell_text(color = text_negative,
                                   weight = "bold",
                                   size = "medium"),
                         cell_fill(color = fill_negative))) %>%
  ### 4TH INNS
  tab_style(locations = cells_body(columns = rel_4th,
                                   rows = rel_4th >= 0.05),
            style = list(cell_text(color = text_positive,
                                   weight = "bold",
                                   size = "medium"),
                         cell_fill(color = fill_positive))) %>%
  tab_style(locations = cells_body(columns = rel_4th,
                                   rows = rel_4th <= -0.05),
            style = list(cell_text(color = text_negative,
                                   weight = "bold",
                                   size = "medium"),
                         cell_fill(color = fill_negative)))
tbl_formatted