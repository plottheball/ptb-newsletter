library(tidyverse)
library(janitor)
library(googlesheets4)
library(gt)
library(gtUtils)
library(gtExtras)

### data imported from: https://www.hockey-reference.com/players/m/mcdavco01.html
regular_season <- read_sheet(ss = "https://docs.google.com/spreadsheets/d/1Dq_0PTurhRb1_sBLc0ZHWBKFy5iuygA4J5JmhYZOt-g/edit?gid=0#gid=0",
                             sheet = "Regular season") %>%
  clean_names()

playoffs <- read_sheet(ss = "https://docs.google.com/spreadsheets/d/1Dq_0PTurhRb1_sBLc0ZHWBKFy5iuygA4J5JmhYZOt-g/edit?gid=0#gid=0",
                             sheet = "Playoffs") %>%
  clean_names()

final_data <- full_join(regular_season, playoffs, suffix = c("_regular", "_post"), by = c("season", "age", "team", "lg")) %>%
  select(1, 2, 3, 4, 5, 6, 7, 11, 12, 16, 17, 18, 22, 23) %>%
  mutate(category = case_when(age < 25 ~ "2020-21 and prior",
                              TRUE ~ season),
         .before = season)

tidy_data <- final_data %>%
  group_by(category) %>%
  summarise(tot_es_toi_reg = sum(es_toi_regular),
            tot_es_sog_reg = sum(es_sog_regular),
            tot_es_toi_post = sum(es_toi_post),
            tot_es_sog_post = sum(es_sog_post),
            tot_pp_toi_reg = sum(pp_toi_regular),
            tot_pp_sog_reg = sum(pp_sog_regular),
            tot_pp_toi_post = sum(pp_toi_post),
            tot_pp_sog_post = sum(pp_sog_post))

career_data <- final_data %>%
  group_by(team) %>%
  summarise(tot_es_toi_reg = sum(es_toi_regular),
            tot_es_sog_reg = sum(es_sog_regular),
            tot_es_toi_post = sum(es_toi_post),
            tot_es_sog_post = sum(es_sog_post),
            tot_pp_toi_reg = sum(pp_toi_regular),
            tot_pp_sog_reg = sum(pp_sog_regular),
            tot_pp_toi_post = sum(pp_toi_post),
            tot_pp_sog_post = sum(pp_sog_post)) %>%
  mutate(team = "Career") %>%
  rename("category" = "team")

tidy_data <- bind_rows(tidy_data, career_data)

for_table <- tidy_data %>%
  mutate(reg_es_sog_p60 = tot_es_sog_reg / tot_es_toi_reg * 60,
         post_es_sog_p60 = tot_es_sog_post / tot_es_toi_post * 60,
         es_diff = post_es_sog_p60 - reg_es_sog_p60,
         reg_pp_sog_p60 = tot_pp_sog_reg / tot_pp_toi_reg * 60,
         post_pp_sog_p60 = tot_pp_sog_post / tot_pp_toi_post * 60,
         pp_diff = post_pp_sog_p60 - reg_pp_sog_p60) %>%
  select(1, 10, 11, 12, 13, 14, 15)

display_table <- for_table %>%
  gt(rowname_col = "category") %>%
  cols_width(category ~ px(175),
             reg_es_sog_p60 ~ px(75),
             post_es_sog_p60 ~ px(75),
             es_diff ~ px(75),
             reg_pp_sog_p60 ~ px(75),
             post_pp_sog_p60 ~ px(75),
             pp_diff ~ px(75))
display_table

header_colour <- "#79807E"
text_positive <- "#CC683D"
text_negative <- "#264B80"
fill_positive <- str_c(text_positive, "10")
fill_negative <- str_c(text_negative, "10")
text_positive_other <- str_c(text_positive, "70")
text_negative_other <- str_c(text_negative, "70")

tbl_formatted <- display_table %>%
  tab_header(title = html("McDavid is getting <b style='color:#CC683D'>more</b> even-strength shots this postseason, but <b style='color:#264B80'>fewer</b> on the PP"),
             subtitle = html("<b>Shots on goal</b> recorded <b>per 60 minutes</b> of even-strength and power-play ice-time by <b>Connor McDavid</b> in his NHL career<br><br>")) %>%
  tab_source_note(source_note = html("<br>'Reg.' = regular season | 'Post.' = postseason<br><br>Data: Hockey Reference | Table: Plot the Ball<br>")) %>%
  tab_stubhead(label = "") %>%
  tab_spanner(label = md("— EVEN STRENGTH —"),
              columns = c(reg_es_sog_p60, post_es_sog_p60, es_diff)) %>%
  tab_spanner(label = md("— POWER PLAY —"),
              columns = c(reg_pp_sog_p60, post_pp_sog_p60, pp_diff)) %>%
  fmt_number(columns = c(reg_es_sog_p60, post_es_sog_p60, reg_pp_sog_p60, post_pp_sog_p60), decimals = 1, force_sign = FALSE) %>%
  fmt_number(columns = c(es_diff, pp_diff), use_seps = TRUE, decimals = 1, force_sign = TRUE) %>%
  cols_label(reg_es_sog_p60 = "Reg.",
             post_es_sog_p60 = "Post.",
             es_diff = "+/-",
             reg_pp_sog_p60 = "Reg.",
             post_pp_sog_p60 = "Post.",
             pp_diff = "+/-") %>%
  cols_align(align = "right",
             columns = everything()) %>%
  cols_align(align = "left",
             columns = category) %>%
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
  tab_style(locations = cells_body(columns = c(reg_es_sog_p60, reg_pp_sog_p60)),
            style = list(cell_borders(sides = c("left"),
                                      weight = px(2),
                                      color = ptb_light_grey))) %>%
  tab_style(locations = cells_body(columns = c(es_diff, pp_diff)),
            style = list(cell_borders(sides = c("left"),
                                      weight = px(1.5),
                                      color = ptb_light_grey))) %>%
  tab_style(locations = cells_stub(rows = c("Career")),
            style = list(cell_borders(sides = c("top"),
                                      weight = px(2.5),
                                      color = ptb_light_grey_v2),
                         cell_fill(color = ptb_light_grey_v4))) %>%
  tab_style(locations = cells_body(rows = c("Career")),
            style = list(cell_borders(sides = c("top"),
                                      weight = px(2.5),
                                      color = ptb_light_grey_v2),
                         cell_fill(color = ptb_light_grey_v4))) %>%
  tab_style(locations = cells_stub(rows = c("2020-21 and prior")),
            style = list(cell_borders(sides = c("bottom"),
                                      weight = px(2),
                                      color = ptb_light_grey),
                         cell_fill(color = ptb_light_grey_v5))) %>%
  tab_style(locations = cells_body(rows = c("2020-21 and prior")),
            style = list(cell_borders(sides = c("bottom"),
                                      weight = px(2),
                                      color = ptb_light_grey),
                         cell_fill(color = ptb_light_grey_v5))) %>%
  tab_style(locations = cells_stub(rows = c("2021-22", "2022-23", "2023-24", "2024-25")),
            style =  list(cell_text(color = ptb_mid_grey_v2))) %>%
  tab_style(locations = cells_body(rows = c("2021-22", "2022-23", "2023-24", "2024-25")),
            style =  list(cell_text(color = ptb_mid_grey_v2))) %>%
  tab_style(locations = cells_body(columns = c(es_diff),
                                   rows = es_diff > 0),
            style = cell_text(color = text_positive)) %>%
  tab_style(locations = cells_body(columns = c(es_diff),
                                   rows = es_diff < 0),
            style = cell_text(color = text_negative)) %>%
  tab_style(locations = cells_body(columns = c(pp_diff),
                                   rows = pp_diff > 0),
            style = cell_text(color = text_positive)) %>%
  tab_style(locations = cells_body(columns = c(pp_diff),
                                   rows = pp_diff < 0),
            style = cell_text(color = text_negative)) %>%
  tab_style(locations = cells_body(columns = c(es_diff, pp_diff),
                                   rows = c("2020-21 and prior", "Career")),
            style = cell_text(color = text_negative_other)) %>%
  tab_style(locations = cells_body(columns = c(es_diff),
                                   rows = "2024-25"),
            style = list(cell_fill(color = fill_positive))) %>%
  tab_style(locations = cells_body(columns = c(pp_diff),
                                   rows = "2024-25"),
            style = list(cell_fill(color = fill_negative))) %>%
  tab_style(locations = cells_stub(rows = "2024-25"),
            style = cell_text(weight = "bold")) %>%
  tab_style(locations = cells_body(rows = "2024-25"),
            style = cell_text(weight = "bold")) %>%
  tab_style(locations = cells_stub(rows = c("2024-25")),
            style = list(cell_borders(sides = c("top"),
                                      weight = px(2),
                                      color = ptb_light_grey))) %>%
  tab_style(locations = cells_body(rows = c("2024-25")),
            style = list(cell_borders(sides = c("top"),
                                      weight = px(2),
                                      color = ptb_light_grey)))
tbl_formatted
