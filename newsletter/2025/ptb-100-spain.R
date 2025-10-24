library(tidyverse)
library(janitor)
library(StatsBombR)
library(gt)
library(gtUtils)
library(gtExtras)

comp <- FreeCompetitions() %>%
  filter(competition_name == "UEFA Women's Euro" | competition_name == "Women's World Cup")
matches <- FreeMatches(comp)

match_dates <- matches %>%
  select(match_id, match_date) %>%
  mutate(match_year = year(match_date))

games_by_team <- matches %>%
  clean_names() %>%
  select(1, 2, 17, 24) %>%
  pivot_longer(cols = c("home_team_home_team_name", "away_team_away_team_name"))

team_summary <- games_by_team %>%
  group_by(value) %>%
  summarise(no_matches = n())

statsbomb_data <- free_allevents(MatchesDF = matches, Parallel = T)
clean_data <- allclean(statsbomb_data)

all_passes <- clean_data %>%
  clean_names() %>%
  filter(type_name == "Pass")

all_passes$pass_outcome_name <- replace_na(all_passes$pass_outcome_name, "Complete")
all_passes$under_pressure <- replace_na(all_passes$under_pressure, FALSE)

all_passes_tidy <- all_passes

pressure_passes <- all_passes_tidy %>%
  filter(under_pressure == TRUE)

labels <- tibble(match_year = c(2019, 2022, 2023, 2025),
                 label = c("WC 2019", "EU 2022", "WC 2023", "EU 2025"))

pressure_passes_tidy <- pressure_passes %>%
  mutate(complete_value = if_else(pass_outcome_name == "Complete", 1, 0),
         complete_distance = if_else(pass_outcome_name == "Complete", pass_length, 0),
         .after = pass_outcome_name) %>%
  left_join(., y = match_dates, by = ("match_id" = "match_id")) %>%
  left_join(., y = labels, by = ("match_year" = "match_year"))

pressure_by_team <- pressure_passes_tidy %>%
  group_by(team_name, label) %>%
  summarise(count = n(),
            complete = sum(complete_value),
            distance = sum(complete_distance)) %>%
  mutate(completion_rate = complete / count,
         ave_complete_dist = distance / complete)

count_by_team <- pressure_by_team %>%
  group_by(team_name) %>%
  summarise(count = n())

top_teams <- count_by_team %>%
  filter(count == 4) %>%
  select(team_name) %>%
  unique() %>%
  unlist()

pressure_filtered <- pressure_by_team %>%
  filter(team_name %in% top_teams) %>%
  select(team_name, label, completion_rate)

pressure_final <- pressure_filtered %>%
  pivot_wider(names_from = "label",
              values_from = "completion_rate") %>%
  clean_names() %>%
  arrange(desc(eu_2025))

pressure_final$team_name <- str_remove_all(pressure_final$team_name, " Women's")

display_table <- pressure_final %>%
  gt(rowname_col = "team_name",
     groupname_col = NA) %>%
  cols_width(team_name ~ px(150),
             wc_2019 ~ px(110),
             eu_2022 ~ px(110),
             wc_2023 ~ px(110),
             eu_2025 ~ px(120))
display_table

header_colour <- "#79807E"

tbl_formatted <- display_table %>%
  tab_header(title = html("At Euro 2025, Spain kept the ball better than any other top side when pressured"),
             subtitle = html("<b>Completion rate</b> of <b>passes under pressure</b> recorded by <b>European national teams</b> at major tournaments since 2019<br><br>")) %>%
  tab_source_note(source_note = html("<br><br>Data: Hudl Statsbomb | Table: Plot the Ball<br>")) %>%
  tab_stubhead(label = "") %>%
  tab_spanner(label = md("— PASS COMPLETION % UNDER PRESSURE —"),
              columns = c(wc_2019, eu_2022, wc_2023, eu_2025)) %>%
  fmt_percent(columns = c(wc_2019, eu_2022, wc_2023, eu_2025), decimals = 0, force_sign = FALSE) %>%
  cols_label(wc_2019 = "WC 2019",
             eu_2022 = "Euro 2022",
             wc_2023 = "WC 2023",
             eu_2025 = "▼ Euro 2025") %>%
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
                                    color = ptb_mid_grey_v2,
                                    size = "large"),
                          cell_borders(sides = c("top", "bottom"),
                                       weight = px(1),
                                       color = ptb_light_grey))) %>%
  tab_style(locations = cells_body(),
            style = list(cell_text(color = "#FFFFFF",
                                   size = "large"),
                         cell_borders(sides = c("top", "bottom"),
                                      weight = px(1),
                                      color = ptb_light_grey))) %>%
  tab_style(locations = cells_source_notes(),
            style = list(cell_text(align = "right",
                                   color = ptb_mid_grey,
                                   size = "medium"))) %>%
  tab_style(locations = cells_body(columns = c(wc_2019)),
            style = list(cell_borders(sides = c("left"),
                                      weight = px(2),
                                      color = ptb_light_grey))) %>%
  tab_style(locations = cells_body(columns = c(eu_2025)),
            style = list(cell_borders(sides = c("left"),
                                      weight = px(1.5),
                                      color = ptb_light_grey))) %>%
  data_color(columns = c(wc_2019, eu_2022, wc_2023, eu_2025),
             palette = c("#F2EDEF", "#80666C"),
             domain = c(0.55, 0.95),
             autocolor_text = FALSE) %>%
  data_color(columns = c(eu_2025),
             palette = c("#F2E6E9", "#800D28"),
             domain = c(0.55, 0.95),
             autocolor_text = FALSE) %>%
  tab_style(locations = cells_stub(rows = "Spain"),
            style =  list(cell_text(color = ptb_dark_grey,
                                    weight = "bold"),
                          cell_fill(color = "#bcbcbc10"))) %>%
  tab_style(locations = cells_body(columns = c(eu_2025),
                                   rows = "Spain"),
            style = list(cell_text(weight = "bold")))
tbl_formatted