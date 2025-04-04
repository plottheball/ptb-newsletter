library(tidyverse)
library(rvest)
library(janitor)
library(ggtext)
library(scales)
library(gt)
library(gtExtras)

base_url <- "https://stats.espncricinfo.com/ci/engine/stats/index.html?class=6;filter=advanced;orderby=start;page="
end_url <- ";size=200;template=results;trophy=117;type=team;view=innings;wrappertype=print"
pages <- seq(1, 12, 1)

urls <- str_c(base_url, pages, end_url)

all_data <- tibble()

for (i in urls) {
  
  Sys.sleep(3)
  
  print(paste0(str_c("Getting page: ", i)))
  
  html <- read_html(i)
  
  tables <- html %>%
    html_nodes("table") %>%
    html_table()
  
  key_table <- tables[[3]] %>%
    clean_names() %>%
    select(start_date, ground, team, opposition, score, overs)
  
  all_data <- bind_rows(all_data, key_table)
  
}

clean_data <- all_data %>%
  filter(score != "DNB") %>%
  mutate(start_date = dmy(start_date)) %>%
  separate(score,
           into = c("runs", "wkts"),
           sep = "[/]") %>%
  separate(overs,
           into = c("completed_overs", "completed_balls"),
           sep = "[.]") %>%
  mutate(runs = as.integer(runs),
         balls = (replace_na(as.integer(completed_balls),0) + (as.integer(completed_overs) * 6))) %>%
  select(-wkts, -completed_overs, -completed_balls) %>%
  mutate(season = year(start_date),
         .before = "start_date")

clean_data$team <- str_replace_all(clean_data$team, fixed("Kings XI"), "Punjab Kings")
clean_data$opposition <- str_replace_all(clean_data$opposition, fixed("v Kings XI"), "v Punjab Kings")

clean_data$team <- str_replace_all(clean_data$team, fixed("Daredevils"), "DC")
clean_data$opposition <- str_replace_all(clean_data$opposition, fixed("v Daredevils"), "v DC")

clean_data$team <- str_replace_all(clean_data$team, fixed("Daredevils"), "DC")
clean_data$opposition <- str_replace_all(clean_data$opposition, fixed("v Daredevils"), "v DC")

summary_by_season <- clean_data %>%
  group_by(season) %>%
  summarise(no_games = n(),
            tot_runs = sum(runs),
            tot_balls = sum(balls)) %>%
  mutate(runs_per_120 = tot_runs / tot_balls * 120)

summary_by_team_season <- clean_data %>%
  group_by(season, team) %>%
  summarise(no_games = n(),
            tot_runs = sum(runs),
            tot_balls = sum(balls)) %>%
  mutate(runs_per_120 = tot_runs / tot_balls * 120)

team_season_vs_ave <- summary_by_team_season %>%
  left_join(., y = summary_by_season, by = "season", suffix = c("_team", "_comp")) %>%
  select(season, team, runs_per_120_team, runs_per_120_comp) %>%
  mutate(runs_diff = round((runs_per_120_team / runs_per_120_comp - 1) * 100))

for_plot <- team_season_vs_ave %>%
  filter(season < 2026 & season > 2021) %>%
  select(-runs_per_120_team, -runs_per_120_comp) %>%
  pivot_wider(names_from = season,
              values_from = runs_diff) %>%
  clean_names() %>%
  filter(!is.na(x2025)) %>%
  arrange(team)

for_plot$team <- str_replace_all(for_plot$team, "CSK", "Chennai Super Kings")
for_plot$team <- str_replace_all(for_plot$team, "DC", "Delhi Capitals")
for_plot$team <- str_replace_all(for_plot$team, "GT", "Gujarat Titans")
for_plot$team <- str_replace_all(for_plot$team, "KKR", "Kolkata Knight Riders")
for_plot$team <- str_replace_all(for_plot$team, "LSG", "Lucknow Super Giants")
for_plot$team <- str_replace_all(for_plot$team, "MI", "Mumbai Indians")
for_plot$team <- str_replace_all(for_plot$team, "RCB", "Royal Challengers Bengaluru")
for_plot$team <- str_replace_all(for_plot$team, "RR", "Rajasthan Royals")
for_plot$team <- str_replace_all(for_plot$team, "SRH", "Sunrisers Hyderabad")

display_tbl <- for_plot %>%
  gt(rowname_col = "team") %>%
  cols_width(team ~ px(280),
             x2022 ~ px(80),
             x2023 ~ px(80),
             x2024 ~ px(80),
             x2025 ~ px(80))
display_tbl

header_colour <- "#243359"
text_positive <- "#096F52"
fill_positive <- str_c(text_positive, "10")
text_negative <- "#8A2243"
fill_negative <- str_c(text_negative, "10")
fill_mid <- "#79807E05"

tbl_formatted <- display_tbl %>%
  tab_header(title = html("IPL teams' run-scoring rates are inconsistent from season to season"),
             subtitle = html("<b>Scoring rate</b> of IPL teams vs. league average since the 2022 season, with seasons at least 5% <b style='color:#096F52'>above</b> or <b style='color:#8A2243'>below</b> highlighted<br><br>")) %>%
  tab_source_note(source_note = html("<br><i>* Through Thursday April 3rd</i><br><br><br>Data: ESPNCricinfo | Table: Plot the Ball<br>")) %>%
  tab_stubhead(label = "") %>%
  tab_spanner(label = md("— SCORING RATE VS. IPL AVE. —"),
              columns = c(x2022, x2023, x2024, x2025)) %>%
  fmt_percent(columns = c(x2022, x2023, x2024, x2025), force_sign = TRUE, use_seps = TRUE, decimals = 0, scale_values = FALSE) %>%
  cols_label(x2022 = "2022",
             x2023 = "2023",
             x2024 = "2024",
             x2025 = "2025*") %>%
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
                                    color = header_colour,
                                    weight = "bold",
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
  tab_style(locations = cells_body(columns = c(x2023, x2024, x2025)),
            style = list(cell_borders(sides = c("left"),
                                      weight = px(1),
                                      color = ptb_light_grey))) %>%
  tab_style(locations = cells_body(columns = c(x2022, x2023, x2024, x2025)),
            style = list(cell_fill(color = fill_mid))) %>%
  ### 2022 SEASON
  tab_style(locations = cells_body(columns = x2022,
                                   rows = x2022 >= 5),
            style = list(cell_text(color = text_positive,
                                   weight = "bold",
                                   size = "medium"),
                         cell_fill(color = fill_positive))) %>%
  tab_style(locations = cells_body(columns = x2022,
                                   rows = x2022 <= -5),
            style = list(cell_text(color = text_negative,
                                   weight = "bold",
                                   size = "medium"),
                         cell_fill(color = fill_negative))) %>%
  ### 2023 SEASON
  tab_style(locations = cells_body(columns = x2023,
                                   rows = x2023 >= 5),
            style = list(cell_text(color = text_positive,
                                   weight = "bold",
                                   size = "medium"),
                         cell_fill(color = fill_positive))) %>%
  tab_style(locations = cells_body(columns = x2023,
                                   rows = x2023 <= -5),
            style = list(cell_text(color = text_negative,
                                   weight = "bold",
                                   size = "medium"),
                         cell_fill(color = fill_negative))) %>%
  ### 2024 SEASON
  tab_style(locations = cells_body(columns = x2024,
                                   rows = x2024 >= 5),
            style = list(cell_text(color = text_positive,
                                   weight = "bold",
                                   size = "medium"),
                         cell_fill(color = fill_positive))) %>%
  tab_style(locations = cells_body(columns = x2024,
                                   rows = x2024 <= -5),
            style = list(cell_text(color = text_negative,
                                   weight = "bold",
                                   size = "medium"),
                         cell_fill(color = fill_negative))) %>%
  ### 2025 SEASON
  tab_style(locations = cells_body(columns = x2025,
                                   rows = x2025 >= 5),
            style = list(cell_text(color = text_positive,
                                   weight = "bold",
                                   size = "medium"),
                         cell_fill(color = fill_positive))) %>%
  tab_style(locations = cells_body(columns = x2025,
                                   rows = x2025 <= -5),
            style = list(cell_text(color = text_negative,
                                   weight = "bold",
                                   size = "medium"),
                         cell_fill(color = fill_negative)))
tbl_formatted
