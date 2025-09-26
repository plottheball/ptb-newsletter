library(tidyverse)
library(rvest)
library(janitor)
library(gt)
library(gtUtils)
library(gtExtras)

### World Cup: https://www.rugby.com.au/fixtures-results?team=All&comp=249&tab=Results
results_html <- read_html("html/rwc_results.html")
urls <- results_html %>%
  html_nodes("a") %>%
  html_attr("href")

all_matches <- urls[grepl("match-centre", urls)] %>%
  unique()

home_score_xpath <- "/html/body/div[1]/div/main/section[2]/div/div/div/div[1]/div[1]/span"
away_score_xpath <- "/html/body/div[1]/div/main/section[2]/div/div/div/div[1]/div[3]/span"
match_date_xpath <- "/html/body/div[1]/div/main/section[2]/div/div/div/div[2]/ul/li[1]/span"
location_xpath <- "/html/body/div[1]/div/main/section[2]/div/div/div/div[2]/ul/li[2]/span"
referee_xpath <- "/html/body/div[1]/div/main/section[2]/div/div/div/div[2]/ul/li[3]/span"

key_stats <- c("Metres", "Carries", "Defenders Beaten", "Clean Breaks", "Passes", "Offloads", "Turnovers Conceded", "Tackles", "Missed Tackles", "Turnovers Won", "Kicks in Play", "Kick From Hand Metres", "Rucks Won", "Rucks Lost", "Penalties Conceded", "Penalty Conceded Own Half", "Yellow Cards", "Red Cards")

all_data <- tibble()

for (i in all_matches) {
  
  print(paste0("Getting match: ", i))
  
  Sys.sleep(1)
  
  stats_url <- str_c(i, "?tab=Match-Stats")
  
  stats_html <- read_html(stats_url)
  
  match_date <- stats_html %>%
    html_nodes(xpath = match_date_xpath) %>%
    html_text() %>%
    parse_date_time("a , b d, y, I:M p")
  
  if(length(match_date) == 0) {
    
    next
    
  }
  
  if(match_date > today()) {
    
    next
    
  }
  
  if(match_date > today()) {
    
    next
    
  }
  
  home_score <- stats_html %>%
    html_nodes(xpath = home_score_xpath) %>%
    html_text() %>%
    str_squish()
  
  away_score <- stats_html %>%
    html_nodes(xpath = away_score_xpath) %>%
    html_text() %>%
    str_squish()
  
  if(is.na(as.integer(home_score))) {
    next
  }
  
  match_location <- stats_html %>%
    html_nodes(xpath = location_xpath) %>%
    html_text()
  
  if(length(match_location) == 0) {
    
    match_location <- NA
    
  }
  
  match_referee <- stats_html %>%
    html_nodes(xpath = referee_xpath) %>%
    html_text()
  
  if(length(match_referee) == 0) {
    
    match_referee <- NA
    
  }
  
  tables <- stats_html %>%
    html_nodes("table") %>%
    html_table()
  
  if(length(tables) < 2) {
    
    next
    
  }
  
  if(nrow(tables[[1]]) == 47) {
    
    table_stats <- tables[[1]] %>%
      clean_names()
    
  } else {
    
    table_stats <- tables[[2]] %>%
      clean_names()
    
  }
  
  home_stats <- table_stats %>%
    select(2, 1) %>%
    mutate(match_id = i,
           team = names(table_stats[1]),
           opponent = names(table_stats[3]),
           date_time = match_date,
           location = match_location,
           referee = match_referee,
           points_for = home_score,
           points_against = away_score,
           .before = x) %>%
    filter(x %in% key_stats)
  
  names(home_stats) <- c("match_id","team", "opponent", "date_time", "location", "referee","points_for", "points_against", "statistic", "value")
  
  away_stats <- table_stats %>%
    select(2, 3) %>%
    mutate(match_id = i,
           team = names(table_stats[3]),
           opponent = names(table_stats[1]),
           date_time = match_date,
           location = match_location,
           referee = match_referee,
           points_for = away_score,
           points_against = home_score,
           .before = x) %>%
    filter(x %in% key_stats)
  
  names(away_stats) <- c("match_id", "team", "opponent", "date_time", "location", "referee","points_for", "points_against", "statistic", "value")
  
  match_stats <- bind_rows(home_stats, away_stats) %>%
    mutate(points_for = as.integer(points_for),
           points_against = as.integer(points_against),
           value = as.integer(value))
  
  all_data <- bind_rows(all_data, match_stats)
  
}

all_clean <- all_data %>%
  filter(date_time > ymd("2021-12-31")) %>%
  arrange(date_time) %>%
  mutate(year = year(date_time),
         .after = "date_time")

teams <- c(unique(unlist(all_clean$team)), unique(unlist(all_clean$opponent))) %>%
  unique()

all_results <- all_clean %>%
  select(1:9) %>%
  unique()

all_summary <- all_results %>%
  group_by(team, year) %>%
  summarise(no_games = n(),
            points_for_game = sum(points_for) / no_games,
            points_against_game = sum(points_against) / no_games,
            margin_per_game = points_for_game - points_against_game) %>%
  arrange(desc(margin_per_game))

statistics_summary <- all_clean %>%
  group_by(team, year, statistic) %>%
  summarise(total_value = sum(value))

statistics_vs_summary <- all_clean %>%
  group_by(opponent, year, statistic) %>%
  summarise(total_value = sum(value))

progression_statistics <- c("Carries", "Metres", "Clean Breaks", "Rucks Won", "Rucks Lost", "Kicks in Play", "Kick From Hand Metres")

progression_summary <- statistics_summary %>%
  filter(statistic %in% progression_statistics) %>%
  pivot_wider(names_from = statistic,
              values_from = total_value) %>%
  clean_names() %>%
  mutate(m_per_carry = metres / carries,
         cb_rate = clean_breaks / carries,
         ruck_success = rucks_won / (rucks_won + rucks_lost),
         progression_via_carry = metres / (metres + kick_from_hand_metres),
         carry_kick_ratio = carries / kicks_in_play,
         total_progression = metres + kick_from_hand_metres)

movement_statistics <- c("Carries", "Passes", "Offloads")

movement_summary <- statistics_summary %>%
  filter(statistic %in% movement_statistics) %>%
  pivot_wider(names_from = statistic,
              values_from = total_value) %>%
  clean_names() %>%
  mutate(offload_rate = offloads / carries,
         pass_carry_ratio = passes / carries)

final_metrics <- full_join(all_summary, progression_summary, by = c("team", "year")) %>%
  full_join(., movement_summary, by = c("team", "year"))

eng_vs_can <- final_metrics %>%
  filter(team == "england_women" | team == "canada_women") %>%
  select(team, year, carry_kick_ratio, progression_via_carry, ruck_success, pass_carry_ratio, offload_rate, cb_rate, m_per_carry) %>%
  pivot_longer(cols = c("carry_kick_ratio", "progression_via_carry", "ruck_success", "pass_carry_ratio", "offload_rate", "cb_rate", "m_per_carry"),
               names_to = "metric",
               values_to = "value") %>%
  pivot_wider(names_from = "team",
              values_from = "value") %>%
  pivot_wider(names_from = "year",
              values_from = c("england_women", "canada_women")) %>%
  select(metric, england_women_2022, england_women_2025, canada_women_2022, canada_women_2025) %>%
  mutate(england_change = (round(england_women_2025, digits = 3) - round(england_women_2022, digits = 3)) * 100,
         .after = "england_women_2025") %>%
  mutate(canada_change = (round(canada_women_2025, digits = 3) - round(canada_women_2022, digits = 3)) * 100,
         .after = "canada_women_2025") %>%
  arrange(metric)

label_fix <- tibble(metric = eng_vs_can$metric,
                    label = c("Carries per kick", "% of carries ending in clean breaks", "Metres gained per carry", "% of carries ending in offloads", "Passes per carry", "% of metres gained via carries", "% of attacking rucks won"),
                    category = c("Exclude", "BALL PROGRESSION", "Exclude", "BALL MOVEMENT", "Exclude", "BALL PROGRESSION", "BREAKDOWN"))

for_table <- left_join(x = eng_vs_can, y = label_fix) %>%
  filter(category != "Exclude")

display_tbl <- for_table %>%
  gt(rowname_col = "label",
     groupname_col = "category") %>%
  cols_hide(columns = c("metric")) %>%
  cols_width(label ~ px(300),
             england_women_2022 ~ px(60),
             england_women_2025 ~ px(60),
             england_change ~ px(60),
             canada_women_2022 ~ px(60),
             canada_women_2025 ~ px(60),
             canada_change ~ px(60)) %>%
  row_group_order(groups = c("BREAKDOWN", "BALL MOVEMENT", "BALL PROGRESSION"))
display_tbl

header_colour <- "#79807E"
text_positive <- "#A63245"
text_negative <- "#5C8299"
fill_positive <- str_c(text_positive, "16")
fill_negative <- str_c(text_negative, "16")
text_positive_other <- str_c(text_positive, "70")
text_negative_other <- str_c(text_negative, "70")
fill_positive_other <- str_c(text_positive, "08")
fill_negative_other <- str_c(text_negative, "08")

tbl_formatted <- display_tbl %>%
  tab_header(title = html("At this World Cup, Canada are relying <b style='color:#A63245'>more</b> on their running game for ball progression"),
             subtitle = html("Performance of <b>England</b> and <b>Canada</b> at the <b>2025 Rugby World Cup</b> so far by a number of metrics, compared to the 2022 World Cup<br><br>")) %>%
  tab_source_note(source_note = html("<br><br>Data: Rugby.com.au | Table: Plot the Ball<br>")) %>%
  tab_stubhead(label = "") %>%
  tab_spanner(label = md("— ENGLAND —"),
              columns = c(england_women_2022, england_women_2025, england_change)) %>%
  tab_spanner(label = md("— CANADA —"),
              columns = c(canada_women_2022, canada_women_2025, canada_change)) %>%
  fmt_percent(columns = c(england_women_2022, england_women_2025, canada_women_2022, canada_women_2025), use_seps = TRUE, decimals = 0) %>%
  fmt_number(columns = c(england_change, canada_change), use_seps = TRUE, decimals = 1, force_sign = TRUE) %>%
  cols_label(england_women_2022 = "2022",
             england_women_2025 = "2025",
             england_change = "+/-",
             canada_women_2022 = "2022",
             canada_women_2025 = "2025",
             canada_change = "+/-") %>%
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
  tab_style(locations = cells_body(columns = c(england_change, canada_change)),
            style = list(cell_borders(sides = c("left"),
                                      weight = px(2),
                                      color = ptb_light_grey))) %>%
  tab_style(locations = cells_body(columns = c(england_women_2025, canada_women_2025)),
            style = list(cell_borders(sides = c("left"),
                                      weight = px(1),
                                      color = ptb_light_grey))) %>%
  tab_style(locations = cells_body(columns = c(canada_women_2022)),
            style = list(cell_borders(sides = c("left"),
                                      weight = px(3),
                                      color = ptb_light_grey))) %>%
  tab_style(locations = cells_row_groups(groups = c(1, 2, 3)),
            style = list(cell_text(color = ptb_mid_grey_v2,
                                   weight = "bold",
                                   size = "large"),
                         cell_borders(color = ptb_light_grey,
                                      weight = px(0)))) %>%
  tab_style(locations = cells_stub(rows = c("% of metres gained via carries")),
            style = list(cell_text(color = ptb_mid_grey_v2,
                                   weight = "bold"),
                         cell_fill(color = ptb_light_grey_v5))) %>%
  tab_style(locations = cells_body(rows = c("% of metres gained via carries")),
            style = list(cell_text(color = ptb_mid_grey_v2,
                                   weight = "bold"),
                         cell_fill(color = ptb_light_grey_v5))) %>%
  tab_style(locations = cells_body(columns = c(england_change),
                                   rows = england_change > 0),
            style = cell_text(color = text_positive)) %>%
  tab_style(locations = cells_body(columns = c(england_change),
                                   rows = england_change < 0),
            style = cell_text(color = text_negative)) %>%
  tab_style(locations = cells_body(columns = c(canada_change),
                                   rows = canada_change > 0),
            style = cell_text(color = text_positive)) %>%
  tab_style(locations = cells_body(columns = c(canada_change),
                                   rows = canada_change < 0),
            style = cell_text(color = text_negative)) %>%
  tab_style(locations = cells_body(columns = c(canada_change),
                                   rows = c("% of attacking rucks won", "% of carries ending in clean breaks")),
            style = list(cell_text(color = text_positive_other),
                         cell_fill(color = fill_positive_other))) %>%
  tab_style(locations = cells_body(columns = c(england_change),
                                   rows = c("% of carries ending in offloads", "% of carries ending in clean breaks")),
            style = list(cell_text(color = text_positive_other),
                         cell_fill(color = fill_positive_other))) %>%
  tab_style(locations = cells_body(columns = c(england_change),
                                   rows = c("% of attacking rucks won")),
            style = list(cell_text(color = text_negative_other),
                         cell_fill(color = fill_negative_other))) %>%
  tab_style(locations = cells_body(columns = c(england_change, canada_change),
                                   rows = "% of carries ending in offloads"),
            style = list(cell_fill(color = fill_positive_other))) %>%
  tab_style(locations = cells_body(columns = c(england_change),
                                   rows = "% of metres gained via carries"),
            style = list(cell_fill(color = fill_negative))) %>%
  tab_style(locations = cells_body(columns = c(canada_change),
                                   rows = c("% of carries ending in offloads")),
            style = list(cell_text(color = text_positive_other),
                         cell_fill(color = fill_positive_other))) %>%
  tab_style(locations = cells_body(columns = c(canada_change),
                                   rows = "% of metres gained via carries"),
            style = list(cell_fill(color = fill_positive)))
tbl_formatted
