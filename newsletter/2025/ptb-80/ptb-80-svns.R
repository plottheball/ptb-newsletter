library(tidyverse)
library(rvest)
library(janitor)
library(ggtext)
library(gt)

olympic_medallists <- c("australia_women_7s", "new_zealand_women_7s", "canada_women_7s", "france_women_7s", "fiji_women_7s", "usa_women_7s")

### NZ 7s: https://www.rugby.com.au/fixtures-results?team=421&comp=All&tab=Results
results_html_1 <- read_html("html/results_list_1_10_mar.html")
urls_1 <- results_html_1 %>%
  html_nodes("a") %>%
  html_attr("href")

### Aus 7s: https://www.rugby.com.au/fixtures-results?team=401&comp=All&tab=Results
results_html_2 <- read_html("html/results_list_2_10_mar.html")
urls_2 <- results_html_2 %>%
  html_nodes("a") %>%
  html_attr("href")

### Canada 7s: https://www.rugby.com.au/fixtures-results?team=403&comp=All&tab=Results
results_html_3 <- read_html("html/results_list_3_10_mar.html")
urls_3 <- results_html_3 %>%
  html_nodes("a") %>%
  html_attr("href")

### France 7s: https://www.rugby.com.au/fixtures-results?team=410&comp=All&tab=Results
results_html_4 <- read_html("html/results_list_4_10_mar.html")
urls_4 <- results_html_4 %>%
  html_nodes("a") %>%
  html_attr("href")

### Fiji 7s: https://www.rugby.com.au/fixtures-results?team=408&comp=All&tab=Results
results_html_5 <- read_html("html/results_list_5_10_mar.html")
urls_5 <- results_html_5 %>%
  html_nodes("a") %>%
  html_attr("href")

### USA 7s: https://www.rugby.com.au/fixtures-results?team=427&comp=All&tab=Results
results_html_6 <- read_html("html/results_list_6_10_mar.html")
urls_6 <- results_html_6 %>%
  html_nodes("a") %>%
  html_attr("href")

all_urls <- c(urls_1, urls_2, urls_3, urls_4, urls_5, urls_6)

all_matches <- all_urls[grepl("match-centre", all_urls)] %>%
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

all_since_2024 <- all_data %>%
  filter(date_time > ymd("2023-12-01")) %>%
  arrange(date_time)

teams <- c(unique(unlist(all_since_2024$team)), unique(unlist(all_since_2024$opponent))) %>%
  unique()

top_since_2024 <- all_since_2024 %>%
  filter(team %in% olympic_medallists & opponent %in% olympic_medallists) %>%
  mutate(year = year(date_time),
         .after = date_time)

missing_check <- top_since_2024 %>%
  filter(is.na(value))

top_results <- top_since_2024 %>%
  select(1:9) %>%
  unique()

top_summary <- top_results %>%
  group_by(team) %>%
  summarise(no_games = n(),
            points_for_game = sum(points_for) / no_games,
            points_against_game = sum(points_against) / no_games,
            margin_per_game = points_for_game - points_against_game) %>%
  arrange(desc(margin_per_game))

statistics_summary <- top_since_2024 %>%
  group_by(team, statistic) %>%
  summarise(total_value = sum(value))

statistics_vs_summary <- top_since_2024 %>%
  group_by(opponent, statistic) %>%
  summarise(total_value = sum(value))

progression_statistics <- c("Carries", "Metres", "Clean Breaks", "Rucks Won", "Rucks Lost", "Kicks in Play", "Kick From Hand Metres")

progression_summary <- statistics_summary %>%
  filter(statistic %in% progression_statistics) %>%
  pivot_wider(names_from = statistic,
              values_from = total_value) %>%
  clean_names() %>%
  mutate(cb_rate = clean_breaks / carries,
         ruck_success = rucks_won / (rucks_won + rucks_lost),
         progression_via_carry = metres / (metres + kick_from_hand_metres),
         carry_kick_ratio = carries / kicks_in_play,
         carry_ruck_ratio = carries / (rucks_won + rucks_lost),
         total_progression = metres + kick_from_hand_metres)

progression_vs_summary <- statistics_vs_summary %>%
  filter(statistic %in% progression_statistics) %>%
  pivot_wider(names_from = statistic,
              values_from = total_value) %>%
  clean_names() %>%
  mutate(cb_rate_vs = clean_breaks / carries,
         ruck_success_vs = rucks_won / (rucks_won + rucks_lost),
         progression_via_carry_vs = metres / (metres + kick_from_hand_metres),
         carry_kick_ratio_vs = carries / kicks_in_play,
         carry_ruck_ratio = carries / (rucks_won + rucks_lost),
         total_progression_vs = metres + kick_from_hand_metres) %>%
  rename(team = opponent)

metres_total <- full_join(progression_summary, progression_vs_summary, by = c("team" = "team")) %>%
  select(team, total_progression, total_progression_vs, ruck_success, ruck_success_vs)

top_final <- left_join(top_summary, metres_total) %>%
  mutate(metres_per_game = total_progression / no_games,
         metres_vs_per_game = total_progression_vs / no_games,
         net_progression = metres_per_game - metres_vs_per_game) %>%
  select(1, 2, 3, 4, 5, 8, 9) %>%
  arrange(desc(margin_per_game))

top_final$team <- str_replace_all(top_final$team, "new_zealand_women_7s", "New Zealand")
top_final$team <- str_replace_all(top_final$team, "australia_women_7s", "Australia")
top_final$team <- str_replace_all(top_final$team, "france_women_7s", "France")
top_final$team <- str_replace_all(top_final$team, "usa_women_7s", "USA")
top_final$team <- str_replace_all(top_final$team, "canada_women_7s", "Canada")
top_final$team <- str_replace_all(top_final$team, "fiji_women_7s", "Fiji")

### Performance of top women's international rugby sevens team in matches against one other since the start of the 2023-24 SVNS series
### POINTS PER GAME-METRES PER GAME-RUCK WIN %
### F-A-+/-
### Rugby.com.au

display_tbl <- top_final %>%
  gt(rowname_col = "team",
     groupname_col = NA) %>%
  cols_width(team ~ px(140),
             no_games ~ px(80),
             points_for_game ~ px(80),
             points_against_game ~ px(80),
             margin_per_game ~ px(80),
             ruck_success ~ px(90),
             ruck_success_vs ~ px(90))
display_tbl

header_colour <- "#79807E"
nzl_colour <- "#2483B3"
aus_colour <-  "#CC8529"
highlight_nzl <- str_c(nzl_colour, "10")
highlight_aus <- str_c(aus_colour, "10")

tbl_formatted <- display_tbl %>%
  tab_header(title = html("<b style='color:#2483B3'>New Zealand</b> secure their own ball at the ruck more effectively than other teams"),
             subtitle = html("Performance of top <b>women's international rugby sevens</b> teams in matches against each other since the <b>2023-24 SVNS</b> series<br><br>")) %>%
  tab_source_note(source_note = html("<br>'Top teams' = Olympic medallists in women's rugby sevens since 2016<br><br>Data: Rugby.com.au | Table: Plot the Ball<br>")) %>%
  tab_stubhead(label = "") %>%
  tab_spanner(label = md("— POINTS PER GAME —"),
              columns = c(points_for_game, points_against_game, margin_per_game)) %>%
  tab_spanner(label = md("— RUCK WIN % —"),
              columns = c(ruck_success, ruck_success_vs)) %>%
  fmt_number(columns = c(no_games), use_seps = TRUE, decimals = 0) %>%
  fmt_number(columns = c(points_for_game, points_against_game), use_seps = TRUE, decimals = 1) %>%
  fmt_number(columns = c(margin_per_game), force_sign = TRUE, use_seps = TRUE, decimals = 1) %>%
  fmt_percent(columns = c(ruck_success, ruck_success_vs), use_seps = TRUE, decimals = 1) %>%
  cols_label(no_games = "Games",
             points_for_game = "Team",
             points_against_game = "Opp.",
             margin_per_game = "▼ +/-",
             ruck_success = "Team",
             ruck_success_vs = "Opp.") %>%
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
  tab_style(locations = cells_body(columns = c(points_for_game)),
            style = list(cell_borders(sides = c("left"),
                                      weight = px(2),
                                      color = ptb_light_grey))) %>%
  tab_style(locations = cells_body(columns = c(margin_per_game)),
            style = list(cell_borders(sides = c("left"),
                                      weight = px(1),
                                      color = ptb_light_grey))) %>%
  tab_style(locations = cells_body(columns = c(ruck_success)),
            style = list(cell_borders(sides = c("left"),
                                      weight = px(1.5),
                                      color = ptb_light_grey))) %>%
  tab_style(locations = cells_body(rows = team == "New Zealand",
                                   columns = c(points_for_game, points_against_game, margin_per_game, ruck_success)),
            style = list(cell_text(color = nzl_colour,
                                   weight = "bold"),
                         cell_fill(color = highlight_nzl))) %>%
  tab_style(locations = cells_body(rows = team == "New Zealand",
                                   columns = c(no_games, ruck_success_vs)),
            style = list(cell_text(color = nzl_colour),
                         cell_fill(color = highlight_nzl))) %>%
  tab_style(locations = cells_stub(rows = team == "New Zealand"),
            style = list(cell_text(color = nzl_colour),
                         cell_fill(color = highlight_nzl))) %>%
  tab_style(locations = cells_body(rows = team == "Australia",
                                   columns = c(ruck_success_vs)),
            style = list(cell_text(color = aus_colour,
                                   weight = "bold"),
                         cell_fill(color = highlight_aus))) %>%
  tab_style(locations = cells_body(rows = team == "Australia",
                                   columns = c(no_games, points_for_game, points_against_game, margin_per_game, ruck_success)),
            style = list(cell_text(color = aus_colour),
                         cell_fill(color = highlight_aus))) %>%
  tab_style(locations = cells_stub(rows = team == "Australia"),
            style = list(cell_text(color = aus_colour),
                         cell_fill(color = highlight_aus))) %>%
  tab_style(locations = cells_body(rows = team == "Canada",
                                   columns = c(no_games)),
            style = list(cell_text(weight = "bold")))
tbl_formatted
