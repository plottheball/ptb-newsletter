library(tidyverse)
library(rvest)
library(janitor)
library(ggtext)

### Six Nations: https://www.rugby.com.au/fixtures-results?team=All&comp=248&tab=Results
results_html_1 <- read_html("html/results_list_1_27_apr.html")
urls_1 <- results_html_1 %>%
  html_nodes("a") %>%
  html_attr("href")

### World Cup: https://www.rugby.com.au/fixtures-results?team=All&comp=249&tab=Results
results_html_2 <- read_html("html/results_list_2_31_dec.html")
urls_2 <- results_html_2 %>%
  html_nodes("a") %>%
  html_attr("href")

### Women's internationals: https://www.rugby.com.au/fixtures-results?team=All&comp=274&tab=Results
results_html_3 <- read_html("html/results_list_3_27_apr.html")
urls_3 <- results_html_3 %>%
  html_nodes("a") %>%
  html_attr("href")

### WXV 1: https://www.rugby.com.au/fixtures-results?team=All&comp=862&tab=Results
results_html_4 <- read_html("html/results_list_4_31_dec.html")
urls_4 <- results_html_4 %>%
  html_nodes("a") %>%
  html_attr("href")

### Pacific Four: https://www.rugby.com.au/fixtures-results?team=All&comp=120&tab=Results
results_html_5 <- read_html("html/results_list_5_31_dec.html")
urls_5 <- results_html_5 %>%
  html_nodes("a") %>%
  html_attr("href")

### WXV 2: https://www.rugby.com.au/fixtures-results?team=All&comp=863&tab=Results
results_html_6 <- read_html("html/results_list_6_31_dec.html")
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

all_since_2023 <- all_data %>%
  filter(date_time > ymd("2022-12-31")) %>%
  arrange(date_time)

teams <- c(unique(unlist(all_since_2023$team)), unique(unlist(all_since_2023$opponent))) %>%
  unique()

### teams which have appeared in WXV 1 in either 2023 or 2024
wxv_one <- c("wallaroos", "canada_women", "england_women", "france_women", "new_zealand_women", "wales_women", "usa_women", "ireland_women")

wxv_since_2023 <- all_since_2023 %>%
  filter(team %in% wxv_one & opponent %in% wxv_one) %>%
  mutate(year = year(date_time),
         .after = date_time)

missing_check <- wxv_since_2023 %>%
  filter(is.na(value))

wxv_results <- wxv_since_2023 %>%
  select(1:9) %>%
  unique()

wxv_summary <- wxv_results %>%
  group_by(team) %>%
  summarise(no_games = n(),
            points_for_game = sum(points_for) / no_games,
            points_against_game = sum(points_against) / no_games,
            margin_per_game = points_for_game - points_against_game) %>%
  arrange(desc(margin_per_game))

statistics_summary_wxv <- wxv_since_2023 %>%
  group_by(team, statistic) %>%
  summarise(total_value = sum(value))

statistics_vs_summary_wxv <- wxv_since_2023 %>%
  group_by(opponent, statistic) %>%
  summarise(total_value = sum(value))

progression_statistics <- c("Carries", "Metres", "Clean Breaks", "Rucks Won", "Rucks Lost", "Kicks in Play", "Kick From Hand Metres")

progression_summary_wxv <- statistics_summary_wxv %>%
  filter(statistic %in% progression_statistics) %>%
  pivot_wider(names_from = statistic,
              values_from = total_value) %>%
  clean_names() %>%
  mutate(cb_rate = clean_breaks / carries,
         ruck_success = rucks_won / (rucks_won + rucks_lost),
         progression_via_carry = metres / (metres + kick_from_hand_metres),
         carry_kick_ratio = carries / kicks_in_play,
         total_progression = metres + kick_from_hand_metres)

progression_vs_summary_wxv <- statistics_vs_summary_wxv %>%
  filter(statistic %in% progression_statistics) %>%
  pivot_wider(names_from = statistic,
              values_from = total_value) %>%
  clean_names() %>%
  mutate(cb_rate_vs = clean_breaks / carries,
         ruck_success_vs = rucks_won / (rucks_won + rucks_lost),
         progression_via_carry_vs = metres / (metres + kick_from_hand_metres),
         carry_kick_ratio_vs = carries / kicks_in_play,
         total_progression_vs = metres + kick_from_hand_metres) %>%
  rename(team = opponent)

movement_statistics <- c("Carries", "Passes", "Offloads")

movement_summary_wxv <- statistics_summary_wxv %>%
  filter(statistic %in% movement_statistics) %>%
  pivot_wider(names_from = statistic,
              values_from = total_value) %>%
  clean_names() %>%
  mutate(offload_rate = offloads / carries,
         pass_carry_ratio = passes / carries)

movement_vs_summary_wxv <- statistics_vs_summary_wxv %>%
  filter(statistic %in% movement_statistics) %>%
  pivot_wider(names_from = statistic,
              values_from = total_value) %>%
  clean_names() %>%
  mutate(offload_rate_vs = offloads / carries,
         pass_carry_ratio_vs = passes / carries) %>%
  rename(team = opponent)

final_metrics_wxv <- full_join(wxv_summary, progression_summary_wxv, by = "team") %>%
  full_join(., progression_vs_summary_wxv, by = "team", suffix = c("", "_vs")) %>%
  full_join(., movement_summary_wxv, by = "team") %>%
  full_join(., movement_vs_summary_wxv, by = "team", suffix = c("", "_vs")) %>%
  mutate(carries_per_game = carries / no_games,
         metres_per_game = metres / no_games,
         kicks_per_game = kicks_in_play / no_games,
         kick_metres_per_game = kick_from_hand_metres / no_games,
         carries_vs_per_game = carries_vs / no_games,
         metres_vs_per_game = metres_vs / no_games,
         kicks_vs_per_game = kicks_in_play_vs / no_games,
         kick_metres_vs_per_game = kick_from_hand_metres_vs / no_games) %>%
  select(1, 2, 3, 4, 5, 40, 41, 42, 43, 13, 14, 15, 16, 33, 34, 44, 45, 46, 47, 25, 26, 27, 28, 38, 39)

progression_for_plot <- final_metrics_wxv %>%
  select(team, kick_metres_per_game, metres_per_game) %>%
  mutate(kick_share = kick_metres_per_game / (kick_metres_per_game + metres_per_game) * 100,
         run_share = -metres_per_game / (kick_metres_per_game + metres_per_game) * 100) %>%
  select(-kick_metres_per_game, -metres_per_game) %>%
  pivot_longer(cols = c("kick_share", "run_share"),
               names_to = "type",
               values_to = "share") 

progression_for_plot <- progression_for_plot %>%
  mutate(category = case_when(type == "kick_share" & team == "new_zealand_women" ~ "Kick Focus",
                              type == "kick_share" & team != "new_zealand_women" ~ "Kick Other",
                              type == "run_share" & team == "new_zealand_women" ~ "Run Focus",
                              type == "run_share" & team != "new_zealand_women" ~ "Run Other",
                              TRUE ~ "!"))

team_labels <- tibble(label = c("england_women", "new_zealand_women",  "canada_women", "france_women", "ireland_women",  "wallaroos",  "usa_women",  "wales_women"),
                      full = c("England", "New Zealand",  "Canada", "France", "Ireland",  "Australia",  "USA",  "Wales"))

team_final <- left_join(progression_for_plot, team_labels, by = c("team" = "label"))

rough_bars <- ggplot(team_final, aes(x = fct_reorder(full, -share), y = share, fill = category)) +
  geom_bar(stat = "identity",
           width = 0.55) +
  scale_fill_manual(values = c("#63A692", "#B8CCC6", "#524573", "#AEA3CC")) +
  geom_hline(yintercept = 0) +
  scale_y_continuous(limits = c(-100, 100),
                     breaks = c(-75, -50, -25, 0, 25, 50, 75),
                     labels = (c("75%", "50", "25", "0", "25", "50", "75%")),
                     expand = c(0, 0))+
  theme(legend.position = "none",
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.ticks.y = element_blank(),
        panel.grid.major.y = element_blank(),
        axis.line.x = element_blank(),
        axis.line.y = element_blank(),
        axis.ticks = element_blank()) +
  coord_flip()
rough_bars

themed_bars <- rough_bars +
  theme_ptb()
themed_bars

labelled_bars <- themed_bars +
  labs(title = "The Black Ferns move the ball upfield<br>with <b style='color:#63A692'>kicks</b> less than any other team",
       subtitle = "Share of each team's <b>metres gained</b> via <b style='color:#5A468C'>running</b><br>and <b style='color:#63A692'>kicking</b> the ball in test matches since 2023<br>",
       caption = "<i>Data for games between WXV 1 qualifiers only</i><br><br>Data: Rugby.com.au | Chart: Plot the Ball")
labelled_bars