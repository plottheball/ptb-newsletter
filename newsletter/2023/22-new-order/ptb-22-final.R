library(tidyverse)
library(rvest)
library(janitor)
library(gt)
library(gtExtras)
library(svglite)
library(scales)
library(ggtext)

base_fixtures_url <- "https://www.espncricinfo.com/series/"
end_fixtures_url <- "/match-schedule-fixtures-and-results"
scorecard_indicator <- "/full-scorecard"

season_ids <- c("plunket-shield-2021-22-1279954",
                "plunket-shield-2022-23-1328613")

fixture_urls <- tibble()

for (i in season_ids) {
  
  season_url <- str_c(base_fixtures_url, i, end_fixtures_url)
  
  html <- read_html(season_url)
  
  match_urls <- html %>%
    html_nodes("a") %>%
    html_attr("href")
  urls_tidy <- match_urls %>%
    as.tibble()
  
  urls_filtered <- urls_tidy %>%
    filter(grepl(i, value) & grepl(scorecard_indicator, value))
  urls_filtered$season <- i
  fixture_urls <- rbind(fixture_urls, urls_filtered)
  fixture_urls <- unique(fixture_urls)
  
}

base_match_url <- "https://www.espncricinfo.com"

all_match_urls <- str_c(base_match_url, fixture_urls$value)

batting_records <- tibble()

for (i in all_match_urls) {
  
  print(paste0("Getting match: ", i))
  
  html <- read_html(i)
  
  all_tables <- html %>%
    html_nodes("table") %>%
    html_table()
  
  if(length(all_tables) < 5) {
    next
  }
  
  first_inns_bat <- all_tables[[1]] %>%
    clean_names()
  
  if(names(first_inns_bat[1]) != "batting") {
    first_inns_bat <- tibble("batting", "x", "r", "b", "m", "x4s", "x6s", "sr", "x_2", "x_3")
  }
  
  second_inns_bat <- all_tables[[3]] %>%
    clean_names()
  
  if(names(second_inns_bat[1]) != "batting") {
    second_inns_bat <- tibble("batting", "x", "r", "b", "m", "x4s", "x6s", "sr", "x_2", "x_3")
  }
  
  third_inns_bat <- all_tables[[5]] %>%
    clean_names()
  
  if(names(third_inns_bat[1]) != "batting") {
    third_inns_bat <- tibble("batting", "x", "r", "b", "m", "x4s", "x6s", "sr", "x_2", "x_3")
  }
  
  fourth_inns_bat <- all_tables[[7]] %>%
    clean_names()
  
  if(names(fourth_inns_bat[1]) != "batting") {
    
    fourth_inns_bat <- all_tables[[6]] %>%
      clean_names()
    if(names(fourth_inns_bat[1]) != "batting") {
      fourth_inns_bat <- tibble("batting", "x", "r", "b", "m", "x4s", "x6s", "sr", "x_2", "x_3")
    }
    
  }
  
  names(first_inns_bat) <- c("batting", "x", "r", "b", "m", "x4s", "x6s", "sr", "x_2", "x_3")
  first_inns_bat$inns_no <- 1
  first_inns_bat$order <- row(first_inns_bat[1])
  first_inns_bat$url <- i
  first_inns_bat <- first_inns_bat %>%
    select(-sr, -x_2, -x_3)
  
  names(second_inns_bat) <- c("batting", "x", "r", "b", "m", "x4s", "x6s", "sr", "x_2", "x_3")
  second_inns_bat$inns_no <- 2
  second_inns_bat$order <- row(second_inns_bat[1])
  second_inns_bat$url <- i
  second_inns_bat <- second_inns_bat %>%
    select(-sr, -x_2, -x_3)
  
  names(third_inns_bat) <- c("batting", "x", "r", "b", "m", "x4s", "x6s", "sr", "x_2", "x_3")
  third_inns_bat$inns_no <- 3
  third_inns_bat$order <- row(third_inns_bat[1])
  third_inns_bat$url <- i
  third_inns_bat <- third_inns_bat %>%
    select(-sr, -x_2, -x_3)
  
  names(fourth_inns_bat) <- c("batting", "x", "r", "b", "m", "x4s", "x6s", "sr", "x_2", "x_3")
  fourth_inns_bat$inns_no <- 4
  fourth_inns_bat$order <- row(fourth_inns_bat[1])
  fourth_inns_bat$url <- i
  fourth_inns_bat <- fourth_inns_bat %>%
    select(-sr, -x_2, -x_3)
  
  all_bat <- rbind(first_inns_bat, second_inns_bat, third_inns_bat, fourth_inns_bat)
  all_bat <- all_bat %>%
    filter(!grepl("Extras", batting) & !grepl("TOTAL", batting) & !grepl("Did not bat:", batting) & !grepl("Fall of wickets:", batting) & !grepl("batting", batting))
  
  batting_records <- rbind(batting_records, all_bat)
  
}

batting_records_final <- batting_records %>%
  mutate(across(batting, str_replace_all, fixed("(c)"), "")) %>%
  mutate(across(batting, str_replace_all, fixed("†"), "")) %>%
  mutate(across(batting, str_squish))

old_names <- names(batting_records_final)
old_names[1] <- "player"
old_names[2] <- "dismissal"
old_names[9] <- "position"
new_names <- old_names
names(batting_records_final) <- new_names
batting_records_final$position <- batting_records_final$position[,1]
batting_records_final[3:9] <- sapply(batting_records_final[3:9], as.integer)
batting_records_filtered <- batting_records_final %>%
  filter(!is.na(b))

batting_records_filtered <- batting_records_filtered %>%
  mutate(category = case_when(position < 4 ~ "Top 3",
                              position < 7 ~ "Middle order",
                              TRUE ~ "Lower order"),
         dismissed_y_n = case_when(dismissal == "not out" ~ 0,
                                   dismissal == "retired hurt" ~ 0,
                                   dismissal == "retired not out" ~ 0,
                                   dismissal == "retired hurt not o" ~ 0,
                                   TRUE ~ 1))

match_splits <- batting_records_filtered %>%
  group_by(url, category) %>%
  summarise(n_runs = sum(r),
            n_balls = sum(b),
            n_dismissals = sum(dismissed_y_n))

runs_vs_exp <- left_join(x = batting_records_filtered, y = match_splits, by = c("url" = "url", "category" = "category"))
runs_vs_exp <- runs_vs_exp %>%
  mutate(other_balls = n_balls - b,
         other_runs = n_runs - r,
         other_dismissals = n_dismissals - dismissed_y_n) %>%
  select(-2, -5, -6, -7, -8, -9) %>%
  relocate(url, .before = player)

players_vs_exp <- runs_vs_exp %>%
  group_by(player) %>%
  summarise(no_inns = n(),
            runs_total = sum(r),
            balls_total = sum(b),
            dismissals_total = sum(dismissed_y_n),
            other_runs_total = sum(other_runs),
            other_balls_total = sum(other_balls),
            other_dismissals_total = sum(other_dismissals),
            average = runs_total / dismissals_total,
            strike_rate = runs_total / balls_total,
            average_other = other_runs_total / other_dismissals_total,
            strike_rate_other = other_runs_total / other_balls_total,
            ave_vs_exp = average - average_other,
            sr_vs_exp = strike_rate - strike_rate_other)

players_final <- players_vs_exp %>%
  filter(balls_total >= 700 & no_inns >= 7)

focus_list <- c("Mitchell Hay",
                "Muhammad Abbas",
                "Jacob Cumming",
                "Thorn Parkes",
                "Rachin Ravindra",
                "Curtis Heaphy",
                "Rhys Mariu")

players_focus <- players_final %>%
  filter(player %in% focus_list) %>%
  select(1, 2, 3, 4, 5, 13)

players_focus <- players_focus %>%
  mutate(balls_per_dis = balls_total / dismissals_total,
         runs_per_dis = runs_total / dismissals_total,
         .after = no_inns) %>%
  select(-balls_total, -runs_total, -dismissals_total) %>%
  arrange(-no_inns)

nz_black <- "#141134"
nz_blue <- "#0dbdbe"

display_tbl <- players_focus %>%
  gt(rowname_col = "player")
display_tbl

display_tbl <- display_tbl %>%
  gt_plt_bar(column = ave_vs_exp, keep_column = TRUE, width = 25, color = nz_blue)
display_tbl

tbl_formatted <- display_tbl %>%
  tab_header(title = html("<b>Who's up next for the Black Caps?</b>"),
             subtitle = html("Performance of selected batters in the Plunket Shield since 2021-22<br><br><i style='font-size:90%'>Note: '+/-' measures runs scored per dismissal compared to expectation, which is<br>calculated in aggregate based on a batter's average relative to the combined<br>average of other players batting in similar positions in their games</i><br><br>")) %>%
  tab_source_note(source_note = html("<br>Top- and middle-order batters aged 23 or younger with >700 balls faced since 2021-22<br><br>Data: ESPNCricinfo | Table: Plot the Ball<br>")) %>%
  tab_stubhead(label = "Player") %>%
  tab_spanner(label = md("— PER DISMISSAL —"),
              columns = c(balls_per_dis, runs_per_dis, ave_vs_exp)) %>%
  fmt_number(columns = c(balls_per_dis, runs_per_dis), decimals = 1) %>%
  fmt_number(columns = ave_vs_exp, decimals = 1, force_sign = TRUE) %>%
  cols_label(no_inns = "Innings",
             balls_per_dis = "Balls",
             runs_per_dis = "Runs",
             ave_vs_exp = "+/-",
             DUPE_COLUMN_PLT = "") %>%
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
                         cell_fill(color = nz_black))) %>%
  tab_style(locations = cells_stub(rows = TRUE),
            style =  list(cell_text(align = "left",
                                    color = ptb_dark_grey,
                                    size = "large"),
                          cell_borders(sides = c("top", "bottom"),
                                       weight = px(1),
                                       color = ptb_light_grey))) %>%
  tab_style(locations = cells_body(),
            style = list(cell_text(color = ptb_dark_grey,
                                   size = "large"),
                         cell_borders(sides = c("top", "bottom"),
                                      weight = px(1),
                                      color = ptb_light_grey))) %>%
  tab_style(locations = cells_source_notes(),
            style = list(cell_text(align = "right",
                                   color = ptb_mid_grey,
                                   size = "medium")))  %>%
  tab_style(locations =  list(cells_stub(rows = everything())),
            style = list(cell_text(weight = "bold"))) %>%
  tab_style(locations = list(cells_body(columns = ave_vs_exp)),
            style = list(cell_text(weight = "bold",
                                   color = nz_blue))) %>%
  tab_style(locations = list(cells_body(columns = c(balls_per_dis, runs_per_dis))),
            style = list(cell_text(color = ptb_mid_grey)))
tbl_formatted
