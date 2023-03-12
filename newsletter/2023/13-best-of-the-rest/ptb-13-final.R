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

id_2022 <- "wbbl-2022-23-1323553"
id_2021 <- "wbbl-2021-22-1269053"
id_2020 <- "wbbl-2020-21-1226776"
season_ids <- c(id_2020, id_2021, id_2022)

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
bowling_records <- tibble()

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
    next
  }
  
  first_inns_bowl <- all_tables[[2]] %>%
    clean_names()
  
  if(names(first_inns_bowl[1]) != "bowling") {
    next
  }
  
  second_inns_bat <- all_tables[[3]] %>%
    clean_names()
  
  if(names(second_inns_bat[1]) != "batting") {
    next
  }
  
  second_inns_bowl <- all_tables[[4]] %>%
    clean_names()
  
  if(names(second_inns_bowl[1]) != "bowling") {
    next
  }
  
  first_inns_bat$inns_no <- 1
  first_inns_bat$order <- row(first_inns_bat[1])
  first_inns_bat$url <- i
  first_inns_bat <- first_inns_bat %>%
    filter(!grepl("Extras", batting) & !grepl("TOTAL", batting) & !grepl("Did not bat:", batting) & !grepl("Fall of wickets:", batting))
  first_inns_bat <- first_inns_bat %>%
    select(-sr, -x_2, -x_3)
  second_inns_bat$inns_no <- 2
  second_inns_bat$order <- row(second_inns_bat[1])
  second_inns_bat$url <- i
  second_inns_bat <- second_inns_bat %>%
    filter(!grepl("Extras", batting) & !grepl("TOTAL", batting) & !grepl("Did not bat:", batting) & !grepl("Fall of wickets:", batting))
  second_inns_bat <- second_inns_bat %>%
    select(-sr, -x_2, -x_3)
  
  first_inns_bowl$inns_no <- 1
  first_inns_bowl$order <- row(first_inns_bowl[1])
  first_inns_bowl$url <- i
  first_inns_bowl <- first_inns_bowl %>%
    select(-econ)
  second_inns_bowl$inns_no <- 2
  second_inns_bowl$order <- row(second_inns_bowl[1])
  second_inns_bowl$url <- i
  second_inns_bowl <- second_inns_bowl %>%
    select(-econ)
  
  all_bat <- rbind(first_inns_bat, second_inns_bat)
  all_bowl <- rbind(first_inns_bowl, second_inns_bowl)
  batting_records <- rbind(batting_records, all_bat)
  bowling_records <- rbind(bowling_records, all_bowl)
  
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

match_run_rates <- batting_records_filtered %>%
  group_by(url) %>%
  summarise(n_balls = sum(b),
            n_runs = sum(r))

runs_vs_exp <- left_join(x = batting_records_filtered, y = match_run_rates, by = c("url" = "url"))
runs_vs_exp <- runs_vs_exp %>%
  mutate(other_balls = n_balls - b,
         other_runs = n_runs - r,
         other_sr = other_runs / other_balls) %>%
  select(-2, -5, -6, -7, -8, -9) %>%
  relocate(url, .before = player)
runs_vs_exp <- runs_vs_exp %>%
  mutate(batter_sr = r / b,
         sr_diff = batter_sr - other_sr,
         vs_exp = b * sr_diff)
runs_vs_exp <- runs_vs_exp %>%
  filter(b > 0)

batters_vs_exp <- runs_vs_exp %>%
  group_by(player) %>%
  summarise(total_inns = n(),
            total_balls = sum(b),
            total_runs = sum(r),
            balls_per_inns = total_balls / total_inns,
            sr = total_runs / total_balls,
            vs_exp_total = sum(vs_exp),
            vs_exp_inns = vs_exp_total / total_inns,
            vs_exp_ball = vs_exp_total / total_balls)

batters_vs_exp_filtered <- arrange(batters_vs_exp, desc(total_balls)) %>%
  filter(total_balls >= 240)

batters_for_table <- batters_vs_exp_filtered %>%
  mutate(runs_per_inns = total_runs / total_inns) %>%
  select(player,
         total_inns,
         balls_per_inns,
         runs_per_inns,
         vs_exp_inns) %>%
  arrange(-vs_exp_inns)
include_list <- c("Heather Knight", "Smriti Mandhana", "Harmanpreet Kaur", "Sophie Devine", "Marizanne Kapp", "Jemimah Rodrigues", "Hayley Matthews", "Chamari Athapaththu", "Chloe Tryon", "Laura Wolvaardt", "Amelia Kerr", "Maddy Green", "Suzie Bates", "Lauren Winfield-Hill")
batters_for_table$category <- if_else(batters_for_table$player %in% include_list, "T20 WC", "Other")
batters_for_table <- batters_for_table %>%
  filter(category == "T20 WC")
batters_for_table$category <- NULL

batters_for_table <- batters_for_table %>%
  arrange(desc(vs_exp_inns))

batters_for_table_final <- batters_for_table %>%
  filter(vs_exp_inns > 0)

wc_green <- "#b4ff0c"
wc_red <- "#ff461f"
wc_navy <- "#10044a"
category_palette <- c(wc_red, ptb_mid_grey)

display_tbl <- batters_for_table_final %>%
  gt(rowname_col = "player")
display_tbl

display_tbl <- display_tbl %>%
  gt_plt_bar(column = vs_exp_inns, keep_column = TRUE, width = 25, color = wc_red)
display_tbl

tbl_formatted <- display_tbl %>%
  tab_header(title = html("<b>Batters to watch in the 2023 T20 World Cup</b>"),
             subtitle = html("Top-performing overseas batters in the WBBL since 2020<br><br><i style='font-size:90%'>Note: '+/-' measures runs scored per innings compared to expectation, which is calculated in each innings based on a batter's scoring rate relative to the scoring rate of other players in that game</i><br>")) %>%
  tab_source_note(source_note = html("Players not competing in the 2023 T20 World Cup excluded<br>Players with <240 balls faced in completed WBBL matches excluded<br><br>Data: ESPNCricinfo | Table: Plot the Ball")) %>%
  tab_stubhead(label = "Player") %>%
  tab_spanner(label = md("— PER INNINGS —"),
              columns = c(balls_per_inns, runs_per_inns, vs_exp_inns)) %>%
  fmt_number(columns = c(balls_per_inns, runs_per_inns), decimals = 1) %>%
  fmt_number(columns = vs_exp_inns, decimals = 1, force_sign = TRUE) %>%
  cols_label(total_inns = "Innings",
             balls_per_inns = "Balls",
             runs_per_inns = "Runs",
             vs_exp_inns = "+/-",
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
                                   size = px(20)))) %>%
  tab_style(locations = cells_title(groups = "subtitle"),
            style = list(cell_text(align = "left",
                                   color = ptb_dark_grey,
                                   size = px(15)))) %>%
  tab_style(locations = list(cells_column_spanners(),
                             cells_column_labels(),
                             cells_stubhead()),
            style = list(cell_text(color = "#FFFFFF",
                                   weight = "bold"),
                         cell_fill(color = wc_navy))) %>%
  tab_style(locations = cells_stub(rows = TRUE),
            style =  list(cell_text(align = "left",
                                    color = ptb_dark_grey,
                                    size = "small"),
                          cell_borders(sides = c("top", "bottom"),
                                       weight = px(1),
                                       color = ptb_light_grey))) %>%
  tab_style(locations = cells_body(),
            style = list(cell_text(color = ptb_dark_grey,
                                   size = "small"),
                         cell_borders(sides = c("top", "bottom"),
                                      weight = px(1),
                                      color = ptb_light_grey))) %>%
  tab_style(locations = cells_source_notes(),
            style = list(cell_text(align = "right",
                                   color = ptb_mid_grey)))  %>%
  tab_style(locations =  list(cells_stub(rows = everything())),
            style = list(cell_text(weight = "bold"))) %>%
  tab_style(locations = list(cells_body(columns = vs_exp_inns)),
            style = list(cell_text(weight = "bold"))) %>%
  tab_style(locations = list(cells_body(columns = c(balls_per_inns, runs_per_inns))),
            style = list(cell_text(color = ptb_mid_grey)))
tbl_formatted
