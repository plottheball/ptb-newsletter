library(tidyverse)
library(rvest)
library(janitor)
library(RcppRoll)
library(ggtext)
library(scales)

base_fixtures_url <- "https://www.espncricinfo.com/series/"
end_fixtures_url <- "/match-schedule-fixtures-and-results"
scorecard_indicator <- "/full-scorecard"

id_2025 <- "wpl-2024-25-1463356"
id_2024 <- "wbbl-2024-25-1442625"
id_2024_ii <- "the-hundred-women-s-competition-2024-1417780"
id_2024_iii <- "women-s-premier-league-2023-24-1411373"
id_2023 <- "wbbl-2023-24-1387171"
id_2023_ii <- "the-hundred-women-s-competition-2023-1355568"
id_2023_iii <- "women-s-premier-league-2022-23-1348825"
id_2022 <- "wbbl-2022-23-1323553"
id_2022_ii <- "the-hundred-women-s-competition-2022-1299144"
id_2021 <- "wbbl-2021-22-1269053"
id_2021_ii <- "the-hundred-women-s-competition-2021-1252659"
id_2020 <- "wbbl-2020-21-1226776"
id_2019 <- "wbbl-2019-20-1188381"
id_2018 <- "wbbl-2018-19-1152722"
id_2017 <- "wbbl-2017-18-1118470"
id_2016 <- "women-s-big-bash-league-2016-17-1023541"
id_2015 <- "women-s-big-bash-league-2015-16-896429"
season_ids <- c(id_2015, id_2016, id_2017, id_2018, id_2019, id_2020, id_2021_ii, id_2021, id_2022_ii, id_2022, id_2023_iii, id_2023_ii, id_2023, id_2024_iii, id_2024_ii, id_2024, id_2025)

fixture_urls <- tibble()

for (i in season_ids) {
  
  season_url <- str_c(base_fixtures_url, i, end_fixtures_url)
  
  comp_no <- unlist(str_split(i, fixed("-")))[4]
  
  html <- read_html(season_url)
  
  match_urls <- html %>%
    html_nodes("a") %>%
    html_attr("href")
  urls_tidy <- match_urls %>%
    as.tibble()
  
  urls_filtered <- urls_tidy %>%
    filter(grepl(comp_no, value) & grepl(scorecard_indicator, value))
  urls_filtered$season <- i
  fixture_urls <- rbind(fixture_urls, urls_filtered)
  fixture_urls <- unique(fixture_urls)
  
}

base_match_url <- "https://www.espncricinfo.com"

match_index <- str_c(base_match_url, fixture_urls$value) %>%
  tibble() %>%
  mutate(index = row_number()) %>%
  clean_names()

all_match_urls <- unlist(match_index$x)
  
focus_team_i <- "sydney-sixers"
focus_team_ii <- "birmingham-phoenix"
focus_team_iii <- "royal-challengers"
focus_match_urls_i <- all_match_urls[grepl(focus_team_i, all_match_urls)]
focus_match_urls_ii <- all_match_urls[grepl(focus_team_ii, all_match_urls)]
focus_match_urls_iii <- all_match_urls[grepl(focus_team_iii, all_match_urls)]
focus_match_urls <- c(focus_match_urls_i, focus_match_urls_ii, focus_match_urls_iii)

final_urls <- match_index %>%
  filter(x %in% focus_match_urls) %>%
  arrange(index) %>%
  select(-index) %>%
  unlist()

batting_records <- tibble()

for (i in final_urls) {
  
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
  
  second_inns_bat <- all_tables[[3]] %>%
    clean_names()
  
  if(names(second_inns_bat[1]) != "batting") {
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
  
  all_bat <- rbind(first_inns_bat, second_inns_bat)
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
  filter(!is.na(b)) %>%
  select(-m)

match_run_rates <- batting_records_filtered %>%
  group_by(url) %>%
  summarise(n_balls = sum(b),
            n_runs = sum(r))

runs_vs_exp <- left_join(x = batting_records_filtered, y = match_run_rates, by = c("url" = "url"))
runs_vs_exp <- runs_vs_exp %>%
  mutate(other_balls = n_balls - b,
         other_runs = n_runs - r,
         other_sr = other_runs / other_balls) %>%
  relocate(url, .before = player)
runs_vs_exp <- runs_vs_exp %>%
  mutate(batter_sr = r / b,
         sr_diff = batter_sr - other_sr,
         vs_exp = b * sr_diff)
runs_vs_exp <- runs_vs_exp %>%
  filter(b > 0) %>%
  relocate(url, .after = vs_exp) %>%
  separate(col = url, sep = "/", into = c("value_1", "value_2", "value_3", "value_4", "season")) %>%
  select(-value_1, -value_2, -value_3, -value_4)

focus_player <- "Ellyse Perry"

focus_innings <- runs_vs_exp %>%
  filter(player == focus_player)

roll_no <- 30

focus_innings <- focus_innings %>%
  mutate(inns_index = row_number(),
         .before = player)

focus_innings$rolling_vs_exp <- roll_sum(focus_innings$vs_exp, n = roll_no, fill = NA, align = "right")
focus_innings$rolling_balls <- roll_sum(focus_innings$b, n = roll_no, fill = NA, align = "right")
focus_innings$vs_exp_per_100 <- focus_innings$rolling_vs_exp / focus_innings$rolling_balls * 100

final_for_plot <- focus_innings %>%
  select(inns_index, vs_exp_per_100) %>%
  mutate(period_one = case_when(inns_index < 73 ~ vs_exp_per_100, TRUE ~ NA),
         period_two = case_when(inns_index > 70 & inns_index < 125 ~ vs_exp_per_100, TRUE ~ NA),
         period_three = case_when(inns_index > 122 ~ vs_exp_per_100, TRUE ~ NA))
  
focus_colour_one <- "#9BB347"
focus_colour_one_light <- "#CFD4BE"
focus_colour_two <- "#4782B3"
focus_colour_two_light <- "#BECAD4"
focus_colour_three <- "#B04682"
focus_colour_three_light <- "#D4BECA"

line_rough <- ggplot(data = final_for_plot, aes(x = inns_index)) +
  geom_ribbon(aes(ymin = 0, ymax = period_one), fill = focus_colour_one_light) +
  geom_line(aes(y = period_one), colour = focus_colour_one, linewidth = 1.5) +
  geom_ribbon(aes(ymin = 0, ymax = period_two), fill = focus_colour_two_light) +
  geom_line(aes(y = period_two), colour = focus_colour_two, linewidth = 1.5) +
  geom_ribbon(aes(ymin = 0, ymax = period_three), fill = focus_colour_three_light) +
  geom_line(aes(y = period_three), colour = focus_colour_three, linewidth = 1.5) +
  scale_y_continuous(name = NULL,
                     limits = c(-24, 48),
                     expand = c(0,0),
                     breaks = seq(-20, 50, 10),
                     labels = label_number(style_positive = "plus")) +
  scale_x_continuous(limits = c(25, 179),
                     expand = c(0,0)) +
  theme(axis.line.x = element_blank(),
        axis.line.y = element_blank(),
        axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        axis.ticks = element_blank(),
        panel.grid.major.x = element_blank()) +
  geom_hline(yintercept = 0,
             colour = ptb_dark_grey) +
  geom_vline(xintercept = 71,
             size = 0.75,
             color = ptb_light_grey,
             linetype = "dashed") +
  geom_vline(xintercept = 123,
             size = 0.75,
             color = ptb_light_grey,
             linetype = "dashed")
line_rough

themed_line <- line_rough +
  theme_ptb()
themed_line

labelled_line <- themed_line +
  labs(title = "Ellyse Perry has been a very efficient<br>T20 batter <b style='color:#B04682'>since the 2023 WBBL</b>",
       subtitle = "<b>Runs scored vs. expectation per 100 balls</b> by <br><b>Ellyse Perry</b> in her <b>franchise T20</b> career so far",
       caption = "<i>Calculated on a 30-innings rolling basis; includes games<br>in the WBBL, the WPL and the Hundred since 2015</i><br><br>Data: ESPNCricinfo | Chart: Plot the Ball")
labelled_line
