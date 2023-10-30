library(tidyverse)
library(rvest)
library(janitor)
library(scales)
library(ggtext)
library(RcppRoll)

base_fixtures_url <- "https://www.espncricinfo.com/series/"
end_fixtures_url <- "/match-schedule-fixtures-and-results"
scorecard_indicator <- "/full-scorecard"

id_2023 <- "wbbl-2023-24-1387171"
id_2022 <- "wbbl-2022-23-1323553"
id_2021 <- "wbbl-2021-22-1269053"
id_2020 <- "wbbl-2020-21-1226776"
id_2019 <- "wbbl-2019-20-1188381"
season_ids <- c(id_2019, id_2020, id_2021, id_2022, id_2023)

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

thunder_match_urls <- all_match_urls[grepl("sydney-thunder-women", all_match_urls)]

batting_records <- tibble()
bowling_records <- tibble()

for (i in thunder_match_urls) {
  
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

litchfield_innings <- runs_vs_exp %>%
  filter(player == "Phoebe Litchfield")

roll_no <- 10

litchfield_innings <- litchfield_innings %>%
  mutate(inns_index = row_number(),
         .before = player)

litchfield_innings$rolling_vs_exp <- roll_sum(litchfield_innings$vs_exp, n = roll_no, fill = NA, align = "right")
litchfield_innings$rolling_balls <- roll_sum(litchfield_innings$b, n = roll_no, fill = NA, align = "right")
litchfield_innings$vs_exp_per_100 <- litchfield_innings$rolling_vs_exp / litchfield_innings$rolling_balls * 100

thunder_green <- "#009300"
thunder_green_light <- "#00930030"

line_rough <- ggplot(data = litchfield_innings, aes(x = inns_index)) +
  geom_path(aes(y = vs_exp_per_100), colour = thunder_green, linewidth = 1.5) +
  geom_ribbon(aes(ymin = 0, ymax = vs_exp_per_100), fill = thunder_green_light) +
  scale_y_continuous(name = NULL,
                     limits = c(-34, 29),
                     expand = c(0,0),
                     breaks = seq(-30, 20, 10),
                     labels = label_number(style_positive = "plus")) +
  scale_x_continuous(limits = c(3, 58),
                     expand = c(0,0)) +
  theme(axis.line.x = element_blank(),
        axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        axis.ticks.x = element_blank(),
        panel.grid.major.x = element_blank())
line_rough

themed_line <- line_rough +
  theme_ptb() +
  geom_hline(yintercept = 0,
             colour = ptb_dark_grey) +
  geom_vline(xintercept = 11,
             size = 0.5,
             color = ptb_light_grey,
             linetype = "dashed") +
  geom_vline(xintercept = 23,
             size = 0.5,
             color = ptb_light_grey,
             linetype = "dashed") +
  geom_vline(xintercept = 36,
             size = 0.5,
             color = ptb_light_grey,
             linetype = "dashed") +
  geom_vline(xintercept = 47,
             size = 0.5,
             color = ptb_light_grey,
             linetype = "dashed")
themed_line

labelled_line <- themed_line +
  labs(title = "<b style='color:#009300'>Litchfield</b> is already having a positive<br>impact with the bat in the WBBL",
       subtitle = "<b>Runs scored vs. expectation per 100 balls</b> by <br><b style='color:#009300'>Phoebe Litchfield</b> in the <b>WBBL</b> since 2019",
       caption = "<i>Calculated on a 10-innings rolling basis</i><br><br>Data: ESPNCricinfo | Chart: Plot the Ball")
labelled_line
