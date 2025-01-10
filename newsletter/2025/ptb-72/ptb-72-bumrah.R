library(tidyverse)
library(rvest)
library(janitor)
library(lemon)
library(scales)
library(ggtext)

away_url_base <- "https://stats.espncricinfo.com/ci/engine/stats/index.html?bowling_pacespin=1;class=1;filter=advanced;home_or_away=2;home_or_away=3;orderby=start;page="
first_page <- 1
away_url_end <- ";size=200;spanmin2=1+Aug+2019;spanval2=span;template=results;type=bowling;view=innings;wrappertype=print"
away_url_full <- str_c(away_url_base, first_page, away_url_end)

away_no_pages_source <- read_html(away_url_full) %>%
  html_node(xpath = "/html/body/div/div[3]/table[2]") %>%
  html_table()
away_no_pages <- unlist(away_no_pages_source[1,1]) %>%
  str_replace(., "Page 1 of ", "") %>%
  as.numeric()

away_rank <- seq(first_page, away_no_pages, 1)

away_links <- str_c(away_url_base, away_rank, away_url_end)

away_bowling_data <- tibble()

for (i in away_links) {
  print(Sys.time())
  Sys.sleep(1)
  
  table_data <- read_html(i) %>%
    html_node(xpath = "/html/body/div/div[3]/table[3]") %>%
    html_table() %>%
    clean_names()
  
  table_data$overs <- as.character(table_data$overs)
  table_data$mdns <- as.character(table_data$mdns)
  table_data$runs <- as.character(table_data$runs)
  table_data$wkts <- as.character(table_data$wkts)
  table_data$econ <- as.character(table_data$econ)
  table_data$inns <- as.character(table_data$inns)
  away_bowling_data <- bind_rows(away_bowling_data, table_data)
}

home_url_base <- "https://stats.espncricinfo.com/ci/engine/stats/index.html?bowling_pacespin=1;class=1;filter=advanced;home_or_away=1;orderby=start;page="
first_page <- 1
home_url_end <- ";size=200;spanmin2=1+Aug+2019;spanval2=span;template=results;type=bowling;view=innings;wrappertype=print"
home_url_full <- str_c(home_url_base, first_page, home_url_end)

home_no_pages_source <- read_html(home_url_full) %>%
  html_node(xpath = "/html/body/div/div[3]/table[2]") %>%
  html_table()
home_no_pages <- unlist(home_no_pages_source[1,1]) %>%
  str_replace(., "Page 1 of ", "") %>%
  as.numeric()

home_rank <- seq(first_page, home_no_pages, 1)

home_links <- str_c(home_url_base, home_rank, home_url_end)

home_bowling_data <- tibble()

for (i in home_links) {
  print(Sys.time())
  Sys.sleep(1)
  
  table_data <- read_html(i) %>%
    html_node(xpath = "/html/body/div/div[3]/table[3]") %>%
    html_table() %>%
    clean_names()
  
  table_data$overs <- as.character(table_data$overs)
  table_data$mdns <- as.character(table_data$mdns)
  table_data$runs <- as.character(table_data$runs)
  table_data$wkts <- as.character(table_data$wkts)
  table_data$econ <- as.character(table_data$econ)
  table_data$inns <- as.character(table_data$inns)
  home_bowling_data <- bind_rows(home_bowling_data, table_data)
}

away_bowling_data$home_away <- "Away"
home_bowling_data$home_away <- "Home"

fast_bowling_data <- bind_rows(away_bowling_data, home_bowling_data)

fast_all_inns_clean <- fast_bowling_data %>%
  select(1, 2, 4, 5, 9, 10, 11, 12) %>%
  filter(overs != "DNB" & overs != "TDNB" & overs != "sub") %>%
  mutate(match_id = str_c(ground, " (", start_date, ")"),
         .before = player) %>%
  separate(col = overs,
           into = c("completed_overs", "completed_balls"),
           sep = "[.]")

fast_all_inns_clean$start_date <- dmy(fast_all_inns_clean$start_date)

fast_all_inns_clean$completed_overs <- as.integer(fast_all_inns_clean$completed_overs)
fast_all_inns_clean$completed_balls <- as.integer(replace_na(fast_all_inns_clean$completed_balls, 0))
fast_all_inns_clean$runs <- as.integer(fast_all_inns_clean$runs)
fast_all_inns_clean$wkts <- as.integer(fast_all_inns_clean$wkts)

fast_all_inns_clean$player <- str_replace_all(fast_all_inns_clean$player, fixed("(1)"), "I")

fast_all_inns_clean <- fast_all_inns_clean %>%
  mutate(balls_bowled = completed_balls + (6 * completed_overs),
         .before = runs) %>%
  select(-completed_overs, -completed_balls) %>%
  separate(player, into = c("name", "country"), sep = "[(]") %>%
  mutate(name = str_squish(name),
         country = str_squish(str_remove_all(country, fixed(")")))) %>%
  arrange(start_date)

fast_match_splits <- fast_all_inns_clean %>%
  group_by(match_id, start_date) %>%
  summarise(tot_balls = sum(balls_bowled),
            tot_runs = sum(runs),
            tot_wkts = sum(wkts))

fast_player_match_splits <- fast_all_inns_clean %>%
  group_by(match_id, start_date, name, country) %>%
  summarise(n_balls = sum(balls_bowled),
            n_runs = sum(runs),
            n_wkts = sum(wkts))

fast_vs_exp <- left_join(x = fast_player_match_splits, y = fast_match_splits, by = c("match_id", "start_date"))

fast_vs_exp <- fast_vs_exp %>%
  mutate(cycle = case_when(start_date <= ymd("2021-06-18") ~ "WTC 2021",
                           start_date <= ymd("2023-06-07") ~ "WTC 2023",
                           start_date <= ymd("2025-06-11") ~ "WTC 2025",
                           TRUE ~ "!"),
         other_balls = tot_balls - n_balls,
         other_runs = tot_runs - n_runs,
         other_wkts = tot_wkts - n_wkts) %>%
  select(-8, -9, -10) %>%
  relocate(cycle, .before = name) %>%
  arrange(start_date)

fast_vs_exp_summary <- fast_vs_exp %>%
  group_by(name, country, cycle) %>%
  summarise(n_matches = n(),
            n_balls = sum(n_balls),
            n_overs = n_balls / 6,
            n_runs = sum(n_runs),
            n_wkts = sum(n_wkts),
            other_balls = sum(other_balls),
            other_runs = sum(other_runs),
            other_wkts = sum(other_wkts)) %>%
  mutate(balls_per_match = n_balls / n_matches,
         wkts_per_match = n_wkts / n_matches,
         ave = n_runs / n_wkts,
         sr = n_balls / n_wkts,
         other_ave = other_runs / other_wkts,
         other_sr = other_balls / other_wkts,
         ave_vs_exp = ave - other_ave,
         sr_vs_exp = sr - other_sr) %>%
  arrange(desc(n_wkts))

fast_vs_exp_final <- fast_vs_exp_summary %>%
  filter(n_matches >= 5 & balls_per_match >= 120 & wkts_per_match >= 2) %>%
  select(name, country, cycle, n_overs, ave_vs_exp)

bumrah_colour_one <- "#66B2E1"
bumrah_colour_two <- "#007fce"
bumrah_colour_three <- "#004C7B"
background_colour <- "#C9C9C9"

rough_scatter <- ggplot(fast_vs_exp_final, aes(x = n_overs, y = ave_vs_exp)) +
  geom_hline(yintercept = 0,
             color = ptb_dark_grey,
             alpha = 0.9) +
  geom_point(color = case_when(fast_vs_exp_final$name == "JJ Bumrah" & fast_vs_exp_final$cycle == "WTC 2021" ~ bumrah_colour_one,
                               fast_vs_exp_final$name == "JJ Bumrah" & fast_vs_exp_final$cycle == "WTC 2023" ~ bumrah_colour_two,
                               fast_vs_exp_final$name == "JJ Bumrah" & fast_vs_exp_final$cycle == "WTC 2025" ~ bumrah_colour_three,
                               TRUE ~ background_colour),
             alpha = case_when(fast_vs_exp_final$name == "JJ Bumrah" ~ 1,
                               TRUE ~ 0.3),
             size = 4.5) +
  scale_y_continuous(name = NULL,
                     limits = c(-27, 27),
                     breaks = seq(-20, 20, 10),
                     labels = label_number(style_positive = "plus"),
                     expand = c(0,0)) +
  scale_x_continuous(name = NULL,
                     limits = c(60, 740),
                     breaks = seq(100, 700, 100),
                     labels = label_comma(),
                     expand = c(0,0)) +
  theme(legend.position = "none",
        axis.line.x = element_blank(),
        axis.line.y = element_blank(),
        axis.ticks = element_blank()) +
  annotate(geom = "text",
           x = 70,
           y = 26.6,
           label = "Runs conceded per\nwicket vs. expectation",
           family = ptb_font,
           colour = ptb_dark_grey,
           fontface = "bold",
           hjust = 0,
           vjust = 1) +
  annotate(geom = "text",
           x = 730,
           y = -20,
           label = "Overs bowled\nin WTC cycle",
           family = ptb_font,
           colour = ptb_dark_grey,
           fontface = "bold",
           hjust = 1,
           vjust = 1)
rough_scatter

themed_scatter <- rough_scatter +
  theme_ptb()
themed_scatter

labelled_scatter <- themed_scatter +
  labs(title = "<span style='color:#007fce;'>Bumrah</span> has become more effective<br>in each successive WTC cycle",
       subtitle = "Performance of <b>fast bowlers</b> vs. their peers in the<br><b>World Test Championship</b> era (since August 2019)",
       caption = "<i>Data as at 8 Jan 2025; fast bowlers with <6 matches played, <120<br>balls per match or <2 wickets per match in a given cycle excluded</i><br><br>Data: ESPNCricinfo | Chart: Plot the Ball")
labelled_scatter
