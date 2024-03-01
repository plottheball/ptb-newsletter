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
test_indicator <- "-test-"

id_2025 <- "icc-world-test-championship-2023-2025-1345943"
id_2023 <- "icc-world-test-championship-2021-2023-1268315"
season_ids <- c(id_2023, id_2025)

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
    filter(grepl(test_indicator, value) & grepl(scorecard_indicator, value))
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
  
  first_inns_bat <- first_inns_bat %>%
    clean_names() %>%
    filter(batting != "")
  
  second_inns_bat <- all_tables[[3]] %>%
    clean_names()

  if(names(second_inns_bat[1]) != "batting") {
    second_inns_bat <- tibble("batting", "x", "r", "b", "m", "x4s", "x6s", "sr", "x_2", "x_3")
  }
  
  second_inns_bat <- second_inns_bat %>%
    clean_names() %>%
    filter(batting != "")
  
  third_inns_bat <- all_tables[[5]] %>%
    clean_names()

  if(names(third_inns_bat[1]) != "batting") {
    third_inns_bat <- tibble("batting", "x", "r", "b", "m", "x4s", "x6s", "sr", "x_2", "x_3")
  }
  
  third_inns_bat <- third_inns_bat %>%
    clean_names() %>%
    filter(batting != "")
  
  fourth_inns_bat <- all_tables[[7]] %>%
    clean_names()

  if(names(fourth_inns_bat[1]) != "batting") {
    fourth_inns_bat <- tibble("batting", "x", "r", "b", "m", "x4s", "x6s", "sr", "x_2", "x_3")
  }
  
  fourth_inns_bat <- fourth_inns_bat %>%
    clean_names() %>%
    filter(batting != "")
  
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
  filter(!is.na(b)) %>%
  unique()

batting_records_filtered <- batting_records_filtered %>%
  mutate(category = case_when(position < 3 ~ "Opener",
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

batting_records_openers <- batting_records_filtered %>%
  filter(category == "Opener")

runs_vs_exp <- left_join(x = batting_records_openers, y = match_splits, by = c("url" = "url", "category" = "category"))
runs_vs_exp <- runs_vs_exp %>%
  mutate(other_balls = n_balls - b,
         other_runs = n_runs - r,
         other_dismissals = n_dismissals - dismissed_y_n) %>%
  select(-2, -5, -6, -7, -8, -9) %>%
  relocate(url, .before = player)

openers_vs_exp <- runs_vs_exp %>%
  group_by(player) %>%
  summarise(no_inns = n(),
            runs_total = sum(r),
            balls_total = sum(b),
            dismissals_total = sum(dismissed_y_n),
            other_runs_total = sum(other_runs) - runs_total,
            other_balls_total = sum(other_balls) - balls_total,
            other_dismissals_total = sum(other_dismissals) - dismissals_total,
            average = runs_total / dismissals_total,
            strike_rate = runs_total / balls_total,
            average_other = other_runs_total / other_dismissals_total,
            strike_rate_other = other_runs_total / other_balls_total,
            ave_vs_exp = average - average_other,
            sr_vs_exp = strike_rate - strike_rate_other)

openers_final <- openers_vs_exp %>%
  filter(no_inns >= 15) %>%
  arrange(desc(ave_vs_exp))

openers_focus <- openers_final %>%
  select(1, 2, 3, 4, 5, 13)

openers_focus <- openers_focus %>%
  mutate(balls_per_dis = balls_total / dismissals_total,
         runs_per_dis = runs_total / dismissals_total,
         .after = no_inns) %>%
  select(-balls_total, -runs_total, -dismissals_total) %>%
  arrange(-no_inns)

jaiswal_colour <- "#007fce"
background_colour <- "#031f5a"

rough_scatter <- ggplot(openers_focus, aes(x = balls_per_dis, y = ave_vs_exp)) +
  geom_hline(yintercept = 0,
             color = ptb_dark_grey,
             alpha = 0.9) +
  geom_point(color = case_when(openers_focus$player == "Yashasvi Jaiswal" ~ jaiswal_colour,
                               TRUE ~ background_colour),
             alpha = case_when(openers_focus$player == "Yashasvi Jaiswal" ~ 1,
                               TRUE ~ 0.25),
             size = 5) +
  scale_y_continuous(name = NULL,
                     limits = c(-34, 34),
                     breaks = seq(-30, 30, 10),
                     labels = label_number(style_positive = "plus"),
                     expand = c(0,0)) +
  scale_x_continuous(name = NULL,
                     limits = c(30, 130),
                     breaks = seq(50, 125, 25),
                     expand = c(0,0)) +
  theme(legend.position = "none",
        axis.line.x = element_blank(),
        axis.line.y = element_blank()) +
  annotate(geom = "text",
           x = 33,
           y = 33,
           label = "Runs scored\nper dismissal\nvs. expectation",
           family = ptb_font,
           colour = ptb_dark_grey,
           fontface = "bold",
           hjust = 0,
           vjust = 1) +
  annotate(geom = "text",
           x = 124,
           y = -25,
           label = "Balls faced\nper dismissal",
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
  labs(title = "<span style='color:#007fce;'>Jaiswal</span> has been the standout test<br>opener of the last two WTC cycles",
       subtitle = "<b>Runs scored per dismissal vs. expectation</b> and<br><b> balls faced per dismissal</b> by opening batters<br>in the World Test Championship since Aug 2021",
       caption = "<i>Data as at 28 Feb 2024; openers with <15 completed innings excluded</i><br><br>Data: ESPNCricinfo | Chart: Plot the Ball")
labelled_scatter

url_base_lhb <- "https://stats.espncricinfo.com/ci/engine/stats/index.html?batting_hand=2;batting_positionmax1=2;batting_positionmin1=1;batting_positionval1=batting_position;class=1;filter=advanced;orderby=start;page="
first_page <- 1
url_end_lhb <- ";qualmin1=1;qualval1=balls_faced;size=200;spanmin1=1+Jan+2000;spanval1=span;template=results;type=batting;view=innings;wrappertype=print"
url_full_lhb <- str_c(url_base_lhb, first_page, url_end_lhb)

url_base_rhb <- "https://stats.espncricinfo.com/ci/engine/stats/index.html?batting_hand=1;batting_positionmax1=2;batting_positionmin1=1;batting_positionval1=batting_position;class=1;filter=advanced;orderby=start;page="
url_end_rhb <- ";qualmin1=1;qualval1=balls_faced;size=200;spanmin1=1+Jan+2000;spanval1=span;template=results;type=batting;view=innings;wrappertype=print"
url_full_rhb <- str_c(url_base_rhb, first_page, url_end_rhb)

no_pages_lhb_source <- read_html(url_full_lhb) %>%
  html_node(xpath = "/html/body/div/div[3]/table[2]") %>%
  html_table()
no_pages_lhb <- unlist(no_pages_lhb_source[1,1]) %>%
  str_replace(., "Page 1 of ", "") %>%
  as.numeric()

no_pages_rhb_source <- read_html(url_full_rhb) %>%
  html_node(xpath = "/html/body/div/div[3]/table[2]") %>%
  html_table()
no_pages_rhb <- unlist(no_pages_rhb_source[1,1]) %>%
  str_replace(., "Page 1 of ", "") %>%
  as.numeric()

rank_lhb <- seq(first_page, no_pages_lhb, 1)
rank_rhb <- seq(first_page, no_pages_rhb, 1)

links_lhb <- str_c(url_base_lhb, rank_lhb, url_end_lhb)
links_rhb <- str_c(url_base_rhb, rank_rhb, url_end_rhb)

lhb_data <- tibble()

for (i in links_lhb) {
  print(Sys.time())
  Sys.sleep(1)
  
  table_data <- read_html(i) %>%
    html_node(xpath = "/html/body/div/div[3]/table[3]") %>%
    html_table() %>%
    clean_names() %>%
    select(1, 2, 4, 5, 6, 10, 11, 12)
  
  table_data$runs <- as.character(table_data$runs)
  table_data$bf <- as.character(table_data$bf)
  table_data$x4s <- as.character(table_data$x4s)
  table_data$x6s <- as.character(table_data$x6s)
  table_data$hand <- "lhb"
  lhb_data <- bind_rows(lhb_data, table_data)
}

rhb_data <- tibble()

for (i in links_rhb) {
  print(Sys.time())
  Sys.sleep(1)
  
  table_data <- read_html(i) %>%
    html_node(xpath = "/html/body/div/div[3]/table[3]") %>%
    html_table() %>%
    clean_names() %>%
    select(1, 2, 4, 5, 6, 10, 11, 12)
  
  table_data$runs <- as.character(table_data$runs)
  table_data$bf <- as.character(table_data$bf)
  table_data$x4s <- as.character(table_data$x4s)
  table_data$x6s <- as.character(table_data$x6s)
  table_data$hand <- "rhb"
  rhb_data <- bind_rows(rhb_data, table_data)
}

batting_data <- bind_rows(lhb_data, rhb_data)

batting_clean <- batting_data

batting_clean$player <- str_replace_all(batting_clean$player,fixed("(1)"), "I")
batting_clean$player <- str_replace_all(batting_clean$player,fixed("(2)"), "II")
batting_clean$player <- str_replace_all(batting_clean$player,fixed("(3)"), "III")

batting_clean <- batting_clean %>%
  separate(col = player,
           into = c("batter_name", "batter_team"),
           sep =  "[(]")

batting_clean$batter_name <- str_squish(batting_clean$batter_name)
batting_clean$batter_team <- str_remove_all(batting_clean$batter_team, fixed(")"))

batting_clean$dismissed <- if_else(str_detect(batting_clean$runs, fixed("*")), 0, 1)
batting_clean$runs <- str_remove_all(batting_clean$runs, fixed("*"))
batting_clean$start_date <- dmy(batting_clean$start_date)
batting_clean[3:6] <- sapply(batting_clean[3:6], as.integer)

batting_clean$x4s <- replace_na(batting_clean$x4s, 0)
batting_clean$x6s <- replace_na(batting_clean$x6s, 0)

batting_clean <- batting_clean %>%
  arrange(start_date, ground, opposition) %>%
  mutate(year = year(start_date))

aggregate_summary <- batting_clean %>%
  group_by(hand) %>%
  summarise(no_inns = n(),
            no_dismissals = sum(dismissed),
            tot_runs = sum(runs),
            tot_bf = sum(bf),
            tot_4s = sum(x4s),
            tot_6s = sum(x6s),
            med_runs = median(runs),
            med_bf = median(bf)) %>%
  mutate(average = tot_runs / no_dismissals,
         bpd = tot_bf / no_dismissals,
         strike_rate = tot_runs / tot_bf,
         bound_4_percent = tot_4s / tot_bf,
         bound_6_percent = tot_6s / tot_bf,
         eff_bound_percent = (tot_4s + (1.5 * tot_6s)) / tot_bf,
         non_bound_sr = (tot_runs - (tot_4s * 4) - (tot_6s * 6)) / (tot_bf - (tot_4s) - (tot_6s)))

team_summary <- batting_clean %>%
  group_by(batter_team, hand) %>%
  summarise(no_inns = n()) %>%
  pivot_wider(names_from = "hand",
              values_from = "no_inns")

team_summary$rhb <- replace_na(team_summary$rhb, 0)
team_summary$lhb <- replace_na(team_summary$lhb, 0)

team_summary <- team_summary %>%
  mutate(total_inns = rhb + lhb,
         lhb_share = lhb / total_inns * 100,
         rhb_share = rhb / total_inns * 100) %>%
  filter(total_inns > 500) %>%
  arrange(desc(rhb_share))

team_final <- team_summary %>%
  select(batter_team, lhb_share, rhb_share) %>%
  pivot_longer(cols = c("lhb_share", "rhb_share"), names_to = "handedness", values_to = "share") %>%
  mutate(share_final = if_else(handedness == "lhb_share", share * -1, share),
         category = case_when(handedness == "lhb_share" & batter_team == "IND" ~ "Left Focus",
                              handedness == "lhb_share" & batter_team != "IND" ~ "Left Other",
                              handedness == "rhb_share" & batter_team == "IND" ~ "Right Focus",
                              handedness == "rhb_share" & batter_team != "IND" ~ "Right Other",
                              TRUE ~ "!"))

team_labels <- tibble(label = c("IND", "NZ",  "PAK", "BAN", "WI",  "SA",  "SL",  "ENG", "AUS"),
                      full = c("India", "New Zealand",  "Pakistan", "Bangladesh", "West Indies",  "South Africa",  "Sri Lanka",  "England", "Australia"))

team_final <- left_join(team_final, team_labels, by = c("batter_team" = "label"))

rough_bars <- ggplot(team_final, aes(x = fct_reorder(full, share_final), y = share_final, fill = category)) +
  geom_bar(stat = "identity",
           width = 0.55) +
  scale_fill_manual(values = c("#E67545", "#E6C5B8", "#1275B3", "#A3BCCC")) +
  geom_hline(yintercept = 0) +
  scale_y_continuous(limits = c(-100, 100),
                     breaks = c(-75, -50, -25, 0, 25, 50, 75),
                     labels = (abs(c(-75, -50, -25, 0, 25, 50, 75))),
                     expand = c(0, 0))+
  theme(legend.position = "none",
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.ticks.y = element_blank(),
        panel.grid.major.y = element_blank(),
        axis.line.x = element_blank(),
        axis.line.y = element_blank()) +
  coord_flip()
rough_bars

themed_bars <- rough_bars +
  theme_ptb()
themed_bars

labelled_bars <- themed_bars +
  labs(title = "India gives few opportunities in test<br>matches to <b style='color:#E67545'>left-handed openers</b>",
       subtitle = "Share of each team's innings by <b>openers</b><br>completed by <b style='color:#E67545'>left-handed</b> and <b style='color:#1275B3'>right-handed</b><br>batters in men's test matches since 2000<br>",
       caption = "<i>Data as at 28 Feb 2024</i><br><br>Data: ESPNCricinfo | Chart: Plot the Ball")
labelled_bars
