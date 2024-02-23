library(tidyverse)
library(rvest)
library(RSelenium)
library(janitor)
library(ggtext)
library(scales)

rD <- rsDriver(browser = "firefox", port = 4445L)
remDr <- rD[["client"]]

ladder_url_base <- "https://www.nrl.com/ladder/?competition=111&round=27&season="
seasons <- seq(1998, 2023, 1)

all_ladders <- tibble()

for (i in seasons) {
  
  Sys.sleep(1)
  print(Sys.time())
  
  ladder_url <- str_c(ladder_url_base, i)
  
  remDr$navigate(ladder_url)
  
  ladder_html <- remDr$getPageSource()[1]
  ladder_html <- unlist(ladder_html)
  
  ladder_html_clean <- read_html(ladder_html)
  
  ladder_table <- ladder_html_clean %>%
    html_nodes("table") %>%
    html_table()
  
  table_final <- ladder_table[[1]] %>%
    clean_names() %>%
    select(x, played, wins, drawn, lost, `for`, against) %>%
    mutate(season = i)
  
  all_ladders <- bind_rows(all_ladders, table_final)
  
}

season_summary <- all_ladders %>%
  group_by(season) %>%
  summarise(team_games = sum(played),
            team_points = sum(`for`),
            season_average = round(team_points / team_games, 2))

rough_line <- ggplot(season_summary, aes(x = season, y = team_average)) +
  geom_line()
rough_line

team_summary <- all_ladders %>%
  select(x, played, `for`, against, season) %>%
  mutate(for_per_game = round(`for` / played, 2),
         against_per_game = round(against / played, 2))

team_vs_avg <- left_join(x = team_summary, y = season_summary, by = c("season" = "season")) %>%
  select(x, season, for_per_game, against_per_game, season_average) %>%
  mutate(attack_margin = round(for_per_game - season_average, 2),
         defence_margin = -round(against_per_game - season_average, 2),
         net_margin = round(for_per_game - against_per_game, 2),
         category = case_when(x == "Panthers" & season > 2018 ~ "Focus",
                              TRUE ~ "Other"))

panthers_colour <- "#CC5289"
other_colour <- "#C2CCC6"

rough_scatter <- ggplot(team_vs_avg, aes(y = attack_margin, x = defence_margin)) +
  geom_vline(xintercept = 0,
             color = ptb_dark_grey,
             alpha = 0.9) +
  geom_hline(yintercept = 0,
             color = ptb_dark_grey,
             alpha = 0.9) +
  geom_point(colour = case_when(team_vs_avg$category == "Focus" ~ panthers_colour,
                                TRUE ~ other_colour),
             alpha = case_when(team_vs_avg$category == "Focus" ~ 1,
                                TRUE ~ 0.25),
             size = 3.5) +
  scale_x_continuous(limits = c(-24, 24),
                     breaks = seq(-20, 20, 10),
                     expand = c(0, 0),
                     labels = label_number(style_positive = "plus")) +
  scale_y_continuous(limits = c(-24, 24),
                     breaks = seq(-20, 20, 10),
                     expand = c(0, 0),
                     labels = label_number(style_positive = "plus")) +
  annotate(geom = "text",
           x = -23,
           y = 23,
           label = "Attacking\nperformance",
           family = ptb_font,
           colour = ptb_dark_grey,
           fontface = "bold",
           hjust = 0,
           vjust = 1) +
  annotate(geom = "text",
           x = -24,
           y = -20,
           label = "points per game",
           family = ptb_font,
           colour = ptb_dark_grey,
           hjust = 0,
           vjust = 0.5) +
  annotate(geom = "text",
           x = -24,
           y = 10,
           label = "points per game",
           family = ptb_font,
           colour = ptb_dark_grey,
           hjust = 0,
           vjust = 0.5) +
  annotate(geom = "text",
           x = 23,
           y = -23,
           label = "Defensive\nperformance",
           family = ptb_font,
           colour = ptb_dark_grey,
           fontface = "bold",
           hjust = 1,
           vjust = 0) +
  theme(axis.line.x = element_blank(),
        axis.line.y = element_blank(),
        axis.ticks = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        legend.position = "none")
rough_scatter

themed_scatter <- rough_scatter +
  theme_ptb()
themed_scatter

labelled_scatter <- themed_scatter +
  labs(title = "<b style='color:#CC5289'>Penrith</b> has an all-time great defence",
       subtitle = "<b>Attacking</b> and <b>defensive performance</b> of NRL<br>teams in each season since 1998 — expressed in<br>points per game <b>relative to league average</b>",
       caption = "Data: NRL.com | Chart: Plot the Ball")
labelled_scatter

draw_url_base <- "https://www.nrl.com/draw/?competition="
comp_id <- 111
url_round <- "&round="
url_season <- "&season="

rounds_2019 <- seq(1, 25, 1)
rounds_2020 <- seq(1, 20, 1)
rounds_2021 <- seq(1, 25, 1)
rounds_2022 <- seq(1, 25, 1)
rounds_2023 <- seq(1, 27, 1)

draw_urls_2019 <- str_c(draw_url_base, comp_id, url_round, rounds_2019, url_season, "2019")
draw_urls_2020 <- str_c(draw_url_base, comp_id, url_round, rounds_2020, url_season, "2020")
draw_urls_2021 <- str_c(draw_url_base, comp_id, url_round, rounds_2021, url_season, "2021")
draw_urls_2022 <- str_c(draw_url_base, comp_id, url_round, rounds_2022, url_season, "2022")
draw_urls_2023 <- str_c(draw_url_base, comp_id, url_round, rounds_2023, url_season, "2023")

draw_urls_final <- c(draw_urls_2019, draw_urls_2020, draw_urls_2021, draw_urls_2022, draw_urls_2023)

match_urls_final <- vector()

for (i in draw_urls_final) {
  
  Sys.sleep(1)
  print(Sys.time())
  remDr$navigate(i)
  
  draw_html <- remDr$getPageSource()[1]
  draw_html <- unlist(draw_html)
  
  draw_html_clean <- read_html(draw_html)
  
  all_urls <- draw_html_clean %>%
    html_nodes("a")
  
  urls_tidy <- all_urls %>%
    as.character() %>%
    as_tibble()
  
  urls_filtered <- urls_tidy %>%
    filter(str_detect(value, "draw/nrl-premiership"))
  
  urls_filtered <- urls_filtered %>%
    separate(col = value, into = c("element", "url", "class"), sep = " ") %>%
    select(-element, -class) %>%
    separate(col = url, into = c("element", "slug"), sep = "=") %>%
    select(-element)
  
  round_urls_final <- pull(urls_filtered, slug)
  
  match_urls_final <- c(match_urls_final, round_urls_final)
  
}

match_urls_final <- str_remove_all(match_urls_final, "\"")
match_urls_final <- unique(match_urls_final)
match_urls_final <- str_c("https://www.nrl.com", match_urls_final)

match_urls_final <- match_urls_final[!grepl(fixed("#tab-news-video"), match_urls_final)]
match_urls_final <- match_urls_final[!grepl(fixed("#tab-team-stats"), match_urls_final)]

player_data <- tibble()

for (i in match_urls_final) {
  
  Sys.sleep(1)
  print(Sys.time())
  remDr$navigate(i)
  match_html <- remDr$getPageSource()[1]
  match_html <- unlist(match_html)
  html_clean <- read_html(match_html)
  
  all_tables <- html_clean %>%
    html_nodes("table") %>%
    html_table()
  table_home <- all_tables %>%
    pluck(1) %>%
    clean_names()
  table_away <- all_tables %>%
    pluck(3) %>%
    clean_names()
  home_tidy <- table_home %>%
    select(c(2, 3, 4, 6, 8, 25, 26, 27, 36, 37, 38, 39, 49, 51, 52, 53, 56, 60))
  away_tidy <- table_away %>%
    select(c(2, 3, 4, 6, 8, 25, 26, 27, 36, 37, 38, 39, 49, 51, 52, 53, 56, 60))
  
  new_names_home <- as_vector(home_tidy[1, 1:18])
  new_names_away <- as_vector(away_tidy[1, 1:18])
  home_final <- home_tidy %>%
    filter(! row_number() %in% 1) %>%
    set_names(new_names_home) %>%
    clean_names() %>%
    mutate(team = "home")
  away_final <- away_tidy %>%
    filter(! row_number() %in% 1) %>%
    set_names(new_names_away) %>%
    clean_names() %>%
    mutate(team = "away")
  
  match_final <- rbind(home_final, away_final)
  match_final$player <- str_replace_all(match_final$player, "\n                \n                  ", " ")
  match_final <- match_final %>%
    separate(mins_played, c("mins", "seconds"))
  match_final[4:18] <- sapply(match_final[4:18], as.integer)
  match_final <- match_final %>%
    replace(is.na(.), 0)
  
  match_final$url <- i
  
  match_final <- match_final %>%
    select(player, number, position, mins, seconds, team, url)
  
  player_data <- rbind(player_data, match_final)
  
}

player_data_final <- player_data
player_data_final$url <- player_data_final$url %>%
  str_remove(., "https://www.nrl.com/draw/nrl-premiership/")
player_data_final <- player_data_final %>%
  separate(col = url, into = c("year", "round", "match"), sep = "/")
player_data_final <- player_data_final %>%
  separate(col = match, into = c("home_team", "away_team"), sep = "-v-")
player_data_final <- player_data_final %>%
  mutate(player_team = case_when(team == "home" ~ home_team,
                                 team == "away" ~ away_team,
                                 TRUE ~ "!"))
player_data_final <- player_data_final %>%
  mutate(player_id = case_when(player == "Sione Katoa" & position == "Hooker" ~ "Sione Katoa (HK)",
                               player == "Sione Katoa" & position == "Interchange" ~ "Sione Katoa (HK)",
                               player == "Sione Katoa" & position == "Winger" ~ "Sione Katoa (WI)",
                               TRUE ~ player))
player_data_final <- player_data_final %>%
  mutate(adj_mins = case_when(mins > 80 ~ 80,
                              TRUE ~ mins))

summary_by_team_game <- player_data_final %>%
  group_by(year, round, player_team) %>%
  summarise(no_players = n(),
            tot_mins = sum(adj_mins))

summary_by_team_season <- summary_by_team_game %>%
  group_by(year, player_team) %>%
  summarise(no_games = n(),
            tot_mins = sum(tot_mins))

summary_by_team <- summary_by_team_season %>%
  group_by(player_team) %>%
  summarise(no_seasons = n(),
            tot_games = sum(no_games),
            tot_mins = sum(tot_mins))

summary_by_player_season <- player_data_final %>%
  group_by(year, player_team, player_id) %>%
  summarise(no_games = n(),
            tot_mins = sum(adj_mins))

summary_by_player <- summary_by_player_season %>%
  group_by(player_team, player_id) %>%
  summarise(no_seasons = n(),
            tot_games = sum(no_games),
            tot_mins = sum(tot_mins))

players_by_2023_team <- summary_by_player_season %>%
  filter(year == 2023 & tot_mins > 0) %>%
  select(1, 2, 3)

summary_by_player_filtered <- summary_by_player %>%
  filter(player_team != "dolphins" & tot_mins > 0) %>%
  mutate(available_mins = 116 * 80,
         mins_share = tot_mins / available_mins * 100)

teams_list <- unique(unlist(summary_by_player_filtered$player_team))

top_13s <- tibble()

for (i in teams_list) {
  
  team_filtered <- summary_by_player_filtered %>%
    filter(player_team == i) %>%
    arrange(desc(mins_share))
  
  top_13 <- team_filtered %>%
    head(13) %>%
    mutate(rank = row_number()) %>%
    select(1, 2, 7, 8)
  
  top_13s <- bind_rows(top_13s, top_13)
  
}

median_mins <- top_13s %>%
  group_by(rank) %>%
  summarise(median_value = median(mins_share))

top_13s_final <- top_13s %>%
  select(1, 3, 4)

top_13s_summary <- top_13s_final %>%
  group_by(player_team) %>%
  summarise(combined = mean(mins_share))

top_13s_final <- top_13s_final %>%
  pivot_wider(names_from = player_team, values_from = mins_share)

top_13s_final <- left_join(top_13s_final, median_mins, by = c("rank" = "rank"))

rough_dots <- ggplot(top_13s_final, aes(y = rank)) +
  geom_point(aes(x = broncos), colour = other_colour, size = 4, alpha = 0.3, shape = 16) +
  geom_point(aes(x = bulldogs), colour = other_colour, size = 4, alpha = 0.3, shape = 16) +
  geom_point(aes(x = cowboys), colour = other_colour, size = 4, alpha = 0.3, shape = 16) +
  geom_point(aes(x = dragons), colour = other_colour, size = 4, alpha = 0.3, shape = 16) +
  geom_point(aes(x = eels), colour = other_colour, size = 4, alpha = 0.3, shape = 16) +
  geom_point(aes(x = knights), colour = other_colour, size = 4, alpha = 0.3, shape = 16) +
  geom_point(aes(x = rabbitohs), colour = other_colour, size = 4, alpha = 0.3, shape = 16) +
  geom_point(aes(x = raiders), colour = other_colour, size = 4, alpha = 0.3, shape = 16) +
  geom_point(aes(x = roosters), colour = other_colour, size = 4, alpha = 0.3, shape = 16) +
  geom_point(aes(x = `sea-eagles`), colour = other_colour, size = 4, alpha = 0.3, shape = 16) +
  geom_point(aes(x = sharks), colour = other_colour, size = 4, alpha = 0.3, shape = 16) +
  geom_point(aes(x = storm), colour = other_colour, size = 4, alpha = 0.3, shape = 16) +
  geom_point(aes(x = titans), colour = other_colour, size = 4, alpha = 0.3, shape = 16) +
  geom_point(aes(x = warriors), colour = other_colour, size = 4, alpha = 0.3, shape = 16) +
  geom_point(aes(x = `wests-tigers`), colour = other_colour, size = 4, alpha = 0.3, shape = 16) +
  geom_point(aes(x = median_value), fill = other_colour, colour = "black", size = 4, shape = 21) +
  geom_point(aes(x = panthers), fill = panthers_colour, colour = "black", size = 4, shape = 21) +
  scale_x_continuous(limits = c(-2, 103),
                     breaks = seq(0, 100, 20),
                     expand = c(0, 0),
                     labels = percent_format(scale = 1)) +
  scale_y_continuous(limits = c(0, 14),
                     breaks = seq(1, 13, 1),
                     expand = c(0, 0),
                     labels = label_ordinal())
rough_dots

themed_dots <- rough_dots +
  theme_ptb() +
  theme(axis.line.x = element_blank(),
        axis.line.y = element_blank(),
        axis.ticks = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank()) +
  geom_vline(xintercept = 0,
             color = ptb_dark_grey) +
  geom_vline(xintercept = 100,
             color = ptb_dark_grey,
             alpha = 0.5) +
  annotate(geom = "text",
           x = 2.5,
           y = 0.5,
           label = "Share of\nminutes",
           family = ptb_font,
           colour = ptb_dark_grey,
           fontface = "bold",
           hjust = 0,
           vjust = 0)
themed_dots

labelled_dots <- themed_dots +
  labs(title = "The <b style='color:#CC5289'>Panthers</b> have built cohesion<br>across their line-up over five seasons",
       subtitle = "Share of <b>possible minutes since 2019</b> played by<br>each NRL team's <b>13 most frequently used</b> players",
       caption = "<i>Data is for the regular season only; Redcliffe Dolphins excluded</i><br><br>Data: NRL.com | Chart: Plot the Ball")
labelled_dots
