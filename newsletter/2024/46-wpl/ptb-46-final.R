library(tidyverse)
library(rvest)
library(janitor)
library(scales)
library(ggtext)

base_fixtures_url <- "https://www.espncricinfo.com/series/"
end_fixtures_url <- "/match-schedule-fixtures-and-results"
scorecard_indicator <- "/full-scorecard"

id_wbbl <- "wbbl-2023-24-1387171"
id_wpl <- "women-s-premier-league-2022-23-1348825"
season_ids <- c(id_wbbl, id_wpl)

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
  
  venue <- unlist(all_tables[[5]][1,1])
  
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
  all_bat$venue <- venue
  all_bowl <- rbind(first_inns_bowl, second_inns_bowl)
  all_bowl$venue <- venue
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
  filter(!is.na(b)) %>%
  mutate(info = str_remove(url, "https://www.espncricinfo.com/series/")) %>%
  select(-url) %>%
  separate(col = info, into = c("comp", "match"), sep = "/")

competition_summary <- batting_records_filtered %>%
  group_by(comp) %>%
  summarise(tot_runs = sum(r),
            tot_bf = sum(b),
            tot_4s = sum(x4s),
            tot_6s = sum(x6s)) %>%
  mutate(run_rate = tot_runs / tot_bf,
         bound_4_percent = tot_4s / tot_bf,
         bound_6_percent = tot_6s / tot_bf,
         eff_bound_percent = (tot_4s + (1.5 * tot_6s)) / tot_bf,
         non_bound_sr = (tot_runs - (tot_4s * 4) - (tot_6s * 6)) / (tot_bf - (tot_4s) - (tot_6s)))

venue_summary <- batting_records_filtered %>%
  group_by(venue, comp) %>%
  summarise(tot_runs = sum(r),
            tot_bf = sum(b),
            tot_4s = sum(x4s),
            tot_6s = sum(x6s)) %>%
  mutate(run_rate = tot_runs / tot_bf,
         bound_4_percent = tot_4s / tot_bf,
         bound_6_percent = tot_6s / tot_bf,
         eff_bound_percent = (tot_4s + (1.5 * tot_6s)) / tot_bf * 100,
         non_bound_sr = (tot_runs - (tot_4s * 4) - (tot_6s * 6)) / (tot_bf - (tot_4s) - (tot_6s)) * 100)

venue_final <- venue_summary %>%
  filter(tot_bf > 240) %>%
  select(1, 2, 4, 7, 10, 11)

wpl_colour <- "#9479d2"
wbbl_colour <- "#dbe02e"
neutral_colour <- "#b0a290"

rough_scatter <- ggplot(venue_final, aes(y = non_bound_sr, x = eff_bound_percent)) +
  geom_point(color = case_when(venue_final$comp == "women-s-premier-league-2022-23-1348825" ~ wpl_colour,
                               TRUE ~ wbbl_colour),
             alpha = case_when(venue_final$comp == "women-s-premier-league-2022-23-1348825" ~ 1,
                               TRUE ~ 0.7),
             size = 5) +
  scale_x_continuous(name = NULL,
                     limits = c(9, 26),
                     breaks = seq(10, 25, 5),
                     expand = c(0,0),
                     labels = percent_format(accuracy = 1, scale = 1)) +
  scale_y_continuous(name = NULL,
                     limits = c(31, 89),
                     breaks = seq(40, 80, 10),
                     expand = c(0,0)) +
  theme(legend.position = "none",
        axis.line.x = element_blank(),
        axis.line.y = element_blank()) +
  annotate(geom = "text",
           x = 10.25,
           y = 87.5,
           label = "Non-boundary\nstrike rate",
           family = ptb_font,
           colour = ptb_dark_grey,
           fontface = "bold",
           hjust = 0,
           vjust = 1) +
  annotate(geom = "text",
           x = 24.5,
           y = 39,
           label = "Effective\nboundary %",
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
  labs(title = "The inaugural <span style='color:#9479d2;'>WPL</span> favoured hitters",
       subtitle = "<b>Non-boundary strike rate</b> and <b>effective<br>boundary %</b> recorded by batters at each<br>venue in the <b style='color:#9479d2;'>2023 WPL</b> and <b style='color:#dbe02e'>2023 WBBL</b>",
       caption = "<i>Venues with <240 total deliveries excluded</i><br><br>Data: ESPNCricinfo | Chart: Plot the Ball")
labelled_scatter


player_summary <- batting_records_filtered %>%
  group_by(player, comp) %>%
  summarise(tot_runs = sum(r),
            tot_bf = sum(b),
            tot_4s = sum(x4s),
            tot_6s = sum(x6s)) %>%
  mutate(run_rate = tot_runs / tot_bf,
         bound_4_percent = tot_4s / tot_bf,
         bound_6_percent = tot_6s / tot_bf,
         eff_bound_percent = (tot_4s + (1.5 * tot_6s)) / tot_bf,
         non_bound_sr = (tot_runs - (tot_4s * 4) - (tot_6s * 6)) / (tot_bf - (tot_4s) - (tot_6s)))

player_comp <- player_summary %>%
  select(1, 2, 4, 7)

player_comp$comp <- str_remove_all(player_comp$comp, "-2023-24-1387171")
player_comp$comp <- str_remove_all(player_comp$comp, "-2022-23-1348825")

player_comp_final <- player_comp %>%
  pivot_wider(names_from = "comp",
              values_from = c("tot_bf", "run_rate")) %>%
  clean_names() %>%
  filter(!is.na(tot_bf_wbbl) & !is.na(tot_bf_women_s_premier_league))

final_list <- player_comp_final %>%
  filter(tot_bf_wbbl > 60 & tot_bf_women_s_premier_league > 60) %>%
  mutate(score_diff = run_rate_women_s_premier_league - run_rate_wbbl)

rough_scatter_2 <- ggplot(final_list, aes(y = run_rate_wbbl, x = run_rate_women_s_premier_league)) +
  geom_point(color = case_when(round(final_list$score_diff, 2) > 0 ~ wpl_colour,
                               round(final_list$score_diff, 2) == 0 ~ neutral_colour,
                               TRUE ~ wbbl_colour),
             alpha = case_when(final_list$score_diff > 0 ~ 0.65,
                               TRUE ~ 0.7),
             size = 5) +
  scale_x_continuous(name = NULL,
                     limits = c(0.4, 2.4),
                     breaks = seq(0.5, 2, 0.5),
                     expand = c(0,0)) +
  scale_y_continuous(name = NULL,
                     limits = c(0.4, 2.4),
                     breaks = seq(0.5, 2, 0.5),
                     expand = c(0,0)) +
  theme(legend.position = "none",
        axis.line.x = element_blank(),
        axis.line.y = element_blank()) +
  annotate(geom = "text",
           x = 0.525,
           y = 2.35,
           label = "2023 WBBL\nruns per ball",
           family = ptb_font,
           colour = ptb_dark_grey,
           fontface = "bold",
           hjust = 0,
           vjust = 1) +
  annotate(geom = "text",
           x = 2.35,
           y = 0.65,
           label = "2023 WPL\nruns per ball",
           family = ptb_font,
           colour = ptb_dark_grey,
           fontface = "bold",
           hjust = 1,
           vjust = 1)
rough_scatter_2

themed_scatter_2 <- rough_scatter_2 +
  theme_ptb() +
  geom_abline(intercept = 0, slope = 1, colour = ptb_dark_grey, alpha = 0.1, linetype = "dashed")
themed_scatter_2

labelled_scatter_2 <- themed_scatter_2 +
  labs(title = "Most batters scored <span style='color:#9479d2;'>faster in the WPL</span>",
       subtitle = "<b>Runs scored per ball</b> in the <b>WBBL</b> and <b>WPL</b> by<br>batters who played in both competitions in 2023",
       caption = "<i>Batters with <60 balls faced in either competition excluded</i><br><br>Data: ESPNCricinfo | Chart: Plot the Ball")
labelled_scatter_2
