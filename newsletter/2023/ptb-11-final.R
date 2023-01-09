library(tidyverse)
library(rvest)
library(janitor)
library(gt)
library(gtExtras)
library(svglite)
library(scales)
library(ggtext)
library(lemon)

base_fixtures_url <- "https://www.espncricinfo.com/series/"
end_fixtures_url <- "/match-schedule-fixtures-and-results"
scorecard_indicator <- "/full-scorecard"

csa_2022 <- "csa-t20-challenge-2022-23-1334878"
csa_2021 <- "csa-provincial-t20-cup-2021-22-1277887"
ipl_2022 <- "indian-premier-league-2022-1298423"
cpl_2022 <- "caribbean-premier-league-2022-1320379"
season_ids <- c(csa_2021, ipl_2022, cpl_2022, csa_2022)

fixture_urls <- tibble()

for (i in season_ids) {
  
  season_url <- str_c(base_fixtures_url, i, end_fixtures_url)
  
  html <- read_html(season_url)
  
  match_urls <- html %>%
    html_nodes("a") %>%
    html_attr("href")
  
  urls_tidy <- match_urls %>%
    as_tibble()
  
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
  filter(!is.na(b))

boundary_data <- batting_records_filtered %>%
  mutate(comp = case_when(str_detect(url, csa_2021) == TRUE ~ "CSA Provincial T20 Cup 2021/22",
                          str_detect(url, csa_2022) == TRUE ~ "CSA T20 Challenge 2022/23",
                          str_detect(url, ipl_2022) == TRUE ~ "Indian Premier League 2022",
                          str_detect(url, cpl_2022) == TRUE ~ "Caribbean Premier League 2022",
                          TRUE ~ "Other"))

boundary_by_comp <- boundary_data %>%
  group_by(comp) %>%
  summarise(tot_b = sum(b),
            tot_4s = sum(x4s),
            tot_6s = sum(x6s),
            boundary_percentage = (tot_4s + tot_6s) / tot_b,
            eff_bound_percentage = (tot_4s + (1.5 * tot_6s)) / tot_b)

players_by_comp <- boundary_data %>%
  group_by(player, comp) %>%
  summarise(tot_b = sum(b),
            tot_4s = sum(x4s),
            tot_6s = sum(x6s),
            boundary_percentage = (tot_4s + tot_6s) / tot_b,
            eff_bound_percentage = (tot_4s + (1.5 * tot_6s)) / tot_b)

player_boundaries_filtered <- players_by_comp %>%
  filter(tot_b >= 60)

player_for_plot <- player_boundaries_filtered %>%
  filter(comp != "CSA Provincial T20 Cup 2021/22" & comp != "CSA T20 Challenge 2022/23")

player_for_plot$comp <- factor(player_for_plot$comp,
                               levels = c("Indian Premier League 2022", "Caribbean Premier League 2022"))

facet_labels <- c("Indian Premier League 2022" = "IPL 2022",
                  "Caribbean Premier League 2022" = "CPL 2022")

mict_primary <- "#245289"
mict_secondary <- "#e9ca7a"
    
rough_scatter_basic <- ggplot(data = player_for_plot, aes(x = tot_b, y = boundary_percentage * 100)) +
  geom_point(color = if_else(player_for_plot$player == "Dewald Brevis", mict_primary, mict_secondary),
             alpha = if_else(player_for_plot$player == "Dewald Brevis", 1, 0.4),
             size = 2.5) +
  theme(legend.position = "none") +
  scale_y_continuous(name = NULL,
                     labels = percent_format(accuracy = 1, scale = 1),
                     limits = c(0, 44),
                     expand = c(0,0),
                     breaks = seq(0, 40, 10)) +
  scale_x_continuous(name = "Balls faced",
                     limits = c(0, 620),
                     expand = c(0,0),
                     breaks = seq(0, 600, 100)) +
  facet_rep_wrap(~comp, nrow = 3, repeat.tick.labels =  TRUE,
                 labeller = labeller(comp = facet_labels))
rough_scatter_basic
  
themed_scatter_basic <- rough_scatter_basic + theme_ptb()
themed_scatter_basic
  
labelled_scatter_basic <- themed_scatter_basic +
  labs(title = "Boundary % can obscure the true level<br>of a T20 batter's skills",
       subtitle = "<b>Boundary %</b> of batters in selected major men's T20<br>tournaments during 2022",
       caption = "<i>Players with fewer than 60 balls faced in a given tournament excluded</i><br><br>Data: ESPNCricinfo | Chart: Plot the Ball")
labelled_scatter_basic
  
rough_scatter_effective <- ggplot(data = player_for_plot, aes(x = tot_b, y = eff_bound_percentage * 100)) +
  geom_point(color = if_else(player_for_plot$player == "Dewald Brevis", mict_primary, mict_secondary),
             alpha = if_else(player_for_plot$player == "Dewald Brevis", 1, 0.4),
             size = 2.5) +
  theme(legend.position = "none") +
  scale_y_continuous(name = NULL,
                     labels = percent_format(accuracy = 1, scale = 1),
                     limits = c(0, 44),
                     expand = c(0,0),
                     breaks = seq(0, 40, 10)) +
  scale_x_continuous(name = "Balls faced",
                     limits = c(0, 620),
                     expand = c(0,0),
                     breaks = seq(0, 600, 100)) +
  facet_rep_wrap(~comp, nrow = 3, repeat.tick.labels =  TRUE,
                 labeller = labeller(comp = facet_labels))
rough_scatter_effective
  
themed_scatter_effective <- rough_scatter_effective + theme_ptb()
themed_scatter_effective
  
labelled_scatter_effective <- themed_scatter_effective +
  labs(title = "'Effective boundary %' — which accounts for<br>the higher value of 6s — is better",
       subtitle = "<b>Effective boundary %</b> of batters in selected major<br>men's T20 tournaments during 2022",
       caption = "<i>Players with fewer than 60 balls faced in a given tournament excluded</i><br><br>Data: ESPNCricinfo | Chart: Plot the Ball")
labelled_scatter_effective
