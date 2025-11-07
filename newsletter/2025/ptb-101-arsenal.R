library(tidyverse)
library(janitor)
library(worldfootballR)
library(googlesheets4)
library(RcppRoll)
library(lemon)
library(scales)
library(ggtext)

all_pl_urls <- fb_match_urls(country = "ENG", gender = "M", season_end_year = seq(2018, 2026, 1), tier = "1st", time_pause = 10)

arsenal_pl_urls <- all_pl_urls[grepl(fixed("Arsenal"), all_pl_urls)]

arsenal_lineups <- fb_match_lineups(arsenal_pl_urls, time_pause = 8)

city_pl_urls <- all_pl_urls[grepl(fixed("Manchester-City"), all_pl_urls)]

city_lineups <- fb_match_lineups(city_pl_urls, time_pause = 8)

liverpool_pl_urls <- all_pl_urls[grepl(fixed("Liverpool"), all_pl_urls)]

liverpool_lineups <- fb_match_lineups(liverpool_pl_urls, time_pause = 8)

arsenal_starters <- arsenal_lineups %>%
  clean_names() %>%
  filter(team == "Arsenal" & starting == "Pitch")

arsenal_matches <- arsenal_starters %>%
  select(matchday, match_url) %>%
  unique() %>%
  arrange(matchday) %>%
  mutate(match_index = row_number())

arsenal_final <- arsenal_starters %>%
  left_join(., arsenal_matches) %>%
  select(match_index, matchday, match_url, team, player_name, player_url)

city_starters <- city_lineups %>%
  clean_names() %>%
  filter(team == "Manchester City" & starting == "Pitch") %>%
  select(matchday, match_url, team, player_name, player_url)

missing_starters <- read_sheet(ss = "https://docs.google.com/spreadsheets/d/1Uka62K-z6qjqI_nCqHUvsKm-n-_aVpWw8hUV5cN3XC0/edit?gid=0#gid=0")

city_starters <- bind_rows(city_starters, missing_starters) %>%
  arrange(matchday)

city_matches <- city_starters %>%
  select(matchday, match_url) %>%
  unique() %>%
  arrange(matchday) %>%
  mutate(match_index = row_number(),
         .before = "matchday")

city_final <- city_starters %>%
  left_join(., city_matches)

liverpool_starters <- liverpool_lineups %>%
  clean_names() %>%
  filter(team == "Liverpool" & starting == "Pitch")

liverpool_matches <- liverpool_starters %>%
  select(matchday, match_url) %>%
  unique() %>%
  arrange(matchday) %>%
  mutate(match_index = row_number())

liverpool_final <- liverpool_starters %>%
  left_join(., liverpool_matches) %>%
  select(match_index, matchday, match_url, team, player_name, player_url)

match_sequence <- seq(1, 313, 1)
ref_no <- 38

arsenal_analysis <- tibble()

for (i in match_sequence) {
  
  game <- arsenal_final %>%
    filter(match_index == i)
  
  ref_games <- arsenal_final %>%
    filter(match_index < i & match_index >= (i - ref_no))
  
  ref_starts <- ref_games %>%
    group_by(player_name, player_url) %>%
    summarise(ref_starts = n())
  
  game <- left_join(game, ref_starts) %>%
    group_by(match_index, matchday, team) %>%
    summarise(similarity_score = sum(ref_starts, na.rm = TRUE))
  
  arsenal_analysis <- bind_rows(arsenal_analysis, game)
  
}

arsenal_rolling <- arsenal_analysis %>%
  mutate(roll_score = roll_sum(x = similarity_score, n = ref_no, align = "right", fill = NA, na.rm = TRUE),
         roll_ave = roll_score / ref_no,
         rebased = roll_ave / (11 * ref_no) * 100) %>%
  filter(match_index > 94)

city_analysis <- tibble()

for (i in match_sequence) {
  
  game <- city_final %>%
    filter(match_index == i)
  
  ref_games <- city_final %>%
    filter(match_index < i & match_index >= (i - ref_no))
  
  ref_starts <- ref_games %>%
    group_by(player_name, player_url) %>%
    summarise(ref_starts = n())
  
  game <- left_join(game, ref_starts) %>%
    group_by(match_index, matchday, team) %>%
    summarise(similarity_score = sum(ref_starts, na.rm = TRUE))
  
  city_analysis <- bind_rows(city_analysis, game)
  
}

city_rolling <- city_analysis %>%
  mutate(roll_score = roll_sum(x = similarity_score, n = ref_no, align = "right", fill = NA, na.rm = TRUE),
         roll_ave = roll_score / ref_no,
         rebased = roll_ave / (11 * ref_no) * 100) %>%
  filter(match_index > 94)

liverpool_analysis <- tibble()

for (i in match_sequence) {
  
  game <- liverpool_final %>%
    filter(match_index == i)
  
  ref_games <- liverpool_final %>%
    filter(match_index <= (i - 1) & match_index >= (i - ref_no))
  
  ref_starts <- ref_games %>%
    group_by(player_name, player_url) %>%
    summarise(ref_starts = n())
  
  game <- left_join(game, ref_starts) %>%
    group_by(match_index, matchday, team) %>%
    summarise(similarity_score = sum(ref_starts, na.rm = TRUE))
  
  liverpool_analysis <- bind_rows(liverpool_analysis, game)
  
}

liverpool_rolling <- liverpool_analysis %>%
  mutate(roll_score = roll_sum(x = similarity_score, n = ref_no, align = "right", fill = NA, na.rm = TRUE),
         roll_ave = roll_score / ref_no,
         rebased = roll_ave / (11 * ref_no) * 100) %>%
  filter(match_index > 94)

rolling_combined <- bind_rows(arsenal_rolling, city_rolling, liverpool_rolling) %>%
  filter(match_index > 96)

background_data <- rolling_combined %>%
  mutate(category = case_when(team == "Arsenal" ~ "Other 1",
                              team == "Liverpool" ~ "Other 2",
                              team == "Manchester City" ~ "Other 3")) %>%
  select(-team)

facet_labels <- c("Arsenal" = "<span style='color:#CC292E'>Arsenal</span>",
                  "Liverpool" = "<span style='color:#8FB3B1'>Liverpool</span>",
                  "Manchester City" = "<span style='color:#578AB3'>Man City</span>")

rough_lineup <- ggplot(rolling_combined, aes(x = match_index, y = rebased)) +
  geom_vline(xintercept = c(304, 266, 228, 190, 152, 114),
             linewidth = 0.75,
             color = ptb_light_grey,
             linetype = "dashed") +
  scale_y_continuous(name = NULL,
                     limits = c(38, 82),
                     expand = c(0, 0),
                     breaks = seq(40, 80, 20),
                     labels = percent_format(scale = 1)) +
  scale_x_continuous(limits = c(94, 313),
                     expand = c(0.2, 0.1)) +
  geom_line(data = background_data, aes(group = category), colour = "#d5d5d5", size = 1.25) +
  geom_line(linewidth = 1.75, alpha = 1, aes(colour = team)) +
  scale_colour_manual(values = c("Arsenal" = "#CC292E",
                                 "Liverpool" = "#8FB3B1",
                                 "Manchester City" = "#578AB3")) +
  facet_rep_wrap(~team,
                 ncol = 1,
                 labeller = labeller(team = facet_labels))
rough_lineup

themed_lineup <- rough_lineup +
  theme_ptb() +
  theme(axis.line = element_blank(),
        axis.ticks = element_blank(),
        axis.text.x = element_blank(),
        panel.grid.major.x = element_blank(),
        axis.title.x = element_blank(),
        legend.position = "none")
themed_lineup

labelled_lineup <- themed_lineup +
  labs(title = "<b style='color:#CC292E'>Arsenal</b>'s team selection has become<br>more consistent during Arteta's tenure",
       subtitle = "Average <b>'similarity score'</b> of the starting line-ups<br>of select Premier League teams since Jan 2020",
       caption = "<br>A team's <b>'similarity score'</b> is calculated by comparing their starting<br>line-up in one Premier League fixture to their previous 38 PL line-ups<br><br><i>Calculated on a 38-game rolling basis; data as at 26 Oct 2025</i><br><br>Data: FBref | Chart: Plot the Ball")
labelled_lineup