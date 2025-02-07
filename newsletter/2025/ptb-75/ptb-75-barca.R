library(tidyverse)
library(rvest)
library(worldfootballR)
library(janitor)
library(lemon)
library(ggtext)

season_match_logs <- c("https://fbref.com/en/squads/15f49df1/2022-2023/matchlogs/all_comps/schedule/Barcelona-Women-Scores-and-Fixtures-Liga-F",
                       "https://fbref.com/en/squads/15f49df1/2023-2024/matchlogs/all_comps/schedule/Barcelona-Women-Scores-and-Fixtures-Liga-F",
                       "https://fbref.com/en/squads/15f49df1/2024-2025/matchlogs/all_comps/schedule/Barcelona-Women-Scores-and-Fixtures-Liga-F")

all_matches <- c()

for (i in season_match_logs) {
  
  Sys.sleep(3)
  
  html <- read_html(i)
  
  links <- html %>%
    html_nodes("a") %>%
    html_attr("href")
  
  match_urls <- unique(links[grepl("/matches/", links)])
  
  all_matches <- c(all_matches, match_urls) %>%
    unique()

}

final_urls <- tibble(complete = all_matches, links = all_matches) %>%
  separate(col = links, sep = "/", into = c("col1", "col2", "col3", "col4", "col5")) %>%
  filter(!is.na(col5)) %>%
  mutate(final = str_c("https://fbref.com", complete),
         .before = complete) %>%
  select(final) %>%
  unique() %>%
  unlist()

final_urls <- final_urls[!grepl(fixed("Champions-League"), x = final_urls)]

all_shots <- fb_match_shooting(match_url = final_urls)

barca_shots <- all_shots %>%
  clean_names() %>%
  filter(squad == "Barcelona") %>%
  select(-squad, -home_away, -match_half, -minute, -p_sx_g) %>%
  mutate(date = ymd(date),
         x_g = as.numeric(x_g),
         distance = as.integer(distance)) %>%
  mutate(shot_no =row_number(),
         season = case_when(date < ymd("2023-07-01") ~ "2022-23",
                            date < ymd("2024-07-01") ~ "2023-24",
                            date < ymd("2025-07-01") ~ "2024-25",
                            TRUE ~ "!"),
         .before = "date")

summary_by_sca <- barca_shots %>%
  group_by(body_part, event_sca_1, event_sca_2) %>%
  summarise(count = n()) %>%
  arrange(desc(count))

shots_in_possession <- barca_shots %>%
  filter(event_sca_1 == "Pass (Live)" & event_sca_2 == "Pass (Live)") %>%
  filter(body_part == "Right Foot" | body_part == "Left Foot") %>%
  select(shot_no, season, distance) %>%
  mutate(distance_bucket = case_when(distance < 10 ~ "A 0-9 yards",
                                     distance < 19 ~ "B 9-18 yards",
                                     TRUE ~ "C 18+ yards"))

possession_summary <- shots_in_possession %>%
  group_by(season, distance_bucket) %>%
  summarise(count = n())

no_games <- barca_shots %>%
  select(season, date) %>%
  unique() %>%
  group_by(season) %>%
  summarise(matches = n())

possession_total <- shots_in_possession %>%
  group_by(season) %>%
  summarise(total = n())

possession_final <- left_join(possession_summary, no_games, by = "season") %>%
  mutate(per_game = count / matches) %>%
  select(-count, -matches) %>%
  arrange(distance_bucket)

facet_labels <- c("A 0-9 yards" = "Shots in possession from <span style='color:#B38C00'>0-9 yards</span>",
                  "B 9-18 yards" = "Shots in possession from <span style='color:#B39B47'>9-18 yards</span>",
                  "C 18+ yards" = "Shots in possession from <span style='color:#B3AB8F'>18+ yards</span>")

rough_bars <- ggplot(possession_final, aes(x = reorder(season, desc(season)), y = per_game, fill = season)) +
  geom_bar(stat = "identity",
           width = 0.6) +
  coord_flip() +
  facet_rep_wrap(~distance_bucket,
                 nrow = 4,
                 repeat.tick.labels =  TRUE,
                 labeller = labeller(distance_bucket = facet_labels)) +
  scale_y_continuous(limits = c(0, 11),
                     breaks = seq(0, 10, 2),
                     expand = c(0, 0)) +
  scale_fill_manual(values = c("grey", "grey", "#CC2941")) +
  theme(legend.position = "none",
        axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        axis.line.x = element_blank(),
        axis.ticks = element_blank(),
        panel.grid.major.y = element_blank())
rough_bars

themed_bars <- rough_bars +
  theme_ptb()
themed_bars

labelled_bars <- themed_bars +
  labs(title = "Barça have become more reliant on<br>shots from distance <b style='color:#CC2941'>this season</b>",
       subtitle = "<b>Shots</b> per game generated <b>in possession</b> by<br><b>Barça Femení</b> in <b>Liga F</b> play since 2022-23",
       caption = "<br><i>'Shots in possession' = shots with feet following two open-play passes</i><br><br>Data: FBref | Chart: Plot the Ball")
labelled_bars
