library(tidyverse)
library(janitor)
library(StatsBombR)
library(scales)
library(ggtext)

comp <- FreeCompetitions() %>%
  filter(competition_name == "UEFA Women's Euro" | competition_name == "Women's World Cup")
matches <- FreeMatches(comp)

games_by_team <- matches %>%
  clean_names() %>%
  select(1, 2, 17, 24) %>%
  pivot_longer(cols = c("home_team_home_team_name", "away_team_away_team_name"))

team_summary <- games_by_team %>%
  group_by(value) %>%
  summarise(no_matches = n())

statsbomb_data <- free_allevents(MatchesDF = matches, Parallel = T)
clean_data <- allclean(statsbomb_data)

match_ends <- clean_data %>%
  clean_names() %>%
  filter(type_name == "Half End") %>%
  filter(period < 5) %>%
  select(1, 3, 4, 5, 6, 16, 22, 121)

extra_time <- match_ends %>%
  group_by(match_id) %>%
  summarise(end_check = sum(period)) %>%
  mutate(extra_time = if_else(end_check == 20, 1, 0))

match_ends <- left_join(match_ends, extra_time, by = c("match_id" = "match_id"))

match_ends <- match_ends %>%
  mutate(time_final = (minute * 60) + second)

match_ends_tidy <- match_ends %>%
  select(2, 6, 7, 8, 11) %>%
  pivot_wider(names_from = period, values_from = time_final) %>%
  clean_names()

match_ends_tidy <- match_ends_tidy %>%
  mutate(first = x1,
         second = x2 - (45 * 60),
         first_extra =  x3 - (90 * 60),
         second_extra = x4 - (105 * 60))

match_ends_tidy$first_extra <- replace_na(match_ends_tidy$first_extra, 0)
match_ends_tidy$second_extra <- replace_na(match_ends_tidy$second_extra, 0)

match_ends_tidy <- match_ends_tidy %>%
  mutate(time_mins = (first + second + first_extra + second_extra) / 60)

team_minutes <- match_ends_tidy %>%
  group_by(team_name) %>%
  summarise(no_matches = n(),
            no_mins = sum(time_mins),
            ave_mins = no_mins / no_matches)

team_summary <- left_join(team_summary, team_minutes, by = c("value" = "team_name", "no_matches" = "no_matches")) %>%
  clean_names()

all_passes <- clean_data %>%
  clean_names() %>%
  filter(type_name == "Pass")

all_passes$pass_outcome_name <- replace_na(all_passes$pass_outcome_name, "Complete")
all_passes$under_pressure <- replace_na(all_passes$under_pressure, FALSE)

all_passes_tidy <- all_passes %>%
  select(1, 2, 4, 5, 6, 7, 11, 16, 18, 20, 22, 26, 29, 30, 43, 45, 47, 49, 51, 121, 122, 123, 150, 151, 154, 155)

pressure_passes <- all_passes_tidy %>%
  filter(under_pressure == TRUE)

pressure_passes_tidy <- pressure_passes %>%
  mutate(complete_value = if_else(pass_outcome_name == "Complete", 1, 0),
         complete_distance = if_else(pass_outcome_name == "Complete", pass_length, 0),
         .after = pass_outcome_name)

no_pressure_tidy <- all_passes_tidy %>%
  filter(under_pressure == FALSE) %>%
  mutate(complete_value = if_else(pass_outcome_name == "Complete", 1, 0),
         complete_distance = if_else(pass_outcome_name == "Complete", pass_length, 0),
         .after = pass_outcome_name)

pressure_summary <- pressure_passes_tidy %>%
  group_by(pass_outcome_name) %>%
  summarise(count = n()) %>%
  mutate(total = sum(count),
         percent_share = count / total * 100)

no_pressure_summary <- no_pressure_tidy %>%
  group_by(pass_outcome_name) %>%
  summarise(count = n()) %>%
  mutate(total = sum(count),
         percent_share = count / total * 100)

pressure_by_team <- pressure_passes_tidy %>%
  group_by(team_name) %>%
  summarise(count = n(),
            complete = sum(complete_value),
            distance = sum(complete_distance)) %>%
  mutate(completion_rate = complete / count,
         ave_complete_dist = distance / complete)

pressure_per_game <- left_join(x = pressure_by_team, y = team_summary, by = c("team_name" = "value"))

pressure_per_game <- pressure_per_game %>%
  mutate(att_per_game = count / no_matches) %>%
  filter(no_matches >= 10) %>%
  filter(team_name != "Australia Women's" & team_name != "United States Women's")

no_pressure_by_team <- no_pressure_tidy %>%
  group_by(team_name) %>%
  summarise(count = n(),
            complete = sum(complete_value),
            distance = sum(complete_distance)) %>%
  mutate(completion_rate = complete / count,
         ave_complete_dist = distance / complete)

no_pressure_per_game <- left_join(x = no_pressure_by_team, y = team_summary, by = c("team_name" = "value"))

no_pressure_per_game <- no_pressure_per_game %>%
  mutate(att_per_game = count / no_matches) %>%
  filter(no_matches >= 10)

pressure_own_half <- pressure_passes_tidy %>%
  filter(location_x < 60)

own_half_by_team <- pressure_own_half %>%
  group_by(team_name) %>%
  summarise(count = n(),
            complete = sum(complete_value),
            distance = sum(complete_distance)) %>%
  mutate(completion_rate = complete / count * 100,
         ave_complete_dist = distance / complete)

own_half_per_game <- left_join(x = own_half_by_team, y = team_summary, by = c("team_name" = "value"))

own_half_per_game <- own_half_per_game %>%
  mutate(att_per_90 = count / no_mins * 90) %>%
  filter(no_matches >= 10) %>%
  filter(team_name != "Australia Women's" & team_name != "United States Women's")

spain_red <- "#E6173D"
background_grey <- "#C9C9C9"

scatter_rough <- ggplot(own_half_per_game, aes(x = att_per_90, y = completion_rate)) +
  geom_point(size = 6,
             color = case_when(own_half_per_game$team_name == "Spain Women's" ~ spain_red,
                               TRUE ~ background_grey),
             alpha = case_when(own_half_per_game$team_name == "Spain Women's" ~ 1,
                               TRUE ~ 0.5)) +
  scale_x_continuous(limits = c(25, 45),
                     breaks = seq(30, 40, 10),
                     expand = c(0,0)) +
  scale_y_continuous(limits = c(55, 85),
                     breaks = seq(60, 80, 10),
                     expand = c(0,0),
                     labels = percent_format(accuracy = 1, scale = 1)) +
  annotate(geom = "text",
           x = 25.5,
           y = 84,
           label = "Completion\nrate",
           family = ptb_font,
           colour = ptb_dark_grey,
           fontface = "bold",
           hjust = 0,
           vjust = 1) +
  annotate(geom = "text",
           x = 44.5,
           y = 59,
           label = "Attempts\nper 90 mins",
           family = ptb_font,
           colour = ptb_dark_grey,
           fontface = "bold",
           hjust = 1,
           vjust = 1) +
  theme(axis.title.x = element_blank(),
        axis.line.x = element_blank(),
        axis.line.y = element_blank())
scatter_rough

scatter_themed <- scatter_rough +
  theme_ptb()
scatter_themed

scatter_labelled <- scatter_themed +
  labs(title = "<span style='color:#E6173D'>Spain</span> excel at high-risk passing",
       subtitle = "Execution of <b>passes inside own half under<br>pressure</b> by <b>European national teams</b> at major<br>tournaments since 2019",
       caption = "<i>Teams with <10 games excluded</i><br><br>Data: StatsBomb | Chart: Plot the Ball")
scatter_labelled
