library(tidyverse)
library(rvest)
library(janitor)
library(ggtext)
library(scales)
library(ggbeeswarm)

cp3_seasons <- seq(2006, 2024, 1)

all_data <- tibble()

for (i in cp3_seasons) {
  
  Sys.sleep(1)
  
  url <- str_c("https://www.basketball-reference.com/leagues/NBA_", i, "_advanced.html")
  
  html <- read_html(url)
  
  tables <- html %>%
    html_nodes("table") %>%
    html_table()
  
  key_table <- tables[[1]] %>%
    clean_names() %>%
    select(2, 3, 4, 5, 6, 7, 26, 27, 28) %>%
    mutate(season_end_year = i)
  
  all_data <- bind_rows(all_data, key_table)
  
}

clean_data <- all_data %>%
  clean_names() %>%
  filter(player != "Player") %>%
  mutate(age = as.integer(age),
         g = as.integer(g),
         mp = as.integer(mp),
         obpm = as.numeric(obpm),
         dbpm = as.numeric(dbpm),
         bpm = as.numeric(bpm),
         id = str_c(player, " (", season_end_year, ")"),
         id_team = str_c(player, " (", season_end_year, ")", " (", tm, ")"))

team_changes <- clean_data %>%
  filter(tm == "TOT") %>%
  select(tm, id)

data_to_remove <- clean_data %>%
  filter(id %in% team_changes$id & tm != "TOT")

final_data <- clean_data %>%
  filter(!id_team %in% data_to_remove$id_team) %>%
  select(-id, -id_team) %>%
  relocate(season_end_year,
           .before = player)

season_data <- final_data %>%
  group_by(season_end_year) %>%
  summarise(tot_mp = sum(mp))

age_data <- final_data %>%
  group_by(season_end_year, age) %>%
  summarise(tot_mp = sum(mp)) %>%
  mutate(category = case_when(age < 21 ~ "Young",
                              age > 33 ~ "Old",
                              TRUE ~ "Other"))

age_summary <- age_data %>%
  group_by(season_end_year, category) %>%
  summarise(mins = sum(tot_mp))

age_summary <- left_join(x = age_summary, y = season_data, by = c("season_end_year" = "season_end_year"))

age_summary <- age_summary %>%
  mutate(share = mins / tot_mp * 100)

young_colour <- "#40807C"
other_colour <- "#E6E6E670"
old_colour <- "#E67391"

rough_area <- ggplot(age_summary, aes(x = season_end_year, y = share, fill = category)) +
  geom_area() +
  scale_fill_manual(values = c(old_colour, other_colour, young_colour)) +
  scale_x_continuous(limits = c(2004, 2026),
                     breaks = seq(2006, 2024, 6),
                     labels = c("2005-06", "'11-12", "'17-18", "2023-24"),
                     expand = c(0,0)) +
  scale_y_continuous(limits = c(0, 100.1),
                     expand = c(0, 0),
                     breaks = c(0, 10, 50, 90, 100),
                     labels = percent_format(scale = 1))
rough_area

themed_area <- rough_area +
  theme_ptb() +
  theme(axis.line = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank(),
        legend.position = "none")
themed_area

labelled_area <- themed_area +
  labs(title = "Both <b style='color:#E67391'>old</b> and <b style='color:#40807C'>young</b> players now play<br>more than they did in the late 2000s",
       subtitle = "% of NBA regular-season minutes played by players<br><b style='color:#E67391'>older than 33</b> and <b style='color:#40807C'>younger than 21</b> since 2005-06",
       caption = "Data: Basketball Reference | Chart: Plot the Ball")
labelled_area

filtered_data <- final_data %>%
  mutate(mp_game = mp / g,
         .after = mp)%>%
  filter(mp >= 1000 & mp_game >= 25) %>%
  arrange(-mp)

young_seasons <- filtered_data %>%
  filter(age < 21)

old_seasons <- filtered_data %>%
  filter(age > 33)

young_old_combined <- bind_rows(young_seasons, old_seasons) %>%
  mutate(category_age = case_when(age < 21 ~ "Young",
                                  age > 33 ~ "Old",
                                  TRUE ~ "!!!"),
         category_bpm = case_when(bpm >= 4 ~ "All-Star",
                                  bpm < -2 ~ "End-of-bench",
                                  TRUE ~ "Other"),
         category = str_c(category_bpm, " (", category_age, ")"),
         season_end_year = fct_relevel(as.character(season_end_year),
                                       "2024", "2023", "2022", "2021", "2020",
                                       "2019", "2018", "2017", "2016", "2015",
                                       "2014", "2013", "2012", "2011", "2010",
                                       "2009", "2008", "2007", "2006"))

young_colour_alt <- "#8FB3B0"
old_colour_alt <- "#E6B8C4"

rough_beeswarm <- ggplot(young_old_combined, aes(x = bpm, y = season_end_year, fill = category)) +
  geom_vline(xintercept = 0,
             color = ptb_dark_grey,
             alpha = 0.9) +
  geom_quasirandom(varwidth = FALSE,
                   width = 0.3,
                   size = 2.5,
                   color = "black",
                   shape = "circle filled") +
  scale_x_continuous(limits = c(-6.9, 9.9),
                     breaks = c(-2, 0, 4, 8),
                     expand = c(0,0),
                     labels = label_number(style_positive = "plus")) +
  scale_y_discrete(labels = c("2023-24", "", "", "", "",
                              "", "'17-18", "", "", "",
                              "", "", "'11-12", "", "",
                              "", "", "", "2005-06")) +
  scale_fill_manual(values = c(old_colour, young_colour, old_colour_alt, young_colour_alt, old_colour_alt, young_colour_alt))
rough_beeswarm
  
themed_beeswarm <- rough_beeswarm +
  theme_ptb() +
  theme(axis.line = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank(),
        panel.grid.major.y = element_blank(),
        legend.position = "none")
themed_beeswarm

labelled_beeswarm <- themed_beeswarm +
  labs(title = "The NBA's current crop of <b style='color:#E67391'>veterans</b><br>still contributes at an all-star level",
       subtitle = "Distribution of <b>BPM</b> ratings of regular NBA players<br><b style='color:#E67391'>older than 33</b> and <b style='color:#40807C'>younger than 21</b> since 2005-06",
       caption = "<i>Players with <1,000 total minutes played or <25 MPG excluded</i><br><br>Data: Basketball Reference | Chart: Plot the Ball")
labelled_beeswarm
