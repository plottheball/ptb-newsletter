library(tidyverse)
library(rvest)
library(janitor)
library(googlesheets4)
library(ggtext)

nba_url <- "https://www.nba.com/news/history-all-nba-teams"

nba_html <- read_html(nba_url)

all_p <- nba_html %>%
  html_nodes("p") %>%
  html_text()

p_tidy <- all_p[4:641]
p_tidy <- p_tidy[!grepl(fixed("Official release & voting totals"), p_tidy)]
p_tidy <- p_tidy[!grepl(fixed("Official release"), p_tidy)]
p_tidy <- p_tidy[!grepl(fixed("Voting Totals, PDF"), p_tidy)]
p_tidy <- p_tidy[!grepl(fixed("FIRST TEAM"), p_tidy)]
p_tidy <- p_tidy[!grepl(fixed("SECOND TEAM"), p_tidy)]
p_tidy <- p_tidy[!grepl(fixed("THIRD TEAM"), p_tidy)]

all_nba_table <- tibble(name = p_tidy)
all_nba_table <- all_nba_table %>%
  separate_wider_delim(cols = name, delim = ":", names = c("position", "player"))
all_nba_table <- all_nba_table %>%
  separate_wider_delim(cols = player, delim = ".", names = c("name_1", "team_1"), too_few = "align_start")
all_nba_table <- all_nba_table %>%
  separate_wider_delim(cols = name_1, delim = ",", names = c("name", "team"), too_few = "align_start")
all_nba_table$team <- replace_na(all_nba_table$team, " Seattle SuperSonics")

all_nba_table <- all_nba_table %>%
  select(-4)

all_nba_table <- all_nba_table %>%
  mutate(row_no = row_number(),
         season_index = ceiling(row_no / 15),
         season_end_year = 2024 - season_index,
         team_index = row_no - (season_index * 15) + 15,
         team_category = case_when(team_index < 6 ~ "First team",
                                   team_index < 11 ~ "Second team",
                                   TRUE ~ "Third team"),
         .before = position) %>%
  clean_names()

all_nba_table$name <- str_squish(all_nba_table$name)
all_nba_table$team <- str_squish(all_nba_table$team)

all_nba_table <- all_nba_table %>%
  select(-1, -2, -4)

all_players <- all_nba_table %>%
  group_by(name) %>%
  summarise(no_apps = n())

# Data manually checked at Basketball Reference and downloaded via Google Sheets
player_nationalities <- read_sheet("https://docs.google.com/spreadsheets/d/1HBUn4L8gHExnznOckvuYUbE2RmYzniqb3IA_HxReF4E/edit?usp=sharing") %>%
  clean_names()

all_nba_table <- all_nba_table %>%
  left_join(y = player_nationalities, by = c("name" = "name"))

table_final <- all_nba_table %>%
  arrange(season_end_year, continent) %>%
  mutate(row_no = row_number(),
         season_index = ceiling(row_no / 15),
         team_index = row_no - (season_index * 15) + 15,
         .before = season_end_year)

category_palette <- c("#F5D67A", "#c9092e", "#47B37D", "#566A8F", "#C69DE0")

waffle_rough <- ggplot(table_final, aes(x = team_index, y = season_end_year, colour = continent)) +
  geom_point(shape = 15, size = 2.5) +
  scale_colour_manual(values = category_palette, aesthetics = "colour") +
  scale_y_reverse(limits = c(2024, 1988),
                  breaks = seq(2020, 1990, -5),
                  labels = seq(2020, 1990, -5),
                  expand = c(0, 0)) +
  scale_x_continuous(limits = c(0, 30),
                     expand = c(0, 0))
waffle_rough

waffle_themed <- waffle_rough +
  theme_ptb() +
  theme(legend.position = "none",
        axis.title.y = element_blank(),
        axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.line = element_blank())
waffle_themed

waffle_labelled <- waffle_themed +
  labs(title = "The NBA is becoming truly global",
       subtitle = "The <b>regions</b> which produced the 15 players picked<br>in each season's <b>All-NBA</b> teams since 1988-89",
       caption = "Data: NBA.com; Basketball Reference | Chart: Plot the Ball")
waffle_labelled
