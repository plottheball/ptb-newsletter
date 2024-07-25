library(tidyverse)
library(rvest)
library(janitor)
library(googlesheets4)
library(ggtext)
library(RcppRoll)

wnba_url <- "https://www.wnba.com/history-all-wnba"

wnba_html <- read_html(wnba_url)

all_p <- wnba_html %>%
  html_nodes("p") %>%
  html_text()

p_tidy <- all_p[14:148]
p_tidy <- p_tidy[!grepl(fixed("ALL-WNBA FIRST TEAM"), p_tidy)]
p_tidy <- p_tidy[!grepl(fixed("All-WNBA First Team"), p_tidy)]
p_tidy <- p_tidy[!grepl(fixed("ALL-WNBA SECOND TEAM"), p_tidy)]
p_tidy <- p_tidy[!grepl(fixed("All-WNBA Second Team"), p_tidy)]
p_tidy <- str_replace_all(p_tidy, fixed("Breanna Stewart, Seattle\nKelsey Plum, Las Vegas\nBreanna Stewart, Seattle"), "Breanna Stewart, Seattle\nKelsey Plum, Las Vegas")
p_tidy <- str_replace_all(p_tidy, fixed("T – Betty Lennox, Minnesota Lynx and Shannon Johnson, Orlando Miracle"), "Betty Lennox, Minnesota Lynx")
p_tidy <- str_replace_all(p_tidy, fixed("T – Angel McCoughtry, Atlanta Dream and Lindsay Whalen, Minnesota Lynx"), "Angel McCoughtry, Atlanta Dream")
p_tidy <- str_replace_all(p_tidy, fixed("Sabrina Ionescu New York"), "Sabrina Ionescu, New York")
p_tidy <- str_replace_all(p_tidy, fixed("Jewell Lloyd"), "Jewell Loyd")
p_tidy <- str_replace_all(p_tidy, fixed("Yolanda Grifith"), "Yolanda Griffith")
p_tidy <- str_replace_all(p_tidy, fixed("Skyler Diggins-Smith"), "Skylar Diggins")
p_tidy <- str_replace_all(p_tidy, fixed("Skylar Diggins-Smith"), "Skylar Diggins")
p_tidy <- str_remove_all(p_tidy, fixed(", Forward-Center"))
p_tidy <- str_remove_all(p_tidy, fixed(", Forward"))
p_tidy <- str_remove_all(p_tidy, fixed(", Guard/Forward"))
p_tidy <- str_remove_all(p_tidy, fixed(", Guard"))
p_tidy <- str_remove_all(p_tidy, fixed(", Center"))

all_wnba <- tibble(name = p_tidy)

all_wnba <- all_wnba %>%
  filter(nchar(name) > 5) %>%
  mutate(index = row_number() + 1,
         year_index = floor(index / 2),
         year = 2024 - year_index,
         team = case_when((index - 1) %% 2 == 0 ~ "Second team",
                          TRUE ~ "First team"),
         .before = name) %>%
  select(-index, -year_index)

all_wnba_clean <- all_wnba %>%
  separate_wider_delim(cols = name, delim = "\n", names = c("player_1", "player_2", "player_3", "player_4","player_5"))

all_wnba_final <- all_wnba_clean %>%
  pivot_longer(cols = c("player_1", "player_2", "player_3", "player_4", "player_5"),
               names_to = "slot",
               values_to = "player") %>%
  select(-slot) %>%
  separate_wider_delim(cols = player, delim = ", ", names = c("player_name", "team_name"))

all_wnba_final$player_name <- str_squish(all_wnba_final$player_name)
all_wnba_final$team_name <- str_squish(all_wnba_final$team_name)

apps_by_player <- all_wnba_final %>%
  group_by(player_name) %>%
  summarise(count = n())

# Data manually checked at Basketball Reference and downloaded via Google Sheets
player_nationalities <- read_sheet(ss = "https://docs.google.com/spreadsheets/d/151DzyDdowUcHv7JwPZdj7JhPzJajdHGFWcozQ17n_TQ/edit?gid=747152664#gid=747152664", sheet = "all_players") %>%
  clean_names()

all_wnba_table <- all_wnba_final %>%
  left_join(y = player_nationalities, by = c("player_name" = "name"))

table_final <- all_wnba_table %>%
  arrange(year, continent) %>%
  mutate(row_no = row_number(),
         season_index = ceiling(row_no / 10),
         team_index = row_no - (season_index * 10) + 10,
         .before = year)

category_palette <- c("#F5D67A", "#c9092e", "#47B37D", "#566A8F", "#C69DE0")

waffle_rough <- ggplot(table_final, aes(x = team_index, y = year, colour = continent)) +
  geom_point(shape = 15, size = 3) +
  scale_colour_manual(values = category_palette, aesthetics = "colour") +
  scale_y_reverse(limits = c(2025, 1995),
                  breaks = seq(2020, 2000, -5),
                  labels = seq(2020, 2000, -5),
                  expand = c(0, 0)) +
  scale_x_continuous(limits = c(0, 16),
                     expand = c(0, 0))
waffle_rough

waffle_themed <- waffle_rough +
  theme_ptb() +
  theme(legend.position = "none",
        axis.title.y = element_blank(),
        axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.line = element_blank(),
        panel.grid.major = element_blank())
waffle_themed

waffle_labelled <- waffle_themed +
  labs(title = "<b style='color:#E6C973'>Americans</b> dominate the WNBA",
       subtitle = "The <b>regions</b> which produced the 10 players picked<br>in each season's <b>All-WNBA</b> teams since 1997",
       caption = "Data: WNBA.com; Basketball Reference | Chart: Plot the Ball")
waffle_labelled

# Results imported via Google Sheets from https://www.usab.com/about/competitive-history-stats#5x5womensteam
usa_results <- read_sheet("https://docs.google.com/spreadsheets/d/1APwblCnCtPCdKGWl99kcb8utRWFtzvXxPzifb8OdglQ/edit?gid=995888537#gid=995888537") %>%
  clean_names() %>%
  select(1, 3, 4, 6, 8, 9)

roll_no <- 30
roll_results <- usa_results
roll_results$rolling_points_for <- roll_sum(roll_results$points_for, n = roll_no, align = "right", fill = NA) / roll_no
roll_results$rolling_points_against <- roll_sum(roll_results$points_against, n = roll_no, align = "right", fill = NA) / roll_no
roll_results$rolling_margin <- roll_results$rolling_points_for - roll_results$rolling_points_against

roll_results_final <- roll_results %>%
  filter(year > 1996)

usa_navy <- "#164080"
usa_red <- "#e41c39"
neutral_white <- "#edeff2"

rough_line <- ggplot(roll_results_final, aes(x = overall_index)) +
  geom_ribbon(aes(ymin = rolling_points_against, ymax = rolling_points_for), fill = neutral_white) +
  geom_path(aes(y = rolling_points_for),
            color = usa_red,
            size = 1.5) +
  geom_path(aes(y = rolling_points_against),
            color = usa_navy,
            size = 1.5) +
  scale_y_continuous(name = NULL,
                     limits = c(45, 105),
                     expand = c(0,0),
                     breaks = seq(50, 100, 10)) +
  scale_x_continuous(limits = c(20, 160)) +
  theme(axis.line.x = element_blank(),
        axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        axis.ticks.x = element_blank(),
        panel.grid.major.x = element_blank())
rough_line

themed_line <- rough_line + theme_ptb()
themed_line

annotated_line <- themed_line +
  geom_vline(xintercept = 59,
             size = 0.7,
             color = neutral_white,
             linetype = "dashed") +
  geom_vline(xintercept = 76,
             size = 0.7,
             color = neutral_white,
             linetype = "dashed") +
  geom_vline(xintercept = 93,
             size = 0.7,
             color = neutral_white,
             linetype = "dashed") +
  geom_vline(xintercept = 110,
             size = 0.7,
             color = neutral_white,
             linetype = "dashed") +
  geom_vline(xintercept = 124,
             size = 0.7,
             color = neutral_white,
             linetype = "dashed") +
  geom_vline(xintercept = 136,
             size = 0.7,
             color = neutral_white,
             linetype = "dashed")
annotated_line

labelled_line <- annotated_line +
  labs(title = "<b style='color:#e41c39'>Team USA</b> are consistently around 30<br>points better than <b style='color:#164080'>their opponents</b>",
       subtitle = "Rolling ave. no. of points <b style='color:#e41c39'>scored</b> and <b style='color:#164080'>conceded</b><br>per game in Olympic and FIBA World Cup<br>competition by <b>Team USA</b> since 1998",
       caption = "<i>Averages calculated on a 30-game rolling basis</i><br><br>Data: USA Basketball | Chart: Plot the Ball")
labelled_line
