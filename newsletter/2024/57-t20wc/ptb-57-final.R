library(tidyverse)
library(rvest)
library(janitor)
library(ggtext)
library(scales)

link_t20i_h2h_home <- "https://stats.espncricinfo.com/ci/engine/stats/index.html?class=3;filter=advanced;home_or_away=1;opposition=1;opposition=2;opposition=25;opposition=3;opposition=4;opposition=5;opposition=6;opposition=7;opposition=8;opposition=9;orderby=matches;result=1;result=2;result=3;size=200;spanmax2=1+Jun+2024;spanmin1=1+Jan+2005;spanval1=span;team=1;team=2;team=25;team=3;team=4;team=5;team=6;team=7;team=8;team=9;template=results;type=team;view=opposition;wrappertype=print"
link_t20i_h2h_away <- "https://stats.espncricinfo.com/ci/engine/stats/index.html?class=3;filter=advanced;home_or_away=2;home_or_away=3;opposition=1;opposition=2;opposition=25;opposition=3;opposition=4;opposition=5;opposition=6;opposition=7;opposition=8;opposition=9;orderby=matches;result=1;result=2;result=3;size=200;spanmax2=1+Jun+2024;spanmin1=1+Jan+2005;spanval1=span;team=1;team=2;team=25;team=3;team=4;team=5;team=6;team=7;team=8;team=9;template=results;type=team;view=opposition;wrappertype=print"
link_odi_h2h_home <- "https://stats.espncricinfo.com/ci/engine/stats/index.html?class=2;filter=advanced;home_or_away=1;opposition=1;opposition=2;opposition=25;opposition=3;opposition=4;opposition=5;opposition=6;opposition=7;opposition=8;opposition=9;orderby=matches;result=1;result=2;result=3;size=200;spanmax2=1+Jun+2024;spanmin1=1+Jan+2005;spanval1=span;team=1;team=2;team=25;team=3;team=4;team=5;team=6;team=7;team=8;team=9;template=results;type=team;view=opposition;wrappertype=print"
link_odi_h2h_away <- "https://stats.espncricinfo.com/ci/engine/stats/index.html?class=2;filter=advanced;home_or_away=2;home_or_away=3;opposition=1;opposition=2;opposition=25;opposition=3;opposition=4;opposition=5;opposition=6;opposition=7;opposition=8;opposition=9;orderby=matches;result=1;result=2;result=3;size=200;spanmax2=1+Jun+2024;spanmin1=1+Jan+2005;spanval1=span;team=1;team=2;team=25;team=3;team=4;team=5;team=6;team=7;team=8;team=9;template=results;type=team;view=opposition;wrappertype=print"

t20i_h2h_home <- read_html(link_t20i_h2h_home) %>%
  html_nodes("table") %>%
  html_table()
t20i_h2h_home <- t20i_h2h_home[[3]] %>%
  clean_names() %>%
  select(1:6) %>%
  mutate(format = "T20I",
         location = "Home",
         index = row_number(),
         pair_index = case_when(index %% 2 == 0 ~ index - 1,
                                TRUE ~ index + 1))
t20i_h2h_home_index <- t20i_h2h_home %>%
  select(team, index) %>%
  rename("opponent" = "team")
t20i_h2h_home_final <- left_join(x = t20i_h2h_home, y = t20i_h2h_home_index, by = c("pair_index" = "index")) %>%
  filter(index %% 2 != 0) %>%
  select(-span, -index, -pair_index) %>%
  relocate(opponent,
           .after = team)

t20i_h2h_away <- read_html(link_t20i_h2h_away) %>%
  html_nodes("table") %>%
  html_table()
t20i_h2h_away <- t20i_h2h_away[[3]] %>%
  clean_names() %>%
  select(1:6) %>%
  mutate(format = "T20I",
         location = "Away/Neutral",
         index = row_number(),
         pair_index = case_when(index %% 2 == 0 ~ index - 1,
                                TRUE ~ index + 1))
t20i_h2h_away_index <- t20i_h2h_away %>%
  select(team, index) %>%
  rename("opponent" = "team")
t20i_h2h_away_final <- left_join(x = t20i_h2h_away, y = t20i_h2h_away_index, by = c("pair_index" = "index")) %>%
  filter(index %% 2 != 0) %>%
  select(-span, -index, -pair_index) %>%
  relocate(opponent,
           .after = team)

odi_h2h_home <- read_html(link_odi_h2h_home) %>%
  html_nodes("table") %>%
  html_table()
odi_h2h_home <- odi_h2h_home[[3]] %>%
  clean_names() %>%
  select(1:6) %>%
  mutate(format = "ODI",
         location = "Home",
         index = row_number(),
         pair_index = case_when(index %% 2 == 0 ~ index - 1,
                                TRUE ~ index + 1))
odi_h2h_home_index <- odi_h2h_home %>%
  select(team, index) %>%
  rename("opponent" = "team")
odi_h2h_home_final <- left_join(x = odi_h2h_home, y = odi_h2h_home_index, by = c("pair_index" = "index")) %>%
  filter(index %% 2 != 0) %>%
  select(-span, -index, -pair_index) %>%
  relocate(opponent,
           .after = team)

odi_h2h_away <- read_html(link_odi_h2h_away) %>%
  html_nodes("table") %>%
  html_table()
odi_h2h_away <- odi_h2h_away[[3]] %>%
  clean_names() %>%
  select(1:6) %>%
  mutate(format = "ODI",
         location = "Away/Neutral",
         index = row_number(),
         pair_index = case_when(index %% 2 == 0 ~ index - 1,
                                TRUE ~ index + 1))
odi_h2h_away_index <- odi_h2h_away %>%
  select(team, index) %>%
  rename("opponent" = "team")
odi_h2h_away_final <- left_join(x = odi_h2h_away, y = odi_h2h_away_index, by = c("pair_index" = "index")) %>%
  filter(index %% 2 != 0) %>%
  select(-span, -index, -pair_index) %>%
  relocate(opponent,
           .after = team)

all_h2h <- bind_rows(t20i_h2h_home_final, t20i_h2h_away_final, odi_h2h_home_final, odi_h2h_away_final) %>%
  mutate(id = str_c(team, " ", opponent),
         .before = team)

all_h2h$mat <- as.integer(all_h2h$mat)
all_h2h$won <- as.integer(all_h2h$won)
all_h2h$lost <- as.integer(all_h2h$lost)
all_h2h$tied <- as.integer(all_h2h$tied)

h2h_summary <- all_h2h %>%
  group_by(id, format) %>%
  summarise(tot_matches = sum(mat),
            tot_won = sum(won),
            win_percent = tot_won / tot_matches * 100)

t20i_summary <- h2h_summary %>%
  filter(format == "T20I" & tot_matches >= 20) %>%
  select(id, format, win_percent)

odi_summary <- h2h_summary %>%
  filter(format == "ODI" & tot_matches >= 20) %>%
  select(id, format, win_percent)

record_comp <- full_join(x = t20i_summary, y = odi_summary, by = c("id" = "id"), suffix = c("_t20i", "_odi")) %>%
  select(-format_t20i, -format_odi) %>%
  filter(!is.na(win_percent_t20i) & !is.na(win_percent_odi)) %>%
  mutate(difference = win_percent_odi - win_percent_t20i,
         category = case_when(win_percent_odi >= (2 / 3 * 100) & win_percent_t20i >= (2 / 3 * 100) ~ "Elite both",
                              win_percent_odi >= (2 / 3 * 100) ~ "Elite ODI",
                              win_percent_t20i >= (2 / 3 * 100) ~ "Elite T20I",
                              TRUE ~ "Other"))

palette_category <- c("#3d5c7d", "#3d997d", "#cc5c85", "#CCCCCC")

rough_scatter <- ggplot(record_comp, aes(y = win_percent_t20i, x = win_percent_odi, colour = category)) +
  geom_point(size = 4, alpha = 0.65) +
  scale_x_continuous(limits = c(0, 95),
                     breaks = seq(0, 75, 25),
                     expand = c(0, 0),
                     label = percent_format(scale = 1)) +
  scale_y_continuous(limits = c(0, 85),
                     breaks = seq(0, 75, 25),
                     expand = c(0, 0),
                     label = percent_format(scale = 1)) +
  scale_colour_manual(values = palette_category, aesthetics = "colour") +
  theme(legend.position = "none")
rough_scatter

themed_scatter <- rough_scatter +
  theme_ptb() +
  theme(axis.title = element_blank(),
        axis.ticks = element_blank()) +
  annotate(geom = "text",
           x = 92,
           y = 3,
           label = "ODI\nWin %",
           family = ptb_font,
           colour = ptb_dark_grey,
           fontface = "bold",
           hjust = 1,
           vjust = 0) +
  annotate(geom = "text",
           x = 3,
           y = 80,
           label = "T20I\nWin %",
           family = ptb_font,
           colour = ptb_dark_grey,
           fontface = "bold",
           hjust = 0,
           vjust = 1)
themed_scatter

labelled_scatter <- themed_scatter +
  labs(title = "The best teams post higher win rates<br>in men's ODIs than in T20Is",
       subtitle = "Comparison of <b>win %</b> in men's <b>T20Is</b> and <b>ODIs</b><br>in match-ups between pairs of ICC Full Members",
       caption = "<i>Match-ups with <20 T20Is or ODIs played since 2005 excluded</i><br><br>Data: ESPNCricinfo | Chart: Plot the Ball")
labelled_scatter

rough_scatter_key <- ggplot(record_comp, aes(y = win_percent_t20i, x = win_percent_odi)) +
  geom_point(size = 4.5,
             alpha = case_when(record_comp$id == "South Africa v Australia" | record_comp$id == "West Indies v England" ~ 1,
                               TRUE ~ 0.4),
             color = case_when(record_comp$id == "South Africa v Australia" ~ "#507368",
                               record_comp$id == "West Indies v England" ~ "#73505D",
                               TRUE ~ "grey")) +
  scale_x_continuous(limits = c(0, 95),
                     breaks = seq(0, 75, 25),
                     expand = c(0, 0),
                     label = percent_format(scale = 1)) +
  scale_y_continuous(limits = c(0, 85),
                     breaks = seq(0, 75, 25),
                     expand = c(0, 0),
                     label = percent_format(scale = 1)) +
  scale_colour_manual(values = key_category, aesthetics = "colour") +
  theme(legend.position = "none")
rough_scatter_key

themed_scatter_key <- rough_scatter_key +
  theme_ptb() +
  theme(axis.title = element_blank(),
        axis.ticks = element_blank()) +
  annotate(geom = "text",
           x = 92,
           y = 3,
           label = "ODI\nWin %",
           family = ptb_font,
           colour = ptb_dark_grey,
           fontface = "bold",
           hjust = 1,
           vjust = 0) +
  annotate(geom = "text",
           x = 3,
           y = 80,
           label = "T20I\nWin %",
           family = ptb_font,
           colour = ptb_dark_grey,
           fontface = "bold",
           hjust = 0,
           vjust = 1)
themed_scatter_key

labelled_scatter_key <- themed_scatter_key +
  labs(title = "The best teams post higher win rates<br>in men's ODIs than in T20Is",
       subtitle = "Comparison of <b>win %</b> in men's <b>T20Is</b> and <b>ODIs</b><br>in match-ups between pairs of ICC Full Members",
       caption = "<i>Match-ups with <20 T20Is or ODIs played since 2005 excluded</i><br><br>Data: ESPNCricinfo | Chart: Plot the Ball")
labelled_scatter_key
