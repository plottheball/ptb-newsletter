library(tidyverse)
library(readxl)
library(janitor)
library(googlesheets4)
library(scales)
library(ggtext)

tour_seasons <- seq(2023, 2023, 1)

expected_wins_all <- tibble()

# season performance CSVs downloaded from following link:
# https://datagolf.com/performance-table

for (i in tour_seasons) {
  expected_wins_import <- read_csv(paste0("dg_performance_", i, ".csv"), show_col_types = FALSE)
  expected_wins_import <- expected_wins_import %>%
    mutate(season = i,
           .after = player_name)
  expected_wins_all <- rbind(expected_wins_all, expected_wins_import)
  rm(expected_wins_import)
}

sg_splits_tidy <- expected_wins_all %>%
  select(1, 2, 8:14) %>%
  separate(col = player_name,
           into = c("surname", "first_name"),
           sep = ", ") %>%
  mutate(player_name = str_c(first_name, " ", surname),
         .after = first_name) %>%
  select(-surname, -first_name) %>%
  filter(shotlink_played != 0)

round_count <- sg_splits_tidy %>%
  group_by(player_name) %>%
  summarise(total_rounds = sum(shotlink_played))

sg_splits_final <- left_join(sg_splits_tidy, round_count, by = "player_name")
sg_splits_final <- sg_splits_final %>%
  mutate(season_weight = shotlink_played / total_rounds,
         putt_weighted = putt_true * season_weight,
         t2g_weighted = t2g_true * season_weight)

sg_splits_summary <- sg_splits_final %>%
  group_by(player_name) %>%
  summarise(no_seasons = n(),
            no_rounds = sum(shotlink_played),
            putt_final = sum(putt_weighted),
            t2g_final = sum(t2g_weighted)) %>%
  filter(no_rounds >= 40)

### https://www.pgatour.com/players/rookies
rookies_url <- "https://docs.google.com/spreadsheets/d/1BYEsosJJ297P2OxdeRhYNW1zGRbOrCfx7Y65wA5dl9Y/edit?usp=sharing"
rookies_table <- read_sheet(rookies_url) %>%
  clean_names()
rookies_table <- rookies_table %>%
  separate(col = player,
           into = c("surname", "first_name"),
           sep = ", ") %>%
  mutate(player_name = str_c(first_name, " ", surname),
         .after = first_name) %>%
  select(-surname, -first_name)

aberg_focus <- "#E67386"
background_colour <- "#444F5A"
bar_colour <- "#DAE6F2"

sg_splits_rookies <- sg_splits_summary %>%
  filter(sg_splits_summary$player_name %in% rookies_table$player_name)

sg_splits_scatter <- ggplot(sg_splits_rookies, aes(x = t2g_final, y = putt_final)) +
  geom_point(color = case_when(sg_splits_rookies$player_name == "Ludvig Aberg" ~ aberg_focus,
                               TRUE ~ background_colour),
             alpha = case_when(sg_splits_rookies$player_name == "Ludvig Aberg" ~ 1,
                               TRUE ~ 0.2),
             size = 5) +
  scale_y_continuous(name = NULL,
                     limits = c(-2.1, 2.1),
                     breaks = seq(-2, 2, 1),
                     expand = c(0,0),
                     labels = label_number(style_positive = "plus")) +
  scale_x_continuous(name = NULL,
                     limits = c(-2.3, 2.3),
                     breaks = seq(-2, 2, 1),
                     expand = c(0,0),
                     labels = label_number(style_positive = "plus")) +
  theme(legend.position = "none",
        axis.line.x = element_blank(),
        axis.line.y = element_blank()) +
  annotate(geom = "text",
           x = -1.98,
           y = 1.9,
           label = "Strokes-Gained\nPutting",
           family = ptb_font,
           colour = ptb_dark_grey,
           fontface = "bold",
           hjust = 0,
           vjust = 1) +
  annotate(geom = "text",
           x = 1.98,
           y = -1.6,
           label = "Strokes-Gained\nTee to Green",
           family = ptb_font,
           colour = ptb_dark_grey,
           fontface = "bold",
           hjust = 1,
           vjust = 1) +
  geom_hline(yintercept = 0,
             colour = ptb_dark_grey) +
  geom_vline(xintercept = 0,
             colour = ptb_dark_grey)
sg_splits_scatter

sg_splits_themed <- sg_splits_scatter +
  theme_ptb()
sg_splits_themed

sg_splits_labelled <- sg_splits_themed +
  labs(title = "<span style='color:#E67386;'>Ludvig Åberg</span> was an above-average<br>player in his first PGA Tour season",
       subtitle = "True Strokes-Gained <b>Putting</b> and <b>Tee to Green</b><br>per round by PGA Tour rookies during 2023",
       caption = "<i>Players with <40 Shotlink rounds excluded</i><br><br>Data: Data Golf | Chart: Plot the Ball")
sg_splits_labelled

top_drivers <- sg_splits_tidy %>%
  filter(shotlink_played >= 40) %>%
  arrange(-ott_true) %>%
  head(10)

top_drivers$label <- if_else(top_drivers$player_name == "Ludvig Aberg", "Ludvig Åberg", top_drivers$player_name)

rough_bar <- ggplot(top_drivers, aes(x = fct_reorder(label, ott_true), y = ott_true, fill = player_name)) +
  geom_bar(stat = "identity", width = 0.7) +
  scale_fill_manual(values = c("Scottie Scheffler" = bar_colour,
                               "Rory McIlroy" = bar_colour,
                               "Patrick Cantlay" = bar_colour,
                               "Viktor Hovland" = bar_colour,
                               "Ludvig Aberg" = aberg_focus,
                               "Kevin Yu" = bar_colour,
                               "Cameron Young" = bar_colour,
                               "Keith Mitchell" = bar_colour,
                               "Brent Grant" = bar_colour,
                               "Corey Conners" = bar_colour)) +
  scale_y_continuous(limits = c(0, 1.4),
                     breaks = seq(0, 1.25, 0.25),
                     expand = c(0,0),
                     labels = label_number(style_positive = "plus")) +
  geom_hline(yintercept = 0,
             colour = ptb_dark_grey) +
  coord_flip() +
  theme(legend.position = "none")
rough_bar

themed_bar <- rough_bar +
  theme_ptb() +
  theme(axis.line.y = element_blank(),
        axis.ticks.y = element_blank(),
        panel.grid.major.y = element_blank())
themed_bar

labelled_bar <- themed_bar +
  labs(y = "True Strokes-Gained Off the Tee per round", 
       title = "<span style='color:#E67386;'>Åberg</span> is already an elite driver",
       subtitle = "Most True Strokes-Gained <b>Off the Tee</b> per round<br>by men's professional golfers during 2023",
       caption = "<i>Players with <40 Shotlink rounds excluded</i><br><br>Data: Data Golf | Chart: Plot the Ball")
labelled_bar
