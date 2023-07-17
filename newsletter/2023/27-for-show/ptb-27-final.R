library(tidyverse)
library(readxl)
library(scales)
library(ggtext)

tour_seasons <- seq(2004, 2023, 1)

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
  filter(no_rounds >= 200)

rory_focus <- "#85b4b4"
scottie_focus <- "#c15d26"
background_colour <- "#444F5A"

sg_splits_scatter <- ggplot(sg_splits_summary, aes(x = t2g_final, y = putt_final)) +
  geom_point(color = case_when(sg_splits_summary$player_name == "Rory McIlroy" ~ rory_focus,
                               sg_splits_summary$player_name == "Scottie Scheffler" ~ scottie_focus,
                               TRUE ~ background_colour),
             alpha = case_when(sg_splits_summary$player_name == "Rory McIlroy" ~ 1,
                               sg_splits_summary$player_name == "Scottie Scheffler" ~ 1,
                               TRUE ~ 0.1),
             size = 3.5) +
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
  labs(title = "<span style='color:#85b4b4;'>Rory</span> and <span style='color:#c15d26;'>Scottie</span> are elite T2G players",
       subtitle = "True Strokes-Gained <b>Putting</b> and <b>Tee to Green</b><br>per round by men's golfers since 2004",
       caption = "<i>Players with <200 Shotlink rounds excluded</i><br><br>Data: Data Golf | Chart: Plot the Ball")
sg_splits_labelled
