# load packages...
library(tidyverse)
# ...for analysis...
library(readxl)
# ...and for visualisation
library(ggtext)
library(scales)

### OBTAIN, CLEAN & ANALYSE DATA

## GRAB SEASON BY SEASON DATA FROM DATAGOLF.COM
# season CSVs downloaded from following link:
# https://datagolf.com/performance-table

# set up sequence for relevant years
tour_seasons <- seq(1996, 2022, 1)

# create blank tibble to house data
expected_wins_all <- tibble()

# read in season CSVs downloaded from datagolf.com and bind to tibble
for (i in tour_seasons) {
  expected_wins_import <- read_csv(paste0("dg_performance_", i, ".csv"), show_col_types = FALSE)
  expected_wins_import <- expected_wins_import %>%
    mutate(season = i,
           .after = player_name)
  expected_wins_all <- rbind(expected_wins_all, expected_wins_import)
  rm(expected_wins_import)
}

## TIDY AND SUMMARISE

# remove irrelevant columns...
expected_wins_final <- expected_wins_all %>%
  select(-seq(8,13,1)) %>%
  # ...and tidy player names
  separate(col = player_name,
           into = c("surname", "first_name"),
           sep = ", ") %>%
  mutate(player_name = str_c(first_name, " ", surname),
         .after = first_name) %>%
  select(-surname, -first_name)

# tally statistics by player, using weighted average function to calculated WA Shots-Gained
summary_player <- expected_wins_final %>%
  group_by(player_name) %>%
  summarise(no_seasons = n(),
            no_events = sum(events_played),
            no_rounds = sum(rounds_played),
            no_wins = sum(wins),
            x_wins_nonmajors = sum(x_wins),
            x_wins_majors = sum(x_wins_majors),
            x_win_rate =sum(x_wins_nonmajors, x_wins_majors)/no_events,
            wa_true_sg = weighted.mean(x = total_true, w = rounds_played))

## FILTER TO RELEVANT DATA FOR GRAPHIC 1

# remove players in database with fewer than 200 rounds played between 1996 and 2022
players_filtered <- summary_player %>%
  filter(no_rounds >= 200)

# calculate distance between Woods and McIlroy for grouping
woods_wa_true_sg <- players_filtered$wa_true_sg[players_filtered$player_name == "Tiger Woods"]
mcilroy_wa_true_sg <- players_filtered$wa_true_sg[players_filtered$player_name == "Rory McIlroy"]
woods_mcilroy_diff <- woods_wa_true_sg - mcilroy_wa_true_sg
mcilroy_cutoff <- mcilroy_wa_true_sg - woods_mcilroy_diff

# add groupings based on distance from McIlroy etc.
players_filtered <- players_filtered %>%
  mutate(category = case_when(player_name == "Tiger Woods" ~ "Tiger",
                              player_name == "Rory McIlroy" ~ "Rory",
                              wa_true_sg >= mcilroy_cutoff ~ "Next tier",
                              wa_true_sg >= 0 ~ "Above average",
                              TRUE ~ "Below average"))

## FILTER TO RELEVANT DATA FOR GRAPHIC 2

# remove players in database with fewer than 5 career wins or 0.5 Major xWins between 1996 and 2022
players_filtered_v2 <- players_filtered %>%
  filter(no_wins >= 5 & x_wins_majors > 0.5)

### VISUALISE

## GRAPHIC 1: SHOTS-GAINED SCATTER

# set up custom palette of colours (NB categories in alphabetical order)
category_palette <- c("#043971", "#444F5A", "#043971", "#85b4b4", "#ed2b34")

# create basic scatter plot
true_sg_scatter <- ggplot(players_filtered, aes(x = no_rounds, y = wa_true_sg, colour = category)) +
  geom_point(size = 3, alpha = case_when(players_filtered$category == "Tiger" | players_filtered$category == "Rory" ~ 1,
                                         players_filtered$category == "Next tier" ~ 0.75,
                                         TRUE ~ 0.2)) +
  scale_x_continuous(name = NULL,
                     limits = c(0, 2600),
                     breaks = seq(0, 2500, 500),
                     expand = c(0,0),
                     labels = comma) +
  scale_y_continuous(name = NULL,
                     limits = c(-2.4, 3.1),
                     breaks = seq(-2, 3, 1),
                     expand = c(0,0)) +
  scale_colour_manual(values = category_palette, aesthetics = "colour") +
  # ...with axis labels inside the plot
  annotate(geom = "text",
           x = 25,
           y = 2.95,
           label = "True Strokes-Gained\nper round",
           family = ptb_font,
           colour = ptb_dark_grey,
           fontface = "bold",
           hjust = 0,
           vjust = 1) +
  annotate(geom = "text",
           x = 2475,
           y = -1.775,
           label = "No. of\nrounds",
           family = ptb_font,
           colour = ptb_dark_grey,
           fontface = "bold",
           hjust = 1,
           vjust = 1) +
  theme(legend.position = "none")

# add custom theme to plot...
true_sg_themed <- true_sg_scatter +
  theme_ptb() +
  theme(axis.line.x = element_blank()) +
  # ...with intercept line at y = 0
  geom_hline(yintercept = 0,
             colour = ptb_dark_grey)

# add labelling to plot
true_sg_labelled <- true_sg_themed +
  labs(title = "<span style='color:#ed2b34;'>Tiger</span>, <span style='color:#85b4b4;'>Rory</span> and <span style='color:#043971;'>the next tier</span>",
       subtitle = "True Strokes-Gained per round since 1996 for all men's<br>golfers with more than 200 total rounds played",
       caption = "'True' SG is calculated relative to PGA Tour average | Data as at 12 Sept 2022<br><br>Data: Data Golf | Chart: Plot the Ball")

## GRAPHIC 2: EXPECTED WINS SCATTER

# set up custom palette of colours (NB categories in alphabetical order)
category_palette_v2 <- c("#043971", "#043971", "#85b4b4", "#ed2b34")

# create basic scatter plot
xwins_scatter <- ggplot(players_filtered_v2, aes(x = x_wins_nonmajors, y = x_wins_majors, colour = category)) +
  geom_point(size = 3, alpha = case_when(players_filtered_v2$category == "Tiger" | players_filtered_v2$category == "Rory" ~ 1,
                                         players_filtered_v2$category == "Next tier" ~ 0.75,
                                         TRUE ~ 0.2)) +
  scale_x_continuous(name = NULL,
                     limits = c(-1, 39),
                     breaks = seq(0, 39, 10),
                     expand = c(0,0),
                     labels = comma) +
  scale_y_continuous(name = NULL,
                     limits = c(-0.25, 9.9),
                     breaks = seq(0, 8, 2),
                     expand = c(0,0)) +
  scale_colour_manual(values = category_palette_v2, aesthetics = "colour") +
  # ...with axis labels inside the plot
  annotate(geom = "text",
           x = 0.5,
           y = 9.75,
           label = "Career xWins\n(Majors)",
           family = ptb_font,
           colour = ptb_dark_grey,
           fontface = "bold",
           hjust = 0,
           vjust = 1) +
  annotate(geom = "text",
           x = 38.5,
           y = 1.25,
           label = "Career xWins\n(non-Majors)",
           family = ptb_font,
           colour = ptb_dark_grey,
           fontface = "bold",
           hjust = 1,
           vjust = 1) +
  theme(legend.position = "none")

# add custom theme to plot...
xwins_themed <- xwins_scatter +
  theme_ptb() +
  theme(axis.line.x = element_blank(),
        axis.line.y = element_blank()) +
  # ...with intercept line at y = 0
  geom_hline(yintercept = 0,
             colour = ptb_dark_grey) +
  # ...and intercept line at x = 0
  geom_vline(xintercept = 0,
             colour = ptb_dark_grey)

# add labelling to plot
xwins_labelled <- xwins_themed +
  labs(title = "No player younger than <span style='color:#85b4b4;'>McIlroy</span> has<br>more expected Major wins than he does",
       subtitle = "Expected wins in non-Majors and in Majors since 1996",
       caption = "Non-Major xWins calculated for an average PGA Tour event | Players with fewer<br>than 5 career wins or 0.5 Major xWins excluded | Data as at 12 Sept 2022<br><br>Data: Data Golf | Chart: Plot the Ball")
xwins_labelled

#### NOTES
#### code for custom theme elements not included above
