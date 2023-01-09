# load packages...
library(tidyverse)
# ...for obtaining data...
library(baseballr)
# ...and for visualisation
library(extrafont)
library(ggtext)
library(lemon)
library(scales)

### OBTAIN, CLEAN & ANALYSE DATA

## PLOT 1: HITTING

# grab Statcast hitting data for last season using 'baseballr' package
batting_data_statcast <- statcast_leaderboards(leaderboard = "exit_velocity_barrels",
                                               year = 2021,
                                               abs = 100,
                                               player_type = "batter")

## PLOT 2: PITCHING

# grab data from Fangraphs for relevant seasons using 'baseballr' package
pitching_data_2021 <- fg_pitcher_leaders(2021, 2021, league = "all", qual = 50, pitcher_type = "sta", ind = 1)
pitching_data_2018 <- fg_pitcher_leaders(2018, 2018, league = "all", qual = 50, pitcher_type = "sta", ind = 1)

# filter data to relevant columns...
pitching_2018_filtered <- pitching_data_2018 %>%
  select(playerid, Season, Name, Team, IP, K_pct, FBv)
pitching_2021_filtered <- pitching_data_2021 %>%
  select(playerid, Season, Name, Team, IP, K_pct, FBv)
# and combine into one dataframe
pitching_final <- rbind(pitching_2018_filtered, pitching_2021_filtered)

### VISUALISE

## PLOT 1: HITTING

# set up custom colours
angels_red <- "#BA0021"
angels_silver <- "#C4CED4"

# create custom plot...
rough_scatter_bat <- ggplot(batting_data_statcast, aes(x = avg_distance, y = brl_percent)) +
  geom_point(color = if_else(batting_data_statcast$player_id==660271, angels_red, angels_silver),
             alpha = if_else(batting_data_statcast$player_id==660271, 1, 0.75),
             size = 3) +
  theme(legend.position = "none") + 
  scale_x_continuous(name = NULL,
                     limits = c(115, 210),
                     expand = c(0,0),
                     breaks = seq(125, 200, 25)) +
  scale_y_continuous(name = NULL,
                     labels = percent_format(accuracy = 1, scale = 1),
                     limits = c(0, 23),
                     expand = c(0,0),
                     breaks = seq(0, 20, 5)) +
  # ...with axis labels inside the plot
  annotate(geom = "text",
           x = 116,
           y = 22.5,
           label = "Barrel %",
           family = ptb_font,
           colour = ptb_dark_grey,
           fontface = "bold",
           hjust = 0,
           vjust = 1) +
  annotate(geom = "text",
           x = 208.5,
           y = 2.5,
           label = "Average\ndistance (ft)",
           family = ptb_font,
           colour = ptb_dark_grey,
           fontface = "bold",
           hjust = 1,
           vjust = 1)
rough_scatter_bat

# add custom theme to plot
themed_scatter_bat <- rough_scatter_bat + theme_ptb()
themed_scatter_bat

# add labelling to plot
final_scatter_bat <- themed_scatter_bat + labs(title = "<span style='color:#BA0021;'>Shohei Ohtani</span> hits a clean ball",
                                               subtitle = "Average batted ball distance (ft) and barrel % of MLB<br>hitters with at least 100 at-bats in 2021",
                                               caption = "Data: MLB Statcast | Chart: Plot the Ball")
final_scatter_bat

## PLOT 2: PITCHING

# create custom plot...
rough_scatter_pitch <- ggplot(pitching_final, aes(x = FBv, y = K_pct)) +
  geom_point(color = if_else(pitching_final$playerid==19755, angels_red, angels_silver),
             alpha = if_else(pitching_final$playerid==19755, 1, 0.75),
             size = 2) +
  theme(legend.position = "none") + 
  scale_x_continuous(name = NULL,
                     limits = c(85, 100),
                     expand = c(0,0),
                     breaks = seq(90, 95, 5)) +
  scale_y_continuous(name = NULL,
                     labels = percent_format(accuracy = 1, scale = 1),
                     limits = c(10, 47),
                     expand = c(0,0),
                     breaks = seq(20, 40, 10)) +
  # ...with axis labels inside the plot
  annotate(geom = "text",
           x = 85.85,
           y = 45.5,
           label = "K %",
           family = ptb_font,
           colour = ptb_dark_grey,
           size = rel(3),
           fontface = "bold",
           hjust = 1,
           vjust = 1) +
  annotate(geom = "text",
           x = 99.75,
           y = 18,
           label = "Average FB\nvelocity (mph)",
           family = ptb_font,
           colour = ptb_dark_grey,
           size = rel(3),
           fontface = "bold",
           hjust = 1,
           vjust = 1) +
  facet_rep_wrap(Season ~ ., nrow = 2, ncol = 1, repeat.tick.labels = TRUE) +
  theme(legend.position = "none",
        strip.background = element_blank(),
        strip.text = element_text(size = rel(1.1),
                                  face = "bold"))
rough_scatter_pitch

# add custom theme to plot
themed_scatter_pitch <- rough_scatter_pitch + theme_ptb()
themed_scatter_pitch

# add labelling to plot
final_scatter_pitch <- themed_scatter_pitch + labs(title = "<span style='color:#BA0021;'>Ohtani</span>'s velocity declined after injury",
                                                   subtitle = "Average fastball velocity (mph) and strikeout % of MLB<br>pitchers with at least 50 innings pitched in 2018 and 2021",
                                                   caption = "Data: Fangraphs | Chart: Plot the Ball")
final_scatter_pitch

#### NOTES
#### code for custom theme elements not included above
