library(tidyverse)
library(worldfootballR)
library(janitor)
library(ggtext)
library(scales)

current_season_end <- 2025

passing_data <- fb_big5_advanced_season_stats(season_end_year = current_season_end,
                                              stat_type = "passing",
                                              team_or_player = "player") %>%
  clean_names()

passing_types_data <- fb_big5_advanced_season_stats(season_end_year = current_season_end,
                                              stat_type = "passing_types",
                                              team_or_player = "player") %>%
  clean_names()

possession_data <- fb_big5_advanced_season_stats(season_end_year = current_season_end,
                                                 stat_type = "possession",
                                                 team_or_player = "player") %>%
  clean_names()

playing_time_data <- fb_big5_advanced_season_stats(season_end_year = current_season_end,
                                                   stat_type = "playing_time",
                                                   team_or_player = "player") %>%
  clean_names()

playing_time_filtered <- playing_time_data %>%
  group_by(player, url, comp, pos) %>%
  summarise(total_mp = sum(min_playing_time)) %>%
  filter(!is.na(total_mp))

pl_df <- playing_time_filtered %>%
  filter(pos == "DF") %>%
  filter(comp == "Premier League") %>%
  select(-3, -4) %>%
  unique()

playing_time_df <- playing_time_data %>%
  filter(url %in% pl_df$url)

pt_grouped_df <- playing_time_df %>%
  group_by(player, url) %>%
  summarise(total_mp = sum(min_playing_time))

passing_filtered_df <- passing_data %>%
  select(2, 3, 4, 5, 25, 26, 32, 33) %>%
  filter(url %in% pl_df$url)

passing_grouped_df <- passing_filtered_df %>%
  group_by(player, url) %>%
  summarise(total_xag = sum(x_ag),
            total_xa = sum(x_a_expected),
            total_prog_passes = sum(prg_p))

passing_types_filtered_df <- passing_types_data %>%
  select(2, 3, 4, 5, 17, 25) %>%
  filter(url %in% pl_df$url)

passing_types_grouped_df <- passing_types_filtered_df %>%
  group_by(player, url) %>%
  summarise(total_throwins = sum(ti_pass))

possession_filtered_df <- possession_data %>%
  select(2, 3, 4, 5, 25, 31, 32) %>%
  filter(url %in% big5_df$url)

possession_grouped_df <- possession_filtered_df %>%
  group_by(player, url) %>%
  summarise(total_prog_carries = sum(prg_c_carries),
            total_prog_receptions = sum(prg_r_receiving))

pl_df_final <- left_join(pt_grouped_df, passing_grouped_df, by = c("player", "url")) %>%
  left_join(., possession_grouped_df, by = c("player", "url")) %>%
  left_join(., passing_types_grouped_df, by = c("player", "url"))

mins_threshold <- 2000

pl_df_filtered <- pl_df_final %>%
  mutate(prog_p_c = total_prog_passes + total_prog_carries,
         prog_p_c_p90 = prog_p_c / total_mp * 90,
         prog_p_p90 = total_prog_passes / total_mp * 90,
         prog_c_p90 = total_prog_carries / total_mp * 90,
         prog_r_p90 = total_prog_receptions / total_mp * 90,
         throwins_p90 = total_throwins / total_mp * 90) %>%
  filter(total_mp >= mins_threshold) %>%
  mutate(grouping = case_when(throwins_p90 > 2 ~ "Wide defenders",
                              throwins_p90 < 2 ~ "Central defenders"))

focus_player_one <- "Dean Huijsen"
focus_colour_one <- "#CCB166"
focus_player_two <- "Trent Alexander-Arnold"
focus_colour_two <- "#DB2C38"
background_data <- "#C9C9C9"

df_plot_rough <- ggplot(pl_df_filtered, aes(y = prog_p_p90, x = prog_c_p90)) +
  geom_point(color = case_when(pl_df_filtered$player == focus_player_one ~  focus_colour_one,
                               pl_df_filtered$player == focus_player_two ~  focus_colour_two,
                               TRUE ~ background_data),
             alpha = case_when(pl_df_filtered$player == focus_player_one ~  1,
                               pl_df_filtered$player == focus_player_two ~  1,
                               TRUE ~ 0.35),
             size = 5) +
  scale_y_continuous(limits = c(0, 9.4),
                     breaks = seq(0, 8, 2),
                     expand = c(0,0)) +
  scale_x_continuous(limits = c(0, 4.9),
                     breaks = seq(2, 4, 2),
                     expand = c(0,0)) +
  facet_wrap(~grouping,
                 nrow = 1,
                 axes = "all",
                 axis.labels = "all_x")
df_plot_rough

df_plot_themed <- df_plot_rough +
  theme_ptb() +
  theme(axis.title = element_blank(),
        axis.ticks = element_blank()) +
  annotate(geom = "text",
           y = 1.9,
           x = 4.7,
           label = "Prog.\ncarries",
           family = ptb_font,
           colour = ptb_dark_grey,
           fontface = "bold",
           hjust = 1,
           vjust = 1) +
  annotate(geom = "text",
           y = 8.8,
           x = 0.3,
           label = "Prog.\npasses",
           family = ptb_font,
           colour = ptb_dark_grey,
           fontface = "bold",
           hjust = 0,
           vjust = 1)
df_plot_themed

df_plot_labelled <- df_plot_themed +
  labs(title = "Two of the best ball-moving defenders<br>in England have signed for Madrid",
       subtitle = "<b>Defenders</b>' contributions (per 90 minutes) to <b>ball<br>progression</b> in the <b>Premier League</b> in 2024-25<br>",
       caption = "<br><i>Players with <2,000 total minutes excluded; 'wide defenders' are<br>those who take more than 2 throw-ins per 90 minutes</i><br><br>Data: FBref | Chart: Plot the Ball")
df_plot_labelled