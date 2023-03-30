library(tidyverse)
library(worldfootballR)
library(janitor)
library(ggtext)
library(scales)

shooting_data <- fb_big5_advanced_season_stats(season_end_year = 2023,
                                               stat_type = "shooting",
                                               team_or_player = "player") %>%
  clean_names()

passing_data <- fb_big5_advanced_season_stats(season_end_year = 2023,
                                              stat_type = "passing",
                                              team_or_player = "player") %>%
  clean_names()

possession_data <- fb_big5_advanced_season_stats(season_end_year = 2023,
                                                 stat_type = "possession",
                                                 team_or_player = "player") %>%
  clean_names()

playing_time_data <- fb_big5_advanced_season_stats(season_end_year = 2023,
                                                   stat_type = "playing_time",
                                                   team_or_player = "player") %>%
  clean_names()

playing_time_filtered <- playing_time_data %>%
  group_by(player, url, pos) %>%
  summarise(total_mp = sum(min_playing_time)) %>%
  filter(total_mp >= (19 * 90))

big5_mf <- playing_time_filtered %>%
  filter(pos == "MF" | pos == "MF,FW" | pos == "MF,DF")

passing_filtered_mf <- passing_data %>%
  select(2, 3, 4, 5, 25, 26, 32, 33) %>%
  filter(url %in% big5_mf$url)

passing_grouped_mf <- passing_filtered_mf %>%
  group_by(player, url) %>%
  summarise(total_xag = sum(x_ag),
            total_xa = sum(x_a),
            total_prog_passes = sum(prg_p))

shooting_filtered_mf <- shooting_data %>%
  select(2, 3, 4, 5, 23, 27) %>%
  filter(url %in% big5_mf$url)

shooting_grouped_mf <- shooting_filtered_mf %>%
  group_by(player, url) %>%
  summarise(total_npxg = sum(npx_g_expected))

possession_filtered_mf <- possession_data %>%
  select(2, 3, 4, 5, 25, 31, 32) %>%
  filter(url %in% big5_mf$url)

possession_grouped_mf <- possession_filtered_mf %>%
  group_by(player, url) %>%
  summarise(total_prog_carries = sum(prg_c_carries),
            total_prog_receptions = sum(prg_r_receiving))

big5_mf_final <- left_join(big5_mf, passing_grouped_mf, by = c("player", "url")) %>%
  left_join(., shooting_grouped_mf, by = c("player", "url")) %>%
  left_join(., possession_grouped_mf, by = c("player", "url"))

big5_mf_final <- big5_mf_final %>%
  mutate(prog_p_r_c = total_prog_passes + total_prog_receptions + total_prog_carries,
         npxg_xag = total_npxg + total_xag,
         prog_p_r_c_p90 = prog_p_r_c / total_mp * 90,
         npxg_xag_p90 = npxg_xag / total_mp * 90)

big5_fw <- playing_time_filtered %>%
  filter(pos == "FW" | pos == "FW,MF" | pos == "FW,DF")

passing_filtered_fw <- passing_data %>%
  select(2, 3, 4, 5, 25, 26, 32, 33) %>%
  filter(url %in% big5_fw$url)

passing_grouped_fw <- passing_filtered_fw %>%
  group_by(player, url) %>%
  summarise(total_xag = sum(x_ag),
            total_xa = sum(x_a),
            total_prog_passes = sum(prg_p))

shooting_filtered_fw <- shooting_data %>%
  select(2, 3, 4, 5, 23, 27) %>%
  filter(url %in% big5_fw$url)

shooting_grouped_fw <- shooting_filtered_fw %>%
  group_by(player, url) %>%
  summarise(total_npxg = sum(npx_g_expected))

possession_filtered_fw <- possession_data %>%
  select(2, 3, 4, 5, 25, 31, 32) %>%
  filter(url %in% big5_fw$url)

possession_grouped_fw <- possession_filtered_fw %>%
  group_by(player, url) %>%
  summarise(total_prog_carries = sum(prg_c_carries),
            total_prog_receptions = sum(prg_r_receiving))

big5_fw_final <- left_join(big5_fw, passing_grouped_fw, by = c("player", "url")) %>%
  left_join(., shooting_grouped_fw, by = c("player", "url")) %>%
  left_join(., possession_grouped_fw, by = c("player", "url"))

big5_fw_final <- big5_fw_final %>%
  mutate(prog_p_r_c = total_prog_passes + total_prog_receptions + total_prog_carries,
         npxg_xag = total_npxg + total_xag,
         prog_p_r_c_p90 = prog_p_r_c / total_mp * 90,
         npxg_xag_p90 = npxg_xag / total_mp * 90)

city_blue <- "#67b0df"
background_data <- "#C9C9C9"

fw_plot_rough <- ggplot(big5_fw_final, aes(x = prog_p_r_c_p90, y = npxg_xag_p90)) +
  geom_point(color = if_else(big5_fw_final$player == "Erling Haaland", city_blue, background_data),
             alpha = if_else(big5_fw_final$player == "Erling Haaland", 1, 0.65),
             size = 3.5) +
  scale_x_continuous(limits = c(0, 31),
                     breaks = seq(0, 30, 10),
                     expand = c(0,0)) +
  scale_y_continuous(limits = c(0, 1.09),
                     breaks = seq(0, 1, 0.25),
                     expand = c(0,0))
fw_plot_rough

fw_plot_themed <- fw_plot_rough +
  theme_ptb() +
  theme(axis.title = element_blank()) +
  annotate(geom = "text",
           x = 1,
           y = 1.08,
           label = "Non-penalty\nxG and xAG\nper 90 mins",
           family = ptb_font,
           colour = ptb_dark_grey,
           fontface = "bold",
           hjust = 0,
           vjust = 1) +
  annotate(geom = "text",
           x = 29.5,
           y = 0.175,
           label = "Progressive carries,\npasses and receptions\nper 90 mins",
           family = ptb_font,
           colour = ptb_dark_grey,
           fontface = "bold",
           hjust = 1,
           vjust = 1)
fw_plot_themed

fw_plot_labelled <- fw_plot_themed +
  labs(title = "<b style='color:#67b0df'>Erling Haaland</b> is a threat in the box",
       subtitle = "<b>Forwards</b>' contribution to chance creation and<br>ball progression in the 'Big Five' leagues in 2022-23",
       caption = "<i>Players with <1,800 league minutes excluded</i><br><br>Data: FBref | Chart: Plot the Ball")
fw_plot_labelled

mf_plot_rough <- ggplot(big5_mf_final, aes(x = prog_p_r_c_p90, y = npxg_xag_p90)) +
  geom_point(color = if_else(big5_mf_final$player == "Kevin De Bruyne", city_blue, background_data),
             alpha = if_else(big5_mf_final$player == "Kevin De Bruyne", 1, 0.65),
             size = 3) +
  scale_x_continuous(limits = c(0, 31),
                     breaks = seq(0, 30, 10),
                     expand = c(0,0)) +
  scale_y_continuous(limits = c(0, 1.09),
                     breaks = seq(0, 1, 0.25),
                     expand = c(0,0))
mf_plot_rough

mf_plot_themed <- mf_plot_rough +
  theme_ptb() +
  theme(axis.title = element_blank()) +
  annotate(geom = "text",
           x = 1,
           y = 1.08,
           label = "Non-penalty\nxG and xAG\nper 90 mins",
           family = ptb_font,
           colour = ptb_dark_grey,
           fontface = "bold",
           hjust = 0,
           vjust = 1) +
  annotate(geom = "text",
           x = 29.5,
           y = 0.175,
           label = "Progressive carries,\npasses and receptions\nper 90 mins",
           family = ptb_font,
           colour = ptb_dark_grey,
           fontface = "bold",
           hjust = 1,
           vjust = 1)
mf_plot_themed

mf_plot_labelled <- mf_plot_themed +
  labs(title = "<b style='color:#67b0df'>Kevin De Bruyne</b> does everything",
       subtitle = "<b>Midfielders</b>' contribution to chance creation and<br>ball progression in the 'Big Five' leagues in 2022-23",
       caption = "<i>Players with <1,800 league minutes excluded</i><br><br>Data: FBref | Chart: Plot the Ball")
mf_plot_labelled
