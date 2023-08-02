library(tidyverse)
library(worldfootballR)
library(janitor)
library(ggtext)
library(scales)

seasons <- seq(2018, 2023, 1)

shooting_data <- fb_big5_advanced_season_stats(season_end_year = seasons,
                                               stat_type = "shooting",
                                               team_or_player = "player") %>%
  clean_names()

passing_data <- fb_big5_advanced_season_stats(season_end_year = seasons,
                                              stat_type = "passing",
                                              team_or_player = "player") %>%
  clean_names()

possession_data <- fb_big5_advanced_season_stats(season_end_year = seasons,
                                                 stat_type = "possession",
                                                 team_or_player = "player") %>%
  clean_names()

playing_time_data <- fb_big5_advanced_season_stats(season_end_year = seasons,
                                                   stat_type = "playing_time",
                                                   team_or_player = "player") %>%
  clean_names()

playing_time_filtered <- playing_time_data %>%
  filter(pos != "GK" & pos != "DF") %>%
  group_by(player, url) %>%
  summarise(total_mp = sum(min_playing_time)) %>%
  filter(total_mp >= (1200 * 6)) 

passing_filtered <- passing_data %>%
  select(2, 3, 4, 5, 25, 26, 32, 33) %>%
  filter(url %in% playing_time_filtered$url)

passing_grouped <- passing_filtered %>%
  group_by(player, url) %>%
  summarise(total_xag = sum(x_ag),
            total_xa = sum(x_a),
            total_prog_passes = sum(prg_p))

shooting_filtered <- shooting_data %>%
  select(2, 3, 4, 5, 23, 27) %>%
  filter(url %in% playing_time_filtered$url)

shooting_grouped <- shooting_filtered %>%
  group_by(player, url) %>%
  summarise(total_npxg = sum(npx_g_expected))

possession_filtered <- possession_data %>%
  select(2, 3, 4, 5, 25, 31, 32) %>%
  filter(url %in% playing_time_filtered$url)

possession_grouped <- possession_filtered %>%
  group_by(player, url) %>%
  summarise(total_prog_carries = sum(prg_c_carries),
            total_prog_receptions = sum(prg_r_receiving))

big5_final <- left_join(playing_time_filtered, passing_grouped, by = c("player", "url")) %>%
  left_join(., shooting_grouped, by = c("player", "url")) %>%
  left_join(., possession_grouped, by = c("player", "url"))

big5_final <- big5_final %>%
  mutate(prog_p_r_c = total_prog_passes + total_prog_receptions + total_prog_carries,
         npxg_xag = total_npxg + total_xag,
         prog_p_r_c_p90 = prog_p_r_c / total_mp * 90,
         npxg_xag_p90 = npxg_xag / total_mp * 90)

barca_red <- "#cd122d"
background_data <- "#C9C9C9"

plot_rough <- ggplot(big5_final, aes(x = prog_p_r_c_p90, y = npxg_xag_p90)) +
  geom_point(color = if_else(big5_final$player == "Ousmane Dembélé", barca_red, background_data),
             alpha = if_else(big5_final$player == "Ousmane Dembélé", 1, 0.5),
             size = 4) +
  scale_x_continuous(limits = c(0, 31),
                     breaks = seq(0, 30, 10),
                     expand = c(0,0)) +
  scale_y_continuous(limits = c(0, 1.09),
                     breaks = seq(0, 1, 0.25),
                     expand = c(0,0))
plot_rough

plot_themed <- plot_rough +
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
           y = 0.22,
           label = "Progressive carries,\npasses and receptions\nper 90 mins",
           family = ptb_font,
           colour = ptb_dark_grey,
           fontface = "bold",
           hjust = 1,
           vjust = 1)
plot_themed

plot_labelled <- plot_themed +
  labs(title = "<b style='color:#cd122d'>Dembélé</b> is an elite ball progressor",
       subtitle = "Players' contribution to <b>chance creation</b> and <b>ball<br>progression</b> in the 'Big Five' leagues since 2017-18",
       caption = "<i>MF and FW only; players with <7,200 league minutes excluded</i><br><br>Data: FBref | Chart: Plot the Ball")
plot_labelled
