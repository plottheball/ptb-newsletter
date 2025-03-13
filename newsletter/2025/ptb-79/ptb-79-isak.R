library(tidyverse)
library(worldfootballR)
library(janitor)
library(ggtext)
library(scales)

shooting_data <- fb_big5_advanced_season_stats(season_end_year = seq(2023, 2025, 1),
                                               stat_type = "shooting",
                                               team_or_player = "player") %>%
  clean_names()

passing_data <- fb_big5_advanced_season_stats(season_end_year = seq(2023, 2025, 1),
                                              stat_type = "passing",
                                              team_or_player = "player") %>%
  clean_names()

possession_data <- fb_big5_advanced_season_stats(season_end_year = seq(2023, 2025, 1),
                                                 stat_type = "possession",
                                                 team_or_player = "player") %>%
  clean_names()

playing_time_data <- fb_big5_advanced_season_stats(season_end_year = seq(2023, 2025, 1),
                                                   stat_type = "playing_time",
                                                   team_or_player = "player") %>%
  clean_names()

playing_time_filtered <- playing_time_data %>%
  group_by(player, url, comp, pos) %>%
  summarise(total_mp = sum(min_playing_time))

pl_fw <- playing_time_filtered %>%
  filter(pos == "FW" | pos == "FW,MF" | pos == "FW,DF") %>%
  filter(comp == "Premier League") %>%
  select(-pos)

pt_fw_final <- playing_time_data %>%
  filter(url %in% pl_fw$url) %>%
  filter(comp == "Premier League")

playing_time_filtered <- pt_fw_final %>%
  group_by(player, url) %>%
  summarise(total_mp = sum(min_playing_time),
            total_matches = sum(mp_playing_time),
            total_starts = sum(starts_starts))

passing_filtered_fw <- passing_data %>%
  select(2, 3, 4, 5, 25, 26, 30, 32, 33) %>%
  filter(url %in% pl_fw_final$url) %>%
  filter(comp == "Premier League")

passing_grouped_fw <- passing_filtered_fw %>%
  group_by(player, url) %>%
  summarise(total_xag = sum(x_ag),
            total_xa = sum(x_a_expected),
            total_prog_passes = sum(prg_p),
            total_ppa = sum(ppa))

shooting_filtered_fw <- shooting_data %>%
  select(2, 3, 4, 5, 10, 20, 23, 27) %>%
  filter(url %in% pl_fw_final$url) %>%
  filter(comp == "Premier League")

shooting_grouped_fw <- shooting_filtered_fw %>%
  group_by(player, url) %>%
  summarise(total_npxg = sum(npx_g_expected),
            total_npg = sum(gls_standard) - sum(pk_standard))

possession_filtered_fw <- possession_data %>%
  select(2, 3, 4, 5, 25, 27, 31, 32) %>%
  filter(url %in% pl_fw_final$url) %>%
  filter(comp == "Premier League")

possession_grouped_fw <- possession_filtered_fw %>%
  group_by(player, url) %>%
  summarise(total_prog_carries = sum(prg_c_carries),
            total_cpa = sum(cpa_carries),
            total_prog_receptions = sum(prg_r_receiving))

pl_fw_for_plot <- left_join(playing_time_filtered, passing_grouped_fw, by = c("player", "url")) %>%
  left_join(., shooting_grouped_fw, by = c("player", "url")) %>%
  left_join(., possession_grouped_fw, by = c("player", "url")) %>%
  filter(total_mp > 2000)

pl_fw_for_plot <- pl_fw_for_plot %>%
  mutate(start_share = total_starts / total_matches,
         prog_p_r_c = total_prog_passes + total_prog_receptions + total_prog_carries,
         npxg_xag = total_npxg + total_xag,
         prog_p_r_c_p90 = prog_p_r_c / total_mp * 90,
         npxg_xag_p90 = npxg_xag / total_mp * 90,
         xag_share = total_xag / npxg_xag * 100) %>%
  filter(xag_share <= (100 / 4) & start_share > 0.6)

striker_progression_analysis <- pl_fw_for_plot %>%
  select(total_mp, total_prog_passes, total_prog_carries, total_prog_receptions, total_ppa, total_cpa) %>%
  mutate(prog_p_p90 = total_prog_passes / total_mp * 90,
         ppa_p90 = total_ppa / total_mp * 90,
         other_prog_p_p90 = (total_prog_passes - total_ppa) / total_mp * 90,
         prog_c_p90 = total_prog_carries / total_mp * 90,
         cpa_p90 = total_cpa / total_mp * 90,
         other_prog_c_p90 = (total_prog_carries - total_cpa) / total_mp * 90,
         prog_r_p90 = total_prog_receptions / total_mp * 90)

striker_shooting_analysis <- pl_fw_for_plot %>%
  select(total_npxg, total_npg) %>%
  mutate(finishing_rate = ((total_npg / total_npxg) - 1) * 100)

focus_colour <- "#991529"
haaland_colour <- "#98C0D9"
jackson_colour <- "#385F8C"
background_data <- "#C9C9C9"

fw_plot_rough <- ggplot(pl_fw_for_plot, aes(x = prog_p_r_c_p90, y = npxg_xag_p90)) +
  geom_point(color = case_when(pl_fw_for_plot$player == "Alexander Isak" ~ focus_colour,
                               pl_fw_for_plot$player == "Erling Haaland" ~ haaland_colour,
                               pl_fw_for_plot$player == "Nicolas Jackson" ~ jackson_colour,
                               TRUE ~ background_data),
             alpha = case_when(pl_fw_for_plot$player == "Alexander Isak" ~ 1,
                               pl_fw_for_plot$player == "Erling Haaland" ~ 0.9,
                               pl_fw_for_plot$player == "Nicolas Jackson" ~ 0.9,
                               TRUE ~ 0.5),
             size = 5.5) +
  scale_x_continuous(limits = c(0, 16),
                     breaks = seq(0, 15, 5),
                     expand = c(0,0)) +
  scale_y_continuous(limits = c(0, 1.19),
                     breaks = seq(0, 1, 0.25),
                     expand = c(0,0))
fw_plot_rough

fw_plot_themed <- fw_plot_rough +
  theme_ptb() +
  theme(axis.title = element_blank(),
        axis.ticks = element_blank()) +
  annotate(geom = "text",
           x = 0.3,
           y = 1.16,
           label = "Non-penalty xG and\n xAG per 90 mins",
           family = ptb_font,
           colour = ptb_dark_grey,
           fontface = "bold",
           hjust = 0,
           vjust = 1) +
  annotate(geom = "text",
           x = 14.5,
           y = 0.195,
           label = "Progressive carries, passes\n and receptions per 90 mins",
           family = ptb_font,
           colour = ptb_dark_grey,
           fontface = "bold",
           hjust = 1,
           vjust = 1)
fw_plot_themed

fw_plot_labelled <- fw_plot_themed +
  labs(title = "<b style='color:#991529'>Isak</b> offers an unusual combination of<br>shot creation and ball progression",
       subtitle = "<b>Strikers</b>' contribution to chance creation and ball<br>progression in the <b>Premier League</b> since 2022-23",
       caption = "<i>Players with <2,000 minutes (and <60% of games started) excluded;<br> 'strikers' are forwards with NPxG at least 3x as high as their xAG</i><br><br>Data: FBref | Chart: Plot the Ball")
fw_plot_labelled
