library(tidyverse)
library(janitor)
library(worldfootballR)
library(scales)
library(ggtext)

mens_euros <- "https://fbref.com/en/comps/676/history/European-Championship-Seasons"
mens_worlds <- "https://fbref.com/en/comps/1/history/World-Cup-Seasons"
womens_euros <- "https://fbref.com/en/comps/162/history/UEFA-Womens-Euro-Seasons"
womens_worlds <- "https://fbref.com/en/comps/106/history/Womens-World-Cup-Seasons"

mens_euros_matches <- fb_match_urls(country = "", gender = "M", season_end_year = 2021, non_dom_league_url = mens_euros)
mens_worlds_matches <- fb_match_urls(country = "", gender = "M", season_end_year = 2022, non_dom_league_url = mens_worlds)
womens_euros_matches <- fb_match_urls(country = "", gender = "F", season_end_year = 2022, non_dom_league_url = womens_euros)
womens_worlds_matches <- fb_match_urls(country = "", gender = "F", season_end_year = 2023, non_dom_league_url = womens_worlds)

mens_all_matches <- c(mens_euros_matches, mens_worlds_matches)
womens_all_matches <- c(womens_euros_matches, womens_worlds_matches)

mens_euros_summary <- fb_advanced_match_stats(match_url = mens_euros_matches, stat_type = "summary", team_or_player = "team")
mens_worlds_summary <- fb_advanced_match_stats(match_url = mens_worlds_matches, stat_type = "summary", team_or_player = "team")

womens_euros_summary <- fb_advanced_match_stats(match_url = womens_euros_matches, stat_type = "summary", team_or_player = "team")
womens_worlds_summary <- fb_advanced_match_stats(match_url = womens_worlds_matches, stat_type = "summary", team_or_player = "team")

mens_euros_final <- mens_euros_summary %>%
  clean_names() %>%
  select(1, 2, 4, 11, 18, 19, 20, 35, 40) %>%
  unique()

mens_worlds_final <- mens_worlds_summary %>%
  clean_names() %>%
  select(1, 2, 4, 11, 18, 19, 20, 36, 41) %>%
  unique()

mens_all_final <- bind_rows(mens_euros_final, mens_worlds_final)

womens_euros_final <- womens_euros_summary %>%
  clean_names() %>%
  select(1, 2, 4, 11, 18, 19, 20, 35, 40) %>%
  unique()

womens_worlds_final <- womens_worlds_summary %>%
  clean_names() %>%
  select(1, 2, 4, 11, 18, 19, 20, 35, 40) %>%
  unique()

womens_all_final <- bind_rows(womens_euros_final, womens_worlds_final)

mens_by_game <- mens_all_final %>%
  group_by(game_url) %>%
  summarise(total_npxg = sum(npx_g_expected),
            total_att_passes = sum(att_passes))

mens_all_viz <- left_join(mens_all_final, mens_by_game, by = c("game_url" = "game_url"))
mens_all_viz <- mens_all_viz %>%
  mutate(npxg_share = npx_g_expected / total_npxg * 100,
         poss_share = att_passes / total_att_passes * 100)

womens_by_game <- womens_all_final %>%
  group_by(game_url) %>%
  summarise(total_npxg = sum(npx_g_expected),
            total_att_passes = sum(att_passes))

womens_all_viz <- left_join(womens_all_final, womens_by_game, by = c("game_url" = "game_url"))
womens_all_viz <- womens_all_viz %>%
  mutate(npxg_share = npx_g_expected / total_npxg * 100,
         poss_share = att_passes / total_att_passes * 100)

spain_red <- "#E6173D"
spain_gold <- "#FFB619"
background_grey <- "#C9C9C9"

scatter_mens_rough <- ggplot(mens_all_viz, aes(x = poss_share, y = npxg_share)) +
  geom_point(size = 4,
              color = case_when(mens_all_viz$team == "Spain" ~ spain_gold,
                               TRUE ~ background_grey),
              alpha = case_when(mens_all_viz$team == "Spain" ~ 0.95,
                               TRUE ~ 0.5)) +
  scale_x_continuous(limits = c(-4, 104),
                     breaks = seq(0, 100, 50),
                     expand = c(0,0),
                     labels = percent_format(accuracy = 1, scale = 1)) +
  scale_y_continuous(limits = c(-4, 104),
                     breaks = seq(50, 100, 50),
                     expand = c(0,0),
                     labels = percent_format(accuracy = 1, scale = 1)) +
  geom_hline(yintercept = 100,
             colour = ptb_mid_grey,
             linetype = "dashed") +
  geom_vline(xintercept = 100,
             colour = ptb_mid_grey,
             linetype = "dashed") +
  geom_hline(yintercept = 0,
             colour = ptb_dark_grey) +
  geom_vline(xintercept = 0,
             colour = ptb_dark_grey) +
  annotate(geom = "text",
           x = 2,
           y = 98,
           label = "Share of\nnon-penalty\nxG",
           family = ptb_font,
           colour = ptb_dark_grey,
           fontface = "bold",
           hjust = 0,
           vjust = 1) +
  annotate(geom = "text",
           x = 98,
           y = 15,
           label = "Share of\npossession",
           family = ptb_font,
           colour = ptb_dark_grey,
           fontface = "bold",
           hjust = 1,
           vjust = 1) +
  theme(axis.title.x = element_blank(),
        axis.line.x = element_blank(),
        axis.line.y = element_blank())
scatter_mens_rough

scatter_mens_themed <- scatter_mens_rough +
  theme_ptb()
scatter_mens_themed

scatter_mens_labelled <- scatter_mens_themed +
  labs(title = "<span style='color:#FFB619'>Spain's men</span> dominate possession",
       subtitle = "Share of <b>non-penalty xG</b> and <b>possession</b> recorded<br>by teams in matches at the 2021 Men's Euros<br>and 2022 Men's World Cup</i>",
       caption = "Data: FBref | Chart: Plot the Ball")
scatter_mens_labelled

scatter_womens_rough <- ggplot(womens_all_viz, aes(x = poss_share, y = npxg_share)) +
  geom_point(size = 4,
             color = case_when(womens_all_viz$team == "Spain" ~ spain_red,
                               TRUE ~ background_grey),
             alpha = case_when(womens_all_viz$team == "Spain" ~ 0.95,
                               TRUE ~ 0.5)) +
  scale_x_continuous(limits = c(-4, 104),
                     breaks = seq(0, 100, 50),
                     expand = c(0,0),
                     labels = percent_format(accuracy = 1, scale = 1)) +
  scale_y_continuous(limits = c(-4, 104),
                     breaks = seq(50, 100, 50),
                     expand = c(0,0),
                     labels = percent_format(accuracy = 1, scale = 1)) +
  geom_hline(yintercept = 100,
             colour = ptb_mid_grey,
             linetype = "dashed") +
  geom_vline(xintercept = 100,
             colour = ptb_mid_grey,
             linetype = "dashed") +
  geom_hline(yintercept = 0,
             colour = ptb_dark_grey) +
  geom_vline(xintercept = 0,
             colour = ptb_dark_grey) +
  annotate(geom = "text",
           x = 2,
           y = 98,
           label = "Share of\nnon-penalty\nxG",
           family = ptb_font,
           colour = ptb_dark_grey,
           fontface = "bold",
           hjust = 0,
           vjust = 1) +
  annotate(geom = "text",
           x = 98,
           y = 15,
           label = "Share of\npossession",
           family = ptb_font,
           colour = ptb_dark_grey,
           fontface = "bold",
           hjust = 1,
           vjust = 1) +
  theme(axis.title.x = element_blank(),
        axis.line.x = element_blank(),
        axis.line.y = element_blank())
scatter_womens_rough

scatter_womens_themed <- scatter_womens_rough +
  theme_ptb()
scatter_womens_themed

scatter_womens_labelled <- scatter_womens_themed +
  labs(title = "<span style='color:#E6173D'>Spain's women</span> dominate possession",
       subtitle = "Share of <b>non-penalty xG</b> and <b>possession</b> recorded<br>by teams in matches at the 2022 Women's Euros<br>and 2023 Women's World Cup</i>",
       caption = "Data: FBref | Chart: Plot the Ball")
scatter_womens_labelled
