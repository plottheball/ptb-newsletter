library(tidyverse)
library(janitor)
library(worldfootballR)
library(ggtext)

seasons <- seq(2018, 2024, 1)

leagues <- c("ENG", "ESP", "FRA", "GER", "ITA")

all_tables <- tibble()

for (i in seasons) {
  
  season_tables <- tibble()
  
  for (j in leagues) {
    
    table <- fb_season_team_stats(country = j, season_end_year = i, gender = "M", tier = "1st", stat_type = "league_table")
    
    season_tables <- bind_rows(season_tables, table)
    
  }
  
  all_tables <- bind_rows(all_tables, season_tables)
  
}

clean_tables <- all_tables %>%
  clean_names() %>%
  select(1:8, 12, 13, 17, 18) %>%
  filter(rk == 1)

data_final <- clean_tables %>%
  select(-2, -3, -6) %>%
  mutate(xgf_per_game = x_g / mp,
         xga_per_game = x_ga / mp,
         g_less_xg_per_game = (gf - x_g) / mp,
         ga_less_xga_per_game = (ga - x_ga) / mp,
         xgd_per_game = xgf_per_game - xga_per_game,
         luck_per_game = g_less_xg_per_game - ga_less_xga_per_game)

rankings <- data_final %>%
  select(2, 3, 10, 11, 12, 13) %>%
  mutate(xgf_rank = rank(xgf_per_game, ties.method = "last"),
         xga_rank = rank(-xga_per_game, ties.method = "last"),
         attack_over_rank = rank(g_less_xg_per_game, ties.method = "last"),
         defence_over_rank = rank(-ga_less_xga_per_game, ties.method = "last"),
         category = (case_when(squad == "Leverkusen" ~ "Leverkusen",
                               squad == "Bayern Munich" ~ "Bayern Munich",
                               TRUE ~ "Other")))

category_palette <- c("#5C9BCC", "#e32728", "#E6E5CF")

rough_scatter <- ggplot(rankings, aes(x = xgf_rank, y = xga_rank, fill = category)) + 
  geom_vline(xintercept = 18,
             color = ptb_dark_grey,
             alpha = 0.5) +
  geom_hline(yintercept = 18,
             color = ptb_dark_grey,
             alpha = 0.5) +
  geom_point(shape = 22, size = 3.5) +
  scale_colour_manual(values = category_palette, aesthetics = "fill") +
  scale_x_continuous(limits = c(0, 36),
                     breaks = seq(1, 35, 1),
                     expand = c(0, 0)) +
  scale_y_continuous(limits = c(0, 36),
                     breaks = seq(1, 35, 1),
                     expand = c(0, 0))
rough_scatter

themed_scatter <- rough_scatter +
  theme_ptb() +
  theme(axis.line = element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_blank(),
        axis.title = element_blank(),
        legend.position = "none")
themed_scatter

labelled_scatter <- themed_scatter +
  labs(title = "How do <b style='color:#e32728'>Leverkusen</b> compare to other<br>recent European title winners?",
       subtitle = "<b>Champions</b> of the 'Big Five' European leagues<br>since 2017-18, ranked by their underlying<br>performance in <b>attack →</b> and <b>defence ↑</b>",
       caption = "<i>'Underlying performance' = xG and xGA per game</i><br><br>Data: FBref | Chart: Plot the Ball")
labelled_scatter

rough_scatter_2 <- ggplot(rankings, aes(x = attack_over_rank, y = defence_over_rank, fill = category)) + 
  geom_vline(xintercept = 18,
             color = ptb_dark_grey,
             alpha = 0.5) +
  geom_hline(yintercept = 18,
             color = ptb_dark_grey,
             alpha = 0.5) +
  geom_point(shape = 22, size = 3.5) +
  scale_colour_manual(values = category_palette, aesthetics = "fill") +
  scale_x_continuous(limits = c(0, 36),
                     breaks = seq(1, 35, 1),
                     expand = c(0, 0)) +
  scale_y_continuous(limits = c(0, 36),
                     breaks = seq(1, 35, 1),
                     expand = c(0, 0))
rough_scatter_2

themed_scatter_2 <- rough_scatter_2 +
  theme_ptb() +
  theme(axis.line = element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_blank(),
        axis.title = element_blank(),
        legend.position = "none")
themed_scatter_2

labelled_scatter_2 <- themed_scatter_2 +
  labs(title = "How did <b style='color:#e32728'>Leverkusen</b> perform relative<br>to their underlying numbers?",
       subtitle = "<b>Champions</b> of the 'Big Five' European leagues<br>since 2017-18, ranked by their overperformance<br>of xG in <b>attack →</b> and <b>defence ↑</b>",
       caption = "<i>'Overperformance of xG' = G less xG and xGA less GA per game</i><br><br>Data: FBref | Chart: Plot the Ball")
labelled_scatter_2
