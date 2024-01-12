library(tidyverse)
library(janitor)
library(worldfootballR)
library(ggtext)
library(scales)

seasons <- seq(2018, 2023, 1)

gk_data <- fb_big5_advanced_season_stats(season_end_year = seasons, stat_type = "keepers", team_or_player = "player")
adv_gk_data <- fb_big5_advanced_season_stats(season_end_year = seasons, stat_type = "keepers_adv", team_or_player = "player")

gk_tidy <- gk_data %>%
  clean_names()

gk_filtered <- gk_tidy %>%
  filter(so_ta >= 50) %>%
  arrange(comp, squad, player) %>%
  select(1, 2, 3, 4, 13, 15)

adv_gk_tidy <- adv_gk_data %>%
  clean_names() %>%
  select(1, 2, 3, 4, 10, 14, 15)

gk_final <- left_join(x = gk_filtered, y = adv_gk_tidy)

gk_final <- gk_final %>%
  mutate(psxg_vs_g = p_sx_g_expected - ga_goals + og_goals,
         per_100_sota = psxg_vs_g / so_ta * 100,
         unique_id = str_remove_all(str_c(squad, comp, player), fixed(" ")))

gk_summary <- gk_final %>%
  group_by(unique_id) %>%
  summarise(count = n(),
            first_season = min(season_end_year),
            last_season = max(season_end_year),
            span = last_season - first_season + 1,
            sum_of_other_seasons = sum(season_end_year) - first_season - last_season,
            missing_seasons_y_n = case_when(span == count ~ 0,
                                            TRUE ~ 1)) %>%
  filter(count > 1)

multi_seasons <- gk_final %>%
  filter(unique_id %in% gk_summary$unique_id) %>%
  select(1, 11, 12) %>%
  arrange(season_end_year)

multi_seasons_final <- multi_seasons %>%
  pivot_wider(names_from = "season_end_year",
              names_prefix = "season_end_",
              values_from = "per_100_sota") %>%
  mutate(y_n_18_19 = str_c(is.na(season_end_2018), is.na(season_end_2019)),
         y_n_19_20 = str_c(is.na(season_end_2019), is.na(season_end_2020)),
         y_n_20_21 = str_c(is.na(season_end_2020), is.na(season_end_2021)),
         y_n_21_22 = str_c(is.na(season_end_2021), is.na(season_end_2022)),
         y_n_22_23 = str_c(is.na(season_end_2022), is.na(season_end_2023)))

all_repeat_seasons <- tibble()

seasons_18_19 <- multi_seasons_final %>%
  filter(y_n_18_19 == "FALSEFALSE") %>%
  select(1, 2, 3) %>%
  rename("season_one" = "season_end_2018",
         "season_two" = "season_end_2019") %>%
  mutate(first = 2018,
         second = 2019)

all_repeat_seasons <- bind_rows(all_repeat_seasons, seasons_18_19)

seasons_19_20 <- multi_seasons_final %>%
  filter(y_n_19_20 == "FALSEFALSE") %>%
  select(1, 3, 4) %>%
  rename("season_one" = "season_end_2019",
         "season_two" = "season_end_2020") %>%
  mutate(first = 2019,
         second = 2020)

all_repeat_seasons <- bind_rows(all_repeat_seasons, seasons_19_20)

seasons_20_21 <- multi_seasons_final %>%
  filter(y_n_20_21 == "FALSEFALSE") %>%
  select(1, 4, 5) %>%
  rename("season_one" = "season_end_2020",
         "season_two" = "season_end_2021") %>%
  mutate(first = 2020,
         second = 2021)

all_repeat_seasons <- bind_rows(all_repeat_seasons, seasons_20_21)

seasons_21_22 <- multi_seasons_final %>%
  filter(y_n_21_22 == "FALSEFALSE") %>%
  select(1, 5, 6) %>%
  rename("season_one" = "season_end_2021",
         "season_two" = "season_end_2022") %>%
  mutate(first = 2021,
         second = 2022)

all_repeat_seasons <- bind_rows(all_repeat_seasons, seasons_21_22)

seasons_22_23 <- multi_seasons_final %>%
  filter(y_n_22_23 == "FALSEFALSE") %>%
  select(1, 6, 7) %>%
  rename("season_one" = "season_end_2022",
         "season_two" = "season_end_2023") %>%
  mutate(first = 2022,
         second = 2023)

all_repeat_seasons <- bind_rows(all_repeat_seasons, seasons_22_23)

all_repeat_seasons <- all_repeat_seasons %>%
  unique()

all_repeat_seasons <- all_repeat_seasons %>%
  mutate(s2_less_s1 = season_two - season_one,
         category = case_when(s2_less_s1 > -2.5 & s2_less_s1 < 2.5 ~ "Roughly equal",
                              s2_less_s1 < -2.5 ~ "Much worse",
                              s2_less_s1 > 2.5 ~ "Much better",
                              TRUE ~ "Different"))

all_repeat_summary <- all_repeat_seasons %>%
  group_by(category) %>%
  summarise(count = n())

better_colour <- "#40806D"
equal_colour <- "#CCCCCC"
worse_colour <- "#CC6685"

rough_scatter <- ggplot(data = all_repeat_seasons, aes(x = season_one, y = season_two)) +
  geom_point(size = 3,
             alpha = 0.5,
             color = case_when(all_repeat_seasons$category == "Roughly equal" ~ equal_colour,
                               all_repeat_seasons$category == "Much worse" ~ worse_colour,
                               all_repeat_seasons$category == "Much better" ~ better_colour,
                               TRUE ~ "yellow")) +
  scale_x_continuous(limits = c(-19, 19),
                     breaks = seq(-15, 15, 5), 
                     expand = c(0, 0),
                     labels = label_number(style_positive = "plus")) +
  scale_y_continuous(limits = c(-16, 16),
                     breaks = seq(-15, 15, 5), 
                     expand = c(0, 0),
                     labels = label_number(style_positive = "plus")) +
  geom_vline(xintercept = 0,
             color = ptb_dark_grey) +
  geom_hline(yintercept = 0,
             color = ptb_dark_grey) +
  annotate(geom = "text",
           x = -17.5,
           y = 14.5,
           label = "Season 2\nperformance",
           family = ptb_font,
           colour = ptb_dark_grey,
           fontface = "bold",
           hjust = 0,
           vjust = 1) +
  annotate(geom = "text",
           x = 17.5,
           y = -11,
           label = "Season 1\nperformance",
           family = ptb_font,
           colour = ptb_dark_grey,
           fontface = "bold",
           hjust = 1,
           vjust = 1) +
  theme(axis.line.x = element_blank(),
        axis.line.y = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        legend.position = "none")
rough_scatter

themed_scatter <- rough_scatter + 
  theme_ptb()
themed_scatter

titled_scatter <- themed_scatter +
  labs(title = "Goalkeepers' performances seem to<br>vary significantly between seasons",
       subtitle = "<b>Goals saved above expected</b> by goalkeepers in<br>the 'Big Five' men's leagues in pairs of consecutive<br>league seasons since 2017-18",
       caption = "<i>Per 100 shots on target faced; seasons with < 50 SOT faced excluded</i><br><br>Data: FBref | Chart: Plot the Ball")
titled_scatter

atleti_blue <- "#465199"

rough_scatter_guide <- ggplot(data = all_repeat_seasons, aes(x = season_one, y = season_two)) +
  geom_point(size = 3,
             alpha = case_when(all_repeat_seasons$unique_id == "AtléticoMadridLaLigaJanOblak" & all_repeat_seasons$second == 2019 ~ 1,
                               TRUE ~ 0.2),
             color = case_when(all_repeat_seasons$unique_id == "AtléticoMadridLaLigaJanOblak" & all_repeat_seasons$second == 2019 ~ atleti_blue,
                               TRUE ~ "grey")) +
  scale_x_continuous(limits = c(-19, 19),
                     breaks = seq(-15, 15, 5), 
                     expand = c(0, 0),
                     labels = label_number(style_positive = "plus")) +
  scale_y_continuous(limits = c(-16, 16),
                     breaks = seq(-15, 15, 5), 
                     expand = c(0, 0),
                     labels = label_number(style_positive = "plus")) +
  geom_vline(xintercept = 0,
             color = ptb_dark_grey) +
  geom_hline(yintercept = 0,
             color = ptb_dark_grey) +
  annotate(geom = "text",
           x = -17.5,
           y = 14.5,
           label = "Season 2\nperformance",
           family = ptb_font,
           colour = ptb_dark_grey,
           fontface = "bold",
           hjust = 0,
           vjust = 1) +
  annotate(geom = "text",
           x = 17.5,
           y = -11,
           label = "Season 1\nperformance",
           family = ptb_font,
           colour = ptb_dark_grey,
           fontface = "bold",
           hjust = 1,
           vjust = 1) +
  theme(axis.line.x = element_blank(),
        axis.line.y = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        legend.position = "none")
rough_scatter

themed_scatter_guide <- rough_scatter_guide + 
  theme_ptb()
themed_scatter_guide

titled_scatter_guide <- themed_scatter_guide +
  labs(title = "How consistent are goalkeepers'<br>performances from season to season?",
       subtitle = "<b>Goals saved above expected</b> by goalkeepers in<br>the 'Big Five' men's leagues in pairs of consecutive<br>league seasons since 2017-18",
       caption = "<i>Per 100 shots on target faced; seasons with < 50 SOT faced excluded</i><br><br>Data: FBref | Chart: Plot the Ball")
titled_scatter_guide
