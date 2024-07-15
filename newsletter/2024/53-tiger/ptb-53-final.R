library(tidyverse)
library(readxl)
library(scales)
library(ggtext)
library(janitor)
library(googlesheets4)

# season CSVs downloaded from following link: https://datagolf.com/performance-table
tour_seasons <- seq(1996, 2024, 1)

expected_wins_all <- tibble()

for (i in tour_seasons) {
  expected_wins_import <- read_csv(paste0("dg_performance_", i, ".csv"), show_col_types = FALSE)
  expected_wins_import <- expected_wins_import %>%
    mutate(season = i,
           .after = player_name)
  expected_wins_all <- rbind(expected_wins_all, expected_wins_import)
  rm(expected_wins_import)
}

sg_splits_tidy <- expected_wins_all %>%
  select(1, 2, 7:14) %>%
  separate(col = player_name,
           into = c("surname", "first_name"),
           sep = ", ") %>%
  mutate(player_name = str_c(first_name, " ", surname),
         .after = first_name) %>%
  select(-surname, -first_name)

tiger_by_year <- sg_splits_tidy %>%
  filter(player_name == "Tiger Woods")

# missing seasons manually imported from: https://datagolf.com/player-profiles?dg_id=5321
tiger_missing <- read_sheet("https://docs.google.com/spreadsheets/d/16hjXuKTgJjEBngCVv1jcxqDtoZTxik0gBq3tSt27zPE/edit#gid=52811703") %>%
  clean_names()

tiger_all <- bind_rows(tiger_by_year, tiger_missing) %>%
  arrange(season)

tiger_final <- tiger_all %>%
  select(1, 2, 3, 10) %>%
  mutate(sg_filtered = case_when(rounds_played >= 20 ~ total_true,
                                 TRUE ~ NA),
         category = case_when(is.na(sg_filtered) ~ NA,
                              sg_filtered > 3.45 ~ "Better",
                              sg_filtered < 3.45 ~ "Worse",
                              TRUE ~ "Check")) %>%
  filter(season < 2021)


tiger_final$index <- row_number(tiger_final$season)
tiger_final$season <- as.character(tiger_final$season)

labels <- c("2020", "", "2018", "", "",
            "", "2014", "", "2012", "", 
            "2010", "", "2008", "", "2006",
            "", "2004", "", "2002", "",
            "2000", "", "1998", "", "1996")

tiger_focus <- "#ed2b34"
tiger_other <- "#EDBEC0"

rough_bar <- ggplot(tiger_final, aes(x = fct_reorder(season, -index), y = sg_filtered, fill = category, color = category)) +
  geom_bar(stat = "identity", width = 0.25) +
  geom_point(size = 2.5) +
  scale_fill_manual(values = c("Better" = tiger_focus,
                               "Worse" = tiger_other)) +
  scale_color_manual(values = c("Better" = tiger_focus,
                               "Worse" = tiger_other)) +
  scale_y_continuous(limits = c(-1.9, 5.9),
                     breaks = c(-1, 0, 1, 2, 3, 4, 5),
                     expand = c(0,0),
                     labels = label_number(style_positive = "plus")) +
  scale_x_discrete(labels = labels) +
  coord_flip() +
  theme(legend.position = "none")
rough_bar

themed_bar <- rough_bar +
  theme_ptb() +
  theme(axis.line = element_blank(),
        axis.ticks = element_blank(),
        panel.grid.major.y = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank()) +
  geom_hline(yintercept = 0,
             colour = ptb_dark_grey) +
  geom_rect(xmin = 3.6, xmax = 5.4, ymin = -1.9, ymax = 5.9,
            fill = "#d5d5d505",
            color = "#d5d5d505")
themed_bar

labelled_bar <- themed_bar +
  labs(title = "Tiger's <b style='color:#ed2b34'>peak years</b> are untouchable",
       subtitle = "<b>True Strokes-Gained</b> per round by <b>Tiger Woods</b><br>in each PGA Tour season between 1996 and 2020",
       caption = "<i>Seasons with <20 rounds played excluded</i><br><br>Data: Data Golf | Chart: Plot the Ball")
labelled_bar

tiger_eras <- tiger_all %>%
  filter(season > 2003) %>%
  select(1, 2, 4, 5:8) %>%
  mutate(era = case_when(season < 2009 ~ "2004-08: Before ACL",
                         season < 2014 ~ "2009-13: After ACL",
                         season < 2018 ~ "2014-17: Back issues",
                         season < 2022 ~ "2018-21: Comeback",
                         TRUE ~ "2022-24: Post-crash"))

era_rounds <- tiger_eras %>%
  group_by(era) %>%
  summarise(era_rounds = sum(shotlink_played))

eras_final <- left_join(tiger_eras, era_rounds)

eras_final <- eras_final %>%
  mutate(share_of_era = shotlink_played / era_rounds,
         ott_weighting = share_of_era * ott_true,
         app_weighting = share_of_era * app_true,
         arg_weighting = share_of_era * arg_true,
         putt_weighting = share_of_era * putt_true)

eras_summary <- eras_final %>%
  group_by(era) %>%
  summarise(era_rounds = sum(shotlink_played),
            `Off tee` = sum(ott_weighting),
            `Approach` = sum(app_weighting),
            `Ar. green` = sum(arg_weighting),
            `Putting` = sum(putt_weighting))

eras_plot <- eras_summary %>%
  select(1, 3, 4, 5, 6) %>%
  pivot_longer(cols = 2:5,
               names_to = "category",
               values_to = "true_sg") %>%
  mutate(label_final = case_when(round(true_sg, 1) > 0 ~ str_c("+", as.character(round(true_sg, 1))),
                                 TRUE ~ as.character(round(true_sg, 1))))

low_colour <- "#7A8BCC"
mid_colour <- "#E6E6E6"
high_colour <- "#993D42"

rough_plot <- ggplot(eras_plot, aes(x = factor(category, level = c("Off tee", "Approach", "Ar. green", "Putting")), y = factor(era, level = c("2022-24: Post-crash", "2018-21: Comeback", "2014-17: Back issues", "2009-13: After ACL", "2004-08: Before ACL")), fill = true_sg)) +
  geom_tile(color = "white",
            lwd = 8,
            linetype = 1) +
  geom_text(aes(label = label_final), color = "#ffffff", size = 3.5, fontface = "bold", family = ptb_font) +
  scale_x_discrete(position = "top") +
  scale_fill_gradient2(low = low_colour, mid = mid_colour, high = high_colour, limits = c(-2, 2))
rough_plot

themed_plot <- rough_plot +
  theme_ptb() +
  theme(legend.position = "none",
        axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        axis.line.x =  element_blank(),
        axis.line.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.y = element_text(hjust = 0),
        axis.text.x = element_text(face = "bold"))
themed_plot

labelled_plot <- themed_plot +
  labs(title = "How have Tiger's skills changed?",
       subtitle = "<b>True Strokes-Gained</b> per round by <b>Tiger Woods</b><br>in each phase of his career, by category",
       caption = "<span style='color:#444F5A90'>Scaled from <b style='color:#7A8BCC'>-2 True SG</b> to <b style='color:#993D42'>+2 True SG</b> per round</span><br><br>Data: Data Golf | Chart: Plot the Ball")
labelled_plot
