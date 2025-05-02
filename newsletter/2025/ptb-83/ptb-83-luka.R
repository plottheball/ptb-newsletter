library(tidyverse)
library(janitor)
library(readxl)
library(scales)
library(ggtext)

### data downloaded from: https://www.basketball-reference.com/players/d/doncilu01.html
luka_shooting_regular <- read_xlsx("luka_shooting_regular.xlsx") %>%
  clean_names() %>%
  select(season, age, team, lg, mp, x0_3_12) %>%
  rename("fg_within_3ft" = "x0_3_12") %>%
  filter(!(season == "2024-25" & (team == "DAL" | team == "LAL"))) %>%
  select(-team)
luka_shooting_post <- read_xlsx("luka_shooting_post.xlsx") %>%
  clean_names() %>%
  select(season, age, team, lg, mp, x0_3_12) %>%
  rename("fg_within_3ft" = "x0_3_12") %>%
  filter(!is.na(age)) %>%
  select(-team)

luka_totals_regular <- read_xlsx("luka_totals_regular.xlsx") %>%
  clean_names() %>%
  select(season, age, team, lg, mp, fga) %>%
  filter(!(season == "2024-25" & (team == "DAL" | team == "LAL"))) %>%
  select(-team)
luka_totals_post <- read_xlsx("luka_totals_post.xlsx") %>%
  clean_names() %>%
  select(season, age, team, lg, mp, fga) %>%
  filter(!is.na(age)) %>%
  select(-team)

luka_regular_final <- full_join(luka_totals_regular, luka_shooting_regular) %>%
  mutate(layups = round(fga * fg_within_3ft),
         category = "Regular season") %>%
  select(season, age, category, fga, layups)

luka_post_final <- full_join(luka_totals_post, luka_shooting_post) %>%
  mutate(layups = round(fga * fg_within_3ft),
         category = "Postseason") %>%
  select(season, age, category, fga, layups)

luka_final <- bind_rows(luka_regular_final, luka_post_final) %>%
  arrange(season)

luka_summary <- luka_final %>%
  group_by(season, age) %>%
  summarise(tot_fga = sum(fga),
            tot_layups = sum(layups)) %>%
  mutate(layup_rate = tot_layups / tot_fga * 100,
         category = case_when(season == "2024-25" ~"Lakers/Mavericks", 
                              season == "2019-20" ~ "Mavericks (peak)",
                              TRUE ~ "Mavericks"))

bar_colour <- "#E6A417"
other_focus <- "#0E5A8C"
background_colour <- "#8FB4CC"

rough_bars <- ggplot(luka_summary, aes(x = fct_reorder(season, -age), y = layup_rate, fill = category)) +
  geom_bar(stat = "identity", width = 0.6) +
  scale_fill_manual(values = c("Lakers/Mavericks" = bar_colour,
                               "Mavericks (peak)" = other_focus,
                               "Mavericks" = background_colour)) +
  scale_y_continuous(limits = c(0, 39),
                     breaks = seq(0, 30, 10),
                     expand = c(0,0),
                     labels = percent_format(scale = 1)) +
  coord_flip() +
  theme(legend.position = "none",
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.ticks.y = element_blank(),
        panel.grid.major.y = element_blank(),
        axis.line.x = element_blank(),
        axis.ticks = element_blank())
rough_bars

themed_bars <- rough_bars +
  theme_ptb()
themed_bars

labelled_bars <- themed_bars +
  labs(title = "Dončić is taking fewer shots from near<br>the basket than earlier in his career",
       subtitle = "Share of <b>Luka Dončić</b>'s field-goal attempts in each<br>season which were taken <b>within 3ft of the basket</b>",
       caption = "<i>Data includes shots taken in both the regular season and the playoffs</i><br><br>Data: Basketball Reference | Chart: Plot the Ball")
labelled_bars
