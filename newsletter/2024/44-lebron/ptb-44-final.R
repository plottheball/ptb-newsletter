library(tidyverse)
library(rvest)
library(janitor)
library(ggtext)
library(scales)

# Players drafted in 2003 with more than 20,000 career regular-season minutes per: https://www.basketball-reference.com/draft/NBA_2003.html
lebron_draft_most_mins <- c("https://www.basketball-reference.com/players/a/anthoca01.html",
                            "https://www.basketball-reference.com/players/w/wadedw01.html",
                            "https://www.basketball-reference.com/players/b/boshch01.html",
                            "https://www.basketball-reference.com/players/k/korveky01.html",
                            "https://www.basketball-reference.com/players/w/westda01.html",
                            "https://www.basketball-reference.com/players/d/diawbo01.html",
                            "https://www.basketball-reference.com/players/h/hinriki01.html",
                            "https://www.basketball-reference.com/players/w/willima01.html",
                            "https://www.basketball-reference.com/players/p/pachuza01.html",
                            "https://www.basketball-reference.com/players/r/ridnolu01.html",
                            "https://www.basketball-reference.com/players/b/blakest01.html")

regular_season_all <- tibble()

for (i in lebron_draft_most_mins) {
  
  Sys.sleep(1)
  
  html <- read_html(i)
  
  all_tables <- html %>%
    html_nodes("table") %>%
    html_table()
  
  regular_season <- all_tables[[4]] %>%
    clean_names() %>%
    select(1, 2, 3, 4, 5, 6, 7, 8) %>%
    filter(!is.na(age) & lg == "NBA") %>%
    mutate(link = i)
  
  regular_season$age <- as.integer(regular_season$age)
  regular_season$g <- as.integer(regular_season$g)
  regular_season$gs <- as.integer(regular_season$gs)
  regular_season$mp <- as.numeric(regular_season$mp)
  
  regular_season_all <- bind_rows(regular_season_all, regular_season)
  
}

regular_season_final <-regular_season_all %>%
  filter(tm != "TOT")

lebron_url <- "https://www.basketball-reference.com/players/j/jamesle01.html"

lebron_html <- read_html(lebron_url)

lebron_tables <- lebron_html %>%
  html_nodes("table") %>%
  html_table()

lebron_minutes <- lebron_tables[[5]] %>%
  clean_names() %>%
  filter(!is.na(age) & lg == "NBA") %>%
  select(1, 2, 3, 4, 5, 6, 7, 8) %>%
  mutate(link = lebron_url)
  
all_draft_class_mins <- bind_rows(lebron_minutes, regular_season_final) %>%
  select(1, 8, 9) %>%
  separate(season, into = c("season_start"), sep = "-", remove = TRUE) %>%
  mutate(season_end = as.integer(season_start) + 1,
         link = str_remove(link, "https://www.basketball-reference.com/players")) %>%
  select(-season_start)

draft_class_mins_final <- all_draft_class_mins %>%
  group_by(season_end, link) %>%
  summarise(tot_mp = sum(mp))

running_total <- pivot_wider(draft_class_mins_final, names_from = link, values_from = tot_mp) %>%
  clean_names()

manual_pre_draft <- tibble("season_end" = 2003,
                           "a_anthoca01_html" = 0,
                           "b_blakest01_html" = 0,
                           "b_boshch01_html" = 0,
                           "d_diawbo01_html" = 0,
                           "h_hinriki01_html" = 0,
                           "j_jamesle01_html" = 0,
                           "k_korveky01_html" = 0,
                           "p_pachuza01_html" = 0,
                           "r_ridnolu01_html" = 0,
                           "w_wadedw01_html" = 0,
                           "w_westda01_html" = 0,
                           "w_willima01_html" = 0)

running_total <- bind_rows(running_total, manual_pre_draft) %>%
  arrange(season_end)

running_total$anthony_sum <- cumsum(running_total$a_anthoca01_html)
running_total$blake_sum <- cumsum(running_total$b_blakest01_html)
running_total$bosh_sum <- cumsum(running_total$b_boshch01_html)
running_total$diaw_sum <- cumsum(running_total$d_diawbo01_html)
running_total$hinrich_sum <- cumsum(running_total$h_hinriki01_html)
running_total$james_sum <- cumsum(running_total$j_jamesle01_html)
running_total$korver_sum <- cumsum(running_total$k_korveky01_html)
running_total$pachulia_sum <- cumsum(running_total$p_pachuza01_html)
running_total$ridnour_sum <- cumsum(running_total$r_ridnolu01_html)
running_total$wade_sum <- cumsum(running_total$w_wadedw01_html)
running_total$west_sum <- cumsum(running_total$w_westda01_html)
running_total$williams_sum <- cumsum(running_total$w_willima01_html)

cavaliers_red <- "#860038"
lakers_yellow <- "#FDB927"
other_focus <- "#99805C"
bar_colour <- "#CCBBA3"

rough_line <- ggplot(running_total, aes(x = season_end)) +
  geom_line(aes(y = running_total$anthony_sum), colour = bar_colour, linewidth = 1, alpha = 0.75) +
  geom_line(aes(y = running_total$blake_sum), colour = bar_colour, linewidth = 1, alpha = 0.75) +
  geom_line(aes(y = running_total$bosh_sum), colour = bar_colour, linewidth = 1, alpha = 0.75) +
  geom_line(aes(y = running_total$diaw_sum), colour = bar_colour, linewidth = 1, alpha = 0.75) +
  geom_line(aes(y = running_total$hinrich_sum), colour = bar_colour, linewidth = 1, alpha = 0.75) +
  geom_line(aes(y = running_total$korver_sum), colour = bar_colour, linewidth = 1, alpha = 0.75) +
  geom_line(aes(y = running_total$pachulia_sum), colour = bar_colour, linewidth = 1, alpha = 0.75) +
  geom_line(aes(y = running_total$ridnour_sum), colour = bar_colour, linewidth = 1, alpha = 0.75) +
  geom_line(aes(y = running_total$wade_sum), colour = bar_colour, linewidth = 1, alpha = 0.75) +
  geom_line(aes(y = running_total$west_sum), colour = bar_colour, linewidth = 1, alpha = 0.75) +
  geom_line(aes(y = running_total$williams_sum), colour = bar_colour, linewidth = 1, alpha = 0.75) +
  geom_line(aes(y = running_total$james_sum), colour = other_focus, linewidth = 1.5)
rough_line

themed_line <- rough_line +
  theme_ptb() +
  scale_x_continuous(limits = c(2002, 2027),
                     expand = c(0, 0),
                     breaks = seq(2003, 2023, 5)) +
  scale_y_continuous(limits = c(0, 59000),
                     expand = c(0, 0),
                     labels = label_comma()) +
  theme(axis.title = element_blank())
themed_line

labelled_line <- themed_line +
  labs(title = "LeBron has outlasted all his peers",
       subtitle = "Cumulative <b>career regular-season minutes</b><br>played by players selected in the 2003 NBA Draft",
       caption = "<i>Data as at 18 Jan 2024</i><br><br>Data: Basketball Reference | Chart: Plot the Ball")
labelled_line

lebron_advanced <- lebron_tables[[7]] %>%
  clean_names() %>%
  filter(!is.na(age) & lg == "NBA")
  
lebron_bpm_clean <- lebron_advanced %>%
  select(1, 2, 28) %>%
  mutate(category = case_when(age == 19 ~ "Rookie",
                              age == 24 ~ "Peak",
                              age == 39 ~ "Current",
                              TRUE ~ "Other"))

labels <- c("2023-24", "", "", "", "", "", "", "", "", "", "", "",
            "", "", "", "2008-09", "", "", "", "", "2003-04")

rough_bar <- ggplot(lebron_bpm_clean, aes(x = fct_reorder(season, -age), y = bpm, fill = category)) +
  geom_bar(stat = "identity", width = 0.5) +
  scale_fill_manual(values = c("Other" = bar_colour,
                               "Rookie" = other_focus,
                               "Peak" = cavaliers_red,
                               "Current" = lakers_yellow)) +
  scale_y_continuous(limits = c(0, 14.9),
                     breaks = c(0, 2, 6, 10),
                     expand = c(0,0),
                     labels = label_number(style_positive = "plus")) +
  scale_x_discrete(labels = labels) +
  coord_flip() +
  theme(legend.position = "none")
rough_bar

themed_bar <- rough_bar +
  theme_ptb() +
  theme(axis.line.x = element_blank(),
        axis.ticks.y = element_blank(),
        panel.grid.major.y = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank())
themed_bar

labelled_bar <- themed_bar +
  labs(title = "LeBron still plays at an All-NBA level",
       subtitle = "LeBron James' estimated <b>Box Plus/Minus</b> per 100<br>possessions in each NBA season since 2003-04",
       caption = "<i>Data as at 18 Jan 2024</i><br><br>Data: Basketball Reference | Chart: Plot the Ball")
labelled_bar
