library(tidyverse)
library(rvest)
library(janitor)
library(lemon)
library(scales)
library(ggtext)

away_url_base <- "https://stats.espncricinfo.com/ci/engine/stats/index.html?bowling_pacespin=2;class=1;filter=advanced;home_or_away=2;home_or_away=3;orderby=start;page="
first_page <- 1
away_url_end <- ";size=200;spanmin2=1+Aug+2019;spanval2=span;template=results;type=bowling;view=innings;wrappertype=print"
away_url_full <- str_c(away_url_base, first_page, away_url_end)

away_no_pages_source <- read_html(away_url_full) %>%
  html_node(xpath = "/html/body/div/div[3]/table[2]") %>%
  html_table()
away_no_pages <- unlist(away_no_pages_source[1,1]) %>%
  str_replace(., "Page 1 of ", "") %>%
  as.numeric()

away_rank <- seq(first_page, away_no_pages, 1)

away_links <- str_c(away_url_base, away_rank, away_url_end)

away_bowling_data <- tibble()

for (i in away_links) {
  print(Sys.time())
  Sys.sleep(2)
  
  table_data <- read_html(i) %>%
    html_node(xpath = "/html/body/div/div[3]/table[3]") %>%
    html_table() %>%
    clean_names()
  
  table_data$overs <- as.character(table_data$overs)
  table_data$mdns <- as.character(table_data$mdns)
  table_data$runs <- as.character(table_data$runs)
  table_data$wkts <- as.character(table_data$wkts)
  table_data$econ <- as.character(table_data$econ)
  table_data$inns <- as.character(table_data$inns)
  away_bowling_data <- bind_rows(away_bowling_data, table_data)
}

home_url_base <- "https://stats.espncricinfo.com/ci/engine/stats/index.html?bowling_pacespin=2;class=1;filter=advanced;home_or_away=1;orderby=start;page="
first_page <- 1
home_url_end <- ";size=200;spanmin2=1+Aug+2019;spanval2=span;template=results;type=bowling;view=innings;wrappertype=print"
home_url_full <- str_c(home_url_base, first_page, home_url_end)

home_no_pages_source <- read_html(home_url_full) %>%
  html_node(xpath = "/html/body/div/div[3]/table[2]") %>%
  html_table()
home_no_pages <- unlist(home_no_pages_source[1,1]) %>%
  str_replace(., "Page 1 of ", "") %>%
  as.numeric()

home_rank <- seq(first_page, home_no_pages, 1)

home_links <- str_c(home_url_base, home_rank, home_url_end)

home_bowling_data <- tibble()

for (i in home_links) {
  print(Sys.time())
  Sys.sleep(2)
  
  table_data <- read_html(i) %>%
    html_node(xpath = "/html/body/div/div[3]/table[3]") %>%
    html_table() %>%
    clean_names()
  
  table_data$overs <- as.character(table_data$overs)
  table_data$mdns <- as.character(table_data$mdns)
  table_data$runs <- as.character(table_data$runs)
  table_data$wkts <- as.character(table_data$wkts)
  table_data$econ <- as.character(table_data$econ)
  table_data$inns <- as.character(table_data$inns)
  home_bowling_data <- bind_rows(home_bowling_data, table_data)
}

away_bowling_data$home_away <- "Away"
home_bowling_data$home_away <- "Home"

spin_bowling_data <- bind_rows(away_bowling_data, home_bowling_data)

spin_all_inns_clean <- spin_bowling_data %>%
  select(1, 2, 4, 5, 9, 10, 11, 12) %>%
  filter(overs != "DNB" & overs != "TDNB" & overs != "sub") %>%
  mutate(match_id = str_c(ground, " (", start_date, ")"),
         .before = player) %>%
  separate(col = overs,
           into = c("completed_overs", "completed_balls"),
           sep = "[.]")

spin_all_inns_clean$start_date <- dmy(spin_all_inns_clean$start_date)

spin_all_inns_clean$completed_overs <- as.integer(spin_all_inns_clean$completed_overs)
spin_all_inns_clean$completed_balls <- as.integer(replace_na(spin_all_inns_clean$completed_balls, 0))
spin_all_inns_clean$runs <- as.integer(spin_all_inns_clean$runs)
spin_all_inns_clean$wkts <- as.integer(spin_all_inns_clean$wkts)

spin_all_inns_clean$player <- str_replace_all(spin_all_inns_clean$player, fixed("(1)"), "I")

spin_all_inns_clean <- spin_all_inns_clean %>%
  mutate(balls_bowled = completed_balls + (6 * completed_overs),
         .before = runs) %>%
  select(-completed_overs, -completed_balls) %>%
  separate(player, into = c("name", "country"), sep = "[(]") %>%
  mutate(name = str_squish(name),
         country = str_squish(str_remove_all(country, fixed(")")))) %>%
  arrange(start_date) %>%
  filter(country != "AFG" & country != "IRE" & country != "ZIM" & opposition != "v Afghanistan" & opposition != "v Ireland" & opposition != "v Zimbabwe")

spin_match_splits <- spin_all_inns_clean %>%
  group_by(match_id, start_date) %>%
  summarise(tot_balls = sum(balls_bowled),
            tot_runs = sum(runs),
            tot_wkts = sum(wkts))

spin_player_match_splits <- spin_all_inns_clean %>%
  group_by(match_id, start_date, name, country) %>%
  summarise(n_balls = sum(balls_bowled),
            n_runs = sum(runs),
            n_wkts = sum(wkts))

spin_vs_exp <- left_join(x = spin_player_match_splits, y = spin_match_splits, by = c("match_id", "start_date"))

spin_vs_exp <- spin_vs_exp %>%
  mutate(cycle = case_when(start_date <= ymd("2021-06-18") ~ "WTC 2021",
                           start_date <= ymd("2023-06-07") ~ "WTC 2023",
                           start_date <= ymd("2025-06-11") ~ "WTC 2025",
                           TRUE ~ "WTC 2027"),
         other_balls = tot_balls - n_balls,
         other_runs = tot_runs - n_runs,
         other_wkts = tot_wkts - n_wkts) %>%
  select(-8, -9, -10) %>%
  relocate(cycle, .before = name) %>%
  arrange(start_date)

spin_vs_exp_summary <- spin_vs_exp %>%
  group_by(name, country) %>%
  summarise(n_matches = n(),
            n_balls = sum(n_balls),
            n_overs = n_balls / 6,
            n_runs = sum(n_runs),
            n_wkts = sum(n_wkts),
            other_balls = sum(other_balls),
            other_runs = sum(other_runs),
            other_wkts = sum(other_wkts)) %>%
  mutate(balls_per_match = n_balls / n_matches,
         wkts_per_match = n_wkts / n_matches,
         ave = n_runs / n_wkts,
         sr = n_balls / n_wkts,
         other_ave = other_runs / other_wkts,
         other_sr = other_balls / other_wkts,
         ave_vs_exp = ave - other_ave,
         sr_vs_exp = sr - other_sr) %>%
  arrange(desc(n_wkts))

spin_vs_exp_final <- spin_vs_exp_summary %>%
  filter(n_balls >= (200 * 6) & balls_per_match >= 120) %>%
  select(name, country, n_balls, ave, other_ave, ave_vs_exp, sr, other_sr, sr_vs_exp) %>%
  arrange(ave_vs_exp) %>%
  mutate(n_overs = n_balls / 6)

aus_colour <- "#E6CC5C"
eng_colour <- "#A3B8CC"
background_colour <- "#C9C9C9"

rough_scatter <- ggplot(spin_vs_exp_final, aes(x = n_overs, y = ave_vs_exp)) +
  geom_hline(yintercept = 0,
             color = ptb_dark_grey,
             alpha = 0.9) +
  geom_point(size = 5,
             color = background_colour,
             alpha = 0.3) +
  geom_point(data = (spin_vs_exp_final %>% filter(name == "NM Lyon")),
             shape = 21,
             fill = aus_colour,
             color = ptb_dark_grey,
             alpha = 1,
             size = 5,
             stroke = 1) +
  geom_point(data = (spin_vs_exp_final %>% filter(name == "Shoaib Bashir")),
             shape = 21,
             fill = eng_colour,
             color = ptb_dark_grey,
             alpha = 1,
             size = 5,
             stroke = 1) +
  scale_y_continuous(name = NULL,
                     limits = c(-24, 37),
                     breaks = seq(-20, 30, 10),
                     labels = label_number(style_positive = "plus"),
                     expand = c(0,0)) +
  scale_x_continuous(name = NULL,
                     limits = c(110, 2460),
                     breaks = seq(500, 2000, 500),
                     labels = label_comma(),
                     expand = c(0,0)) +
  theme(legend.position = "none",
        axis.line.x = element_blank(),
        axis.line.y = element_blank(),
        axis.ticks = element_blank()) +
  annotate(geom = "text",
           x = 170,
           y = 36.6,
           label = "Runs conceded per\nwicket vs. expectation",
           family = ptb_font,
           colour = ptb_dark_grey,
           fontface = "bold",
           hjust = 0,
           vjust = 1) +
  annotate(geom = "text",
           x = 2400,
           y = -17.5,
           label = "Overs bowled",
           family = ptb_font,
           colour = ptb_dark_grey,
           fontface = "bold",
           hjust = 1,
           vjust = 1)
rough_scatter

themed_scatter <- rough_scatter +
  theme_ptb()
themed_scatter

labelled_scatter <- themed_scatter +
  labs(title = "<b style='color:#CCB652'>Nathan Lyon</b>'s consistent record sets<br>him apart from every other spinner",
       subtitle = "Performance of <b>spin bowlers</b> vs. their peers in the<br><b>World Test Championship</b> era",
       caption = "<i>Spin bowlers with <200 overs bowled or <20 overs per match excluded</i><br><br>'WTC era' began on 1 Aug 2019 | Excl. matches vs. AFG, IRE and ZIM<br><br>Data: ESPNCricinfo | Chart: Plot the Ball")
labelled_scatter