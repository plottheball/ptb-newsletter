library(tidyverse)
library(rvest)
library(janitor)
library(RcppRoll)
library(ggtext)

batting_url <- "https://stats.espncricinfo.com/ci/engine/stats/index.html?class=10;filter=advanced;orderby=start;size=200;team=289;template=results;type=team;view=innings;wrappertype=print"
bowling_url <- "https://stats.espncricinfo.com/ci/engine/stats/index.html?class=10;filter=advanced;orderby=start;size=200;team=289;team_view=bowl;template=results;type=team;view=innings;wrappertype=print"

batting_tables <- read_html(batting_url) %>%
  html_nodes("table") %>%
  html_table()

batting_table <- batting_tables[[3]] %>%
  clean_names() %>%
  select(1, 2, 3, 8, 10) %>%
  mutate(category = "Batting", .after = team)

batting_table$start_date <- dmy(batting_table$start_date)
batting_table <- batting_table %>%
  mutate(match_index = row_number(),
         .before = team)

bowling_tables <- read_html(bowling_url) %>%
  html_nodes("table") %>%
  html_table()

bowling_table <- bowling_tables[[3]] %>%
  clean_names() %>%
  select(1, 2, 3, 8, 10) %>%
  mutate(category = "Bowling", .after = team)

bowling_table$start_date <- dmy(bowling_table$start_date)

bowling_table <- bowling_table %>%
  mutate(match_index = row_number(),
         .before = team)

data_all <- bind_rows(batting_table, bowling_table) %>%
  filter(score != "DNB") %>%
  filter(overs != "0.0")

data_all <- data_all %>%
  separate(col = overs,
           into = c("completed_overs", "completed_balls"),
           sep = "[.]") %>%
  separate(col = score,
           into = c("runs_scored", "wickets"),
           sep = "[/]")

data_all[4:7] <- sapply(data_all[4:7], as.integer)

data_all$completed_balls <- replace_na(data_all$completed_balls, 0)
data_all$wickets <- replace_na(data_all$wickets, 10)
data_all$total_balls <- data_all$completed_balls + (6 * data_all$completed_overs)

aus_batting_final <- data_all %>%
  filter(category == "Batting")

aus_bowling_final <- data_all %>%
  filter(category == "Bowling")

roll_no <- 30

aus_batting_final$roll_runs <- (roll_sum(aus_batting_final$runs_scored, n = roll_no, align = "right", fill = NA))
aus_batting_final$roll_wkts <- (roll_sum(aus_batting_final$wickets, n = roll_no, align = "right", fill = NA))
aus_batting_final$roll_bf <- (roll_sum(aus_batting_final$total_balls, n = roll_no, align = "right", fill = NA))
aus_batting_final$roll_rpi <- aus_batting_final$roll_runs / aus_batting_final$roll_bf * 120
aus_batting_final$roll_ave <- aus_batting_final$roll_runs / aus_batting_final$roll_wkts

aus_bowling_final$roll_runs <- (roll_sum(aus_bowling_final$runs_scored, n = roll_no, align = "right", fill = NA))
aus_bowling_final$roll_wkts <- (roll_sum(aus_bowling_final$wickets, n = roll_no, align = "right", fill = NA))
aus_bowling_final$roll_bf <- (roll_sum(aus_bowling_final$total_balls, n = roll_no, align = "right", fill = NA))
aus_bowling_final$roll_rpi <- aus_bowling_final$roll_runs / aus_bowling_final$roll_bf * 120
aus_bowling_final$roll_ave <- aus_bowling_final$roll_runs / aus_bowling_final$roll_wkts

rough_line_rpi <- ggplot() +
  geom_path(data = aus_batting_final, aes(x = match_index, y = roll_rpi), colour = "#1f5447", linewidth = 1.25) +
  geom_path(data = aus_bowling_final, aes(x = match_index, y = roll_rpi), colour = "#e2e444", linewidth = 1.25) +
  scale_y_continuous(name = NULL,
                     limits = c(95, 185),
                     expand = c(0,0),
                     breaks = seq(100, 180, 20)) +
  scale_x_continuous(limits = c(-30, 215),
                     expand = c(0,0)) +
  theme(axis.line.x = element_blank(),
        axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        axis.ticks.x = element_blank(),
        panel.grid.major.x = element_blank())
rough_line_rpi

themed_line_rpi <- rough_line_rpi +
  theme_ptb() +
  geom_vline(xintercept = 38,
             size = 0.5,
             color = ptb_light_grey,
             linetype = "dashed") +
  geom_vline(xintercept = 82,
             size = 0.5,
             color = ptb_light_grey,
             linetype = "dashed") +
  geom_vline(xintercept = 127,
             size = 0.5,
             color = ptb_light_grey,
             linetype = "dashed")
themed_line_rpi

labelled_line_rpi <- themed_line_rpi +
  labs(title = "<b style='color:#1f5447'>Australia</b>'s <b style='color:#e2e444'>WT20I opponents</b> are<br>scoring faster than ever against them",
       subtitle = "Average number of <b>runs scored per 20 overs</b> by <br><b style='color:#1f5447'>Australia</b> and <b style='color:#e2e444'>their opponents</b> in Women's T20Is",
       caption = "<i>Averages calculated on a 30-innings rolling basis</i><br><br>Data: ESPNCricinfo | Chart: Plot the Ball")
labelled_line_rpi
