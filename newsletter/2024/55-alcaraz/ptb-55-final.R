library(tidyverse)
library(rvest)
library(janitor)
library(RcppRoll)
library(ggtext)
library(scales)
library(lemon)
library(googlesheets4)

### Historic data downloaded from: https://github.com/JeffSackmann/tennis_slam_pointbypoint
seasons <- seq(1998, 2023, 1)

all_data <- tibble()

for (i in seasons) {
  
  file_name <- str_c("atp_matches_", i, ".csv")
  
  data <- read_csv(file_name) %>%
    clean_names()
  
  data$season <- i
  
  all_data <- bind_rows(all_data, data)
  
}

clean_data <- all_data %>%
  select(1, 2, 3, 6, 8, 11, 16, 19, 24, 50)

player_list_winners <- clean_data %>%
  select(5, 6) %>%
  rename("player_id" = "winner_id", "player_name" = "winner_name") %>%
  unique()

player_list_losers <- clean_data %>%
  select(7, 8) %>%
  rename("player_id" = "loser_id", "player_name" = "loser_name") %>%
  unique()

player_list_final <- bind_rows(player_list_winners, player_list_losers) %>%
  unique()

player_list_focus <- player_list_final %>%
  filter(player_name == "Carlos Alcaraz" | player_name == "Roger Federer" | player_name == "Rafael Nadal" | player_name == "Novak Djokovic")

match_data_focus <- clean_data %>%
  filter(winner_id %in% player_list_focus$player_id | loser_id %in% player_list_focus$player_id)

match_data_focus <- match_data_focus %>%
  mutate(federer_y_n = case_when(winner_name == "Roger Federer" ~ "Win",
                                 loser_name == "Roger Federer" ~ "Loss",
                                 TRUE ~ NA),
         nadal_y_n = case_when(winner_name == "Rafael Nadal" ~ "Win",
                               loser_name == "Rafael Nadal" ~ "Loss",
                               TRUE ~ NA),
         djokovic_y_n = case_when(winner_name == "Novak Djokovic" ~ "Win",
                                  loser_name == "Novak Djokovic" ~ "Loss",
                                  TRUE ~ NA),
         alcaraz_y_n = case_when(winner_name == "Carlos Alcaraz" ~ "Win",
                                 loser_name == "Carlos Alcaraz" ~ "Loss",
                                  TRUE ~ NA))

### 2024 data update manually downloaded from: https://www.tennisabstract.com/cgi-bin/player.cgi?p=CarlosAlcaraz
update_import <- read_sheet("https://docs.google.com/spreadsheets/d/1-P_yXziL4e8khCPEs0u-VAEjprMHlHugYKVdeowv2G0/edit?usp=sharing") %>%
  clean_names()

match_data_focus <- bind_rows(match_data_focus, update_import)

roll_no <- 50

federer_matches <- match_data_focus %>%
  filter(!is.na(federer_y_n)) %>%
  select(-nadal_y_n, -djokovic_y_n, -alcaraz_y_n) %>%
  rename("result" = "federer_y_n") %>%
  mutate(match_index = row_number(),
         win_y_n = if_else(result == "Win", 1, 0),
         federer_roll_win_share = roll_sum(win_y_n, n = roll_no, align = "right", fill = NA) / roll_no * 100)

nadal_matches <- match_data_focus %>%
  filter(!is.na(nadal_y_n)) %>%
  select(-federer_y_n, -djokovic_y_n, -alcaraz_y_n) %>%
  rename("result" = "nadal_y_n") %>%
  mutate(match_index = row_number(),
         win_y_n = if_else(result == "Win", 1, 0),
         nadal_roll_win_share = roll_sum(win_y_n, n = roll_no, align = "right", fill = NA) / roll_no * 100)

djokovic_matches <- match_data_focus %>%
  filter(!is.na(djokovic_y_n)) %>%
  select(-federer_y_n, -nadal_y_n, -alcaraz_y_n) %>%
  rename("result" = "djokovic_y_n") %>%
  mutate(match_index = row_number(),
         win_y_n = if_else(result == "Win", 1, 0),
         djokovic_roll_win_share = roll_sum(win_y_n, n = roll_no, align = "right", fill = NA) / roll_no * 100)

alcaraz_matches <- match_data_focus %>%
  filter(!is.na(alcaraz_y_n)) %>%
  select(-federer_y_n, -djokovic_y_n, -nadal_y_n) %>%
  rename("result" = "alcaraz_y_n") %>%
  mutate(match_index = row_number(),
         win_y_n = if_else(result == "Win", 1, 0),
         alcaraz_roll_win_share = roll_sum(win_y_n, n = roll_no, align = "right", fill = NA) / roll_no * 100)

federer_roll <- federer_matches %>%
  select(match_index, federer_roll_win_share)

nadal_roll <- nadal_matches %>%
  select(match_index, nadal_roll_win_share)

djokovic_roll <- djokovic_matches %>%
  select(match_index, djokovic_roll_win_share)

alcaraz_roll <- alcaraz_matches %>%
  select(match_index, alcaraz_roll_win_share)

all_roll <- left_join(federer_roll, nadal_roll, by = c("match_index" = "match_index")) %>%
  left_join(., djokovic_roll, by = c("match_index" = "match_index")) %>%
  left_join(., alcaraz_roll, by = c("match_index" = "match_index")) %>%
  filter(match_index >= roll_no)

alcaraz_max <- max(alcaraz_roll$match_index)

career_start <- all_roll %>%
  filter(match_index <= alcaraz_max)

alcaraz_colour <- "#CD6035"
federer_colour <- "#C4B4E0"
nadal_colour <- "#E0DEB4"
djokovic_colour <- "#B4E0DA"

rough_plot <- ggplot(career_start, aes(x = match_index)) +
  geom_line(aes(y = federer_roll_win_share), colour = federer_colour, linewidth = 1.25, alpha = 0.75) +
  geom_line(aes(y = nadal_roll_win_share), colour = nadal_colour, linewidth = 1.25, alpha = 0.75) +
  geom_line(aes(y = djokovic_roll_win_share), colour = djokovic_colour, linewidth = 1.25, alpha = 0.75) +
  geom_line(aes(y = alcaraz_roll_win_share), colour = alcaraz_colour, linewidth = 1.75)
rough_plot

themed_plot <- rough_plot +
  theme_ptb() +
  scale_x_continuous(limits = c(0, 240),
                     expand = c(0, 0),
                     breaks = seq(0, 200, 50)) +
  scale_y_continuous(limits = c(0, 100),
                     breaks = seq(0, 100, 25),
                     expand = c(0, 0),
                     labels = percent_format(scale = 1)) +
  geom_hline(yintercept = 100,
             color = ptb_dark_grey,
             linewidth = 1.5,
             alpha = 0.3) +
  theme(axis.ticks.y = element_blank(),
        axis.line.y = element_blank())
themed_plot

labelled_plot <- themed_plot +
  labs(x = "Match no.", 
       title = "<b style='color:#CD6035'>Alcaraz</b> started his career strongly —<br>but his win % has dropped recently",
       subtitle = "Rolling-average win percentage of <b style='color:#CD6035'>Carlos Alcaraz</b><br>over his first 221 ATP singles matches, compared to<br>each of the <b>'Big Three'</b> over their first 221 matches",
       caption = "<i>Win percentage calculated on a 50-match rolling basis</i><br><br>Data: Tennis Abstract | Chart: Plot the Ball")
labelled_plot

### Match Charting Project data manually downloaded from: https://www.tennisabstract.com/charting/meta.html
mcp_data <- read_sheet("https://docs.google.com/spreadsheets/d/1VY9EnTbSD3gHCrDbnQM3UaVc1hwo5ivrNOwdqVTD-S0/edit?usp=sharing") %>%
  clean_names()

mcp_service <- mcp_data %>%
  filter(category == "Sv")

service_summary <- mcp_service %>%
  group_by(player, category) %>%
  summarise(no_points = sum(pts))

service_clean <- mcp_service %>%
  select(1, 2, 3, 4, 5) %>%
  left_join(., y = service_summary, by = c("player" = "player", "category" = "category")) %>%
  mutate(share = pts / no_points,
         category_wider = case_when(length_shots == "1-3 shots" | length_shots == "4-6 shots" ~ "<7",
                                    TRUE ~ "7 or more"))

service_final <- service_clean %>%
  group_by(player, category_wider) %>%
  summarise(tot_pts = sum(pts),
            tot_w = sum(w),
            tot_share = sum(share) * 100) %>%
  filter(category_wider == "7 or more") %>%
  mutate(win_percent = tot_w / tot_pts * 100)

rough_scatter <- ggplot(service_final, aes(x = tot_share, y = win_percent, color = player)) +
  geom_point(size = 7,
             alpha = 0.9) +
  scale_x_continuous(limits = c(6, 34),
                     breaks = seq(10, 30, 5),
                     expand = c(0, 0),
                     labels = percent_format(scale = 1)) +
  scale_y_continuous(limits = c(54, 66),
                     breaks = seq(55, 65, 5),
                     expand = c(0, 0),
                     labels = percent_format(scale = 1)) +
  scale_color_manual(values = c(alcaraz_colour, djokovic_colour, nadal_colour, federer_colour))
rough_scatter

themed_scatter <- rough_scatter +
  theme_ptb() +
  theme(legend.position = "none",
        axis.line = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank()) +
  annotate(geom = "text",
           x = 7,
           y = 65.75,
           label = "Win % on service points\nlasting 7+ shots",
           family = ptb_font,
           colour = ptb_dark_grey,
           fontface = "bold",
           hjust = 0,
           vjust = 1) +
  annotate(geom = "text",
           x = 33,
           y = 56.5,
           label = "% of service points\nlasting 7+ shots",
           family = ptb_font,
           colour = ptb_dark_grey,
           fontface = "bold",
           hjust = 1,
           vjust = 1)
themed_scatter

labelled_scatter <- themed_scatter +
  labs(title = "<b style='color:#CD6035'>Alcaraz</b> wins long points on his serve<br>more often than the 'Big Three'",
       subtitle = "Performance of <b style='color:#CD6035'>Carlos Alcaraz</b> over a sample of<br>service points lasting at least 7 shots, compared to<br>each of the <b>'Big Three'</b>",
       caption = "Data: Tennis Abstract | Chart: Plot the Ball")
labelled_scatter
