library(tidyverse)
library(rvest)
library(janitor)
library(RcppRoll)
library(scales)
library(ggtext)

urls <- c("b/bedarco01/gamelog/2024", "b/bedarco01/gamelog/2025",
          "c/carlsle01/gamelog/2024", "c/carlsle01/gamelog/2025",
          "f/fantiad01/gamelog/2024", "f/fantiad01/gamelog/2025",
          "s/smithwi01/gamelog/2025",
          "c/celebma01/gamelog/2025",
          "s/slafkju01/gamelog/2023", "s/slafkju01/gamelog/2024", "s/slafkju01/gamelog/2025",
          "c/coolelo01/gamelog/2024", "c/coolelo01/gamelog/2025",
          "w/wrighsh01/gamelog/2023", "w/wrighsh01/gamelog/2024", "w/wrighsh01/gamelog/2025",
          "g/gauthcu01/gamelog/2024", "g/gauthcu01/gamelog/2025")

all_games <- tibble()

for (i in urls) {
  
  Sys.sleep(1)
  
  html <- read_html(str_c("https://www.hockey-reference.com/players/", i))
  
  tables <- html %>%
    html_nodes("table") %>%
    html_table()
  
  key_table <- tables[[1]] %>%
    clean_names() %>%
    select(x_2, x_11, x_14) %>%
    rename("date" = "x_2",
           "shots_on_goal" = "x_11",
           "time_on_ice" = "x_14") %>%
    filter(date != "Date") %>%
    mutate(date = ymd(date),
           player = i,
           .before = "date") %>%
    arrange(date)

  all_games <- bind_rows(all_games, key_table)
  
}

all_games <- all_games %>%
  separate(col = "time_on_ice", into = c("toi_mins", "toi_secs"), sep = ":") %>%
  mutate(toi_final = as.integer(toi_mins) + (as.integer(toi_secs) / 60),
         shots_on_goal = as.integer(shots_on_goal)) %>%
  select(-toi_mins, -toi_secs)

all_games$player <- str_remove_all(all_games$player, "/gamelog/2023")
all_games$player <- str_remove_all(all_games$player, "/gamelog/2024")
all_games$player <- str_remove_all(all_games$player, "/gamelog/2025")

rolling_total <- tibble()

roll_no <- 20

for (i in unique(all_games$player)) {
  
  filtered_games <- all_games %>%
    filter(player == i) %>%
    arrange(date) %>%
    mutate(game_index = row_number(),
           .before = "date")
  
  filtered_games$roll_toi <- roll_sum(filtered_games$toi_final, n = roll_no, align = "right", fill = NA)
  filtered_games$roll_sog <- roll_sum(filtered_games$shots_on_goal, n = roll_no, align = "right", fill = NA)
  filtered_games$roll_sog_p60 <- filtered_games$roll_sog / filtered_games$roll_toi * 60

  rolling_total <- bind_rows(rolling_total, filtered_games)
  
}

rolling_final <- rolling_total %>%
  select(player, game_index, roll_sog_p60) %>%
  filter(!is.na(roll_sog_p60)) %>%
  pivot_wider(names_from = player,
              values_from = roll_sog_p60) %>%
  clean_names()

background_colour <- "#C3D8E6"
focus_colour <- "#CF0A2C"

rough_line <- ggplot(rolling_final, aes(x = game_index)) +
  geom_vline(xintercept = 82,
             linewidth = 0.7,
             color = "#e0e4ef",
             linetype = "dashed") +
  geom_vline(xintercept = 164,
             linewidth = 0.7,
             color = "#e0e4ef",
             linetype = "dashed") +
  scale_y_continuous(limits = c(1, 14),
                     breaks = seq(3, 12, 3),
                     expand = c(0, 0)) +
  scale_x_continuous(limits = c(0, 199),
                     breaks = seq(0, 164, 82),
                     expand = c(0, 0)) +
  geom_line(aes(y = rolling_final$g_gauthcu01), colour = background_colour, linewidth = 2, alpha = 0.5) +
  geom_line(aes(y = rolling_final$w_wrighsh01), colour = background_colour, linewidth = 2, alpha = 0.5) +
  geom_line(aes(y = rolling_final$c_coolelo01), colour = background_colour, linewidth = 2, alpha = 0.5) +
  geom_line(aes(y = rolling_final$s_slafkju01), colour = background_colour, linewidth = 2, alpha = 0.5) +
  geom_line(aes(y = rolling_final$c_celebma01), colour = background_colour, linewidth = 2, alpha = 0.5) +
  geom_line(aes(y = rolling_final$s_smithwi01), colour = background_colour, linewidth = 2, alpha = 0.5) +
  geom_line(aes(y = rolling_final$f_fantiad01), colour = background_colour, linewidth = 2, alpha = 0.5) +
  geom_line(aes(y = rolling_final$c_carlsle01), colour = background_colour, linewidth = 2, alpha = 0.5) +
  geom_line(aes(y = rolling_final$b_bedarco01), colour = focus_colour, linewidth = 2, alpha = 1)
rough_line

themed_line <- rough_line +
  theme_ptb() +
  theme(axis.line = element_blank(),
        axis.text.x = element_blank(), 
        axis.ticks = element_blank(),
        panel.grid.major.x = element_blank(),
        axis.title.x = element_blank())
themed_line

labelled_line <- themed_line +
  labs(title = "<b style='color:#CF0A2C'>Connor Bedard</b> is getting away fewer<br>shots on goal than he did last season",
       subtitle = "Number of <b>shots on goal</b> recorded <b>per 60 mins</b><br>by <b>top-five picks</b> in the NHL Draft <b>since 2022</b>",
       caption = "<i>Calculated on a 20-game rolling basis; data as at 10 Feb 2025</i><br><br>Data: Hockey Reference | Chart: Plot the Ball")
labelled_line
