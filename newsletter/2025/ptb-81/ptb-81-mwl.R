library(tidyverse)
library(janitor)
library(ggtext)

years <- seq(2007, 2025, 1)

### https://www.pgatour.com/stats/detail/02407
### Highest point of the shot (in feet and inches) relative to the tee on Par 4 and Par 5 tee shots where a valid radar measurement was taken. (2407)
apex_data <- tibble()

for (i in years) {
  
  year_data <- read.csv(str_c("apex_", i, ".csv")) %>%
    clean_names()
  
  year_data$year <- i
  
  apex_data <- bind_rows(apex_data, year_data)
  
}

apex_clean <- apex_data %>%
  select(year, player_id, player, total_attempts, avg) %>%
  separate(col = avg,
           into = c("avg_ft", "avg_in"),
           sep = "[']") %>%
  mutate(avg_ft = as.integer(avg_ft),
         avg_in = replace_na(as.integer(str_squish(str_remove_all(avg_in, fixed("\"")))), 0),
         avg_yds = (avg_ft + (avg_in / 12)) / 3,
         adj_total_yds = avg_yds * total_attempts)

apex_summary <- apex_clean %>%
  group_by(player_id, player) %>%
  summarise(no_attempts = sum(total_attempts),
            aggregate_apex_height_yds = sum(adj_total_yds),
            average_apex_yds = aggregate_apex_height_yds / no_attempts,
            average_apex_ft = average_apex_yds * 3) %>%
  arrange(desc(average_apex_yds)) %>%
  filter(no_attempts > 600) %>%
  select(-aggregate_apex_height_yds)

active_ids <- apex_clean %>%
  filter(year > 2024) %>%
  select(player_id) %>%
  unique() %>%
  unlist()

apex_active <- apex_summary %>%
  filter(player_id %in% active_ids)

### https://www.pgatour.com/stats/detail/02409
### Distance from tee to the point of ground impact on Par 4 and Par 5 tee shots where a valid radar measurement was taken. (2409)

carry_data <- tibble()

for (i in years) {
  
  year_data <- read.csv(str_c("carry_", i, ".csv")) %>%
    clean_names()
  
  year_data$year <- i
  
  carry_data <- bind_rows(carry_data, year_data)
  
}

carry_clean <- carry_data %>%
  select(year, player_id, player, total_attempts, avg) %>%
  mutate(adj_total_yds = avg * total_attempts)

carry_summary <- carry_clean %>%
  group_by(player_id, player) %>%
  summarise(no_attempts = sum(total_attempts),
            aggregate_carry_yds = sum(adj_total_yds),
            average_carry_yds = aggregate_carry_yds / no_attempts,
            average_carry_ft = average_carry_yds * 3) %>%
  arrange(desc(average_carry_yds)) %>%
  filter(no_attempts > 600) %>%
  select(-aggregate_carry_yds)

carry_active <- carry_summary %>%
  filter(player_id %in% active_ids)

final_active <- full_join(apex_active, carry_active, by = c("player_id", "player")) %>%
  clean_names() %>%
  filter(!is.na(no_attempts_y)) %>%
  select(-no_attempts_x, -no_attempts_y) %>%
  mutate(carry_apex_ratio = average_carry_yds / average_apex_yds)

focus_one <- "#3D7199"
focus_two <- "#A3CCCC"
focus_three <- "#CCA3AA"
focus_four <- "#CCBAA3"
background_grey <- "#C9C9C9"

rough_scatter <- ggplot(final_active, aes(x = average_carry_yds, y = average_apex_yds)) +
  geom_point(size = 4.5,
             color = case_when(final_active$player == "Min Woo Lee" ~ focus_one,
                               final_active$player == "Rory McIlroy" ~ focus_two,
                               final_active$player == "Ludvig Åberg" ~ focus_three,
                               final_active$player == "Scottie Scheffler" ~ focus_four,
                               TRUE ~ background_grey),
             alpha = case_when(final_active$player == "Min Woo Lee" ~ 1,
                               final_active$player == "Rory McIlroy" ~ 0.9,
                               final_active$player == "Ludvig Åberg" ~ 0.9,
                               final_active$player == "Scottie Scheffler" ~ 0.9,
                               TRUE ~ 0.25)) +
  scale_x_continuous(limits = c(261, 319),
                     breaks = seq(270, 310, 10),
                     expand = c(0,0)) +
  scale_y_continuous(limits = c(16, 54),
                     breaks = seq(20, 50, 10),
                     expand = c(0,0)) +
  theme(axis.title.x = element_blank(),
        axis.ticks = element_blank(),
        axis.line.x = element_blank(),
        axis.line.y = element_blank()) +
  annotate(geom = "text",
           x = 262,
           y = 52,
           label = "Average\napex (yds)",
           family = ptb_font,
           colour = ptb_dark_grey,
           fontface = "bold",
           hjust = 0,
           vjust = 1) +
  annotate(geom = "text",
           x = 318,
           y = 22,
           label = "Average\ncarry (yds)",
           family = ptb_font,
           colour = ptb_dark_grey,
           fontface = "bold",
           hjust = 1,
           vjust = 1)
rough_scatter

scatter_themed <- rough_scatter +
  theme_ptb()
scatter_themed

scatter_labelled <- scatter_themed +
  labs(title = "<b style='color:#3D7199'>Min Woo Lee</b> carries the ball further<br>off the tee than everyone else on tour",
       subtitle = "Ave. <b>apex</b> and <b>carry</b> (in yds) of <b>tracked drives</b> by<br>all active PGA Tour players in their careers to date",
       caption = "<i>Data available since 2007; players with <600 tracked drives excluded</i><br><br>Data: PGA Tour | Chart: Plot the Ball")
scatter_labelled
