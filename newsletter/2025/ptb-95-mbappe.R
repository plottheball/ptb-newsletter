library(tidyverse)
library(janitor)
library(worldfootballR)
library(RcppRoll)
library(lemon)
library(ggtext)

urls_2018 <- fb_match_urls(country = "FRA", gender = "M", season_end_year = 2018, tier = "1st")
urls_2019 <- fb_match_urls(country = "FRA", gender = "M", season_end_year = 2019, tier = "1st")
urls_2020 <- fb_match_urls(country = "FRA", gender = "M", season_end_year = 2020, tier = "1st")
urls_2021 <- fb_match_urls(country = "FRA", gender = "M", season_end_year = 2021, tier = "1st")
urls_2022 <- fb_match_urls(country = "FRA", gender = "M", season_end_year = 2022, tier = "1st")
urls_2023 <- fb_match_urls(country = "FRA", gender = "M", season_end_year = 2023, tier = "1st")
urls_2024 <- fb_match_urls(country = "FRA", gender = "M", season_end_year = 2024, tier = "1st")
urls_2025 <- fb_match_urls(country = "ESP", gender = "M", season_end_year = 2025, tier = "1st")
urls_2026 <- fb_match_urls(country = "ESP", gender = "M", season_end_year = 2026, tier = "1st")

ligue_1_urls <- c(urls_2018, urls_2019, urls_2020, urls_2021,
                  urls_2022, urls_2023, urls_2024)
psg_urls <- ligue_1_urls[grepl(pattern = "Paris-Saint-Germain", x = ligue_1_urls)]

la_liga_urls <- c(urls_2025, urls_2026)
madrid_urls <- la_liga_urls[grepl(pattern = "Real-Madrid", x = la_liga_urls)]

mbappe_urls <- c(psg_urls, madrid_urls)

all_shots <- tibble()

for (i in mbappe_urls) {
  
  Sys.sleep(3)
  
  match_shots <- fb_match_shooting(i)
  
  all_shots <- bind_rows(all_shots, match_shots)
  
}

mbappe_shots <- all_shots %>%
  clean_names() %>%
  filter(player == "Kylian Mbappé") %>%
  mutate(shot_id = row_number(),
         .before = "date")

roll_num <- 120

rolling_xg <- mbappe_shots %>%
  select(shot_id, date, squad, player, x_g, distance, body_part) %>%
  mutate(x_g = as.numeric(x_g),
         distance = as.integer(distance),
         roll_xg = roll_sum(x_g, n = roll_num, align = "right", fill = NA),
         roll_xg_per_shot = roll_xg / roll_num,
         roll_dist = roll_sum(distance, n = roll_num, align = "right", fill = NA),
         roll_dist_per_shot = roll_dist / roll_num)

for_plot <- rolling_xg %>%
  select(shot_id, squad, roll_xg_per_shot, roll_dist_per_shot) %>%
  filter(!is.na(roll_xg_per_shot)) %>%
  pivot_longer(c("roll_xg_per_shot", "roll_dist_per_shot"), names_to = "category", values_to = "value")

facet_labels <- c("roll_xg_per_shot" = "Ave. shot quality (xG)",
                  "roll_dist_per_shot" = "Ave. shot distance (yds)")

psg_colour <- "#406580"
real_colour <- "#E6AC00"

rough_line <- ggplot(for_plot, aes(x = shot_id, y = value)) +
  geom_line(linewidth = 1.5, aes(colour = squad)) +
  geom_vline(xintercept = 785,
             size = 1,
             color = ptb_light_grey,
             linetype = "dashed") +
  facet_rep_wrap(~category, ncol = 1, scales = "free_y", labeller = labeller(category = facet_labels)) + 
  scale_colour_manual(values = c("Paris S-G" = psg_colour,
                                 "Real Madrid" = real_colour)) +
  scale_y_continuous(expand = c(0.4, 0)) +
  scale_x_continuous(limits = c(100, 1030)) +
  theme(legend.position = "none",
        axis.line.x = element_blank(),
        axis.line.y = element_blank(),
        axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        axis.ticks = element_blank(),
        panel.grid.major.x = element_blank())
rough_line

themed_line <- rough_line +
  theme_ptb()
themed_line

labelled_line <- themed_line +
  labs(title = "Kylian Mbappé doesn't take his shots<br>from as close to goal as he used to",
       subtitle = "Average <b>distance</b> (in yds) and <b>quality</b> (in xG) of<br>Kylian Mbappé's shots for <b style='color:#406580'>PSG</b> and <b style='color:#E6AC00'>Real Madrid</b><br>in league play since 2017",
       caption = "<i>Penalties excluded; averages calculated on a 120-shot rolling basis</i><br><br>Data: FBref | Chart: Plot the Ball")
labelled_line
