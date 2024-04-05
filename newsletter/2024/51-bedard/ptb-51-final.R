library(tidyverse)
library(janitor)
library(readxl)
library(rvest)
library(ggtext)
library(lubridate)
library(scales)

### Data for all available seasons downloaded from Stats section of NHL.com: https://www.nhl.com/stats/skaters
rookies_1 <- read_xlsx("rookies-1.xlsx") %>%
  clean_names()
rookies_2 <- read_xlsx("rookies-2.xlsx") %>%
  clean_names()
rookies_3 <- read_xlsx("rookies-3.xlsx") %>%
  clean_names()
rookies_4 <- read_xlsx("rookies-4.xlsx") %>%
  clean_names()
rookies_5 <- read_xlsx("rookies-5.xlsx") %>%
  clean_names()
rookies_6 <- read_xlsx("rookies-6.xlsx") %>%
  clean_names()
rookies_7 <- read_xlsx("rookies-7.xlsx") %>%
  clean_names()
rookies_8 <- read_xlsx("rookies-8.xlsx") %>%
  clean_names()
rookies_9 <- read_xlsx("rookies-9.xlsx") %>%
  clean_names()
rookies_10 <- read_xlsx("rookies-10.xlsx") %>%
  clean_names()

all_rookies <- rbind(rookies_1, rookies_2, rookies_3, rookies_4, rookies_5, rookies_6, rookies_7, rookies_8, rookies_9, rookies_10) %>%
  clean_names()

all_rookies <- all_rookies %>%
  filter(gp >= 41) %>%
  select(1:9, 23)

all_rookies$toi_gp <- ms(all_rookies$toi_gp)

bio_1 <- read_xlsx("bio-1.xlsx") %>%
  clean_names()
bio_2 <- read_xlsx("bio-2.xlsx") %>%
  clean_names()
bio_3 <- read_xlsx("bio-3.xlsx") %>%
  clean_names()
bio_4 <- read_xlsx("bio-4.xlsx") %>%
  clean_names()
bio_5 <- read_xlsx("bio-5.xlsx") %>%
  clean_names()
bio_6 <- read_xlsx("bio-6.xlsx") %>%
  clean_names()
bio_7 <- read_xlsx("bio-7.xlsx") %>%
  clean_names()
bio_8 <- read_xlsx("bio-8.xlsx") %>%
  clean_names()
bio_9 <- read_xlsx("bio-9.xlsx") %>%
  clean_names()
bio_10 <- read_xlsx("bio-10.xlsx") %>%
  clean_names()
bio_11 <- read_xlsx("bio-11.xlsx") %>%
  clean_names()

all_bio <- rbind(bio_1, bio_2, bio_3, bio_4, bio_5, bio_6, bio_7, bio_8, bio_9, bio_10, bio_11) %>%
  clean_names()

all_bio <- all_bio %>%
  select(1, 5, 15)

all_rookies <- left_join(all_rookies, all_bio, by = "player") %>%
  clean_names()

all_rookies$season <- as.integer(substr(as.character(all_rookies$season), 5, 8))
all_rookies$x1st_season <- as.integer(substr(as.character(all_rookies$x1st_season), 5, 8))
all_rookies$season_check <- all_rookies$season - all_rookies$x1st_season
all_rookies$dob <- ymd(all_rookies$dob)

all_rookies <- all_rookies %>%
  filter(season_check < 2) %>%
  mutate(season_date = ymd(str_c(season, "01-31")),
         .before = season)

all_rookies$rookie_age <- interval(all_rookies$dob, all_rookies$season_date) / years(1)

rookies_u20 <- all_rookies %>%
  filter(rookie_age < 20) %>%
  select(-13, -14) %>%
  mutate(time_share = toi_gp /  ms("60:00"),
         ppg = p / gp,
         points_p60 = p / gp / time_share,
         .after = p)

rookies_final <- rookies_u20 %>%
  select(1, 11, 12, 13, 14) %>%
  mutate(time_for_plot = minute(toi_gp) + (second(toi_gp) / 60))

bedard_focus <- "#CF0A2C"
background_colour <- "#C3D8E6"

rookies_scatter <- ggplot(rookies_final, aes(x = time_for_plot, y = points_p60)) +
  geom_point(color = case_when(rookies_final$player == "Connor Bedard" ~ bedard_focus,
                               TRUE ~ background_colour),
             alpha = case_when(rookies_final$player == "Connor Bedard" ~ 1,
                               TRUE ~ 0.4),
             size = 4) +
  scale_y_continuous(name = NULL,
                     limits = c(0, 4.4),
                     breaks = seq(0, 4, 1),
                     expand = c(0,0)) +
  scale_x_continuous(name = NULL,
                     limits = c(0, 24),
                     breaks = seq(0, 20, 5),
                     expand = c(0,0)) +
  theme(legend.position = "none") +
  annotate(geom = "text",
           x = 0.4,
           y = 4.25,
           label = "Points per\n60 mins",
           family = ptb_font,
           colour = ptb_dark_grey,
           fontface = "bold",
           hjust = 0,
           vjust = 1) +
  annotate(geom = "text",
           x = 23,
           y = 0.65,
           label = "Minutes\nper game",
           family = ptb_font,
           colour = ptb_dark_grey,
           fontface = "bold",
           hjust = 1,
           vjust = 1)
rookies_scatter

scatter_themed <- rookies_scatter +
  theme_ptb()
scatter_themed

scatter_labelled <- scatter_themed +
  labs(title = "<b style='color:#CF0A2C'>Bedard</b> has put up points like a great<br>rookie — but not an exceptional one",
       subtitle = "<b>Points scored</b> and <b>minutes played</b> by 18- and<br>19-year-old rookies in the NHL since 2000-01",
       caption = "<i>Data as at 4 Apr 2024; players with <41 games excluded</i><br><br>Data: NHL.com | Chart: Plot the Ball")
scatter_labelled

microstats_raw <- read_xlsx("season-totals-sheet.xlsx") %>%
  clean_names()

forwards_current <- microstats_raw %>%
  filter(year == "2023-24" & pos != "G" & pos != "D")

forwards_summary_5v5 <- forwards_current %>%
  group_by(number, player, team) %>%
  summarise(no_games = n(),
            tot_mins = sum(x5v5_toi),
            tot_zone_entries = sum(zone_entries_29),
            tot_carries = sum(carries_30),
            tot_exits_w_poss = sum(exits_w_possession),
            tot_carried_exits = sum(carried_exits))

forwards_filtered_5v5 <- forwards_summary_5v5 %>%
  filter(tot_mins >= 300)

forwards_final <- forwards_filtered_5v5 %>%
  mutate(oz_controlled_entries_per_60 = tot_carries / tot_mins * 60,
         dz_successful_exits_per_60 = tot_exits_w_poss / tot_mins * 60)

background_colour_ii <- "#2c2c2c"

progression_scatter <- ggplot(forwards_final, aes(x = dz_successful_exits_per_60, y = oz_controlled_entries_per_60)) +
  geom_point(color = case_when(forwards_final$number == 98 ~ bedard_focus,
                               TRUE ~ background_colour_ii),
             alpha = case_when(forwards_final$number == 98 ~ 1,
                               TRUE ~ 0.2),
             size = 4) +
  scale_y_continuous(name = NULL,
                     limits = c(2, 23),
                     breaks = seq(0, 20, 5),
                     expand = c(0,0)) +
  scale_x_continuous(name = NULL,
                     limits = c(2, 16),
                     breaks = seq(0, 15, 5),
                     expand = c(0,0)) +
  theme(legend.position = "none") +
  annotate(geom = "text",
           x = 2.25,
           y = 22.5,
           label = "Controlled\nOZ Entries\nper 60 mins",
           family = ptb_font,
           colour = ptb_dark_grey,
           fontface = "bold",
           hjust = 0,
           vjust = 1) +
  annotate(geom = "text",
           x = 14.75,
           y = 6.5,
           label = "Successful\nDZ Exits\nper 60 mins",
           family = ptb_font,
           colour = ptb_dark_grey,
           fontface = "bold",
           hjust = 1,
           vjust = 1)
progression_scatter

progression_themed <- progression_scatter +
  theme_ptb() +
  theme(axis.line = element_blank())
progression_themed

progression_labelled <- progression_themed +
  labs(title = "<b style='color:#CF0A2C'>Bedard</b>'s ability to progress the puck <br>up the ice already stands out",
       subtitle = "NHL forwards' contribution to <b>puck progression</b><br><b>at 5v5</b> in a sample of minutes during 2023-24",
       caption = "<i>Data as at 4 Apr 2024; players with <300 tracked minutes excluded</i><br><br>Data: All Three Zones | Chart: Plot the Ball")
progression_labelled
