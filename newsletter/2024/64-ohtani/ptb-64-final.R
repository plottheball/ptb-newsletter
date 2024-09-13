library(tidyverse)
library(janitor)
library(googlesheets4)
library(ggtext)
library(scales)

### season-level data imported from Baseball Reference via Google Sheets:
baserunning_2024 <- read_sheet(ss = "https://docs.google.com/spreadsheets/d/1sqmpLBBINieUo6Lqy6E4m0eW1L-SwaDB6eBQh51kuSA/edit?usp=sharing",
                               sheet = "baserunning_2024") %>%
  clean_names() %>%
  mutate(season = 2024,
         .after = "rk")
baserunning_2023 <- read_sheet(ss = "https://docs.google.com/spreadsheets/d/1sqmpLBBINieUo6Lqy6E4m0eW1L-SwaDB6eBQh51kuSA/edit?usp=sharing",
                               sheet = "baserunning_2023") %>%
  clean_names() %>%
  mutate(season = 2023,
         .after = "rk")
batting_2024 <- read_sheet(ss = "https://docs.google.com/spreadsheets/d/1sqmpLBBINieUo6Lqy6E4m0eW1L-SwaDB6eBQh51kuSA/edit?usp=sharing",
                               sheet = "batting_2024") %>%
  clean_names() %>%
  mutate(season = 2024,
         .after = "rk")
batting_2023 <- read_sheet(ss = "https://docs.google.com/spreadsheets/d/1sqmpLBBINieUo6Lqy6E4m0eW1L-SwaDB6eBQh51kuSA/edit?usp=sharing",
                               sheet = "batting_2023") %>%
  clean_names() %>%
  mutate(season = 2023,
         .after = "rk")

baserunning_tidy <- bind_rows(baserunning_2023, baserunning_2024) %>%
  select(rk, season, name, pa, tm, sbo, sb, cs) %>%
  filter(!is.na(rk))

batting_tidy <- bind_rows(batting_2023, batting_2024) %>%
  select(rk, season, player, team, pa, ab, hr) %>%
  mutate(team = str_replace_all(team, "2TM", "TOT"),
         player = str_replace_all(player, "Kiké Hernández", "Enrique Hernández")) %>%
  filter(!is.na(rk))

all_tidy <- full_join(x = baserunning_tidy, y = batting_tidy, by = join_by("name" == "player", "tm" == "team", "season" == "season")) %>%
  filter(!is.na(rk.x) & !is.na(rk.y)) %>%
  clean_names() %>%
  select(-rk_x, -rk_y, -pa_x) %>%
  mutate(sb_rate = sb / sbo * 100,
         sba_rate = (sb + cs) / sbo * 100,
         hr_rate = hr / pa_y * 100)

rough_scatter <- ggplot(all_tidy, aes(y = sb_rate, x = hr_rate)) +
  geom_point(size = 4.5,
             alpha = case_when(all_tidy$name == "Shohei Ohtani*" & all_tidy$season == 2023 ~ 0.8,
                               all_tidy$name == "Shohei Ohtani*" & all_tidy$season == 2024 ~ 1,
                               all_tidy$name == "Elly De La Cruz#" & all_tidy$season == 2024 ~ 0.25,
                               TRUE ~ 0.25),
             colour = case_when(all_tidy$name == "Shohei Ohtani*" & all_tidy$season == 2023 ~ "#BA0021",
                               all_tidy$name == "Shohei Ohtani*" & all_tidy$season == 2024 ~ "#005A9C",
                               all_tidy$name == "Elly De La Cruz#" & all_tidy$season == 2024 ~ "#000000",
                               TRUE ~ "#B8C0CC")) +
  scale_x_continuous(limits = c(0, 11),
                     breaks = seq(0, 10, 5),
                     expand = c(0, 0),
                     label = percent_format(scale = 1)) +
  scale_y_continuous(limits = c(0, 47),
                     breaks = seq(0, 40, 10),
                     expand = c(0, 0),
                     label = percent_format(scale = 1))
  theme(legend.position = "none")
rough_scatter

themed_scatter <- rough_scatter +
  theme_ptb() +
  theme(axis.title = element_blank(),
        axis.ticks = element_blank()) +
  annotate(geom = "text",
           x = 11,
           y = 7.5,
           label = "Home runs as %\nof plate apps.",
           family = ptb_font,
           colour = ptb_dark_grey,
           fontface = "bold",
           hjust = 1,
           vjust = 1) +
  annotate(geom = "text",
           x = 0.25,
           y = 40,
           label = "Stolen bases as %\nof SB opportunities",
           family = ptb_font,
           colour = ptb_dark_grey,
           fontface = "bold",
           hjust = 0,
           vjust = 0.5)
themed_scatter

labelled_scatter <- themed_scatter +
  labs(title = "Ohtani has run much more often in<br><b style='color:#005A9C'>2024</b> than he did in <b style='color:#BA0021'>2023</b>",
       subtitle = "<b>Stolen base %</b> and <b>home run %</b> recorded by<br>batters during the <b>2023</b> and <b>2024</b> MLB seasons",
       caption = "<i>Data as at 13 Sep 2024; batters with <502 PAs excluded</i><br><br>Data: Baseball Reference | Chart: Plot the Ball")
labelled_scatter

### game logs imported from Baseball Reference via Google Sheets:
ohtani_2024 <- read_sheet(ss = "https://docs.google.com/spreadsheets/d/1sqmpLBBINieUo6Lqy6E4m0eW1L-SwaDB6eBQh51kuSA/edit?usp=sharing",
                               sheet = "ohtani_2024") %>%
  clean_names() %>%
  mutate(sb_att = sb + cs,
         category = case_when(rk < 98 ~ "Pre-All-Star" ,
                              TRUE ~ "Post-All-Star"))

ohtani_attempts <- ohtani_2024 %>%
  select(rk, sb_att, category) %>%
  mutate(y_coord = -ceiling(rk / 10),
         x_coord = rk - (-y_coord * 10) + 10)

rough_waffle <- ggplot(ohtani_attempts, aes(x = x_coord, y = y_coord)) +
  geom_point(shape = 15,
             size = 5,
             colour = case_when(ohtani_attempts$sb_att == 0 ~ "#E6E6E6",
                                ohtani_attempts$sb_att == 1 ~ "#91B5CF",
                                ohtani_attempts$sb_att == 2 ~ "#3F84B5",
                                ohtani_attempts$sb_att == 3 ~ "#005A9C")) +
  geom_text(aes(label = sb_att),
            colour = case_when(ohtani_attempts$sb_att == 0 ~ "#E6E6E6",
                               ohtani_attempts$sb_att == 1 ~ "#005A9C",
                               ohtani_attempts$sb_att == 2 ~ "#CFDCE6",
                               ohtani_attempts$sb_att == 3 ~ "#E6EDF2"),
            size = 3,
            fontface = "bold",
            family = ptb_font)
rough_waffle

themed_waffle <- rough_waffle +
  theme_ptb() +
  theme(legend.position = "none",
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_blank(),
        axis.line = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  scale_x_continuous(limits = c(-2, 16),
                     expand = c(0, 0)) +
  scale_y_continuous(limits = c(-16, 0),
                     expand = c(0, 0))
themed_waffle

labelled_waffle <- themed_waffle +
  labs(title = "Ohtani has run much more often in<br>the second half of the 2024 season",
       subtitle = "Number of <b>stolen bases attempted</b> by <b>Shohei<br>Ohtani</b> in each of his first 143 games of <b>2024</b>",
       caption = "<i>Data as at 13 Sep 2024</i><br><br>Data: Baseball Reference | Chart: Plot the Ball")
labelled_waffle
