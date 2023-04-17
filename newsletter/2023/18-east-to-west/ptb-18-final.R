library(tidyverse)
library(janitor)
library(readxl)
library(ggtext)

### Data for all available seasons downloaded from Stats section of NHL.com: https://www.nhl.com/stats/teams?report=powerplaytime&reportType=season

pp_data_historic <- read_xlsx("pp-data-0910-2122.xlsx") %>%
  clean_names()

pp_data_current <- read_xlsx("pp-data-2223.xlsx") %>%
  clean_names()

pp_data <- rbind(pp_data_historic, pp_data_current)

pp_tidy <- pp_data %>%
  select(1, 2, 3, 5, 6, 7)

pp_tidy$season <- as.integer(substr(as.character(pp_tidy$season), 5, 8))

pp_tidy <- pp_tidy %>%
  separate(col = pp_toi, into = c("mins", "secs"), sep = ":")

pp_tidy$mins <- as.integer(pp_tidy$mins)
pp_tidy$secs <- as.integer(pp_tidy$secs)

pp_tidy <- pp_tidy %>%
  mutate(pp_toi_secs = (mins * 60) + secs,
         .before = mins) %>%
  select(-mins, -secs)

pp_tidy <- pp_tidy %>%
  mutate(opp_per_gp = pp_opp / gp,
         toi_per_opp = pp_toi_secs / pp_opp,
         gf_per_60 = pp_gf / pp_toi_secs * 60 * 60)

top_pp <- pp_tidy %>%
  arrange(-gf_per_60) %>%
  head(13) %>%
  mutate(label = str_c(team, " (", season, ")"),
         .before = team)

top_pp$label <- str_remove_all(top_pp$label, "Edmonton ")
top_pp$label <- str_remove_all(top_pp$label, "Tampa Bay ")
top_pp$label <- str_remove_all(top_pp$label, "Toronto ")
top_pp$label <- str_remove_all(top_pp$label, "St. Louis ")
top_pp$label <- str_remove_all(top_pp$label, "Washington ")
top_pp$label <- str_remove_all(top_pp$label, "Florida ")
top_pp$label <- str_remove_all(top_pp$label, "Boston ")
top_pp$label <- str_remove_all(top_pp$label, "Carolina ")
top_pp$label <- str_remove_all(top_pp$label, "Pittsburgh ")



oilers23_orange <- "#FF4C00"
oilers_orange_other <- "#E6B5A1"
nhl_blue <- "#C3D8E6"

rough_bar <- ggplot(top_pp, aes(x = fct_reorder(label, gf_per_60), y = gf_per_60, fill = label)) +
  geom_bar(stat = "identity", width = 0.6) +
  scale_fill_manual(values = c("Oilers (2023)" = oilers23_orange,
                               "Oilers (2022)" = oilers_orange_other,
                               "Oilers (2021)" = oilers_orange_other,
                               "Oilers (2020)" = oilers_orange_other,
                               "Bruins (2019)" = nhl_blue,
                               "Hurricanes (2021)" = nhl_blue,
                               "Panthers (2019)" = nhl_blue,
                               "Penguins (2018)" = nhl_blue,
                               "Blues (2022)" = nhl_blue,
                               "Lightning (2019)" = nhl_blue,
                               "Maple Leafs (2022)" = nhl_blue,
                               "Capitals (2010)" = nhl_blue,
                               "Capitals (2013)" = nhl_blue)) +
  scale_y_continuous(limits = c(0, 19),
                     breaks = seq(0, 15, 5),
                     expand = c(0,0)) +
  geom_hline(yintercept = 0,
             colour = ptb_dark_grey) +
  coord_flip() +
  theme(legend.position = "none")
rough_bar

themed_bar <- rough_bar +
  theme_ptb() +
  theme(axis.line.y = element_blank(),
        axis.ticks.y = element_blank(),
        panel.grid.major.y = element_blank())
themed_bar

labelled_bar <- themed_bar +
  labs(y = " Goals scored per 60 PP mins", 
       title = "The <b style='color:#FF4C00'>2023 Oilers</b> have the most efficient<br>power play in the NHL's history",
       subtitle = "NHL teams with the most <b>goals scored per 60 minutes<br> on the power play</b> in a single season since 2010",
       caption = "Data: NHL.com | Chart: Plot the Ball")
labelled_bar
