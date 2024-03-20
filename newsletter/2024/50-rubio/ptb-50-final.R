library(tidyverse)
library(janitor)
library(readxl)
library(ggbeeswarm)
library(ggtext)
library(lemon)
library(scales)

### EuroLeague season data downloaded from: https://www.basketball-reference.com/international/euroleague/
### NBA season data downloaded from: https://www.basketball-reference.com/leagues/
### NBA player data downloaded from: https://cleaningtheglass.com/stats/players

rubio_seasons <- seq(2012, 2023, 1)

rubio_all <- tibble()

for (i in rubio_seasons) {
  
  rubio_data <- read.csv(str_c("players_offensive_overview_", i, ".csv")) %>% 
    clean_names()
  
  rubio_data$year <- i

  rubio_all <- bind_rows(rubio_all, rubio_data)
  
}

rubio_all$usage <- str_remove_all(rubio_all$usage, "%")
rubio_all$usage <- as.numeric(rubio_all$usage)
rubio_all$year <- as.character(rubio_all$year)

rubio_clean <- rubio_all %>%
  select(1, 16, 3, 4, 5, 9, 7) %>%
  filter(min > 500 & usage > 15 & (pos == "Point" | pos == "Combo")) %>%
  mutate(year = fct_relevel(year,
                            "2023", "2022", "2021", "2020", "2019", "2018", "2017", "2016", "2015", "2014", "2013"),
         custom_color = if_else(player == "Ricky Rubio", "Focus", "Other"),
         custom_alpha = if_else(player == "Ricky Rubio", 1, 0.2))

min_blue <- "#2B78B3"

rough_beeswarm <- ggplot(rubio_clean, aes(psa, year, alpha = custom_alpha)) +
  geom_quasirandom(varwidth = FALSE,
                   width = 0.25,
                   size = 3,
                   color = "black",
                   fill = min_blue,
                   shape = "circle filled") +
  theme(legend.position = "none",
        axis.line.x = element_blank(),
        panel.grid.major.y = element_blank()) +
  scale_x_continuous(name = NULL,
                     limits = c(65, 145),
                     breaks = seq(80, 140, 20),
                     expand = c(0,0))
rough_beeswarm

themed_beeswarm <- rough_beeswarm +
  theme_ptb() +
  theme(axis.line.y = element_blank(),
        axis.ticks = element_blank())
themed_beeswarm

labelled_beeswarm <- themed_beeswarm +
  labs(title = "<span style='color:#2B78B3;'>Ricky Rubio</span> couldn't consistently score<br>at an efficient rate in the NBA",
       subtitle = "<b>Points scored per 100 Shot Attempts</b> by <b>guards</b><br>in each NBA season since 2011-12",
       caption = "<i>Players with <500 minutes played or <15% usage excluded</i><br><br>Data: Cleaning the Glass | Chart: Plot the Ball")
labelled_beeswarm

league_seasons <- seq(2011, 2024, 1)

euro_all <- tibble()

for (i in league_seasons) {
  
  euro_data <- read.csv(str_c("euro_", i, ".csv")) %>% 
    clean_names()
  
  euro_data$year <- i
  
  euro_all <- bind_rows(euro_all, euro_data)
  
}

euro_clean <- euro_all %>%
  group_by(year) %>%
  summarise(tot_fga = sum(fga),
            tot_3pa = sum(x3pa)) %>%
  mutate(share_3p = tot_3pa / tot_fga * 100)

euro_clean$league <- "EuroLeague"

nba_all <- tibble()

for (i in league_seasons) {
  
  nba_data <- read.csv(str_c("nba_", i, ".csv")) %>% 
    clean_names()
  
  nba_data$year <- i
  
  nba_all <- bind_rows(nba_all, nba_data)
  
}

nba_clean <- nba_all %>%
  group_by(year) %>%
  summarise(tot_fga = sum(fga),
            tot_3pa = sum(x3pa)) %>%
  mutate(share_3p = tot_3pa / tot_fga * 100)

nba_clean$league <- "NBA"

league_comp <- bind_rows(nba_clean, euro_clean) %>%
  select(year, league, share_3p)

first_last <- league_comp %>%
  filter(year == 2011 | year == 2024)

facet_labels <- c("EuroLeague" = "<span style='color:#fa5500'>EuroLeague</b> (three-point line: 6.75m)</span>",
                  "NBA" = "<span style='color:#041a4d'>NBA</b> (three-point line: 7.24m)</span>")

rough_area <- ggplot(league_comp, aes(x = year, y = share_3p)) +
  geom_path(linewidth = 1.5, aes(colour = league)) +
  geom_ribbon(aes(ymin = 0, ymax = share_3p, fill = league)) +
  geom_point(size = 2.5,
             data = first_last,
             aes(colour = league)) +
  scale_y_continuous(limits = c(0, 69),
                     breaks = seq(0, 60, 20),
                     expand = c(0,0),
                     labels = percent_format(scale = 1)) +
  scale_x_continuous(limits = c(2008, 2027),
                     breaks = seq(2011, 2024, 4),
                     labels = c("2010-11", "'14-15", "'18-19", "'22-23"),
                     expand = c(0,0)) +
  scale_colour_manual(values = c("EuroLeague" = "#fa5500",
                                 "NBA" = "#041a4d")) +
  scale_fill_manual(values = c("EuroLeague" = "#fa550050",
                                 "NBA" = "#041a4d50")) +
  facet_rep_wrap(~league,
                 nrow = 2,
                 repeat.tick.labels =  TRUE,
                 labeller = labeller(league = facet_labels)) +
  theme(legend.position = "none",
        axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        axis.line.y = element_blank(),
        axis.ticks.y = element_blank())
rough_area

themed_area <- rough_area +
  theme_ptb()
themed_area

labelled_area <- themed_area +
  labs(title = "A similar share of <b style='color:#fa5500'>EuroLeague</b> shots<br>are now 3s — despite the shorter line",
       subtitle = "% of field-goal attempts which were <b>three-point<br>attempts</b> in <b style='color:#fa5500'>EuroLeague</b> and the <b style='color:#041a4d'>NBA</b> since <b>2010</b>",
       caption = "<i>Data as at 16 Mar 2024</i><br><br>Data: Basketball Reference | Chart: Plot the Ball")
labelled_area
