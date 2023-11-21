library(tidyverse)
library(readxl)
library(janitor)
library(ggbeeswarm)
library(ggtext)
library(scales)

### DATA DOWNLOADED FROM: https://cleaningtheglass.com/stats/players?season=2023
players_overview <- read_csv("players_offensive_overview_21_11_2023.csv") %>%
  clean_names()
players_shooting <- read_csv("players_shooting_frequency_21_11_2023.csv") %>%
  clean_names()

key_data_overview <- players_overview %>%
  select(1, 2, 3, 4, 5, 7, 9, 11)

key_data_overview$usage <- str_remove_all(key_data_overview$usage, "%")
key_data_overview$usage <- as.numeric(key_data_overview$usage)

key_data_overview$ast_percent <- str_remove_all(key_data_overview$ast_percent, "%")
key_data_overview$ast_percent <- as.numeric(key_data_overview$ast_percent)

filtered_data <- key_data_overview %>%
  filter(min >= 300) %>%
  mutate(pos = fct_relevel(pos,
                           "Point", "Combo", "Wing", "Forward", "Big"),
         custom_color = if_else(player == "Zion Williamson", "Focus", "Other"),
         custom_alpha = if_else(player == "Zion Williamson", 1, 0.4))

rough_beeswarm <- ggplot(filtered_data, aes(usage, pos, alpha = custom_alpha)) +
  geom_quasirandom(varwidth = TRUE,
                   size = 3.5,
                   color = "black",
                   fill = "#B39359",
                   shape = "circle filled") +
  theme(legend.position = "none",
        axis.line.x = element_blank(),
        panel.grid.major.y = element_blank()) +
  scale_x_continuous(name = NULL,
                     limits = c(0, 47),
                     breaks = seq(0, 40, 10),
                     expand = c(0,0),
                     labels = percent_format(accuracy = 1, scale = 1))
rough_beeswarm

themed_beeswarm <- rough_beeswarm +
  theme_ptb()
themed_beeswarm

labelled_beeswarm <- themed_beeswarm +
  labs(title = "<span style='color:#B39359;'>Zion</span> is a ball-dominant big",
       subtitle = "Distribution of <b>usage percentages</b> of players<br>at each position during the 2023-24 NBA season",
       caption = "<i>Players with <300 minutes played excluded</i><br><br>Data: Cleaning the Glass | Chart: Plot the Ball")
labelled_beeswarm

high_usage <- filtered_data %>%
  filter(usage >= 30)

key_data_shooting <- players_shooting %>%
  select(1, 2, 3, 4, 5, 7, 21)

key_data_shooting$e_fg_percent <- str_remove_all(key_data_shooting$e_fg_percent, "%")
key_data_shooting$e_fg_percent <- as.numeric(key_data_shooting$e_fg_percent)

key_data_shooting$all_three <- str_remove_all(key_data_shooting$all_three, "%")
key_data_shooting$all_three <- as.numeric(key_data_shooting$all_three)

filtered_shooting <- key_data_shooting %>%
  filter(min > 300) %>%
  mutate(pos = fct_relevel(pos,
                           "Point", "Combo", "Wing", "Forward", "Big"))

high_usage_all <- left_join(x = high_usage, y = filtered_shooting, by = c("player", "age", "team", "pos", "min"))

rough_beeswarm_high_usage <- ggplot(high_usage_all, aes(e_fg_percent, pos, alpha = custom_alpha)) +
  geom_quasirandom(varwidth = TRUE,
                   size = 4.5,
                   color = "black",
                   fill = "#B39359",
                   shape = "circle filled") +
  theme(legend.position = "none",
        axis.line.x = element_blank(),
        axis.line.y = element_blank(),
        panel.grid.major.y = element_blank()) +
  scale_x_continuous(name = NULL,
                     limits = c(27, 74),
                     breaks = seq(30, 70, 10),
                     expand = c(0,0),
                     labels = percent_format(accuracy = 1, scale = 1))
rough_beeswarm_high_usage

themed_beeswarm_high_usage <- rough_beeswarm_high_usage +
  theme_ptb()
themed_beeswarm_high_usage

labelled_beeswarm_high_usage <- themed_beeswarm_high_usage +
  labs(title = "<span style='color:#B39359;'>Zion</span> hasn't scored efficiently this year",
       subtitle = "Distribution of <b>effective field goal percentages</b><br>of <b>high-usage</b> NBA players in 2023-24",
       caption = "<i>Players with <300 minutes played or <30% usage excluded</i><br><br>Data: Cleaning the Glass | Chart: Plot the Ball")
labelled_beeswarm_high_usage
