library(tidyverse)
library(readxl)
library(janitor)
library(ggbeeswarm)
library(ggtext)
library(scales)

### DATA DOWNLOADED FROM: https://cleaningtheglass.com/stats/players?season=2022
players_overview <- read_csv("players_offensive_overview_23_10_2023.csv") %>%
  clean_names()

key_data_overview <- players_overview %>%
  select(1, 2, 3, 4, 5, 11)

key_data_overview$ast_percent <- str_remove_all(key_data_overview$ast_percent, "%")
key_data_overview$ast_percent <- as.numeric(key_data_overview$ast_percent)

filtered_data <- key_data_overview %>%
  filter(min > 1000) %>%
  mutate(pos = fct_relevel(pos,
                           "Point", "Combo", "Wing", "Forward", "Big"),
         custom_color = if_else(player == "Josh Giddey", "Focus", "Other"),
         custom_alpha = if_else(player == "Josh Giddey", 1, 0.4))

rough_beeswarm <- ggplot(filtered_data, aes(ast_percent, pos, alpha = custom_alpha)) +
  geom_quasirandom(varwidth = TRUE,
                   size = 3.5,
                   color = "black",
                   fill = "#EF3B24",
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
  labs(title = "<span style='color:#EF3B24;'>Josh Giddey</span> stands out for his passing",
       subtitle = "Distribution of <b>assist percentages</b> of players<br>at each position during the 2022-23 NBA season",
       caption = "<i>Players with <1,000 minutes played excluded</i><br><br>Data: Cleaning the Glass | Chart: Plot the Ball")
labelled_beeswarm
