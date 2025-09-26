library(tidyverse)
library(readxl)
library(janitor)
library(ggbeeswarm)
library(ggtext)
library(lemon)
library(scales)

### data imported from https://baseballsavant.mlb.com
fb_data <- read_csv("stats.csv") %>%
  clean_names()

fb_clean <- fb_data %>%
  select(1, 2, 3, 7, 8, 9) %>%
  filter(pitch_count > 500 & !is.na(n_ff_formatted) & year > 2017) %>%
  mutate(year = as.character(year))

for_plot <- fb_clean %>%
  select(1, 3, 6) %>%
  mutate(year = fct_relevel(year,
                            "2025", "2024", "2023", "2022", "2021", "2020", "2019", "2018"))

ohtani_angels <- for_plot %>%
  filter(last_name_first_name == "Ohtani, Shohei" & year != "2025")

ohtani_dodgers <- for_plot %>%
  filter(last_name_first_name == "Ohtani, Shohei" & year == "2025")

background_grey <- "#bcbcbc"
angels_colour <- "#CC294F"
dodgers_colour <- "#0068B3"

rough_beeswarm <- ggplot(for_plot, aes(ff_avg_speed, year)) +
  geom_quasirandom(varwidth = FALSE,
                   width = 0.25,
                   size = 3.5,
                   color = "white",
                   fill = background_grey,
                   shape = "circle filled",
                   alpha = 0.1) +
  theme(legend.position = "none",
        axis.line.x = element_blank(),
        axis.line.y = element_blank(),
        panel.grid.major.y = element_blank(),
        axis.ticks = element_blank()) +
  scale_x_continuous(name = NULL,
                     limits = c(76, 109),
                     breaks = seq(80, 105, 5),
                     expand = c(0,0)) +
  geom_quasirandom(data = ohtani_angels,
                   varwidth = FALSE,
                   width = 0.25,
                   size = 3.5,
                   color = "black",
                   fill = angels_colour,
                   shape = "circle filled",
                   alpha = 1) +
  geom_quasirandom(data = ohtani_dodgers,
                   varwidth = FALSE,
                   width = 0.25,
                   size = 3.5,
                   color = "black",
                   fill = dodgers_colour,
                   shape = "circle filled",
                   alpha = 1)
rough_beeswarm

themed_beeswarm <- rough_beeswarm +
  theme_ptb()
themed_beeswarm

labelled_beeswarm <- themed_beeswarm +
  labs(title = "With the <b style='color:#0068B3'>Dodgers</b>, Ohtani is throwing<br> harder than he ever did at the <b style='color:#CC294F'>Angels</b>",
       subtitle = "Ave. speed (in mph) of <b>Shohei Ohtani</b>'s fastball<br>in each season with the <b style='color:#CC294F'>Angels</b> and the <b style='color:#0068B3'>Dodgers</b>",
       caption = "<i>Pitchers with <500 total pitches thrown in a season excluded</i><br><br>Data: MLB Statcast | Chart: Plot the Ball")
labelled_beeswarm