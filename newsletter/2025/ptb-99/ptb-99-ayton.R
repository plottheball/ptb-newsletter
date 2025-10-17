library(tidyverse)
library(janitor)
library(readxl)
library(ggbeeswarm)
library(ggtext)
library(lemon)
library(scales)

### NBA player data downloaded from: https://cleaningtheglass.com/stats/players

ayton_seasons <- seq(2019, 2025, 1)

ayton_all <- tibble()

for (i in ayton_seasons) {
  
  ayton_data <- read.csv(str_c("players_offensive_overview_", i, ".csv")) %>% 
    clean_names()
  
  ayton_data$year <- i
  
  ayton_all <- bind_rows(ayton_all, ayton_data)
  
}

ayton_all$usage <- str_remove_all(ayton_all$usage, "%")
ayton_all$usage <- as.numeric(ayton_all$usage)
ayton_all$year <- as.character(ayton_all$year)

for_plot <- ayton_all %>%
  select(1, 16, 3, 4, 5, 9, 7) %>%
  filter(min > 500 & usage > 15 & (pos == "Big")) %>%
  mutate(year = fct_relevel(year,
                            "2025", "2024", "2023", "2022", "2021", "2020", "2019"))

ayton_phx <- for_plot %>%
  filter(player == "Deandre Ayton" & team == "PHX")

ayton_por <- for_plot %>%
  filter(player == "Deandre Ayton" & team == "POR")

background_grey <- "#bcbcbc"
phx_colour <- "#948FB3"
por_colour <- "#CC3D40"

rough_beeswarm <- ggplot(for_plot, aes(psa, year)) +
  geom_quasirandom(varwidth = FALSE,
                   width = 0.25,
                   size = 4,
                   color = "white",
                   fill = background_grey,
                   shape = "circle filled",
                   alpha = 0.25) +
  theme(legend.position = "none",
        axis.line.x = element_blank(),
        axis.line.y = element_blank(),
        panel.grid.major.y = element_blank(),
        axis.ticks = element_blank()) +
  scale_x_continuous(name = NULL,
                     limits = c(71, 159),
                     breaks = seq(80, 150, 10),
                     expand = c(0,0)) +
  geom_quasirandom(data = ayton_phx,
                   varwidth = FALSE,
                   width = 0.25,
                   size = 4,
                   color = "black",
                   fill = phx_colour,
                   shape = "circle filled",
                   alpha = 1) +
  geom_quasirandom(data = ayton_por,
                   varwidth = FALSE,
                   width = 0.25,
                   size = 4,
                   color = "black",
                   fill = por_colour,
                   shape = "circle filled",
                   alpha = 1)
rough_beeswarm

themed_beeswarm <- rough_beeswarm +
  theme_ptb()
themed_beeswarm

labelled_beeswarm <- themed_beeswarm +
  labs(title = "Deandre Ayton was less efficient than<br>the average NBA 'big' in <b style='color:#CC3D40'>Portland</b>",
       subtitle = "<b>Points per 100 shot attempts</b> scored by <b>Deandre<br>Ayton</b> in each NBA season since 2018-19",
       caption = "<i>Players with <500 minutes played or <15% usage excluded</i><br><br>Data: Cleaning the Glass | Chart: Plot the Ball")
labelled_beeswarm
