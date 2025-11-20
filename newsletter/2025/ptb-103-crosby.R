library(tidyverse)
library(janitor)
library(googlesheets4)
library(scales)
library(ggtext)

# data downloaded from: https://x.com/domluszczyszyn/status/1933908119504982043?s=20
all_ratings <- read_sheet(ss = "https://docs.google.com/spreadsheets/d/1mQHz3L2UqZKl0-6Mv2Fg7n14Mr6FCE4MJluKD8qm8bc/edit?gid=678507251#gid=678507251") %>%
  clean_names()

clean_ratings <- all_ratings %>%
  select(x1, player, team, position, gp, toi, off_rtg, def_rtg, net_rtg)

crosby_clean <- clean_ratings %>%
  filter(player == "Sidney Crosby") %>%
  mutate(net_per_82 = net_rtg / gp * 82)

x24_25_clean <- clean_ratings %>%
  filter(x1 == "2024-25") %>%
  mutate(net_per_82 = net_rtg / gp * 82)

player_summary <- clean_ratings %>%
  group_by(player) %>%
  summarise(tot_gp = sum(gp),
            off_rating = sum(off_rtg),
            def_rating = sum(def_rtg),
            net_total = sum(net_rtg)) %>%
  arrange(desc(tot_gp)) %>%
  filter(tot_gp >= (400)) %>%
  mutate(net_per_82 = net_total / tot_gp * 82)

pit_colour <- "#CC3D47"
edm_colour <- "#CCA3A6"
background_colour <- "#C9C9C9"

rough_scatter <- ggplot(player_summary, aes(y = net_total, x = net_per_82)) +
  geom_hline(yintercept = 0,
             color = ptb_dark_grey,
             alpha = 0.9) +
  geom_vline(xintercept = 0,
             color = ptb_dark_grey,
             alpha = 0.9) +
  geom_point(size = 4,
             color = background_colour,
             alpha = 0.15) +
  geom_point(data = (player_summary %>% filter(player == "Sidney Crosby")),
             shape = 21,
             fill = pit_colour,
             color = ptb_dark_grey,
             alpha = 1,
             size = 4,
             stroke = 1) +
  geom_point(data = (player_summary %>% filter(player == "Connor McDavid")),
             shape = 21,
             fill = edm_colour,
             color = ptb_dark_grey,
             alpha = 1,
             size = 4,
             stroke = 1) +
  scale_y_continuous(name = NULL,
                     limits = c(-190, 390),
                     breaks = seq(-100, 300, 100),
                     labels = label_number(style_positive = "plus"),
                     expand = c(0,0)) +
  scale_x_continuous(name = NULL,
                     limits = c(-19, 39),
                     breaks = seq(-10, 30, 10),
                     labels = label_number(style_positive = "plus"),
                     expand = c(0,0)) +
  theme(legend.position = "none",
        axis.line.x = element_blank(),
        axis.line.y = element_blank(),
        axis.ticks = element_blank()) +
  annotate(geom = "text",
           x = -17,
           y = 370,
           label = "Aggregate\nnet rating",
           family = ptb_font,
           colour = ptb_dark_grey,
           fontface = "bold",
           hjust = 0,
           vjust = 1) +
  annotate(geom = "text",
           x = 37,
           y = -100,
           label = "Net rating\nper 82 games",
           family = ptb_font,
           colour = ptb_dark_grey,
           fontface = "bold",
           hjust = 1,
           vjust = 1)
rough_scatter

themed_scatter <- rough_scatter +
  theme_ptb()
themed_scatter

labelled_scatter <- themed_scatter +
  labs(title = "<b style='color:#CC3D47'>Crosby</b> has had more impact than<br>any other skater in the modern era",
       subtitle = "<b>On-ice impact</b> of NHL skaters between 2007 and<br>2025, measured in <b>net goals above average</b>",
       caption = "<i>Players with <400 games between 2007-08 and 2024-25 excluded</i><br><br>Data: Dom Luszczyszyn/The Athletic | Chart: Plot the Ball")
labelled_scatter