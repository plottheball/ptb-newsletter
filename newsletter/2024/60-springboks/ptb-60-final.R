library(tidyverse)
library(googlesheets4)
library(janitor)
library(ggtext)
library(scales)

### DATA COLLATED FROM VARIOUS SOURCES AND IMPORTED FROM GOOGLE SHEETS:
performance_table <- read_sheet(ss = "https://docs.google.com/spreadsheets/d/1fr5TYljpiJYs8abVnyo-2bhBabgfn73BVLF3yO-jUZ0/",
                                sheet = "Final table") %>%
  clean_names() %>%
  mutate(category = case_when(team == "Ireland" ~ "IRE",
                              team == "South Africa" ~ "RSA",
                              TRUE ~ "Other"))

palette_category <- c("#166642", "#CCCCCC", "#E6B52E")

rough_scatter <- ggplot(performance_table, aes(y = percent_of_points, x = win_percent, colour = category)) +
  geom_point(size = 5.5, alpha = case_when(performance_table$team == "Ireland " ~ 1,
                                           performance_table$team == "South Africa" ~ 1,
                                           TRUE ~ 0.5)) +
  scale_x_continuous(limits = c(0, 0.9),
                     breaks = seq(0, 0.75, 0.25),
                     expand = c(0, 0),
                     label = percent_format(scale = 100)) +
  scale_y_continuous(limits = c(0, 0.775),
                     breaks = seq(0, 0.75, 0.25),
                     expand = c(0, 0),
                     label = percent_format(scale = 100)) +
  scale_colour_manual(values = palette_category, aesthetics = "colour") +
  theme(legend.position = "none")
rough_scatter

themed_scatter <- rough_scatter +
  theme_ptb() +
  theme(axis.title = element_blank(),
        axis.ticks = element_blank()) +
  annotate(geom = "text",
           x = 0.875,
           y = 0.03,
           label = "Win %",
           family = ptb_font,
           colour = ptb_dark_grey,
           fontface = "bold",
           hjust = 1,
           vjust = 0) +
  annotate(geom = "text",
           x = 0.03,
           y = 0.725,
           label = "% of\npoints",
           family = ptb_font,
           colour = ptb_dark_grey,
           fontface = "bold",
           hjust = 0,
           vjust = 1)
themed_scatter

labelled_scatter <- themed_scatter +
  labs(title = "<b style='color:#166642'>Ireland</b> were the best team of the last<br>cycle — by both win % and point share",
       subtitle = "<b>Win %</b> of men's international rugby teams vs. Tier<br>1 opponents between 2020 and 2023, compared to<br>the <b>share of points</b> they scored in those fixtures",
       caption = "<i>'Tier 1 opponents' includes the British & Irish Lions</i><br><br>Data: Plot the Ball | Chart: Plot the Ball")
labelled_scatter

### DATA DOWNLOADED FROM ALL.RUGBY AND IMPORTED FROM GOOGLE SHEETS:
starters_table <- read_sheet(ss = "https://docs.google.com/spreadsheets/d/1fr5TYljpiJYs8abVnyo-2bhBabgfn73BVLF3yO-jUZ0/",
                                sheet = "Starters - Saturday") %>%
  clean_names() %>%
  select(1, 2, 3, 6) %>%
  mutate(share_final = if_else(team == "Ireland", share * 100, share * -100))

rough_bars <- ggplot(starters_table, aes(x = position, y = share_final, fill = team)) +
  geom_bar(stat = "identity",
           width = 0.35) +
  scale_fill_manual(values = c("#166642", "#E6B52E")) +
  geom_hline(yintercept = 0) +
  scale_y_continuous(limits = c(-110, 110),
                     breaks = seq(-100, 100, 50),
                     labels = (abs(seq(-100, 100, 50))),
                     expand = c(0, 0)) +
  scale_x_continuous(limits = c(0, 16),
                     breaks = seq(1, 15, 1),
                     expand = c(0, 0)) +
  theme(legend.position = "none",
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.ticks.y = element_blank(),
        panel.grid.major.y = element_blank(),
        axis.line.x = element_blank(),
        axis.line.y = element_blank()) +
  coord_flip()
rough_bars

themed_bars <- rough_bars +
  theme_ptb()
themed_bars

labelled_bars <- themed_bars +
  labs(title = "<b style='color:#E6B52E'>South Africa</b> still look a lot like their<br>World-Cup-winning side from last year",
       subtitle = "% of team's games vs. Tier 1 opponents between<br>2020 and 2023 started by the players selected for<br>the first test between <b style='color:#E6B52E'>South Africa</b> and <b style='color:#166642'>Ireland</b>",
       caption = "Data: All.Rugby | Chart: Plot the Ball")
labelled_bars
