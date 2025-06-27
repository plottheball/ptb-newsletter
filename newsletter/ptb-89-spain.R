library(tidyverse)
library(googlesheets4)
library(janitor)
library(lemon)
library(ggview)
library(ggtext)
library(scales)

# data downloaded from Sofascore and imported via Google Sheets
top_vs_top <- read_sheet(ss = "https://docs.google.com/spreadsheets/d/1N0bgP6PcsXn2XMG3E_VCRK27CgjUdDSGXrGQNUXrEjo/edit?gid=793290875#gid=793290875",
                         sheet = "Top vs. top") %>%
  clean_names() %>%
  select(team, gd_mp) %>%
  rename("gd_top" = "gd_mp")

top_vs_bottom <- read_sheet(ss = "https://docs.google.com/spreadsheets/d/1N0bgP6PcsXn2XMG3E_VCRK27CgjUdDSGXrGQNUXrEjo/edit?gid=793290875#gid=793290875",
                         sheet = "Top vs. bottom") %>%
  clean_names() %>%
  select(team, gd_mp) %>%
  rename("gd_bottom" = "gd_mp")

combined_record <- full_join(top_vs_top, top_vs_bottom, by = "team")

rough_scatter <- ggplot(combined_record, aes(x = gd_bottom, y = gd_top, colour = team)) +
  geom_hline(yintercept = 0) +
  geom_vline(xintercept = 0) +
  geom_point(size = 7.5,
             alpha = 0.9) +
  scale_x_continuous(limits = c(-0.4, 4.4),
                     labels = label_number(style_positive = "plus")) +
  scale_y_continuous(limits = c(-2.4, 2.4),
                     labels = label_number(style_positive = "plus")) +
  scale_colour_manual(values = c("#D92B40", "#113E66", "#20AE80", "#C9C9C9", "#C9C9C9", "#C9C9C9", "#FFB619", "#2475B3")) +
  theme(legend.position = "none")
rough_scatter

themed_scatter <- rough_scatter +
  theme_ptb() +
  theme(axis.title = element_blank(),
        axis.ticks = element_blank(),
        axis.line = element_blank()) +
  annotate(geom = "text",
           y = 2.3,
           x = 0.2,
           label = "GD per game\nvs. 'top eight'",
           family = ptb_font,
           colour = ptb_dark_grey,
           fontface = "bold",
           hjust = 0,
           vjust = 1) +
  annotate(geom = "text",
           y = -1.7,
           x = 4.35,
           label = "GD per game\nvs. other qualifiers",
           family = ptb_font,
           colour = ptb_dark_grey,
           fontface = "bold",
           hjust = 1,
           vjust = 1)
themed_scatter

labelled_scatter <- themed_scatter +
  labs(title = "<b style='color:#FFB619'>Spain</b> stand apart from the other<br>contenders at Euro 2025",
       subtitle = "Performance of eight top teams at Euro 2025 in<br>games against other qualifiers since Sept 2022",
       caption = "'GD' = goal difference | 'top eight' = best teams by overall GD<br><br>Data: Sofascore | Chart: Plot the Ball")
labelled_scatter