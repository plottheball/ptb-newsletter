library(tidyverse)
library(janitor)
library(googlesheets4)
library(ggtext)

# data downloaded from: https://theanalyst.com/club-rugby-stats
back_rows <- read_sheet(ss = "https://docs.google.com/spreadsheets/d/1Ce1Z1uz3fF7O_JxoY7rY0dN4wH6vCzF9JkcCJhG08_c/",
                        sheet = "Summary") %>%
  clean_names()

for_plot <- back_rows %>%
  select(1, 4, 5) %>%
  pivot_longer(cols = c("b_carries_tackles_80", "a_total_ruck_involvements_80"),
               names_to = "category",
               values_to = "value") %>%
  mutate(value_final = if_else(category == "a_total_ruck_involvements_80", value * -1, value),
         sort_value = if_else(category == "a_total_ruck_involvements_80", NA, value),
         key = case_when(category == "a_total_ruck_involvements_80" & player == "Henry Pollock" ~ "Left Focus",
                         category == "a_total_ruck_involvements_80" & player != "Henry Pollock" ~ "Left Other",
                         category == "b_carries_tackles_80" & player == "Henry Pollock" ~ "Right Focus",
                         category == "b_carries_tackles_80" & player != "Henry Pollock" ~ "Right Other",
                         TRUE ~ "!"),
         label = case_when(player == "Henry Pollock" ~ str_c("<b>", player, "</b>"),
                           TRUE ~ player))

rough_bars <- ggplot(for_plot, aes(x = fct_reorder(label, sort_value), y = value_final, fill = key)) +
  geom_bar(stat = "identity",
           width = 0.55) +
  scale_fill_manual(values = c("#CCB47A", "#E6D8B8", "#af001e", "#CCA3AA")) +
  geom_hline(yintercept = 0) +
  scale_y_continuous(limits = c(-39, 39),
                     breaks = c(-30, -20, -10, 0, 10, 20, 30),
                     labels = (abs(c(-30, -20, -10, 0, 10, 20, 30))),
                     expand = c(0, 0))+
  theme(legend.position = "none",
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.ticks.y = element_blank(),
        panel.grid.major.y = element_blank(),
        axis.line.x = element_blank(),
        axis.line.y = element_blank(),
        axis.ticks = element_blank(),
        axis.text.y = element_text(hjust = 0)) +
  coord_flip()
rough_bars

themed_bars <- rough_bars +
  theme_ptb()
themed_bars

labelled_bars <- themed_bars +
  labs(title = "At club level, Pollock saw more <b style='color:#af001e'>on-ball</b><br>action than other Lions back-rowers",
       subtitle = "<b style='color:#CCB47A'>Breakdown</b> and <b style='color:#af001e'>on-ball</b> involvements per 80 mins<br> recorded by <b>Lions back-rowers</b> in 2024-25",
       caption = "'<b style='color:#CCB47A'>Breakdown</b>' = att. and def. rucks | '<b style='color:#af001e'>on-ball</b>' = carries and tackles<br><br><i>Data for domestic club matches only</i><br><br>Data: Opta Analyst | Chart: Plot the Ball")
labelled_bars