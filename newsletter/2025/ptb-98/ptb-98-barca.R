library(tidyverse)
library(janitor)
library(googlesheets4)
library(RcppRoll)
library(ggtext)
library(scales)

### match logs downloaded from: https://fbref.com/en/squads/15f49df1/history/Barcelona-Women-Stats-and-History
match_logs <- read_sheet("https://docs.google.com/spreadsheets/d/18jPR7S0OdFB7lPxV1MIRliI0BQyY3a4VsxB1eFj-MrY/edit") %>%
  clean_names()

matches_clean <- match_logs %>%
  select(date, comp, gf, x_g, ga, x_ga) %>%
  arrange(date) %>%
  mutate(match_index = row_number(),
         .before = "date") 

roll_no <- 20

matches_rolling <- matches_clean %>%
  mutate(roll_gf = roll_sum(gf, n = roll_no, fill = NA, align = "right"),
         roll_xgf = roll_sum(x_g, n = roll_no, fill = NA, align = "right"),
         roll_ga = roll_sum(ga, n = roll_no, fill = NA, align = "right"),
         roll_xga = roll_sum(x_ga, n = roll_no, fill = NA, align = "right"),
         roll_adj_gf = (roll_gf * 0.3) + (roll_xgf * 0.7),
         roll_adj_ga = (roll_ga * 0.3) + (roll_xga * 0.7),
         adj_gf_pg = roll_adj_gf / roll_no,
         adj_ga_pg = roll_adj_ga / roll_no)

for_plot <- matches_rolling %>%
  select(match_index, adj_gf_pg, adj_ga_pg) %>%
  filter(!is.na(adj_gf_pg)) %>%
  mutate(giraldez_agf = if_else(match_index <= 83, adj_gf_pg, NA),
         giraldez_aga = if_else(match_index <= 83, adj_ga_pg, NA),
         romeu_agf = if_else(match_index >= 82, adj_gf_pg, NA),
         romeu_aga = if_else(match_index >= 82, adj_ga_pg, NA),
         adj_gd = adj_gf_pg - adj_ga_pg)

main_colour <- "#E6A1AB"
main_colour_alt <- "#B3AB8F"
focus_colour_one_light <- "#F5F5F5"
other_colour <- "#CC2941"
other_colour_alt <- "#B38C00"
focus_colour_two_light <- "#E6E6E6"

rough_line <- ggplot(for_plot, aes(x = match_index)) +
  geom_hline(yintercept = 0,
             colour = ptb_dark_grey) +
  geom_vline(xintercept = 82,
             linewidth = 1,
             color = ptb_light_grey,
             linetype = "dashed") +
  scale_y_continuous(limits = c(0, 5.9),
                     breaks = seq(0, 5, 1),
                     expand = c(0, 0)) +
  scale_x_continuous(limits = c(0, 155),
                     expand = c(0, 0)) +
  geom_ribbon(aes(ymin = giraldez_aga, ymax = giraldez_agf), fill = focus_colour_one_light) +
  geom_line(aes(y = for_plot$giraldez_agf), colour = main_colour, linewidth = 1.75, alpha = 1) +
  geom_line(aes(y = for_plot$giraldez_aga), colour = main_colour_alt, linewidth = 1.75, alpha = 1) +
  geom_ribbon(aes(ymin = romeu_aga, ymax = romeu_agf), fill = focus_colour_two_light) +
  geom_line(aes(y = for_plot$romeu_agf), colour = other_colour, linewidth = 1.75, alpha = 1) +
  geom_line(aes(y = for_plot$romeu_aga), colour = other_colour_alt, linewidth = 1.75, alpha = 1) +
  theme(axis.line.x = element_blank(),
        axis.line.y = element_blank(),
        axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        axis.ticks = element_blank(),
        panel.grid.major.x = element_blank())
rough_line

themed_line <- rough_line +
  theme_ptb()
themed_line

labelled_line <- themed_line +
  labs(title = "After hitting a low point in March,<br>Pere Romeu has Barça back on track",
       subtitle = "No. of '<b>adjusted goals</b>' <b style='color:#CC2941'>for</b> and <b style='color:#B38C00'>against</b> per game<br>recorded by <b>Barça Femení</b> in Liga F and the<br>Champions League <b>since September 2022</b>",
       caption = "<br><i>'Adjusted goals' is a blend of actual goals (30%) and expected<br>goals (70%); averages calculated on a 20-game rolling basis</i><br><br>Data: FBref | Chart: Plot the Ball")
labelled_line
