library(tidyverse)
library(readxl)
library(janitor)
library(ggalt)
library(ggtext)
library(scales)

### ALL DATA DOWNLOADED FROM SYNERGY SPORTS: https://shop.synergysports.com/products/deep-dives-wnba-monthly-subscription
summary_halfcourt_2022 <- read_xlsx("summary_halfcourt_2022.xlsx") %>%
  clean_names() %>%
  select(jersey_number, player, team, gp, poss, pts, ppp) %>%
  mutate(category = "Overall Half Court",
         season = 2022,
         .before = jersey_number)

summary_halfcourt_2023 <- read_xlsx("summary_halfcourt_2023.xlsx") %>%
  clean_names() %>%
  select(jersey_number, player, team, gp, poss, pts, ppp) %>%
  mutate(category = "Overall Half Court",
         season = 2023,
         .before = jersey_number)

summary_transition_2022 <- read_xlsx("summary_transition_2022.xlsx") %>%
  clean_names() %>%
  select(jersey_number, player, team, gp, poss, pts, ppp) %>%
  mutate(category = "Transition",
         season = 2022,
         .before = jersey_number)

summary_transition_2023 <- read_xlsx("summary_transition_2023.xlsx") %>%
  clean_names() %>%
  select(jersey_number, player, team, gp, poss, pts, ppp) %>%
  mutate(category = "Transition",
         season = 2023,
         .before = jersey_number)

summary_all <- bind_rows(summary_halfcourt_2022, summary_halfcourt_2023, summary_transition_2022, summary_transition_2023)

guards <- c("Ariel Atkins", "Chelsea Gray", "Rhyne Howard", "Sabrina Ionescu", "Jewell Loyd", "Kelsey Plum", "Diana Taurasi", "Jackie Young", "Allisha Gray")

summary_guards <- summary_all %>%
  filter(player %in% guards)

guards_final <- summary_guards %>%
  group_by(player, category) %>%
  summarise(tot_gp = sum(gp),
            tot_poss = sum(poss),
            tot_pts = sum(pts)) %>%
  mutate(poss_game = tot_poss / tot_gp,
         pts_per_poss = tot_pts / tot_poss)

tr_vs_hc <- guards_final %>%
  select(1, 2, 7) %>%
  pivot_wider(names_from = "category",
              values_from = "pts_per_poss") %>%
  clean_names() %>%
  mutate(diff = (transition / overall_half_court * 100) - 100,
         category = case_when(diff >= 5 ~ "Higher",
                              diff <= -5 ~ "Lower",
                              TRUE ~ "Same"))

tr_colour <- "#194680"
equal_colour <- "#CCCCCC"
hc_colour <- "#D91636"

rough_dumbbell <- ggplot(tr_vs_hc, aes(x = overall_half_court, xend = transition, y = fct_reorder(player, transition))) + 
  geom_dumbbell(colour = equal_colour,
                size = 4,
                alpha = case_when(tr_vs_hc$category == "Same" ~ 0.75,
                                  TRUE ~ 1),
                colour_x = hc_colour,
                colour_xend = tr_colour) +
  scale_x_continuous(name = NULL,
                     limits = c(0.65, 1.35),
                     breaks = seq(0.8, 1.2, 0.2))
rough_dumbbell

themed_dumbbell <- rough_dumbbell +
  theme_ptb() +
  theme(axis.title = element_blank(),
        axis.line = element_blank(),
        panel.grid.major.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.y = element_text(hjust = 0))
themed_dumbbell

labelled_dumbbell <- themed_dumbbell +
  labs(title = "Taurasi is the only Team USA guard<br>who is more efficient in the <b style='color:#D91636'>half-court</b>",
       subtitle = "Points scored per possession in the <b style='color:#D91636'>half-court</b> and<br>in <b style='color:#194680'>transition</b> in the WNBA since 2022",
       caption = "<i>Data is for the regular season only</i><br><br>Data: Synergy Sports | Chart: Plot the Ball")
labelled_dumbbell

play_types_on <- c("prball", "iso", "handoff", "post")
play_types_off <- c("spot", "screen", "cut", "prroll", "oreb")

on_ball_data <- tibble()

for (i in play_types_on) {
  
  data_2022 <- read_xlsx(str_c(i, "_2022.xlsx")) %>%
    clean_names() %>%
    select(jersey_number, player, team, gp, poss, pts, ppp) %>%
    mutate(category = i,
           group = "On ball",
           season = 2022,
           .before = jersey_number)
  data_2023 <- read_xlsx(str_c(i, "_2023.xlsx")) %>%
    clean_names() %>%
    select(jersey_number, player, team, gp, poss, pts, ppp) %>%
    mutate(category = i,
           group = "On ball",
           season = 2023,
           .before = jersey_number)
  
  on_ball_data <- bind_rows(on_ball_data, data_2022, data_2023)
  
}

off_ball_data <- tibble()

for (i in play_types_off) {
  
  data_2022 <- read_xlsx(str_c(i, "_2022.xlsx")) %>%
    clean_names() %>%
    select(jersey_number, player, team, gp, poss, pts, ppp) %>%
    mutate(category = i,
           group = "Off ball",
           season = 2022,
           .before = jersey_number)
  data_2023 <- read_xlsx(str_c(i, "_2023.xlsx")) %>%
    clean_names() %>%
    select(jersey_number, player, team, gp, poss, pts, ppp) %>%
    mutate(category = i,
           group = "Off ball",
           season = 2023,
           .before = jersey_number)
  
  off_ball_data <- bind_rows(off_ball_data, data_2022, data_2023)
  
}

all_by_type <- bind_rows(on_ball_data, off_ball_data)

guards_by_type <- all_by_type %>%
  filter(player %in% guards)

guards_type_final <- guards_by_type %>%
  group_by(player, group) %>%
  summarise(tot_poss = sum(poss),
            tot_pts = sum(pts)) %>%
  pivot_wider(names_from = "group",
              values_from = c("tot_poss", "tot_pts")) %>%
  clean_names() %>%
  mutate(on_ball_share = tot_poss_on_ball / sum(tot_poss_on_ball + tot_poss_off_ball),
         on_ball_ppp = tot_pts_on_ball / tot_poss_on_ball,
         off_ball_ppp = tot_pts_off_ball / tot_poss_off_ball,
         ppp_diff = on_ball_ppp - off_ball_ppp,
         hc_ppp = (on_ball_share * on_ball_ppp) + ((1 - on_ball_share) * off_ball_ppp))

catchshoot_2022 <- read_xlsx("catchshoot_2022.xlsx") %>%
  clean_names() %>%
  select(jersey_number, player, team, gp, x3fg_att, x3_fg_made) %>%
  mutate(category = "Catch & shoot",
         season = 2022,
         .before = jersey_number)

catchshoot_2023 <- read_xlsx("catchshoot_2023.xlsx") %>%
  clean_names() %>%
  select(jersey_number, player, team, gp, x3fg_att, x3_fg_made) %>%
  mutate(category = "Catch & shoot",
         season = 2023,
         .before = jersey_number)

catchshoot_all <- bind_rows(catchshoot_2022, catchshoot_2023)

catchshoot_guards <- catchshoot_all %>%
  filter(player %in% guards) %>%
  group_by(player) %>%
  summarise(tot_3p_att = sum(x3fg_att),
            tot_3p_made = sum(x3_fg_made)) %>%
  mutate(cs_3_percent = tot_3p_made / tot_3p_att * 100) %>%
  select(1, 4)

prpass_2022 <- read_xlsx("prpass_2022.xlsx") %>%
  clean_names() %>%
  select(jersey_number, player, team, gp, poss, pts, ppp) %>%
  mutate(category = "Pick & roll (incl. pass)",
         season = 2022,
         .before = jersey_number)

prpass_2023 <- read_xlsx("prpass_2023.xlsx") %>%
  clean_names() %>%
  select(jersey_number, player, team, gp, poss, pts, ppp) %>%
  mutate(category = "Pick & roll (incl. pass)",
         season = 2023,
         .before = jersey_number)

prpass_all <- bind_rows(prpass_2022, prpass_2023)

prpass_guards <- prpass_all %>%
  filter(player %in% guards) %>%
  group_by(player) %>%
  summarise(tot_poss = sum(poss),
            tot_pts = sum(pts)) %>%
  mutate(pr_ppp = tot_pts / tot_poss) %>%
  select(1, 4)

on_off_final <- left_join(prpass_guards, catchshoot_guards, by = c("player" = "player")) %>%
  mutate(cs_rank = rank(cs_3_percent),
         pr_rank = rank(pr_ppp))

rough_scatter <- ggplot(on_off_final, aes(x = pr_ppp, y = cs_3_percent)) +
  geom_point(size = 5.3,
             alpha = 0.9,
             color = hc_colour) +
  scale_x_continuous(limits = c(0.55, 1.25),
                     breaks = seq(0.6, 1.2, 0.2),
                     expand = c(0, 0)) +
  scale_y_continuous(limits = c(27.5, 52.5),
                     breaks = seq(30, 50, 10),
                     expand = c(0, 0),
                     labels = percent_format(scale = 1))
rough_scatter

themed_scatter <- rough_scatter +
  theme_ptb() +
  theme(legend.position = "none",
        axis.line = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank()) +
  annotate(geom = "text",
           x = 0.575,
           y = 51.5,
           label = "Catch &\nShoot 3P%",
           family = ptb_font,
           colour = ptb_dark_grey,
           fontface = "bold",
           hjust = 0,
           vjust = 1) +
  annotate(geom = "text",
           x = 1.225,
           y = 32,
           label = "Pick & Roll PPP\n(incl. passes)",
           family = ptb_font,
           colour = ptb_dark_grey,
           fontface = "bold",
           hjust = 1,
           vjust = 1)
themed_scatter

labelled_scatter <- themed_scatter +
  labs(title = "Jackie Young is Team USA's standout<br>guard in half-court offense",
       subtitle = "Shooting % on <b>catch & shoot</b> three-point attempts<br> and PPP generated as the <b>pick & roll</b> ball-handler<br>in the WNBA since 2022",
       caption = "<i>Data is for the regular season only</i><br><br>Data: Synergy Sports | Chart: Plot the Ball")
labelled_scatter
