library(tidyverse)
library(janitor)
library(rvest)
library(RcppRoll)
library(ggtext)

url_base <- "https://stats.espncricinfo.com/ci/engine/stats/index.html?bowling_pacespin=1;class=6;filter=advanced;orderby=start;page="
pages <- seq(1, 6, 1)
url_end <- ";size=200;spanmin1=1+Jan+2013;spanval1=span;team=4346;template=results;type=bowling;view=innings;wrappertype=print"
urls <- str_c(url_base, pages, url_end)

all_pace <- tibble()

for (i in urls) {
  
  Sys.sleep(3)
  
  html <- read_html(i)
  
  tables <- html %>%
    html_nodes("table") %>%
    html_table()
  
  key_table <- tables[[3]] %>%
    clean_names()
  
  all_pace <- bind_rows(all_pace, key_table)
  
}

pace_clean <- all_pace %>%
  select(-mdns, -econ, -x) %>%
  filter(overs != "DNB" & overs != "sub") %>%
  mutate(start_date = dmy(start_date),
         runs = as.integer(runs),
         wkts = as.integer(wkts)) %>%
  separate(col = overs,
           into = c("completed_overs", "completed_balls"),
           sep = "[.]") %>%
  mutate(completed_overs = as.integer(completed_overs),
         completed_balls = as.integer(completed_balls),
         bb = completed_balls + (completed_overs * 6),
         .before = "runs") %>%
  select(-completed_overs, -completed_balls) %>%
  mutate(match_id = str_c(ground, " ", opposition, " (", start_date, ")")) %>%
  arrange(start_date)

all_games <- unique(unlist(pace_clean$match_id))
bumrah_games <- pace_clean %>%
  filter(player == "JJ Bumrah") %>%
  select(match_id) %>%
  unique() %>%
  unlist()

bumrah_clean <- pace_clean %>%
  filter(match_id %in% bumrah_games & player == "JJ Bumrah")

other_clean <- pace_clean %>%
  filter(match_id %in% bumrah_games & player != "JJ Bumrah")

other_summary <- other_clean %>%
  group_by(match_id) %>%
  summarise(other_bb = sum(bb),
            other_runs = sum(runs),
            other_wkts = sum(wkts))

bumrah_analysis <- left_join(bumrah_clean, other_summary, by = "match_id", suffix = c("_focus", "_other")) %>%
  select(match_id, inns, bb, runs, wkts, other_bb, other_runs, other_wkts) %>%
  mutate(match_index = row_number(),
         .before = "match_id")

roll_no <- 30

bumrah_final <- bumrah_analysis %>%
  mutate(roll_bb = roll_sum(bb, n = roll_no, fill = NA, align = "right"),
         roll_runs = roll_sum(runs, n = roll_no, fill = NA, align = "right"),
         roll_rr = roll_runs / roll_bb,
         roll_other_bb = roll_sum(other_bb, n = roll_no, fill = NA, align = "right"),
         roll_other_runs = roll_sum(other_runs, n = roll_no, fill = NA, align = "right"),
         roll_rr_other = roll_other_runs / roll_other_bb,
         rr_diff = roll_rr - roll_rr_other,
         rr_diff_p24 = rr_diff * 24,
         rr_p24 = roll_rr * 24,
         rr_other_p24 = roll_rr_other * 24) %>%
  select(1, 2, 18, 19, 17) %>%
  mutate(period_one_focus = case_when(match_index < 125 ~ rr_p24, TRUE ~ NA),
         period_one_other = case_when(match_index < 125 ~ rr_other_p24, TRUE ~ NA),
         period_two_focus = case_when(match_index > 122 ~ rr_p24, TRUE ~ NA),
         period_two_other = case_when(match_index > 122 ~ rr_other_p24, TRUE ~ NA))

focus_colour_one <- "#869FBF"
other_colour_one <- "#E6D9B8"
focus_colour_one_light <- "#F2F2F2"
focus_colour_two <- "#173F73"
other_colour_two <- "#E6C15C"
focus_colour_two_light <- "#D9D9D9"

line_rough <- ggplot(data = bumrah_final, aes(x = match_index)) +
  geom_vline(xintercept = c(123),
             size = 1,
             color = ptb_light_grey,
             linetype = "dashed") +
  geom_ribbon(aes(ymin = period_one_focus, ymax = period_one_other), fill = focus_colour_one_light) +
  geom_line(aes(y = period_one_other), colour = other_colour_one, linewidth = 1.75) +
  geom_line(aes(y = period_one_focus), colour = focus_colour_one, linewidth = 1.75) +
  geom_ribbon(aes(ymin = period_two_focus, ymax = period_two_other), fill = focus_colour_two_light) +
  geom_line(aes(y = period_two_other), colour = other_colour_two, linewidth = 1.75) +
  geom_line(aes(y = period_two_focus), colour = focus_colour_two, linewidth = 1.75) +
  scale_y_continuous(name = NULL,
                     limits = c(16, 49),
                     expand = c(0,0),
                     breaks = seq(20, 45, 5)) +
  scale_x_continuous(limits = c(15, 199),
                     expand = c(0,0)) +
  theme(axis.line.x = element_blank(),
        axis.line.y = element_blank(),
        axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        axis.ticks = element_blank(),
        panel.grid.major.x = element_blank())
line_rough

themed_line <- line_rough +
  theme_ptb()
themed_line

labelled_line <- themed_line +
  labs(title = "The gap between <b style='color:#1F5499'>Bumrah</b> and <b style='color:#D1AB3E'>his IPL<br>teammates</b> has never been wider",
       subtitle = "<b>Runs conceded per 24 balls</b> by <b style='color:#1F5499'>Jasprit Bumrah</b><br>in T20s for the <b>Mumbai Indians</b>, compared to the<br>team's <b style='color:#D1AB3E'>other fast bowlers</b>",
       caption = "<i>Calculated on a 30-innings rolling basis</i><br><br>Data: ESPNCricinfo | Chart: Plot the Ball")
labelled_line