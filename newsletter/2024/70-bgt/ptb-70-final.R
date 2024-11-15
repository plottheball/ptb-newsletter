library(tidyverse)
library(rvest)
library(janitor)
library(RcppRoll)
library(ggtext)

url_base <- "https://stats.espncricinfo.com/ci/engine/stats/index.html?class=1;filter=advanced;orderby=age;orderbyad=reverse;page="
first_page <- 1
url_end_bat <- ";size=200;spanmin1=1+Jan+1990;spanval1=span;team=2;team=6;template=results;type=batting;view=innings;wrappertype=print"
url_end_bowl <- ";size=200;spanmin1=1+Jan+1990;spanval1=span;team=2;team=6;template=results;type=bowling;view=innings;wrappertype=print"
url_full_bat <- str_c(url_base, first_page, url_end_bat)
url_full_bowl <- str_c(url_base, first_page, url_end_bowl)

no_pages_bat_source <- read_html(url_full_bat) %>%
  html_node(xpath = "/html/body/div/div[3]/table[2]") %>%
  html_table()
no_pages_bat <- unlist(no_pages_bat_source[1,1]) %>%
  str_replace(., "Page 1 of ", "") %>%
  as.numeric()

rank_bat <- seq(first_page, no_pages_bat, 1)
links_bat <- str_c(url_base, rank_bat, url_end_bat)

no_pages_bowl_source <- read_html(url_full_bowl) %>%
  html_node(xpath = "/html/body/div/div[3]/table[2]") %>%
  html_table()
no_pages_bowl <- unlist(no_pages_bowl_source[1,1]) %>%
  str_replace(., "Page 1 of ", "") %>%
  as.numeric()

rank_bowl <- seq(first_page, no_pages_bowl, 1)
links_bowl <- str_c(url_base, rank_bowl, url_end_bowl)

batting_data <- tibble()

for (i in links_bat) {
  
  print(Sys.time())
  Sys.sleep(0.5)
  
  table_data <- read_html(i) %>%
    html_node(xpath = "/html/body/div/div[3]/table[3]") %>%
    html_table() %>%
    clean_names() %>%
    select(player, bf, age, opposition, ground, start_date)
  
  table_data$bf <- as.character(table_data$bf)

  batting_data <- bind_rows(batting_data, table_data)
  
}

bowling_data <- tibble()

for (i in links_bowl) {
  
  print(Sys.time())
  Sys.sleep(0.5)
  
  table_data <- read_html(i) %>%
    html_node(xpath = "/html/body/div/div[3]/table[3]") %>%
    html_table() %>%
    clean_names() %>%
    select(player, overs, age, opposition, ground, start_date)
  
  table_data$overs <- as.character(table_data$overs)
  
  bowling_data <- bind_rows(bowling_data, table_data)
  
}

batting_clean <- batting_data %>%
  filter(bf != "-") %>%
  separate(col = age,
           into = c("age_y", "age_d"),
           sep = "y ") %>%
  separate(col = player,
           into = c("player", "team"),
           sep = "[(]") %>%
  mutate(bf = as.integer(bf),
         age_y = as.integer(age_y),
         age_d = as.integer(str_remove_all(age_d, "d")),
         age = age_y + (age_d / 365.25),
         team = str_remove_all(team, "[)]"),
         match_id = str_c(team, " ", opposition, " (", ground, " - ", start_date, ")"),
         start_date = dmy(start_date)) %>%
  select(-age_y, -age_d) %>%
  arrange(start_date)

bowling_clean <- bowling_data %>%
  filter(overs != "DNB" & overs != "TDNB" & overs != "sub") %>%
  separate(col = overs,
           into = c("overs_o", "overs_b"),
           sep = "[.]") %>%
  separate(col = age,
           into = c("age_y", "age_d"),
           sep = "y ") %>%
  separate(col = player,
           into = c("player", "team"),
           sep = "[(]") %>%
  mutate(overs_o = as.integer(overs_o),
         overs_b = as.integer(overs_b),
         bb = overs_b + (overs_o * 6),
         age_y = as.integer(age_y),
         age_d = as.integer(str_remove_all(age_d, "d")),
         age = age_y + (age_d / 365.25),
         team = str_remove_all(team, "[)]"),
         match_id = str_c(team, " ", opposition, " (", ground, " - ", start_date, ")"),
         start_date = dmy(start_date)) %>%
  select(-age_y, -age_d, -overs_o, -overs_b) %>%
  arrange(start_date)

match_summary_bat <- batting_clean %>%
  group_by(team, match_id, start_date) %>%
  summarise(all_bf = sum(bf)) %>%
  arrange(start_date)

match_summary_bowl <- bowling_clean %>%
  group_by(team, match_id, start_date) %>%
  summarise(all_bb = sum(bb)) %>%
  arrange(start_date)

match_summary <- full_join(x = match_summary_bat, y = match_summary_bowl, by = c("team" = "team", "match_id" = "match_id", "start_date" = "start_date")) %>%
  unique()

ind_matches <- match_summary %>%
  filter(team == "IND") %>%
  arrange(start_date) %>%
  select(team, match_id, start_date)
ind_matches$team_match_no <- row_number(ind_matches$start_date)

aus_matches <- match_summary %>%
  filter(team == "AUS") %>%
  arrange(start_date) %>%
  select(team, match_id, start_date)
aus_matches$team_match_no <- row_number(aus_matches$start_date)

match_summary_final <- left_join(x = match_summary, y = ind_matches, by = c("team" = "team", "match_id" = "match_id", "start_date" = "start_date")) %>%
  left_join(., y = aus_matches, by = c("team" = "team", "match_id" = "match_id", "start_date" = "start_date"), suffix = c("_ind", "_aus")) %>%
  mutate(match_no_final = if_else(is.na(team_match_no_ind), team_match_no_aus, team_match_no_ind)) %>%
  select(-team_match_no_ind, -team_match_no_aus)

batting_summary <- batting_clean %>%
  group_by(start_date, match_id, team, player, age) %>%
  summarise(tot_bf = sum(bf))

bowling_summary <- bowling_clean %>%
  group_by(start_date, match_id, team, player, age) %>%
  summarise(tot_bb = sum(bb))

summary_all <- full_join(x = batting_summary, y = bowling_summary, by = c("team" = "team", "match_id" = "match_id", "start_date" = "start_date", "player" = "player", "age" = "age"))

summary_final <- left_join(x = summary_all, y = match_summary_final, by = c("team" = "team", "match_id" = "match_id", "start_date" = "start_date")) %>%
  relocate(match_no_final, .before = start_date) %>%
  mutate(player_share_bat = tot_bf / all_bf,
         player_share_bowl = tot_bb / all_bb,
         age_weight_bat = player_share_bat * age,
         age_weight_bowl = player_share_bowl * age)

bat_bowl_age <- summary_final %>%
  group_by(match_no_final, start_date, team, match_id, all_bf, all_bb) %>%
  summarise(weight_ave_age_bat = sum(age_weight_bat, na.rm = TRUE),
            weight_ave_age_bowl = sum(age_weight_bowl, na.rm = TRUE))

roll_no <- 30

age_ind <- bat_bowl_age %>%
  filter(team == "IND") %>%
  mutate(all_balls = all_bf + all_bb) %>%
  filter(!is.na(all_balls) & all_balls > 500)

age_ind$match_no_final <- 0 - max(row_number(age_ind$match_no_final)) + row_number(age_ind$match_no_final)
age_ind$roll_ave_age_bat <- roll_sum(x = age_ind$weight_ave_age_bat, n = roll_no, align = "right", fill = NA, na.rm = TRUE) / roll_no
age_ind$roll_ave_age_bowl <- roll_sum(x = age_ind$weight_ave_age_bowl, n = roll_no, align = "right", fill = NA, na.rm = TRUE) / roll_no

age_aus <- bat_bowl_age %>%
  filter(team == "AUS") %>%
  mutate(all_balls = all_bf + all_bb) %>%
  filter(!is.na(all_balls) & all_balls > 500)

age_aus$match_no_final <- 0 - max(row_number(age_aus$match_no_final)) + row_number(age_aus$match_no_final)
age_aus$roll_ave_age_bat <- roll_sum(x = age_aus$weight_ave_age_bat, n = roll_no, align = "right", fill = NA, na.rm = TRUE) / roll_no
age_aus$roll_ave_age_bowl <- roll_sum(x = age_aus$weight_ave_age_bowl, n = roll_no, align = "right", fill = NA, na.rm = TRUE) / roll_no

data_for_comp <- bind_rows(age_aus, age_ind)

background_grey <- "#E6E6E6"
india_orange <- "#F28D61"
australia_green <- "#174D3F"
colours <- c(australia_green, india_orange)

rough_line_bat <- ggplot(data_for_comp, aes(x = match_no_final, y = roll_ave_age_bat, colour = team)) +
  geom_vline(xintercept = -200,
             size = 0.7,
             color = background_grey,
             linetype = "dashed") +
  geom_vline(xintercept = -150,
             size = 0.7,
             color = background_grey,
             linetype = "dashed") +
  geom_vline(xintercept = -100,
             size = 0.7,
             color = background_grey,
             linetype = "dashed") +
  geom_vline(xintercept = -50,
             size = 0.7,
             color = background_grey,
             linetype = "dashed") +
  geom_line(linetype = "solid", linewidth = 1.5) +
  scale_x_continuous(limits = c(-200, 0),
                     expand = c(0.1, 0.1),
                     labels = NULL) +
  scale_y_continuous(limits = c(23, 37),
                     breaks = seq(24, 36, 3)) +
  scale_colour_manual(values = colours)
rough_line_bat

themed_line_bat <- rough_line_bat +
  theme_ptb() +
  theme(axis.line = element_blank(),
        axis.ticks = element_blank(),
        axis.text.x = element_blank(),
        panel.grid.major.x = element_blank(),
        axis.title.x = element_blank(),
        legend.position = "none")
themed_line_bat

labelled_line_bat <- themed_line_bat +
  labs(title = "With an influx of new talent, <b style='color:#F28D61'>India</b>'s<br>batting line-up is getting younger",
       subtitle = "Average age (in years) of the <b>batting line-ups</b> of<br><b style='color:#F28D61'>India</b> and <b style='color:#165949'>Australia</b> in men's tests since 2005",
       caption = "<i>Weighted by balls faced and calculated on a 30-match rolling basis</i><br><br>Data: ESPNCricinfo | Chart: Plot the Ball")
labelled_line_bat

rough_line_bowl <- ggplot(data_for_comp, aes(x = match_no_final, y = roll_ave_age_bowl, colour = team)) +
  geom_vline(xintercept = -200,
             size = 0.7,
             color = background_grey,
             linetype = "dashed") +
  geom_vline(xintercept = -150,
             size = 0.7,
             color = background_grey,
             linetype = "dashed") +
  geom_vline(xintercept = -100,
             size = 0.7,
             color = background_grey,
             linetype = "dashed") +
  geom_vline(xintercept = -50,
             size = 0.7,
             color = background_grey,
             linetype = "dashed") +
  geom_line(linetype = "solid", linewidth = 1.5) +
  scale_x_continuous(limits = c(-200, 0),
                     expand = c(0.1, 0.1),
                     labels = NULL) +
  scale_y_continuous(limits = c(23, 37),
                     breaks = seq(24, 36, 3)) +
  scale_colour_manual(values = colours)
rough_line_bowl

themed_line_bowl <- rough_line_bowl +
  theme_ptb() +
  theme(axis.line = element_blank(),
        axis.ticks = element_blank(),
        axis.text.x = element_blank(),
        panel.grid.major.x = element_blank(),
        axis.title.x = element_blank(),
        legend.position = "none")
themed_line_bowl

labelled_line_bowl <- themed_line_bowl +
  labs(title = "The bowling attacks of both <b style='color:#F28D61'>India</b> and<br><b style='color:#165949'>Australia</b> remain reliant on older stars",
       subtitle = "Average age (in years) of the <b>bowling attacks</b> of<br><b style='color:#F28D61'>India</b> and <b style='color:#165949'>Australia</b> in men's tests since 2005",
       caption = "<i>Weighted by balls bowled and calculated on a 30-match rolling basis</i><br><br>Data: ESPNCricinfo | Chart: Plot the Ball")
labelled_line_bowl
