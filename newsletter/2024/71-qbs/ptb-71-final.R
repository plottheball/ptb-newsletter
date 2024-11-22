library(tidyverse)
library(rvest)
library(janitor)
library(ggtext)
library(lemon)
library(scales)
library(gt)
library(gtExtras)
library(gtUtils)

base_url <- "https://www.pro-football-reference.com/years/"
years_adv <- seq(2018, 2024, by = 1)
years_pre <- seq(2011, 2017, by = 1)
end_url_adv <- "/advanced.htm"

all_air_yards <- tibble()
all_accuracy <- tibble()
all_pressure <- tibble()
all_play_type <- tibble()
all_rushing <- tibble()
all_receiving <- tibble()

for (i in years_adv) {
  
  Sys.sleep(1)
  
  print(Sys.time())
  
  url <- str_c(base_url, i, end_url_adv)
  
  html <- read_html(url)
  
  tables <- html %>%
    html_nodes("table") %>%
    html_table()
  
  air_yards <- tables[[1]] %>%
    clean_names()
  names(air_yards) <- unlist(air_yards[1, ])
  air_yards <- air_yards[2:nrow(air_yards), ] %>%
    clean_names() %>%
    mutate(season = i)
  all_air_yards <- bind_rows(all_air_yards, air_yards)
  
  accuracy <- tables[[2]] %>%
    clean_names()
  names(accuracy) <- unlist(accuracy[1, ])
  accuracy <- accuracy[2:nrow(accuracy), ] %>%
    clean_names() %>%
    mutate(season = i)
  all_accuracy <- bind_rows(all_accuracy, accuracy)
  
  pressure <- tables[[3]] %>%
    clean_names()
  names(pressure) <- unlist(pressure[1, ])
  pressure <- pressure[2:nrow(pressure), ] %>%
    clean_names() %>%
    mutate(season = i)
  all_pressure <- bind_rows(all_pressure, pressure)
  
  play_type <- tables[[4]] %>%
    clean_names()
  names(play_type) <- unlist(play_type[1, ])
  play_type <- play_type[2:nrow(play_type), ] %>%
    clean_names() %>%
    mutate(season = i)
  all_play_type <- bind_rows(all_play_type, play_type)
  
  rushing <- tables[[5]] %>%
    clean_names() %>%
    mutate(season = i)
  all_rushing <- bind_rows(all_rushing, rushing)
  
  receiving <- tables[[6]] %>%
    clean_names() %>%
    mutate(season = i)
  all_receiving <- bind_rows(all_receiving, receiving)
    
}

air_yards_clean <- all_air_yards %>%
  select(season, tm, g, cmp, att, yds, iay, cay, yac) %>%
  mutate(g = as.integer(g),
         cmp = as.integer(cmp),
         att = as.integer(att),
         yds = as.integer(yds),
         iay = as.integer(iay),
         cay = as.integer(cay),
         yac = as.integer(yac))

air_yards_season <- air_yards_clean %>%
  group_by(season) %>%
  summarise(no_games = sum(g),
            no_pass_cmp = sum(cmp),
            no_pass_att = sum(att),
            no_pass_yds = sum(yds),
            no_iay = sum(iay),
            no_cay = sum(cay),
            no_yac = sum(yac))

### ADVANCED RUSHING YARDS

rushing_clean <- all_rushing %>%
  select(season, tm, g, att, yds, ybc, yac) %>%
  mutate(g = as.integer(g),
         att = as.integer(att),
         yds = as.integer(yds),
         ybc = as.integer(ybc),
         yac = as.integer(yac)) %>%
  rename("rush_att" = "att",
         "rush_yds" = "yds",
         "rush_yds_bc" = "ybc",
         "rush_yds_ac" = "yac")

rushing_season <- rushing_clean %>%
  group_by(season) %>%
  summarise(no_games = sum(g),
            no_rush_att = sum(rush_att),
            no_rush_yds = sum(rush_yds),
            no_rush_yds_bc = sum(rush_yds_bc),
            no_rush_yds_ac = sum(rush_yds_ac))

### data downloaded from pro-football-reference.com
pre_data_pass <- tibble()

for (i in years_pre) {
  
  pass_data <- read.csv(file = str_c("pass_offense_", i, ".csv")) %>%
    clean_names() %>%
    mutate(season = i)
  
  pre_data_pass <- bind_rows(pre_data_pass, pass_data)

}

pre_pass_clean <- pre_data_pass %>%
  select(season, tm, g, cmp, att, yds, yds_1) %>%
  rename("sack_yds" = "yds_1")

pre_pass_season <- pre_pass_clean %>%
  group_by(season) %>%
  summarise(no_games = sum(g),
            no_pass_cmp = sum(cmp),
            no_pass_att = sum(att),
            no_pass_yds = sum(yds),
            no_sack_yds = sum(sack_yds))

passing_all <- bind_rows(pre_pass_season, air_yards_season)

pre_data_rush <- tibble()

for (i in years_pre) {
  
  rush_data <- read.csv(file = str_c("rush_offense_", i, ".csv")) %>%
    clean_names() %>%
    mutate(season = i)
  
  pre_data_rush <- bind_rows(pre_data_rush, rush_data)
  
}

pre_rush_clean <- pre_data_rush %>%
  select(season, tm, g, att, yds) %>%
  rename("rush_att" = "att",
         "rush_yds" = "yds")

pre_rush_season <- pre_rush_clean %>%
  group_by(season) %>%
  summarise(no_games = sum(g),
            no_rush_att = sum(rush_att),
            no_rush_yds = sum(rush_yds))

rushing_all <- bind_rows(pre_rush_season, rushing_season)

all_yardage <- full_join(x = passing_all, y = rushing_all, by = c("season" = "season", "no_games" = "no_games"))

pace_of_play <- all_yardage %>%
  select(season, no_games, no_pass_att, no_rush_att) %>%
  mutate(plays_per_game = (no_pass_att + no_rush_att) / no_games)

yardage_composition <- all_yardage %>%
  mutate(season = season,
         plays_per_game = (no_pass_att + no_rush_att) / no_games,
         adj_pass_yds_p60 = (no_pass_yds + no_sack_yds) / no_games / plays_per_game * 60,
         a_pass_air_p60 = no_cay / no_games / plays_per_game * 60,
         b_pass_ground_p60 = no_yac / no_games / plays_per_game * 60,
         adj_rush_yds_p60 = if_else(season < 2018, no_rush_yds / no_games / plays_per_game * 60, NA),
         c_rush_pre_p60 = no_rush_yds_bc  / no_games / plays_per_game * 60,
         d_rush_post_p60 = no_rush_yds_ac  / no_games / plays_per_game * 60,
         .keep = "none")

yardage_final <- yardage_composition %>%
  pivot_longer(cols = c("adj_pass_yds_p60", "a_pass_air_p60", "b_pass_ground_p60", "adj_rush_yds_p60", "c_rush_pre_p60", "d_rush_post_p60"),
               names_to = "category",
               values_to = "value") %>%
  filter(!is.na(value) & season > 2017)

first_last <- yardage_final %>%
  filter(season == 2018 | season == 2024)

facet_labels <- c("a_pass_air_p60" = "<span style='color:#003069'>Pass yards: pre-catch</span>",
                  "b_pass_ground_p60" = "<span style='color:#ADBBCC'>Pass yards: post-catch</span>",
                  "c_rush_pre_p60" = "<span style='color:#d60303'>Rush yards: pre-contact</span>",
                  "d_rush_post_p60" = "<span style='color:#CC9999'>Rush yards: post-contact</span>")

nfl_navy_dark <- "#062040"
nfl_navy <- "#003069"
nfl_navy_desaturated <- "#405C80"
nfl_navy_light <- "#ADBBCC"
nfl_navy_fill <- "#ADBBCC20"
nfl_red <- "#d60303"
nfl_red_desaturated <- "#E68A8A"
nfl_red_light <- "#CC9999"
mid_grey <- "#E1E2E680"
light_grey <- "#F0F1F580"

rough_area <- ggplot(yardage_final, aes(x = season, y = value)) +
  geom_ribbon(aes(ymin = 0, ymax = value, fill = category)) +
  geom_path(linewidth = 1.5, aes(colour = category)) +
  geom_point(size = 2.5,
            data = first_last,
            aes(colour = category)) +
  scale_y_continuous(limits = c(0, 160),
                     breaks = seq(0, 150, 50),
                     expand = c(0,0)) +
  scale_x_continuous(limits = c(2016.5, 2025.5),
                     breaks = seq(2018, 2024, 3),
                     expand = c(0,0)) +
  scale_colour_manual(values = c("a_pass_air_p60" = nfl_navy,
                                 "b_pass_ground_p60" = nfl_navy_light,
                                 "c_rush_pre_p60" = nfl_red,
                                 "d_rush_post_p60" = nfl_red_light)) +
  scale_fill_manual(values = c("a_pass_air_p60" = mid_grey,
                                 "b_pass_ground_p60" = light_grey,
                                 "c_rush_pre_p60" = mid_grey,
                                 "d_rush_post_p60" = light_grey)) +
  facet_rep_wrap(~category,
                 nrow = 2,
                 repeat.tick.labels =  TRUE,
                 labeller = labeller(category = facet_labels)
                 ) +
  theme(legend.position = "none",
        axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        axis.line.y = element_blank(),
        axis.ticks.y = element_blank())
rough_area

themed_area <- rough_area +
  theme_ptb()
themed_area

labelled_area <- themed_area +
  labs(title = "Teams are gaining fewer <span style='color:#003980'>air yards</span><br>and more <span style='color:#d60303'>pre-contact rushing yards</span>",
       subtitle = "Ave. number of <b>offensive yards</b> per 60 team plays<br>generated in each <b>phase</b> in the NFL since <b>2018</b>",
       caption = "<i>Excludes yards lost to sacks; data as at 20 Nov 2024</i><br><br>Data: Pro Football Reference | Chart: Plot the Ball")
labelled_area

air_yards_only <- all_yardage %>%
  filter(season > 2017) %>%
  select(season, no_games, no_pass_cmp, no_pass_att, no_iay, no_cay, no_rush_att) %>%
  mutate(plays_per_game = (no_pass_att + no_rush_att) / no_games,
         pass_att_p60 = no_pass_att / no_games / plays_per_game * 60,
         cay_per_cmp = no_cay / no_pass_cmp,
         iay_per_att = no_iay / no_pass_att,
         icay_per_icmp = (no_iay - no_cay) / (no_pass_att - no_pass_cmp),
         cmp_rate = no_pass_cmp / no_pass_att * 100)

ay_table <- air_yards_only %>%
  select(season, pass_att_p60, cmp_rate, cay_per_cmp, icay_per_icmp)

display_tbl <- ay_table %>%
  gt(rowname_col = "season") %>%
  cols_width(season ~ px(90),
             pass_att_p60 ~ px(100),
             cmp_rate ~ px(130),
             cay_per_cmp ~ px(140),
             icay_per_icmp ~ px(140))
display_tbl

tbl_formatted <- display_tbl %>%
  tab_header(title = html("<b>QBs no longer throw as far downfield — on either <b style='color:#086249'>completions</b> or <b style='color:#952548'>incompletions</b><br></b>"),
             subtitle = html("<b style='color:#086249'>Actual</b> air yards per <b style='color:#086249'>complete</b> pass and <b style='color:#952548'>intended</b> air yards per <b style='color:#952548'>incomplete</b> pass recorded by NFL QBs since <b>2018</b><br><br>")) %>%
  tab_source_note(source_note = html("<br>* Per 60 team plays<br><br><br><i>Data as at 20 Nov 2024</i><br><br>Data: Pro Football Reference | Table: Plot the Ball<br>")) %>%
  tab_stubhead(label = "Season") %>%
  tab_spanner(label = md("— AIR YARDS PER: —"),
              columns = c(cay_per_cmp, icay_per_icmp)) %>%
  fmt_number(columns = c(pass_att_p60, cay_per_cmp, icay_per_icmp), use_seps = TRUE, decimals = 1) %>%
  fmt_percent(columns = c(cmp_rate), decimals = 1, scale_values = FALSE) %>%
  cols_label(pass_att_p60 = "Attempts*",
             cmp_rate = "Completion %",
             cay_per_cmp = "Completion",
             icay_per_icmp = "Incompletion") %>%
  cols_align(align = "right",
             columns = everything()) %>%
  opt_table_font(font = ptb_font) %>%
  tab_options(table.border.top.width = px(0),
              table.border.bottom.width = px(0),
              heading.border.bottom.width = px(0),
              column_labels.border.bottom.width = px(0),
              column_labels.border.top.width = px(0),
              table_body.border.top.width = px(0),
              table_body.border.bottom.width = px(0),
              stub.border.width = px(2),
              stub.border.color = ptb_light_grey) %>%
  tab_style(locations = cells_title(groups = "title"),
            style = list(cell_text(align = "left",
                                   color = ptb_dark_grey,
                                   size = px(30)))) %>%
  tab_style(locations = cells_title(groups = "subtitle"),
            style = list(cell_text(align = "left",
                                   color = ptb_dark_grey,
                                   size = px(20)))) %>%
  tab_style(locations = list(cells_column_spanners(),
                             cells_column_labels(),
                             cells_stubhead()),
            style = list(cell_text(color = "#FFFFFF",
                                   weight = "bold",
                                   size = "large"),
                         cell_fill(color = nfl_navy_dark))) %>%
  tab_style(locations = cells_stub(rows = TRUE),
            style =  list(cell_text(align = "left",
                                    color = ptb_dark_grey,
                                    size = "large"),
                          cell_borders(sides = c("top", "bottom"),
                                       weight = px(1),
                                       color = ptb_light_grey))) %>%
  tab_style(locations = cells_column_labels(columns = c(cay_per_cmp, icay_per_icmp)),
            style = list(cell_text(align = "center"))) %>%
  tab_style(locations = cells_body(columns = c(cay_per_cmp, icay_per_icmp)),
            style = list(cell_text(color = ptb_dark_grey,
                                   size = "large",
                                   align = "center"),
                         cell_borders(sides = c("top", "bottom"),
                                      weight = px(1),
                                      color = ptb_light_grey))) %>%
  tab_style(locations = cells_body(rows = season == 2024,
                                   columns = c(cay_per_cmp, icay_per_icmp)),
            style = list(cell_text(weight = "bold"),
                         cell_fill(color = light_grey))) %>%
  tab_style(locations = cells_body(rows = season == 2024),
            style = list(cell_fill(color = nfl_navy_fill))) %>%
  tab_style(locations = cells_stub(rows = season == 2024),
            style = list(cell_text(weight = "bold"),
                         cell_fill(color = nfl_navy_fill))) %>%
  tab_style(locations = cells_body(columns = c(cay_per_cmp)),
                         cell_borders(sides = c("left"),
                                      weight = px(1.5),
                                      color = ptb_light_grey)) %>%
  tab_style(locations = cells_body(columns = c(pass_att_p60, cmp_rate)),
            style = list(cell_text(color = ptb_mid_grey,
                                   size = "large"),
                         cell_borders(sides = c("top", "bottom"),
                                      weight = px(1),
                                      color = ptb_light_grey))) %>%
  tab_style(locations = cells_source_notes(),
            style = list(cell_text(align = "right",
                                   color = ptb_mid_grey,
                                   size = "medium"))) %>%
  gt_color_pills(columns = cay_per_cmp,
                 digits = 1,
                 pill_height = 25,
                 palette = c("#A3DCCC", "#053D2E"),
                 domain = c(min(ay_table$cay_per_cmp) - 0.1, max(ay_table$cay_per_cmp) + 0.1)) %>%
  gt_color_pills(columns = icay_per_icmp,
                 digits = 1,
                 pill_height = 25,
                 palette = c("#F0C4D1", "#4D1325"),
                 domain = c(min(ay_table$icay_per_icmp) - 0.1, max(ay_table$icay_per_icmp) + 0.1))
tbl_formatted
