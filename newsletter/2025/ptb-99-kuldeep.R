library(tidyverse)
library(rvest)
library(janitor)
library(lemon)
library(scales)
library(ggtext)
library(gt)
library(gtUtils)
library(gtExtras)

away_url_base <- "https://stats.espncricinfo.com/ci/engine/stats/index.html?bowling_pacespin=2;class=1;filter=advanced;home_or_away=2;home_or_away=3;orderby=start;page="
first_page <- 1
away_url_end <- ";size=200;spanmin2=1+Aug+2019;spanval2=span;template=results;type=bowling;view=innings;wrappertype=print"
away_url_full <- str_c(away_url_base, first_page, away_url_end)

away_no_pages_source <- read_html(away_url_full) %>%
  html_node(xpath = "/html/body/div/div[3]/table[2]") %>%
  html_table()
away_no_pages <- unlist(away_no_pages_source[1,1]) %>%
  str_replace(., "Page 1 of ", "") %>%
  as.numeric()

away_rank <- seq(first_page, away_no_pages, 1)

away_links <- str_c(away_url_base, away_rank, away_url_end)

away_bowling_data <- tibble()

for (i in away_links) {
  print(Sys.time())
  Sys.sleep(2)
  
  table_data <- read_html(i) %>%
    html_node(xpath = "/html/body/div/div[3]/table[3]") %>%
    html_table() %>%
    clean_names()
  
  table_data$overs <- as.character(table_data$overs)
  table_data$mdns <- as.character(table_data$mdns)
  table_data$runs <- as.character(table_data$runs)
  table_data$wkts <- as.character(table_data$wkts)
  table_data$econ <- as.character(table_data$econ)
  table_data$inns <- as.character(table_data$inns)
  away_bowling_data <- bind_rows(away_bowling_data, table_data)
}

home_url_base <- "https://stats.espncricinfo.com/ci/engine/stats/index.html?bowling_pacespin=2;class=1;filter=advanced;home_or_away=1;orderby=start;page="
first_page <- 1
home_url_end <- ";size=200;spanmin2=1+Aug+2019;spanval2=span;template=results;type=bowling;view=innings;wrappertype=print"
home_url_full <- str_c(home_url_base, first_page, home_url_end)

home_no_pages_source <- read_html(home_url_full) %>%
  html_node(xpath = "/html/body/div/div[3]/table[2]") %>%
  html_table()
home_no_pages <- unlist(home_no_pages_source[1,1]) %>%
  str_replace(., "Page 1 of ", "") %>%
  as.numeric()

home_rank <- seq(first_page, home_no_pages, 1)

home_links <- str_c(home_url_base, home_rank, home_url_end)

home_bowling_data <- tibble()

for (i in home_links) {
  print(Sys.time())
  Sys.sleep(2)
  
  table_data <- read_html(i) %>%
    html_node(xpath = "/html/body/div/div[3]/table[3]") %>%
    html_table() %>%
    clean_names()
  
  table_data$overs <- as.character(table_data$overs)
  table_data$mdns <- as.character(table_data$mdns)
  table_data$runs <- as.character(table_data$runs)
  table_data$wkts <- as.character(table_data$wkts)
  table_data$econ <- as.character(table_data$econ)
  table_data$inns <- as.character(table_data$inns)
  home_bowling_data <- bind_rows(home_bowling_data, table_data)
}

away_bowling_data$home_away <- "Away"
home_bowling_data$home_away <- "Home"

spin_bowling_data <- bind_rows(away_bowling_data, home_bowling_data)

spin_all_inns_clean <- spin_bowling_data %>%
  select(1, 2, 4, 5, 9, 10, 11, 12) %>%
  filter(overs != "DNB" & overs != "TDNB" & overs != "sub") %>%
  mutate(match_id = str_c(ground, " (", start_date, ")"),
         .before = player) %>%
  separate(col = overs,
           into = c("completed_overs", "completed_balls"),
           sep = "[.]")

spin_all_inns_clean$start_date <- dmy(spin_all_inns_clean$start_date)

spin_all_inns_clean$completed_overs <- as.integer(spin_all_inns_clean$completed_overs)
spin_all_inns_clean$completed_balls <- as.integer(replace_na(spin_all_inns_clean$completed_balls, 0))
spin_all_inns_clean$runs <- as.integer(spin_all_inns_clean$runs)
spin_all_inns_clean$wkts <- as.integer(spin_all_inns_clean$wkts)

spin_all_inns_clean$player <- str_replace_all(spin_all_inns_clean$player, fixed("(1)"), "I")

spin_all_inns_clean <- spin_all_inns_clean %>%
  mutate(balls_bowled = completed_balls + (6 * completed_overs),
         .before = runs) %>%
  select(-completed_overs, -completed_balls) %>%
  separate(player, into = c("name", "country"), sep = "[(]") %>%
  mutate(name = str_squish(name),
         country = str_squish(str_remove_all(country, fixed(")")))) %>%
  arrange(start_date) %>%
  filter(country != "AFG" & country != "IRE" & country != "ZIM" & opposition != "v Afghanistan" & opposition != "v Ireland" & opposition != "v Zimbabwe")

spin_match_splits <- spin_all_inns_clean %>%
  group_by(match_id, start_date) %>%
  summarise(tot_balls = sum(balls_bowled),
            tot_runs = sum(runs),
            tot_wkts = sum(wkts))

spin_player_match_splits <- spin_all_inns_clean %>%
  group_by(match_id, start_date, name, country) %>%
  summarise(n_balls = sum(balls_bowled),
            n_runs = sum(runs),
            n_wkts = sum(wkts))

spin_vs_exp <- left_join(x = spin_player_match_splits, y = spin_match_splits, by = c("match_id", "start_date"))

spin_vs_exp <- spin_vs_exp %>%
  mutate(cycle = case_when(start_date <= ymd("2021-06-18") ~ "WTC 2021",
                           start_date <= ymd("2023-06-07") ~ "WTC 2023",
                           start_date <= ymd("2025-06-11") ~ "WTC 2025",
                           TRUE ~ "WTC 2027"),
         other_balls = tot_balls - n_balls,
         other_runs = tot_runs - n_runs,
         other_wkts = tot_wkts - n_wkts) %>%
  select(-8, -9, -10) %>%
  relocate(cycle, .before = name) %>%
  arrange(start_date)

spin_vs_exp_summary <- spin_vs_exp %>%
  group_by(name, country) %>%
  summarise(n_matches = n(),
            n_balls = sum(n_balls),
            n_overs = n_balls / 6,
            n_runs = sum(n_runs),
            n_wkts = sum(n_wkts),
            other_balls = sum(other_balls),
            other_runs = sum(other_runs),
            other_wkts = sum(other_wkts)) %>%
  mutate(balls_per_match = n_balls / n_matches,
         wkts_per_match = n_wkts / n_matches,
         ave = n_runs / n_wkts,
         sr = n_balls / n_wkts,
         other_ave = other_runs / other_wkts,
         other_sr = other_balls / other_wkts,
         ave_vs_exp = ave - other_ave,
         sr_vs_exp = sr - other_sr) %>%
  arrange(desc(n_wkts))

spin_vs_exp_final <- spin_vs_exp_summary %>%
  filter(n_balls >= (200 * 6) & balls_per_match >= 120) %>%
  select(name, country, n_balls, ave, other_ave, ave_vs_exp, sr, other_sr, sr_vs_exp) %>%
  arrange(ave_vs_exp)

for_table <- spin_vs_exp_final %>%
  filter(country == "IND")

names_fix <- tibble(name = for_table$name,
                    bowler = c("Kuldeep Yadav", "Ravichandran Ashwin", "Axar Patel", "Ravindra Jadeja", "Washington Sundar"))

for_table <- left_join(x = for_table, y = names_fix)

final_table <- for_table %>%
  select(bowler, n_balls, ave, other_ave, ave_vs_exp, sr, other_sr, sr_vs_exp) %>%
  arrange(ave_vs_exp)

display_tbl <- final_table %>%
  gt(rowname_col = "bowler",
     groupname_col = NA) %>%
  cols_hide(columns = "name") %>%
  cols_width(bowler ~ px(190),
             n_balls ~ px(60),
             ave ~ px(70),
             other_ave ~ px(70),
             ave_vs_exp ~ px(60),
             sr ~ px(70),
             other_sr ~ px(70),
             sr_vs_exp ~ px(60))
display_tbl

header_colour <- "#79807E"
ind_colour <- "#004C7B"
highlight_ind <- str_c(ind_colour, "07")

tbl_formatted <- display_tbl %>%
  tab_header(title = html("<b style='color:#004C7B'>Kuldeep</b> has a much better record than the other spinners in matches he's played"),
             subtitle = html("Performance of <b>Indian spin bowlers in the WTC era</b>, compared to the record of other spinners in the same games<br><br>")) %>%
  tab_source_note(source_note = html("<br>'Average' = runs conceded per wicket | 'Strike rate' = balls bowled (BB) per wicket<br><br>'Other' = other spin bowlers in a player's matches<br><br>'WTC era' began on 1 Aug 2019 | Excl. matches vs. AFG, IRE and ZIM<br><br>Data: ESPNCricinfo | Chart: Plot the Ball<br>")) %>%
  tab_stubhead(label = "") %>%
  tab_spanner(label = md("— AVERAGE —"),
              columns = c(ave, other_ave, ave_vs_exp)) %>%
  tab_spanner(label = md("— STRIKE RATE —"),
              columns = c(sr, other_sr, sr_vs_exp)) %>%
  fmt_number(columns = c(n_balls), use_seps = TRUE, decimals = 0) %>%
  fmt_number(columns = c(ave, other_ave, sr, other_sr), use_seps = TRUE, decimals = 1) %>%
  fmt_number(columns = c(ave_vs_exp, sr_vs_exp), use_seps = TRUE, decimals = 1, force_sign = TRUE) %>%
  cols_label(n_balls = "BB",
             ave = "Bowler",
             other_ave = "Other",
             ave_vs_exp = "▲ +/-",
             sr = "Bowler",
             other_sr = "Other",
             sr_vs_exp = "+/-",) %>%
  cols_align(align = "right",
             columns = everything()) %>%
  cols_align(align = "center",
             columns = c("n_balls")) %>%
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
                                   weight = "bold",
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
                         cell_fill(color = header_colour))) %>%
  tab_style(locations = cells_stub(rows = TRUE),
            style =  list(cell_text(align = "left",
                                    color = ptb_mid_grey,
                                    size = "large"),
                          cell_borders(sides = c("top", "bottom"),
                                       weight = px(1),
                                       color = ptb_light_grey))) %>%
  tab_style(locations = cells_body(),
            style = list(cell_text(color = ptb_mid_grey,
                                   size = "large"),
                         cell_borders(sides = c("top", "bottom"),
                                      weight = px(1),
                                      color = ptb_light_grey))) %>%
  tab_style(locations = cells_source_notes(),
            style = list(cell_text(align = "right",
                                   color = ptb_mid_grey,
                                   size = "medium"))) %>%
  tab_style(locations = cells_body(columns = c(ave, sr)),
            style = list(cell_borders(sides = c("left"),
                                      weight = px(2),
                                      color = ptb_light_grey))) %>%
  tab_style(locations = cells_body(columns = c(ave_vs_exp, sr_vs_exp)),
            style = list(cell_borders(sides = c("left"),
                                      weight = px(1.5),
                                      color = ptb_light_grey_v4))) %>%
  tab_style(locations = cells_body(rows = bowler == "Kuldeep Yadav"),
            style = list(cell_text(color = ind_colour,
                                   weight = "bold"),
                         cell_fill(color = highlight_ind))) %>%
  tab_style(locations = cells_stub(rows = bowler == "Kuldeep Yadav"),
            style = list(cell_text(color = ind_colour,
                                   weight = "bold"),
                         cell_fill(color = highlight_ind)))
tbl_formatted