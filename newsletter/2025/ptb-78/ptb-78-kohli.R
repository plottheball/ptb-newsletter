library(tidyverse)
library(rvest)
library(janitor)
library(gt)
library(gtUtils)
library(gtExtras)

top_url_base <- "https://stats.espncricinfo.com/ci/engine/stats/index.html?batting_positionmax1=3;batting_positionmin1=1;batting_positionval1=batting_position;class=2;filter=advanced;orderby=start;page="
first_page <- 1
top_url_end <- ";size=200;spanval1=span;template=results;type=batting;view=innings;wrappertype=print"
top_url_full <- str_c(top_url_base, first_page, top_url_end)

top_no_pages_source <- read_html(top_url_full) %>%
  html_node(xpath = "/html/body/div/div[3]/table[2]") %>%
  html_table()
top_no_pages <- unlist(top_no_pages_source[1,1]) %>%
  str_replace(., "Page 1 of ", "") %>%
  as.numeric()

top_rank <- seq(first_page, top_no_pages, 1)

top_links <- str_c(top_url_base, top_rank, top_url_end)

top_batting_data <- tibble()

for (i in top_links) {
  print(Sys.time())
  Sys.sleep(1)
  
  table_data <- read_html(i) %>%
    html_node(xpath = "/html/body/div/div[3]/table[3]") %>%
    html_table() %>%
    clean_names()
  
  # combine with existing record (with re-classifications to prevent 'Can't combine...' errors)
  table_data$runs <- as.character(table_data$runs)
  table_data$mins <- as.character(table_data$mins)
  table_data$bf <- as.character(table_data$bf)
  table_data$x4s <- as.character(table_data$x4s)
  table_data$x6s <- as.character(table_data$x6s)
  table_data$sr <- as.character(table_data$sr)
  table_data$inns <- as.character(table_data$inns)
  top_batting_data <- bind_rows(top_batting_data, table_data)
}

top_batting_data$player <- str_replace_all(top_batting_data$player, fixed("(1)"), "I")
top_batting_data$player <- str_replace_all(top_batting_data$player, fixed("(2)"), "II")
top_batting_data$player <- str_replace_all(top_batting_data$player, fixed("(3)"), "III")
top_batting_data$player <- str_replace_all(top_batting_data$player, fixed("(4)"), "IV")
top_batting_data$player <- str_replace_all(top_batting_data$player, fixed("(5)"), "V")

top_all_inns_clean <- top_batting_data %>%
  select(1, 2, 4, 10, 11, 12) %>%
  filter(runs != "DNB" & runs != "TDNB") %>%
  mutate(match_id = str_c(ground, " (", start_date, ")"),
         start_date = dmy(start_date),
         .before = player) %>%
  separate(player, into = c("name", "country"), sep = "[(]") %>%
  mutate(name = str_squish(name),
         country = str_squish(str_remove_all(country, fixed(")"))))

top_all_inns_clean$dismissed <- if_else(str_detect(top_all_inns_clean$runs, fixed("*")), 0, 1)
top_all_inns_clean$runs <- str_remove_all(top_all_inns_clean$runs, fixed("*"))
top_all_inns_clean$runs <- as.integer(top_all_inns_clean$runs)
top_all_inns_clean$bf <- as.integer(top_all_inns_clean$bf)

team_list <- unique(unlist(top_all_inns_clean$country))
test_teams <- c("AUS", "WI", "PAK", "ZIM", "BAN", "SA", "ENG", "NZ", "IND", "SL")
opposition_list <- unique(unlist(top_all_inns_clean$opposition))
test_opposition <- c("v West Indies", "v Australia", "v Pakistan", "v Bangladesh", "v Zimbabwe",
                     "v England", "v South Africa", "v New Zealand", "v India", "v Sri Lanka")

top_all_inns_clean <- top_all_inns_clean %>%
  filter(country %in% test_teams & opposition %in% test_opposition)

top_match_splits <- top_all_inns_clean %>%
  group_by(match_id) %>%
  summarise(tot_inns = n(),
            tot_runs = sum(runs),
            tot_balls = sum(bf),
            tot_dismissals = sum(dismissed))

top_player_match_splits <- top_all_inns_clean %>%
  group_by(match_id, name, country) %>%
  summarise(n_inns = n(),
            n_runs = sum(runs),
            n_balls = sum(bf),
            n_dismissals = sum(dismissed))

top_runs_vs_exp <- left_join(x = top_player_match_splits, y = top_match_splits, by = c("match_id"))

top_runs_vs_exp <- top_runs_vs_exp %>%
  mutate(other_inns = tot_inns - n_inns,
         other_runs = tot_runs - n_runs,
         other_balls = tot_balls - n_balls,
         other_dismissals = tot_dismissals - n_dismissals) %>%
  select(-8, -9, -10, -11)

top_vs_exp_summary <- top_runs_vs_exp %>%
  group_by(name, country) %>%
  summarise(n_inns = sum(n_inns),
            n_runs = sum(n_runs),
            n_balls = sum(n_balls),
            n_dismissals = sum(n_dismissals),
            other_inns = sum(other_inns),
            other_runs = sum(other_runs),
            other_balls = sum(other_balls),
            other_dismissals = sum(other_dismissals)) %>%
  mutate(ave = n_runs / n_dismissals,
         sr = n_runs / n_balls * 100,
         other_ave = other_runs / other_dismissals,
         other_sr = other_runs / other_balls * 100,
         ave_vs_exp = ave - other_ave,
         sr_vs_exp = sr - other_sr) %>%
  arrange(desc(n_inns))

top_vs_exp_final <- top_vs_exp_summary %>%
  filter(n_inns >= 60 & n_balls >= (60 * 50))

active_players_top <- top_all_inns_clean %>%
  filter(start_date > dmy("20-11-2023")) %>%
  select(name) %>%
  unique() %>%
  unlist()

top_vs_exp_active <- top_vs_exp_final %>%
  filter(name %in% active_players_top)

names_fix <- tibble(name = final_for_table$name,
                    batter = c("Virat Kohli", "Rohit Sharma", "Kane Williamson", "Babar Azam", "Joe Root", "Steven Smith", "Kusal Mendis", "Fakhar Zaman", "Shai Hope", "Imam-ul-Haq"))

top_vs_exp_active <- left_join(x = top_vs_exp_active, y = names_fix)

final_table <- top_vs_exp_active %>%
  select(batter, n_inns, ave, other_ave, ave_vs_exp, sr, other_sr, sr_vs_exp) %>%
  arrange(desc(sr_vs_exp))

display_tbl <- final_table %>%
  gt(rowname_col = "batter",
     groupname_col = NA) %>%
  cols_hide(columns = "name") %>%
  cols_width(batter ~ px(150),
             n_inns ~ px(50),
             ave ~ px(75),
             other_ave ~ px(75),
             ave_vs_exp ~ px(75),
             sr ~ px(75),
             other_sr ~ px(75),
             sr_vs_exp ~ px(75))
display_tbl

header_colour <- "#79807E"
ind_colour <- "#5B85AA"
eng_colour <-  "#E07A5F"
aus_colour <- "#E6C35C"
nzl_colour <- "#141134"
highlight_ind <- str_c(ind_colour, "20")
highlight_eng <- str_c(eng_colour, "10")
highlight_aus <- str_c(aus_colour, "10")
highlight_nzl <- str_c(nzl_colour, "10")

tbl_formatted <- display_tbl %>%
  tab_header(title = html("<b style='color:#5B85AA'>Virat Kohli</b> has scored his ODI runs much faster than <b style='color:#141134'>Williamson</b>, <b style='color:#E6C35C'>Smith</b> and <b style='color:#E07A5F'>Root</b>"),
             subtitle = html("Career performance of <b>active top-order batters</b> in men's ODIs, compared to the record of <b>other top-order batters in their games</b><br><br>")) %>%
  tab_source_note(source_note = html("<br>'Other' = performance of other top-order batters (i.e. positions 1-3) in a player's games<br><br>'Average' = runs scored per dismissal | 'Strike rate' = runs scored per 100 balls<br><br><i>Players with >60 innings and >3,000 balls faced in their ODI careers only; excludes those not active in current ODI World Cup cycle (i.e. since Dec 2023)</i><br><br>Data: ESPNCricinfo | Table: Plot the Ball<br>")) %>%
  tab_stubhead(label = "") %>%
  tab_spanner(label = md("— AVERAGE —"),
              columns = c(ave, other_ave, ave_vs_exp)) %>%
  tab_spanner(label = md("— STRIKE RATE —"),
              columns = c(sr, other_sr, sr_vs_exp)) %>%
  fmt_number(columns = c(ave, other_ave, sr, other_sr), use_seps = TRUE, decimals = 1) %>%
  fmt_number(columns = c(ave_vs_exp, sr_vs_exp), use_seps = TRUE, decimals = 1, force_sign = TRUE) %>%
  cols_label(n_inns = "Inns",
             ave = "Batter",
             other_ave = "Other",
             ave_vs_exp = "+/-",
             sr = "Batter",
             other_sr = "Other",
             sr_vs_exp = "▼ +/-",) %>%
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
  tab_style(locations = cells_body(rows = batter == "Virat Kohli"),
            style = list(cell_text(color = ind_colour,
                                   weight = "bold"),
                         cell_fill(color = highlight_ind))) %>%
  tab_style(locations = cells_stub(rows = batter == "Virat Kohli"),
            style = list(cell_text(color = ind_colour,
                                   weight = "bold"),
                         cell_fill(color = highlight_ind))) %>%
  tab_style(locations = cells_body(rows = batter == "Kane Williamson"),
            style = list(cell_text(color = nzl_colour),
                         cell_fill(color = highlight_nzl))) %>%
  tab_style(locations = cells_stub(rows = batter == "Kane Williamson"),
            style = list(cell_text(color = nzl_colour),
                         cell_fill(color = highlight_nzl))) %>%
  tab_style(locations = cells_body(rows = batter == "Steven Smith"),
            style = list(cell_text(color = aus_colour),
                         cell_fill(color = highlight_aus))) %>%
  tab_style(locations = cells_stub(rows = batter == "Steven Smith"),
            style = list(cell_text(color = aus_colour),
                         cell_fill(color = highlight_aus))) %>%
  tab_style(locations = cells_body(rows = batter == "Joe Root"),
            style = list(cell_text(color = eng_colour),
                         cell_fill(color = highlight_eng))) %>%
  tab_style(locations = cells_stub(rows = batter == "Joe Root"),
            style = list(cell_text(color = eng_colour),
                         cell_fill(color = highlight_eng)))
tbl_formatted
