library(tidyverse)
library(rvest)
library(janitor)
library(scales)
library(ggtext)
library(gt)
library(gtUtils)
library(gtExtras)


### TOP-ORDER (1-3) BATTERS IN HOME TESTS IN WTC ERA
top_url_base_home <- "https://stats.espncricinfo.com/ci/engine/stats/index.html?batting_positionmax1=3;batting_positionmin1=1;batting_positionval1=batting_position;class=1;filter=advanced;home_or_away=1;orderby=start;page="
first_page <- 1
top_url_end_home <- ";size=200;spanmin1=1+Aug+2019;spanval1=span;template=results;type=batting;view=innings;wrappertype=print"
top_url_full_home <- str_c(top_url_base_home, first_page, top_url_end_home)

top_no_pages_source_home <- read_html(top_url_full_home) %>%
  html_node(xpath = "/html/body/div/div[3]/table[2]") %>%
  html_table()
top_no_pages_home <- unlist(top_no_pages_source_home[1,1]) %>%
  str_replace(., "Page 1 of ", "") %>%
  as.numeric()

top_rank_home <- seq(first_page, top_no_pages_home, 1)

top_links_home <- str_c(top_url_base_home, top_rank_home, top_url_end_home)

top_data_home <- tibble()

for (i in top_links_home) {
  print(Sys.time())
  Sys.sleep(2)
  
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
  top_data_home <- bind_rows(top_data_home, table_data)
}

top_data_home$category <- "Home"
top_data_home$position <- "Top"

### TOP-ORDER (1-3) BATTERS IN AWAY/NEUTRAL TESTS IN WTC ERA
top_url_base_away <- "https://stats.espncricinfo.com/ci/engine/stats/index.html?batting_positionmax1=3;batting_positionmin1=1;batting_positionval1=batting_position;class=1;filter=advanced;home_or_away=2;home_or_away=3;orderby=start;page="
first_page <- 1
top_url_end_away <- ";size=200;spanmin1=1+Aug+2019;spanval1=span;template=results;type=batting;view=innings;wrappertype=print"
top_url_full_away <- str_c(top_url_base_away, first_page, top_url_end_away)

top_no_pages_source_away <- read_html(top_url_full_away) %>%
  html_node(xpath = "/html/body/div/div[3]/table[2]") %>%
  html_table()
top_no_pages_away <- unlist(top_no_pages_source_away[1,1]) %>%
  str_replace(., "Page 1 of ", "") %>%
  as.numeric()

top_rank_away <- seq(first_page, top_no_pages_away, 1)

top_links_away <- str_c(top_url_base_away, top_rank_away, top_url_end_away)

top_data_away <- tibble()

for (i in top_links_away) {
  print(Sys.time())
  Sys.sleep(2)
  
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
  top_data_away <- bind_rows(top_data_away, table_data)
}

top_data_away$category <- "Away"
top_data_away$position <- "Top"

### MIDDLE-ORDER (4-6) BATTERS IN HOME TESTS IN WTC ERA
mid_url_base_home <- "https://stats.espncricinfo.com/ci/engine/stats/index.html?batting_positionmax1=6;batting_positionmin1=4;batting_positionval1=batting_position;class=1;filter=advanced;home_or_away=1;orderby=start;page="
first_page <- 1
mid_url_end_home <- ";size=200;spanmin1=1+Aug+2019;spanval1=span;template=results;type=batting;view=innings;wrappertype=print"
mid_url_full_home <- str_c(mid_url_base_home, first_page, mid_url_end_home)

mid_no_pages_source_home <- read_html(mid_url_full_home) %>%
  html_node(xpath = "/html/body/div/div[3]/table[2]") %>%
  html_table()
mid_no_pages_home <- unlist(mid_no_pages_source_home[1,1]) %>%
  str_replace(., "Page 1 of ", "") %>%
  as.numeric()

mid_rank_home <- seq(first_page, mid_no_pages_home, 1)

mid_links_home <- str_c(mid_url_base_home, mid_rank_home, mid_url_end_home)

mid_data_home <- tibble()

for (i in mid_links_home) {
  print(Sys.time())
  Sys.sleep(2)
  
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
  mid_data_home <- bind_rows(mid_data_home, table_data)
}

mid_data_home$category <- "Home"
mid_data_home$position <- "Middle"

### MIDDLE-ORDER (4-6) BATTERS IN AWAY/NEUTRAL TESTS IN WTC ERA
mid_url_base_away <- "https://stats.espncricinfo.com/ci/engine/stats/index.html?batting_positionmax1=6;batting_positionmin1=4;batting_positionval1=batting_position;class=1;filter=advanced;home_or_away=2;home_or_away=3;orderby=start;page="
first_page <- 1
mid_url_end_away <- ";size=200;spanmin1=1+Aug+2019;spanval1=span;template=results;type=batting;view=innings;wrappertype=print"
mid_url_full_away <- str_c(mid_url_base_away, first_page, mid_url_end_away)

mid_no_pages_source_away <- read_html(mid_url_full_away) %>%
  html_node(xpath = "/html/body/div/div[3]/table[2]") %>%
  html_table()
mid_no_pages_away <- unlist(mid_no_pages_source_away[1,1]) %>%
  str_replace(., "Page 1 of ", "") %>%
  as.numeric()

mid_rank_away <- seq(first_page, mid_no_pages_away, 1)

mid_links_away <- str_c(mid_url_base_away, mid_rank_away, mid_url_end_away)

mid_data_away <- tibble()

for (i in mid_links_away) {
  print(Sys.time())
  Sys.sleep(2)
  
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
  mid_data_away <- bind_rows(mid_data_away, table_data)
}

mid_data_away$category <- "Away"
mid_data_away$position <- "Middle"

### COMBINE ALL DATA

all_data <- bind_rows(top_data_home, top_data_away, mid_data_home, mid_data_away)

all_data$player <- str_replace_all(all_data$player, fixed("(1)"), "I")
all_data$player <- str_replace_all(all_data$player, fixed("(2)"), "II")
all_data$player <- str_replace_all(all_data$player, fixed("(3)"), "III")

all_inns_clean <- all_data %>%
  select(13, 14, 1, 2, 4, 10, 11, 12) %>%
  filter(runs != "DNB" & runs != "TDNB") %>%
  mutate(match_id = str_c(ground, " (", start_date, ")"),
         start_date = dmy(start_date),
         .before = player) %>%
  separate(player, into = c("name", "country"), sep = "[(]") %>%
  mutate(name = str_squish(name),
         country = str_squish(str_remove_all(country, fixed(")"))))

all_inns_clean$dismissed <- if_else(str_detect(all_inns_clean$runs, fixed("*")), 0, 1)
all_inns_clean$runs <- str_remove_all(all_inns_clean$runs, fixed("*"))
all_inns_clean$runs <- as.integer(all_inns_clean$runs)
all_inns_clean$bf <- as.integer(all_inns_clean$bf)

all_inns_clean <- all_inns_clean %>%
  filter(country != "AFG" & country != "IRE" & country != "ZIM" & opposition != "v Afghanistan" & opposition != "v Ireland" & opposition != "v Zimbabwe")

match_splits <- all_inns_clean %>%
  group_by(match_id, position) %>%
  summarise(tot_inns = n(),
            tot_runs = sum(runs),
            tot_balls = sum(bf),
            tot_dismissals = sum(dismissed))

player_match_splits <- all_inns_clean %>%
  group_by(match_id, name, country, position) %>%
  summarise(n_inns = n(),
            n_runs = sum(runs),
            n_balls = sum(bf),
            n_dismissals = sum(dismissed))

runs_vs_exp <- left_join(x = player_match_splits, y = match_splits, by = c("match_id", "position"))

runs_vs_exp <- runs_vs_exp %>%
  mutate(other_inns = tot_inns - n_inns,
         other_runs = tot_runs - n_runs,
         other_balls = tot_balls - n_balls,
         other_dismissals = tot_dismissals - n_dismissals) %>%
  select(-9, -10, -11, -12)

vs_exp_final <- runs_vs_exp %>%
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
  arrange(desc(n_inns)) %>%
  filter(n_inns >= 40)

eng_batters <- vs_exp_final %>%
  filter(country == "ENG") %>%
  arrange(desc(ave_vs_exp)) %>%
  select(1, 2, 3, 11, 13, 15, 12, 14, 16)

for_table <- vs_exp_final %>%
  arrange(desc(ave_vs_exp)) %>%
  head(6) %>%
  select(1, 3, 11, 13, 15, 12, 14, 16)

names_fix <- tibble(name = for_table$name,
                    batter = c("Kane Williamson", "Joe Root", "Harry Brook", "Steven Smith", "Yashasvi Jaiswal", "Marnus Labuschagne"))

for_table <- left_join(x = for_table, y = names_fix)

final_table <- for_table %>%
  select(batter, n_inns, ave, other_ave, ave_vs_exp, sr, other_sr, sr_vs_exp) %>%
  arrange(desc(ave_vs_exp))

display_tbl <- final_table %>%
  gt(rowname_col = "batter",
     groupname_col = NA) %>%
  cols_hide(columns = "name") %>%
  cols_width(batter ~ px(200),
             n_inns ~ px(50),
             ave ~ px(70),
             other_ave ~ px(70),
             ave_vs_exp ~ px(60),
             sr ~ px(70),
             other_sr ~ px(70),
             sr_vs_exp ~ px(60))
display_tbl

header_colour <- "#79807E"
eng_colour <- "#2E6399"
eng_colour_v2 <-  "#004080"
highlight_eng <- str_c(eng_colour, "07")
highlight_eng_v2 <- str_c(eng_colour_v2, "14")

tbl_formatted <- display_tbl %>%
  tab_header(title = html("<b style='color:#004D99'>Harry Brook</b> has scored his test runs much faster than other standout top-six batters"),
             subtitle = html("Performance of <b>top-six batters in the WTC era</b>, compared to the record of other batters in the same role in the same games<br><br>")) %>%
  tab_source_note(source_note = html("<br>'Average' = runs scored per dismissal | 'Strike rate' = runs scored per 100 balls<br><br>'Other' = batters in same role (i.e. top- or middle-order) in a player's matches<br><br>'WTC era' began on 1 Aug 2019 | Excl. matches vs. AFG, IRE and ZIM<br><br>Data: ESPNCricinfo | Chart: Plot the Ball<br>")) %>%
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
             ave_vs_exp = "▼ +/-",
             sr = "Batter",
             other_sr = "Other",
             sr_vs_exp = "+/-",) %>%
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
  tab_style(locations = cells_body(columns = c(ave_vs_exp, sr_vs_exp)),
            style = list(cell_borders(sides = c("left"),
                                      weight = px(1.5),
                                      color = ptb_light_grey_v4))) %>%
  tab_style(locations = cells_body(rows = batter == "Joe Root"),
            style = list(cell_text(color = eng_colour),
                         cell_fill(color = highlight_eng))) %>%
  tab_style(locations = cells_stub(rows = batter == "Joe Root"),
            style = list(cell_text(color = eng_colour),
                         cell_fill(color = highlight_eng))) %>%
  tab_style(locations = cells_body(rows = batter == "Harry Brook"),
            style = list(cell_text(color = eng_colour_v2,
                                   weight = "bold"),
                         cell_fill(color = highlight_eng_v2))) %>%
  tab_style(locations = cells_stub(rows = batter == "Harry Brook"),
            style = list(cell_text(color = eng_colour_v2,
                                   weight = "bold"),
                         cell_fill(color = highlight_eng_v2)))
tbl_formatted
