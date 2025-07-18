library(tidyverse)
library(rvest)
library(janitor)
library(scales)
library(ggtext)

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
  filter(n_inns > 40)

inactive <- c("CA Pujara", "AM Rahane", "V Kohli", "RG Sharma")

for_plot <- vs_exp_final %>%
  filter(country == "IND") %>%
  select(name, ave_vs_exp) %>%
  mutate(category = case_when(name == "Shubman Gill" ~ "Focus",
                              name %in% inactive ~ "Other",
                              TRUE ~ "Active")) %>%
  arrange(desc(ave_vs_exp))

labels <- tibble(name = c("YBK Jaiswal", "RR Pant", "Shubman Gill", "RG Sharma", "KL Rahul", "V Kohli", "AM Rahane", "CA Pujara"),
                 label = c("<span style='color:#5386A6'>Yashasvi Jaiswal</span>", "<span style='color:#5386A6'>Rishabh Pant</span>", "<b style='color:#004C7B'>Shubman Gill</b>", "<span style='color:#bcbcbc'>Rohit Sharma</span>", "<span style='color:#5386A6'>KL Rahul</span>", "<span style='color:#bcbcbc'>Virat Kohli</span>", "<span style='color:#bcbcbc'>Ajinkya Rahane</span>", "<span style='color:#bcbcbc'>Cheteshwar Pujara</span>"))

for_plot <- full_join(for_plot, labels, by = "name")

active_colour <- "#66B2E1"
gill_colour <- "#004C7B"
other_colour <- "#C9C9C9"

#### BAR CHART

rough_bars <- ggplot(for_plot, aes(x = fct_reorder(label, ave_vs_exp), y = ave_vs_exp, fill = category)) +
  geom_bar(stat = "identity",
           width = 0.65) +
  scale_fill_manual(values = c(active_colour, gill_colour, other_colour)) +
  geom_hline(yintercept = 0) +
  scale_y_continuous(limits = c(-6, 19.9),
                     breaks = c(-5, 0, 5, 10, 15),
                     labels = label_number(style_positive = "plus"),
                     expand = c(0, 0)) +
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
  labs(title = "India have four strong, experienced<br>contributors in their current top six",
       subtitle = "Runs scored per dismissal vs. expectation by <br><b>top-six</b> batters for <b>India</b> in the <b>WTC era</b>",
       caption = "'Expectation' = average of batters in same role in same matches<br><br>'WTC era' began on 1 Aug 2019 | Excl. matches vs. AFG, IRE and ZIM<br><br>Data: ESPNCricinfo | Chart: Plot the Ball")
labelled_bars