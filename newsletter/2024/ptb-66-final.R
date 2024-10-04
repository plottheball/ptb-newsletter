library(tidyverse)
library(rvest)
library(janitor)
library(RcppRoll)
library(ggtext)
library(ggh4x)

url_start <- "https://stats.espncricinfo.com/ci/engine/stats/index.html?class=10;filter=advanced;orderby=start;page="
page_no_first <- 1
batting_url_end <- ";size=200;team=1026;team=1863;team=2614;team=289;team=3379;team=3867;team_view=bat;template=results;type=team;view=innings;wrappertype=print"
bowling_url_end <- ";size=200;team=1026;team=1863;team=2614;team=289;team=3379;team=3867;team_view=bowl;template=results;type=team;view=innings;wrappertype=print"

batting_url_first <- str_c(url_start, page_no_first, batting_url_end)
bowling_url_first <- str_c(url_start, page_no_first, bowling_url_end)

no_pages_batting <- read_html(batting_url_first) %>%
  html_node(xpath = "/html/body/div/div[3]/table[2]") %>%
  html_table()
no_pages_batting <- unlist(no_pages_batting[1, 1]) %>%
  str_replace(., "Page 1 of ", "") %>%
  as.numeric()

no_pages_bowling <- read_html(bowling_url_first) %>%
  html_node(xpath = "/html/body/div/div[3]/table[2]") %>%
  html_table()
no_pages_bowling <- unlist(no_pages_bowling[1, 1]) %>%
  str_replace(., "Page 1 of ", "") %>%
  as.numeric()

pages_batting <- seq(1, no_pages_batting, 1)
batting_table_all <- tibble()

for (i in pages_batting) {
  
  Sys.sleep(1)
  
  url <- str_c(url_start, i, batting_url_end)
  
  batting_tables <- read_html(url) %>%
    html_nodes("table") %>%
    html_table()

  batting_table <- batting_tables[[3]] %>%
    clean_names() %>%
    select(1, 2, 3, 8, 9, 10) %>%
    mutate(category = "Batting", .after = team)
  
  batting_table_all <- bind_rows(batting_table_all, batting_table)
  
}

batting_table_all$start_date <- dmy(batting_table_all$start_date)
batting_table_all <- batting_table_all %>%
  mutate(match_index = row_number(),
         .before = team)

pages_bowling <- seq(1, no_pages_bowling, 1)
bowling_table_all <- tibble()

for (i in pages_bowling) {
  
  Sys.sleep(1)
  
  url <- str_c(url_start, i, bowling_url_end)
  
  bowling_tables <- read_html(url) %>%
    html_nodes("table") %>%
    html_table()
  
  bowling_table <- bowling_tables[[3]] %>%
    clean_names() %>%
    select(1, 2, 3, 8, 10) %>%
    mutate(category = "Bowling", .after = team)
  
  bowling_table_all <- bind_rows(bowling_table_all, bowling_table)
  
}

bowling_table_all$start_date <- dmy(bowling_table_all$start_date)
bowling_table_all <- bowling_table_all %>%
  mutate(match_index = row_number(),
         .before = team)

data_all <- bind_rows(batting_table_all, bowling_table_all) %>%
  filter(score != "DNB") %>%
  filter(overs != "0.0")

data_all <- data_all %>%
  separate(col = overs,
           into = c("completed_overs", "completed_balls"),
           sep = "[.]") %>%
  separate(col = score,
           into = c("runs_scored", "wickets"),
           sep = "[/]")

data_all[4:7] <- sapply(data_all[4:7], as.integer)

data_all$completed_balls <- replace_na(data_all$completed_balls, 0)
data_all$wickets <- replace_na(data_all$wickets, 10)
data_all$total_balls <- data_all$completed_balls + (6 * data_all$completed_overs)

auto_qualifiers <- c("v ENG Women", "v AUS Women", "v NZ Women", "v India Women", "v SA Women", "v WI Women")

top_six_matches <- data_all %>%
  filter(opposition %in% auto_qualifiers) %>%
  select(2, 3, 4, 5, 8, 9, 10, 11) %>%
  relocate(total_balls,
           .before = wickets) %>%
  relocate(opposition,
           .after = team) %>%
  relocate(start_date,
           ground,
           .before = team)

top_six_matches$team <- str_remove_all(top_six_matches$team, " Women")
top_six_matches$opposition <- str_remove_all(top_six_matches$opposition, " Women")
top_six_matches$opposition <- str_remove_all(top_six_matches$opposition, "v ")
top_six_matches$team <- str_replace_all(top_six_matches$team, "India", "IND")
top_six_matches$opposition <- str_replace_all(top_six_matches$opposition, "India", "IND")
top_six_matches$year <- year(top_six_matches$start_date)

roll_no <- 60

top_six_final <- top_six_matches %>%
  filter(category == "Batting" & year > 2015 & start_date < "2024-10-01") %>%
  mutate(inns_id = row_number(),
         .before = start_date) %>%
  filter(inns_id >= 2) %>%
  mutate(roll_runs = roll_sum(x = runs_scored, n = roll_no, fill = NA, align = "right"),
         roll_bf = roll_sum(x = total_balls, n = roll_no, fill = NA, align = "right"),
         roll_wkts = roll_sum(x = wickets, n = roll_no, fill = NA, align = "right"),
         runs_per_120 = roll_runs / roll_bf * 120,
         wkts_per_120 = roll_wkts / roll_bf * 120)

top_six_for_plot <- top_six_final %>%
  select(inns_id, runs_per_120, wkts_per_120) %>%
  pivot_longer(cols = c("runs_per_120", "wkts_per_120"),
               names_to = "category",
               values_to = "value") %>%
  filter(!is.na(value))

facet_labels <- c("runs_per_120" = "<b style='color:#2A1F66'>Runs scored per 20 overs</b>",
                  "wkts_per_120" = "<b style='color:#E64555'>Wickets lost per 20 overs</b>")

rough_line <- ggplot(top_six_for_plot, aes(x = inns_id, y = value)) +
  geom_vline(xintercept = 61,
             linewidth = 0.7,
             color = "#e0e4ef",
             linetype = "dashed") +
  geom_vline(xintercept = 138,
             linewidth = 0.7,
             color = "#e0e4ef",
             linetype = "dashed") +
  geom_vline(xintercept = 182,
             linewidth = 0.7,
             color = "#e0e4ef",
             linetype = "dashed") +
  geom_vline(xintercept = 234,
             linewidth = 0.7,
             color = "#e0e4ef",
             linetype = "dashed") +
  geom_vline(xintercept = 274,
             linewidth = 0.7,
             color = "#e0e4ef",
             linetype = "dashed") +
  geom_vline(xintercept = 337,
             linewidth = 0.7,
             color = "#e0e4ef",
             linetype = "dashed") +
  geom_vline(xintercept = 391,
             linewidth = 0.7,
             color = "#e0e4ef",
             linetype = "dashed") +
  geom_path(size = 1.5, aes(colour = category)) +
  scale_colour_manual(values = c("runs_per_120" = "#2A1F66",
                                 "wkts_per_120" = "#E64555")) +
  facet_wrap(~category, nrow = 2, ncol = 1, scales = "free_y", labeller = labeller(category = facet_labels)) +
  facetted_pos_scales(y = list(category == "runs_per_120" ~ scale_y_continuous(limits = c(80, 210),
                                                                               breaks = seq(100, 200, 50),
                                                                               labels = seq(100, 200, 50)),
                               category == "wkts_per_120" ~ scale_y_continuous(limits = c(3.5, 9.5),
                                                                               breaks = seq(4, 8, 2),
                                                                               labels = seq(4, 8, 2)))) +
  scale_x_continuous(limits = c(30, 480),
                     expand = c(0, 0))
rough_line

themed_line <- rough_line +
  theme_ptb() +
  theme(axis.line = element_blank(),
        axis.ticks = element_blank(),
        axis.text.x = element_blank(),
        panel.grid.major.x = element_blank(),
        legend.position = "none",
        axis.title.x = element_blank())
themed_line

labelled_line <- themed_line +
  labs(title = "<b style='color:#2A1F66'>Bat</b> vs. <b style='color:#E64555'>ball</b> in WT20Is, visualised",
       subtitle = "Average number of <b style='color:#2A1F66'>runs scored</b> and <b style='color:#E64555'>wickets lost</b><br>per 20 overs in Women's T20Is<span style='color:#444F5A75'>&ast;</span> since 2018<br><span style='font-size:8pt;color:#444F5A75'>* Matches between Australia, England, India, New Zealand, South Africa and the West Indies</span>",
       caption = "<i>Calculated on a 60-innings rolling basis; data as at 1 Oct 2024</i><br><br>Data: ESPNCricinfo | Chart: Plot the Ball")
labelled_line

excl_aus_final <- top_six_matches %>%
  filter(team != "AUS" & opposition != "AUS") %>%
  filter(category == "Batting" & year > 2012 & start_date < "2024-10-01") %>%
  mutate(inns_id = row_number(),
         .before = start_date) %>%
  filter(inns_id >= 33) %>%
  mutate(roll_runs = roll_sum(x = runs_scored, n = roll_no, fill = NA, align = "right"),
         roll_bf = roll_sum(x = total_balls, n = roll_no, fill = NA, align = "right"),
         roll_wkts = roll_sum(x = wickets, n = roll_no, fill = NA, align = "right"),
         runs_per_120 = roll_runs / roll_bf * 120,
         wkts_per_120 = roll_wkts / roll_bf * 120)

excl_aus_for_plot <- excl_aus_final %>%
  select(inns_id, runs_per_120, wkts_per_120) %>%
  pivot_longer(cols = c("runs_per_120", "wkts_per_120"),
               names_to = "category",
               values_to = "value") %>%
  filter(!is.na(value))

facet_labels_excl_aus <- c("runs_per_120" = "<b style='color:#364D49'>Runs scored per 20 overs</b>",
                  "wkts_per_120" = "<b style='color:#CC8FC1'>Wickets lost per 20 overs</b>")

rough_line_excl_aus <- ggplot(excl_aus_for_plot, aes(x = inns_id, y = value)) +
  geom_vline(xintercept = 92,
             linewidth = 0.7,
             color = "#e0e4ef",
             linetype = "dashed") +
  geom_vline(xintercept = 145,
             linewidth = 0.7,
             color = "#e0e4ef",
             linetype = "dashed") +
  geom_vline(xintercept = 177,
             linewidth = 0.7,
             color = "#e0e4ef",
             linetype = "dashed") +
  geom_vline(xintercept = 205,
             linewidth = 0.7,
             color = "#e0e4ef",
             linetype = "dashed") +
  geom_vline(xintercept = 235,
             linewidth = 0.7,
             color = "#e0e4ef",
             linetype = "dashed") +
  geom_vline(xintercept = 279,
             linewidth = 0.7,
             color = "#e0e4ef",
             linetype = "dashed") +
  geom_vline(xintercept = 313,
             linewidth = 0.7,
             color = "#e0e4ef",
             linetype = "dashed") +
  geom_path(size = 1.5, aes(colour = category)) +
  scale_colour_manual(values = c("runs_per_120" = "#364D49",
                                 "wkts_per_120" = "#CC8FC1")) +
  facet_wrap(~category, nrow = 2, ncol = 1, scales = "free_y", labeller = labeller(category = facet_labels_excl_aus)) +
  facetted_pos_scales(y = list(category == "runs_per_120" ~ scale_y_continuous(limits = c(80, 210),
                                                                               breaks = seq(100, 200, 50),
                                                                               labels = seq(100, 200, 50)),
                               category == "wkts_per_120" ~ scale_y_continuous(limits = c(3.5, 9.5),
                                                                               breaks = seq(4, 8, 2),
                                                                               labels = seq(4, 8, 2)))) +
  scale_x_continuous(limits = c(60, 375),
                     expand = c(0, 0))
rough_line_excl_aus

themed_line_excl_aus <- rough_line_excl_aus +
  theme_ptb() +
  theme(axis.line = element_blank(),
        axis.ticks = element_blank(),
        axis.text.x = element_blank(),
        panel.grid.major.x = element_blank(),
        legend.position = "none",
        axis.title.x = element_blank())
themed_line_excl_aus

labelled_line_excl_aus <- themed_line_excl_aus +
  labs(title = "<b style='color:#364D49'>Bat</b> vs. <b style='color:#CC8FC1'>ball</b> in WT20Is, visualised",
       subtitle = "Average number of <b style='color:#364D49'>runs scored</b> and <b style='color:#CC8FC1'>wickets lost</b><br>per 20 overs in Women's T20Is<span style='color:#444F5A75'>&ast;</span> since 2018<br><span style='font-size:8pt;color:#444F5A75'>* Matches between England, India, New Zealand, South Africa and the West Indies</span>",
       caption = "<i>Calculated on a 60-innings rolling basis; data as at 1 Oct 2024</i><br><br>Data: ESPNCricinfo | Chart: Plot the Ball")
labelled_line_excl_aus
