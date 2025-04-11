library(tidyverse)
library(rvest)
library(janitor)
library(scales)
library(ggtext)

url_base_lhb <- "https://stats.espncricinfo.com/ci/engine/stats/index.html?batting_hand=2;batting_positionmax1=2;batting_positionmin1=1;batting_positionval1=batting_position;class=3;filter=advanced;orderby=start;page="
first_page <- 1
url_end_lhb <- ";qualmin1=1;qualval1=balls_faced;size=200;spanmin1=1+Jan+2000;spanval1=span;template=results;type=batting;view=innings;wrappertype=print"
url_full_lhb <- str_c(url_base_lhb, first_page, url_end_lhb)

url_base_rhb <- "https://stats.espncricinfo.com/ci/engine/stats/index.html?batting_hand=1;batting_positionmax1=2;batting_positionmin1=1;batting_positionval1=batting_position;class=3;filter=advanced;orderby=start;page="
url_end_rhb <- ";qualmin1=1;qualval1=balls_faced;size=200;spanmin1=1+Jan+2000;spanval1=span;template=results;type=batting;view=innings;wrappertype=print"
url_full_rhb <- str_c(url_base_rhb, first_page, url_end_rhb)

no_pages_lhb_source <- read_html(url_full_lhb) %>%
  html_node(xpath = "/html/body/div/div[3]/table[2]") %>%
  html_table()
no_pages_lhb <- unlist(no_pages_lhb_source[1,1]) %>%
  str_replace(., "Page 1 of ", "") %>%
  as.numeric()

no_pages_rhb_source <- read_html(url_full_rhb) %>%
  html_node(xpath = "/html/body/div/div[3]/table[2]") %>%
  html_table()
no_pages_rhb <- unlist(no_pages_rhb_source[1,1]) %>%
  str_replace(., "Page 1 of ", "") %>%
  as.numeric()

rank_lhb <- seq(first_page, no_pages_lhb, 1)
rank_rhb <- seq(first_page, no_pages_rhb, 1)

links_lhb <- str_c(url_base_lhb, rank_lhb, url_end_lhb)
links_rhb <- str_c(url_base_rhb, rank_rhb, url_end_rhb)

lhb_data <- tibble()

for (i in links_lhb) {
  print(Sys.time())
  Sys.sleep(3)
  
  table_data <- read_html(i) %>%
    html_node(xpath = "/html/body/div/div[3]/table[3]") %>%
    html_table() %>%
    clean_names() %>%
    select(1, 2, 4, 5, 6, 10, 11, 12)
  
  table_data$runs <- as.character(table_data$runs)
  table_data$bf <- as.character(table_data$bf)
  table_data$x4s <- as.character(table_data$x4s)
  table_data$x6s <- as.character(table_data$x6s)
  table_data$hand <- "lhb"
  lhb_data <- bind_rows(lhb_data, table_data)
}

rhb_data <- tibble()

for (i in links_rhb) {
  print(Sys.time())
  Sys.sleep(3)
  
  table_data <- read_html(i) %>%
    html_node(xpath = "/html/body/div/div[3]/table[3]") %>%
    html_table() %>%
    clean_names() %>%
    select(1, 2, 4, 5, 6, 10, 11, 12)
  
  table_data$runs <- as.character(table_data$runs)
  table_data$bf <- as.character(table_data$bf)
  table_data$x4s <- as.character(table_data$x4s)
  table_data$x6s <- as.character(table_data$x6s)
  table_data$hand <- "rhb"
  rhb_data <- bind_rows(rhb_data, table_data)
}

batting_data <- bind_rows(lhb_data, rhb_data)

batting_clean <- batting_data

batting_clean$player <- str_replace_all(batting_clean$player,fixed("(1)"), "I")
batting_clean$player <- str_replace_all(batting_clean$player,fixed("(2)"), "II")
batting_clean$player <- str_replace_all(batting_clean$player,fixed("(3)"), "III")

batting_clean <- batting_clean %>%
  separate(col = player,
           into = c("batter_name", "batter_team"),
           sep =  "[(]")

batting_clean$batter_name <- str_squish(batting_clean$batter_name)
batting_clean$batter_team <- str_remove_all(batting_clean$batter_team, fixed(")"))

batting_clean$dismissed <- if_else(str_detect(batting_clean$runs, fixed("*")), 0, 1)
batting_clean$runs <- str_remove_all(batting_clean$runs, fixed("*"))
batting_clean$start_date <- dmy(batting_clean$start_date)
batting_clean[3:6] <- sapply(batting_clean[3:6], as.integer)

batting_clean$x4s <- replace_na(batting_clean$x4s, 0)
batting_clean$x6s <- replace_na(batting_clean$x6s, 0)

top_nine <- c("IND", "NZ",  "PAK", "BAN", "WI",  "SA",  "SL",  "ENG", "AUS")

batting_clean <- batting_clean %>%
  arrange(start_date, ground, opposition) %>%
  mutate(year = year(start_date)) %>%
  filter(batter_team %in% top_nine)

aggregate_summary <- batting_clean %>%
  group_by(hand) %>%
  summarise(no_inns = n(),
            no_dismissals = sum(dismissed),
            tot_runs = sum(runs),
            tot_bf = sum(bf),
            tot_4s = sum(x4s),
            tot_6s = sum(x6s),
            med_runs = median(runs),
            med_bf = median(bf)) %>%
  mutate(average = tot_runs / no_dismissals,
         bpd = tot_bf / no_dismissals,
         strike_rate = tot_runs / tot_bf,
         bound_4_percent = tot_4s / tot_bf,
         bound_6_percent = tot_6s / tot_bf,
         eff_bound_percent = (tot_4s + (1.5 * tot_6s)) / tot_bf,
         non_bound_sr = (tot_runs - (tot_4s * 4) - (tot_6s * 6)) / (tot_bf - (tot_4s) - (tot_6s)))

team_summary <- batting_clean %>%
  group_by(batter_team, hand) %>%
  summarise(no_inns = n()) %>%
  pivot_wider(names_from = "hand",
              values_from = "no_inns")

team_summary$rhb <- replace_na(team_summary$rhb, 0)
team_summary$lhb <- replace_na(team_summary$lhb, 0)

team_summary <- team_summary %>%
  mutate(total_inns = rhb + lhb,
         lhb_share = lhb / total_inns * 100,
         rhb_share = rhb / total_inns * 100) %>%
  filter(total_inns > 1) %>%
  arrange(desc(rhb_share))

team_final <- team_summary %>%
  select(batter_team, lhb_share, rhb_share) %>%
  pivot_longer(cols = c("lhb_share", "rhb_share"), names_to = "handedness", values_to = "share") %>%
  mutate(share_final = if_else(handedness == "lhb_share", share * -1, share),
         category = case_when(handedness == "lhb_share" & batter_team == "IND" ~ "Left Focus",
                              handedness == "lhb_share" & batter_team != "IND" ~ "Left Other",
                              handedness == "rhb_share" & batter_team == "IND" ~ "Right Focus",
                              handedness == "rhb_share" & batter_team != "IND" ~ "Right Other",
                              TRUE ~ "!"))

team_labels <- tibble(label = c("IND", "NZ",  "PAK", "BAN", "WI",  "SA",  "SL",  "ENG", "AUS"),
                      full = c("India", "New Zealand",  "Pakistan", "Bangladesh", "West Indies",  "South Africa",  "Sri Lanka",  "England", "Australia"))

team_final <- left_join(team_final, team_labels, by = c("batter_team" = "label"))

rough_bars <- ggplot(team_final, aes(x = fct_reorder(full, share_final), y = share_final, fill = category)) +
  geom_bar(stat = "identity",
           width = 0.55) +
  scale_fill_manual(values = c("#E67545", "#E6C5B8", "#1275B3", "#A3BCCC")) +
  geom_hline(yintercept = 0) +
  scale_y_continuous(limits = c(-100, 100),
                     breaks = c(-75, -50, -25, 0, 25, 50, 75),
                     labels = (abs(c(-75, -50, -25, 0, 25, 50, 75))),
                     expand = c(0, 0))+
  theme(legend.position = "none",
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.ticks.y = element_blank(),
        panel.grid.major.y = element_blank(),
        axis.line.x = element_blank(),
        axis.line.y = element_blank(),
        axis.ticks = element_blank()) +
  coord_flip()
rough_bars

themed_bars <- rough_bars +
  theme_ptb()
themed_bars

labelled_bars <- themed_bars +
  labs(title = "India gives more opportunities to <b style='color:#E67545'>left-<br>handed openers</b> in T20Is than in tests",
       subtitle = "Share of each team's innings by <b>openers</b><br>completed by <b style='color:#E67545'>left-handed</b> and <b style='color:#1275B3'>right-handed</b><br>batters in men's T20I matches since 2005<br>",
       caption = "Data: ESPNCricinfo | Chart: Plot the Ball")
labelled_bars
