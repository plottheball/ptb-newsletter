library(tidyverse)
library(rvest)
library(janitor)
library(scales)
library(ggtext)

draft_years <- seq(2000, 2023, 1)

url_start <- "https://www.hockey-reference.com/draft/NHL_"
url_end <- "_entry.html"
all_draft_urls <- str_c(url_start, draft_years, url_end)

all_draft_selections <- tibble()

for (i in all_draft_urls) {
  
  Sys.sleep(1)
  
  html <- read_html(i)
  
  tables <- html %>%
    html_nodes("table") %>%
    html_table()
  
  draft_table <- tables[[1]] %>%
    clean_names()
  
  names <- unlist(draft_table[1, ])
  
  names(draft_table) <- names
  
  table_final <- draft_table %>%
    clean_names() %>%
    select(overall, team, player, nat, pos, age, gp) %>%
    filter(overall != "Overall" & overall != "") %>%
    mutate(year = i)
  
  all_draft_selections <- bind_rows(all_draft_selections, table_final)
  
}

draft_tidy <- all_draft_selections %>%
  filter(player != "forfeited pick" & player != "invalid pick") %>%
  relocate(year, .before = overall)

draft_tidy$overall <- as.integer(draft_tidy$overall)
draft_tidy$age <- as.integer(draft_tidy$age)
draft_tidy$gp <- replace_na(as.integer(draft_tidy$gp), 0)
draft_tidy$year <- as.integer(str_remove(str_remove(draft_tidy$year, fixed(url_start)), fixed(url_end)))

first_round <- draft_tidy %>%
  filter(overall < 31) %>%
  mutate(pos_category = if_else(pos == "G", "Goalie", "Skater"))

first_round_summary <- first_round %>%
  group_by(year, pos_category) %>%
  summarise(count = n()) %>%
  pivot_wider(names_from = "pos_category",
              values_from = "count") %>%
  clean_names()

first_round_final <- first_round_summary %>%
  select(1, 2) %>%
  mutate(goalie_final = replace_na(goalie, 0),
         label = as.character(year),
         zero_y_n = goalie_final == 0)

labels <- c("2023", "'22", "'21", "'20",
            "'19", "'18", "'17", "'16", "'15", 
            "'14", "'13", "2012", "2011", "'10",
            "'09", "'08", "'07", "'06", "'05",
            "'04", "'03", "'02", "'01", "2000")

rough_lollipop <- ggplot(first_round_final, aes(x = fct_reorder(label, -year), y = goalie_final)) +
  geom_hline(yintercept = 0,
             colour = ptb_dark_grey) +
  geom_point(shape = 21, size = 3.75, color = ptb_dark_grey, fill = case_when(first_round_final$zero_y_n == TRUE ~ "#d5d5d5",
                                                                              TRUE ~ "#D97F2B")) +
  geom_text(aes(label = goalie, y = goalie + 0.25), colour = case_when(first_round_final$zero_y_n == TRUE ~ "#ffffff", TRUE ~ "#D97F2B"), fontface = "bold", family = ptb_font) +
  scale_y_continuous(limits = c(-0.5, 7),
                     breaks = c(0, 1, 2, 3, 4, 5, 6),
                     expand = c(0,0)) +
  scale_x_discrete(labels = labels) +
  coord_flip() +
  theme(legend.position = "none")
rough_lollipop

themed_lollipop <- rough_lollipop +
  theme_ptb() +
  theme(axis.line = element_blank(),
        axis.ticks = element_blank(),
        panel.grid.major.y = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank())
themed_lollipop

labelled_lollipop <- themed_lollipop +
  labs(title = "Only five <b style='color:#D97F2B'>goalies</b> have gone in the top<br>30 of the NHL Draft since 2017",
       subtitle = "Number of <b style='color:#D97F2B'>goalies</b> selected within the first 30<br>picks in each NHL Draft between 2000 and 2023",
       caption = "Data: Hockey Reference | Chart: Plot the Ball")
labelled_lollipop

draft_final <- draft_tidy %>%
  filter(overall < 211) %>%
  mutate(pos_category = if_else(pos == "G", "Goalie", "Skater"),
         round = ceiling(overall / 30))

gp_by_pos <- draft_final %>%
  group_by(pos_category) %>%
  summarise(tot_gp = sum(gp))

gp_by_round_pos <- draft_final %>%
  group_by(pos_category, round) %>%
  summarise(tot_gp = sum(gp))

position_analysis <- left_join(x = gp_by_round_pos, y = gp_by_pos, by = c("pos_category" = "pos_category"), suffix = c("_round", "_total")) %>%
  mutate(round_share = tot_gp_round / tot_gp_total * 100) %>%
  select(pos_category, round, round_share) %>%
  pivot_wider(names_from = "pos_category",
              values_from = "round_share") %>%
  clean_names()

position_final <- position_analysis %>%
  pivot_longer(cols = c("goalie", "skater"), names_to = "group", values_to = "share") %>%
  mutate(share_final = if_else(group == "skater", share * -1, share))

rough_bars <- ggplot(position_final, aes(x = round, y = share_final, fill = group)) +
  geom_bar(stat = "identity",
           width = 0.55) +
  scale_fill_manual(values = c("#D97F2B", "#334C80")) +
  geom_hline(yintercept = 0) +
  scale_y_continuous(limits = c(-70, 70),
                     breaks = c(-50, -25, 0, 25, 50),
                     labels = (abs(c(-50, -25, 0, 25, 50))),
                     expand = c(0, 0)) +
  scale_x_continuous(limits = c(0, 8),
                     breaks = c(1, 2, 3, 4, 5, 6, 7),
                     labels = label_ordinal(),
                     expand = c(0, 0)) +
  theme(legend.position = "none",
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.ticks.y = element_blank(),
        panel.grid.major.y = element_blank(),
        axis.line.x = element_blank(),
        axis.line.y = element_blank()) +
  coord_flip()
rough_bars

themed_bars <- rough_bars +
  theme_ptb()
themed_bars

labelled_bars <- themed_bars +
  labs(title = "Draft positions for NHL <b style='color:#D97F2B'>goalies</b><br>vary more widely than for <b style='color:#334C80'>skaters</b>",
       subtitle = "Career games played by <b style='color:#334C80'>skaters</b> and <b style='color:#D97F2B'>goalies</b><br>drafted in <b>each round of the NHL Draft</b> between<br>2000 and 2023, as a share of the overall total",
       caption = "<i>Due to changes in the number of NHL teams over time, a 'round'<br>has been defined as a set of 30 consecutive selections</i><br><br>Data: Hockey Reference | Chart: Plot the Ball")
labelled_bars
