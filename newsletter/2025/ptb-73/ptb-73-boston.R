library(tidyverse)
library(rvest)
library(janitor)
library(googlesheets4)
library(scales)
library(ggtext)

basic_url <- "https://www.basketball-reference.com/wnba/years/2024_totals.html"
basic_html <- read_html(basic_url)
basic_tables <- basic_html %>%
  html_nodes("table") %>%
  html_table()
basic_table <- basic_tables[[1]] %>%
  clean_names() %>%
  filter(player != "Player") %>%
  select(player, team, pos, g, mp, x3p, x3pa, x2p, x2pa, ft, fta)

advanced_url <- "https://www.basketball-reference.com/wnba/years/2024_advanced.html"
advanced_html <- read_html(advanced_url)
advanced_tables <- advanced_html %>%
  html_nodes("table") %>%
  html_table()
advanced_table <- advanced_tables[[1]] %>%
  clean_names() %>%
  filter(player != "Player") %>%
  select(player, team, pos, g, mp, orb_percent, trb_percent, ast_percent, stl_percent, blk_percent, usg_percent, ws)

shooting_url <- "https://www.basketball-reference.com/wnba/years/2024_shooting.html"
shooting_html <- read_html(shooting_url)
shooting_tables <- shooting_html %>%
  html_nodes("table") %>%
  html_table()
shooting_table <- shooting_tables[[1]] %>%
  clean_names() %>%
  filter(x != "Player") %>%
  select(x, x_2, x_3, percent_of_fg_astd, percent_of_fg_astd_2) %>%
  rename("player" = "x",
         "team" = "x_2",
         "pos" = "x_3",
         "x2p_astd" = "percent_of_fg_astd",
         "x3p_astd" = "percent_of_fg_astd_2")

all_data <- full_join(x = basic_table, y = advanced_table, by = c("player", "team", "pos", "g", "mp")) %>%
  full_join(x = ., y = shooting_table, by = c("player", "team", "pos"))

all_data[, 4:11] <- sapply(all_data[, 4:11], as.integer)
all_data[, 12:20] <- sapply(all_data[, 12:20], as.numeric)

team_change_list <- all_data %>%
  filter(team == "TOT")

filtered_data <- all_data %>%
  filter(!player %in% team_change_list$player)

removed_data <- all_data %>%
  filter(player %in% team_change_list$player)

unrivaled_rosters <- read_sheet(ss = "https://docs.google.com/spreadsheets/d/1Ez-dAxd_asQi6TXYfthqkM6Jb_TSK7mus27-WKC7EVU/edit?gid=0#gid=0",
                                sheet = "Rosters") %>%
  clean_names() %>%
  select(player, team) %>%
  rename("unrivaled_team" = "team")

final_data <- bind_rows(filtered_data, team_change_list) %>%
  arrange(desc(mp))

final_data <- full_join(final_data, unrivaled_rosters, by = ("player")) %>%
  filter(mp > (10 * 40)) %>%
  mutate(unrivaled_y_n = !is.na(unrivaled_team),
         .before = g)

minutes_check <- final_data %>%
  head(20) %>%
  filter(unrivaled_y_n == TRUE)

ballhandlers <- final_data %>%
  filter(ast_percent >= 30)

for_plot <- final_data %>%
  select(player, unrivaled_y_n, orb_percent, blk_percent) %>%
  mutate(orb_jittered = jitter(orb_percent),
         blk_jittered = jitter(blk_percent),
         orb_check = (orb_jittered - orb_percent) / orb_percent * 100,
         blk_check = (blk_jittered - blk_percent) / blk_percent * 100)

rough_scatter <- ggplot(for_plot, aes(y = blk_jittered, x = orb_jittered)) +
  geom_point(color = case_when(for_plot$unrivaled_y_n == TRUE & for_plot$player == "Aliyah Boston" ~ "#533cb2",
                               for_plot$unrivaled_y_n == TRUE & for_plot$player != "Aliyah Boston" ~ "#66b1da",
                               TRUE ~ "#C9C9C9"),
             alpha = case_when(for_plot$unrivaled_y_n == TRUE & for_plot$player == "Aliyah Boston" ~ 1,
                               for_plot$unrivaled_y_n == TRUE & for_plot$player == "Angel Reese" ~ 1,
                               for_plot$unrivaled_y_n == TRUE & for_plot$player != "Aliyah Boston" ~ 0.8,
                               TRUE ~ 0.2),
             size = 5) +
  scale_y_continuous(name = NULL,
                     limits = c(0, 7.4),
                     breaks = seq(0, 6, 2),
                     labels = percent_format(scale = 1),
                     expand = c(0,0)) +
  scale_x_continuous(name = NULL,
                     limits = c(0, 17.9),
                     breaks = seq(0, 15, 5),
                     labels = percent_format(scale = 1),
                     expand = c(0,0)) +
  theme(legend.position = "none",
        axis.ticks = element_blank()) +
  annotate(geom = "text",
           x = 0.3,
           y = 7.3,
           label = "Block %",
           family = ptb_font,
           colour = ptb_dark_grey,
           fontface = "bold",
           hjust = 0,
           vjust = 1) +
  annotate(geom = "text",
           x = 16.5,
           y = 1.25,
           label = "Offensive\nRebound %",
           family = ptb_font,
           colour = ptb_dark_grey,
           fontface = "bold",
           hjust = 1,
           vjust = 1)
rough_scatter

themed_scatter <- rough_scatter +
  theme_ptb()
themed_scatter

labelled_scatter <- themed_scatter +
  labs(title = "<span style='color:#533cb2;'>Aliyah Boston</span> plays nearer the basket<br>than most <b style='color:#66b1da'>other players at Unrivaled</b>",
       subtitle = "<b>Block %</b> and <b>Offensive Rebound %</b> recorded by<br>each WNBA player during the 2024 season",
       caption = "<i>Players with <400 minutes played excluded</i><br><br>Data: Basketball Reference | Chart: Plot the Ball")
labelled_scatter
