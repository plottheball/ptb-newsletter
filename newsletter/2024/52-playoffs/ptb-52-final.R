library(tidyverse)
library(rvest)
library(janitor)
library(ggalt)
library(ggtext)

nba_url <- "https://www.nba.com/news/history-all-nba-teams"

nba_html <- read_html(nba_url)

all_p <- nba_html %>%
  html_nodes("p") %>%
  html_text()

p_tidy <- all_p[4:641]
p_tidy <- p_tidy[!grepl(fixed("Official release & voting totals"), p_tidy)]
p_tidy <- p_tidy[!grepl(fixed("Official release"), p_tidy)]
p_tidy <- p_tidy[!grepl(fixed("Voting Totals, PDF"), p_tidy)]
p_tidy <- p_tidy[!grepl(fixed("FIRST TEAM"), p_tidy)]
p_tidy <- p_tidy[!grepl(fixed("SECOND TEAM"), p_tidy)]
p_tidy <- p_tidy[!grepl(fixed("THIRD TEAM"), p_tidy)]

all_nba_table <- tibble(name = p_tidy)
all_nba_table <- all_nba_table %>%
  separate_wider_delim(cols = name, delim = ":", names = c("position", "player"))
all_nba_table <- all_nba_table %>%
  separate_wider_delim(cols = player, delim = ".", names = c("name_1", "team_1"), too_few = "align_start")
all_nba_table <- all_nba_table %>%
  separate_wider_delim(cols = name_1, delim = ",", names = c("name", "team"), too_few = "align_start")
all_nba_table$team <- replace_na(all_nba_table$team, " Seattle SuperSonics")

all_nba_table <- all_nba_table %>%
  select(-4)

all_nba_table <- all_nba_table %>%
  mutate(row_no = row_number(),
         season_index = ceiling(row_no / 15),
         season_end_year = 2024 - season_index,
         team_index = row_no - (season_index * 15) + 15,
         team_category = case_when(team_index < 6 ~ "First team",
                                   team_index < 11 ~ "Second team",
                                   TRUE ~ "Third team"),
         .before = position) %>%
  clean_names()

all_nba_table$name <- str_squish(all_nba_table$name)
all_nba_table$team <- str_squish(all_nba_table$team)

all_nba_modern <- all_nba_table %>%
  select(-1, -2, -4) %>%
  filter(season_end_year > 2006)

modern_multi <- all_nba_table %>%
  group_by(name) %>%
  summarise(no_apps = n()) %>%
  arrange(desc(no_apps)) %>%
  filter(no_apps > 2)

active_multi_all_nba <- c("https://www.basketball-reference.com/players/j/jamesle01.html",
                          "https://www.basketball-reference.com/players/p/paulch01.html",
                          "https://www.basketball-reference.com/players/d/duranke01.html",
                          "https://www.basketball-reference.com/players/w/westbru01.html",
                          "https://www.basketball-reference.com/players/c/curryst01.html",
                          "https://www.basketball-reference.com/players/l/lillada01.html",
                          "https://www.basketball-reference.com/players/a/antetgi01.html",
                          "https://www.basketball-reference.com/players/h/hardeja01.html",
                          "https://www.basketball-reference.com/players/g/georgpa01.html",
                          "https://www.basketball-reference.com/players/b/butleji01.html",
                          "https://www.basketball-reference.com/players/e/embiijo01.html",
                          "https://www.basketball-reference.com/players/l/leonaka01.html",
                          "https://www.basketball-reference.com/players/d/davisan02.html",
                          "https://www.basketball-reference.com/players/d/doncilu01.html",
                          "https://www.basketball-reference.com/players/j/jokicni01.html",
                          "https://www.basketball-reference.com/players/g/goberru01.html",
                          "https://www.basketball-reference.com/players/j/jordade01.html",
                          "https://www.basketball-reference.com/players/d/derozde01.html",
                          "https://www.basketball-reference.com/players/t/tatumja01.html",
                          "https://www.basketball-reference.com/players/i/irvinky01.html")

regular_season_all <- tibble()
playoffs_all <- tibble()

for (i in active_multi_all_nba) {
  
  Sys.sleep(2)
  
  html <- read_html(i)
  
  all_tables <- html %>%
    html_nodes("table") %>%
    html_table()
  
  regular_season <- all_tables[[2]] %>%
    clean_names() %>%
    select(1, 2, 3, 4, 5, 6, 7, 8) %>%
    filter(!is.na(age) & lg == "NBA") %>%
    mutate(link = i)
  
  regular_season$age <- as.integer(regular_season$age)
  regular_season$g <- as.integer(regular_season$g)
  regular_season$gs <- as.integer(regular_season$gs)
  regular_season$mp <- as.numeric(regular_season$mp)
  
  regular_season_all <- bind_rows(regular_season_all, regular_season)
  
  playoffs <- all_tables[[3]] %>%
    clean_names() %>%
    select(1, 2, 3, 4, 5, 6, 7, 8) %>%
    filter(!is.na(age) & lg == "NBA") %>%
    mutate(link = i)
  
  playoffs$age <- as.integer(playoffs$age)
  playoffs$g <- as.integer(playoffs$g)
  playoffs$gs <- as.integer(playoffs$gs)
  playoffs$mp <- as.numeric(playoffs$mp)
  
  playoffs_all <- bind_rows(playoffs_all, playoffs)
  
}

combined_data <- full_join(x = regular_season_all, y = playoffs_all, by = c("link" = "link", "season" = "season"), suffix = c("_regular", "_playoff"))

combined_filtered <- combined_data %>%
  filter(gs_regular > 40 & gs_playoff > 3) %>%
  mutate(playoff_bump = (mp_playoff / mp_regular * 100) - 100,
         category = case_when(playoff_bump >= 5 ~ "Higher",
                              playoff_bump <= -5 ~ "Lower",
                              TRUE ~ "Same"))

category_summary <- combined_filtered %>%
  group_by(category) %>%
  summarise(count = n())

higher_colour <- "#40806D"
equal_colour <- "#CCCCCC"
lower_colour <- "#CC6685"

rough_scatter <- ggplot(combined_filtered, aes(x = mp_regular, y = mp_playoff)) +
  geom_point(size = 3.5,
             alpha = 0.5,
             color = case_when(combined_filtered$category == "Same" ~ equal_colour,
                               combined_filtered$category == "Lower" ~ lower_colour,
                               combined_filtered$category == "Higher" ~ higher_colour,
                               TRUE ~ "yellow")) +
  scale_x_continuous(limits = c(20, 49),
                     breaks = seq(24, 48, 8),
                     expand = c(0, 0)) +
  scale_y_continuous(limits = c(20, 49),
                     breaks = seq(24, 48, 8),
                     expand = c(0, 0))
rough_scatter

themed_scatter <- rough_scatter +
  theme_ptb() +
  theme(legend.position = "none",
        axis.line = element_blank(),
        axis.title = element_blank()) +
  annotate(geom = "text",
           x = 20.5,
           y = 47.5,
           label = "Postseason\nminutes\nper game",
           family = ptb_font,
           colour = ptb_dark_grey,
           fontface = "bold",
           hjust = 0,
           vjust = 1) +
  annotate(geom = "text",
           x = 47.5,
           y = 24.75,
           label = "Regular season\nminutes per game",
           family = ptb_font,
           colour = ptb_dark_grey,
           fontface = "bold",
           hjust = 1,
           vjust = 1)
themed_scatter

labelled_scatter <- themed_scatter +
  labs(title = "NBA stars play <b style='color:#4D9982'>more minutes in the<br>playoffs</b> than the regular season",
       subtitle = "<b>Minutes played</b> per game in the <b>postseason</b> and<br><b>regular season</b> by active NBA players with 3+<br>All-NBA selections — in each qualifying year",
       caption = "<i>Years with <41 regular-season or <4 postseason starts excluded</i><br><br>Data: Basketball Reference | Chart: Plot the Ball")
labelled_scatter

current_year <- combined_data %>%
  filter(season == "2023-24") %>%
  select(1, 2, 3, 4, 5, 6, 7, 8, 9) %>%
  mutate(starts_percent = gs_regular / g_regular) %>%
  filter(starts_percent > 0.5)

current_playoff_career <- combined_data %>%
  filter(link %in% current_year$link & !is.na(g_playoff)) %>%
  select(9, 10, 11, 12, 13, 14, 15, 16) %>%
  mutate(est_tot_mp = mp_playoff * g_playoff)

playoff_summary <- current_playoff_career %>%
  group_by(link) %>%
  summarise(tot_playoff_g = sum(g_playoff),
            tot_playoff_mp = sum(est_tot_mp),
            ave_playoff_mp = tot_playoff_mp / tot_playoff_g)

current_year_final <- left_join(current_year, playoff_summary, by = c("link" = "link")) %>%
  select(link, mp_regular, ave_playoff_mp) %>%
  mutate(diff = ave_playoff_mp - mp_regular,
         category = case_when(abs(diff) < 0.7 ~ "Close",
                              TRUE ~ "Other")) %>%
  arrange(-diff)

current_year_final$name <- c("LeBron James", "Damian Lillard", "Paul George", "Stephen Curry", "Kevin Durant", "Jimmy Butler", "Nikola Jokić",
                             "Jayson Tatum", "Anthony Davis", "James Harden", "Kyrie Irving", "Kawhi Leonard", "Joel Embiid", "G Antetokounmpo",
                             "Luka Dončić", "DeMar DeRozan", "Rudy Gobert")

rough_dumbbell <- ggplot(current_year_final, aes(x = mp_regular, xend = ave_playoff_mp, y = fct_reorder(name, ave_playoff_mp))) + 
  geom_dumbbell(colour = equal_colour,
                size = 2.5,
                alpha = case_when(current_year_final$category == "Close" ~ 0.8,
                                  TRUE ~ 1),
                colour_x = lower_colour,
                colour_xend = higher_colour) +
  scale_x_continuous(name = NULL,
                     limits = c(20, 49),
                     breaks = seq(24, 48, 8))
rough_dumbbell

themed_dumbbell <- rough_dumbbell +
  theme_ptb() +
  theme(axis.title = element_blank(),
        axis.line = element_blank(),
        panel.grid.major.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.y = element_text(hjust = 0))
themed_dumbbell

labelled_dumbbell <- themed_dumbbell +
  labs(title = "Who has more to give in the <b style='color:#4D9982'>playoffs</b>?",
       subtitle = "Minutes played per game in the <b style='color:#CC6685'>2023-24 regular<br>season</b> by NBA players with 3+ All-NBA selections,<br> compared to their <b style='color:#4D9982'>career playoff average</b>",
       caption = "<i>Data as at 11 Apr 2024; players with <41 regular-season starts excluded</i><br><br>Data: Basketball Reference | Chart: Plot the Ball")
labelled_dumbbell
