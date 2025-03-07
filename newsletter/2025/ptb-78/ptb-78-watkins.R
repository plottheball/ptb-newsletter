library(tidyverse)
library(RSelenium)
library(rvest)
library(janitor)
library(ggtext)
library(scales)

rD <- rsDriver(browser = "firefox", port = 4445L)
remDr <- rD[["client"]]
remDr$open()

base_url <- "https://herhoopstats.com/stats/ncaa/research/player_single_seasons/?division=1&min_season="
mid_url <- "&max_season="
end_url <- "&games=all&pos_g=1&pos_f=1&pos_c=1&min_experience=1&max_experience=4&criteria_type=values&criteria0=mp_decimal&comp0=ge&threshold0=800&&stats_to_show=per_40&submit=true"
years <- seq(2010, 2025, 1)
urls <- str_c(base_url, years, mid_url, years, end_url)

all_data <- tibble()

for (i in urls) {
  
  Sys.sleep(1)
  
  remDr$navigate(i)
  
  html <- remDr$getPageSource()[1]
  html <- unlist(html)
  html_clean <- read_html(html)
  
  all_tables <- html_clean %>%
    html_nodes("table") %>%
    html_table()
  
  key_table <- all_tables[[1]] %>%
    clean_names()
  
  all_data <- bind_rows(all_data, key_table)
  
}

key_info <- all_data %>%
  select(1, 2, 3, 4, 8, 10, 16, 24, 26, 27)

key_info$min <- str_replace(key_info$min, fixed(","), "")
key_info$min <- as.integer(key_info$min)

end_url_advanced <- "&games=all&pos_g=1&pos_f=1&pos_c=1&min_experience=1&max_experience=4&criteria_type=values&criteria0=mp_decimal&comp0=ge&threshold0=800&&stats_to_show=advanced&submit=true"
urls_advanced <- str_c(base_url, years, mid_url, years, end_url_advanced)

adv_data <- tibble()

for (i in urls_advanced) {
  
  Sys.sleep(1)
  
  remDr$navigate(i)
  
  html <- remDr$getPageSource()[1]
  html <- unlist(html)
  html_clean <- read_html(html)
  
  all_tables <- html_clean %>%
    html_nodes("table") %>%
    html_table()
  
  key_table <- all_tables[[1]] %>%
    clean_names()
  
  adv_data <- bind_rows(adv_data, key_table)
  
}

key_info_adv <- adv_data %>%
  select(1, 2, 3, 4, 5, 10, 17)

key_info_adv$min <- str_replace(key_info_adv$min, fixed(","), "")
key_info_adv$min <- as.integer(key_info_adv$min)
key_info_adv$usg_percent <- str_replace(key_info_adv$usg_percent, fixed("%"), "")
key_info_adv$usg_percent <- as.numeric(key_info_adv$usg_percent)
key_info_adv$x3p_rate <- str_replace(key_info_adv$x3p_rate, fixed("%"), "")
key_info_adv$x3p_rate <- as.numeric(key_info_adv$x3p_rate)
key_info_adv$ast_percent <- str_replace(key_info_adv$ast_percent, fixed("%"), "")
key_info_adv$ast_percent <- as.numeric(key_info_adv$ast_percent)

info_final <- full_join(key_info, key_info_adv, by = c("season", "player", "team", "min"))

perimeter_creators <- info_final %>%
  filter(usg_percent >= 30 & x3p_rate >= 20 & ast_percent >= 20) %>%
  mutate(stl_plus_blk = stl + blk)

usc_red <- "#990000"
iowa_yellow <- "#E6B800"
background_grey <- "#C9C9C9"

scatter_rough <- ggplot(data = perimeter_creators, aes(x = stl_plus_blk, y = pts)) +
  geom_jitter(width = 0.01,
              height = 0.01,
              size = 4,
              color = case_when(perimeter_creators$player == "JuJu Watkins" ~ usc_red,
                                perimeter_creators$player == "Caitlin Clark" ~ iowa_yellow,
                                TRUE ~ background_grey),
              alpha = case_when(perimeter_creators$player == "JuJu Watkins" ~ 1,
                                perimeter_creators$player == "Caitlin Clark" ~ 0.8,
                                TRUE ~ 0.3))  +
  scale_x_continuous(limits = c(0, 6.9),
                     breaks = seq(0, 6, 2),
                     expand = c(0,0)) +
  scale_y_continuous(limits = c(11, 39),
                     breaks = seq(15, 35, 5),
                     expand = c(0,0)) +
  theme(axis.title.x = element_blank(),
        axis.ticks = element_blank(),
        axis.line.x = element_blank()) +
  annotate(geom = "text",
           x = 0.2,
           y = 38,
           label = "Points scored\nper 40 mins",
           family = ptb_font,
           colour = ptb_dark_grey,
           fontface = "bold",
           hjust = 0,
           vjust = 1) +
  annotate(geom = "text",
           x = 6.7,
           y = 15,
           label = "Steals + blocks\nper 40 mins",
           family = ptb_font,
           colour = ptb_dark_grey,
           fontface = "bold",
           hjust = 1,
           vjust = 1)
scatter_rough

scatter_themed <- scatter_rough +
  theme_ptb()
scatter_themed

scatter_labelled <- scatter_themed +
  labs(title = "<span style='color:#990000'>Watkins</span> is a unique defensive force<br>among high-usage perimeter players",
       subtitle = "<b>Points</b> and <b>steals + blocks</b> recorded by <b>perimeter<br>creators</b> in D-I NCAAW basketball since 2009-10",
       caption = "<i>'Perimeter creators' are players with >30% usage rate, >20% assist rate<br>and >20% 3P rate; player-seasons with <800 minutes played excluded</i><br><br>Data: Her Hoop Stats | Chart: Plot the Ball")
scatter_labelled
