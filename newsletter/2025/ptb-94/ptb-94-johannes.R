library(tidyverse)
library(rvest)
library(janitor)
library(ggtext)
library(scales)

totals_urls <- str_c("https://www.basketball-reference.com/wnba/years/", seq(1997, 2025, 1), "_totals.html")

all_data <- tibble()

for (i in totals_urls) {
  
  Sys.sleep(3)
  
  totals_table <- read_html(i) %>%
    html_nodes("table") %>%
    html_table()
  
  totals_table <- totals_table[[1]] %>%
    clean_names() %>%
    filter(player != "Player") %>%
    select(player, team, g, mp, fga, x3pa, x3p, ast, tov) %>%
    mutate(g = as.integer(g),
           mp = as.integer(mp),
           fga = as.integer(fga),
           x3pa = as.integer(x3pa),
           x3p = as.integer(x3p),
           ast = as.integer(ast),
           tov = as.integer(tov),
           ref = i)
  
  all_data <- bind_rows(all_data, totals_table)
  
}

totals_summary <- all_data %>%
  filter(team != "TOT") %>%
  group_by(player) %>%
  summarise(tot_mp = sum(mp),
            tot_3pa = sum(x3pa),
            tot_3pm = sum(x3p))

shooting_3p <- totals_summary %>%
  mutate(x3pa_36 = tot_3pa / tot_mp * 36,
         success_rate = tot_3pm / tot_3pa) %>%
  filter(tot_3pm >= 100)

active_players <- all_data %>%
  select(player, ref) %>%
  unique() %>%
  filter(ref == "https://www.basketball-reference.com/wnba/years/2025_totals.html")

active_shooters <- shooting_3p %>%
  filter(player %in% active_players$player) %>%
  mutate(frequency_rank = rank(x3pa_36),
         success_rank = rank(success_rate))

liberty_seafoam <- "#5C9988"
background_grey <- "#C9C9C9"

scatter_rough <- ggplot(data = active_shooters, aes(x = x3pa_36, y = success_rate)) +
  geom_jitter(width = 0.00,
              height = 0.00,
              size = 5,
              color = case_when(active_shooters$player == "Marine Johannes" ~ liberty_seafoam,
                                TRUE ~ background_grey),
              alpha = case_when(active_shooters$player == "Marine Johannes" ~ 1,
                                TRUE ~ 0.3))  +
  scale_x_continuous(limits = c(0, 10.4),
                     breaks = seq(2, 10, 2),
                     expand = c(0, 0)) +
  scale_y_continuous(limits = c(0.21, 0.49),
                     breaks = seq(0.25, 0.45, 0.05),
                     labels = percent_format(),
                     expand = c(0,0)) +
  theme(axis.title.x = element_blank(),
        axis.ticks = element_blank(),
        axis.line.x = element_blank()) +
  annotate(geom = "text",
           x = 0.5,
           y = 0.48,
           label = "Three-point\nsuccess rate",
           family = ptb_font,
           colour = ptb_dark_grey,
           fontface = "bold",
           hjust = 0,
           vjust = 1) +
  annotate(geom = "text",
           x = 9.9,
           y = 0.25,
           label = "Three-point\nFGA per 36",
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
  labs(title = "<span style='color:#5C9988'>Johannès</span> is accurate from three-point<br>range for such a high-volume shooter",
       subtitle = "Career <b>success rate</b> of active WNBA players from<br><b>three-point range</b>, compared to the number of<br><b>attempts</b> they take <b>per 36 minutes</b>",
       caption = "Players with <100 career three-point attempts excluded</i><br><br>Data: Basketball Reference | Chart: Plot the Ball")
scatter_labelled
