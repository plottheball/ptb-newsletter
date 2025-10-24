library(tidyverse)
library(rvest)
library(janitor)
library(ggtext)

base_url <- "https://www.zerotackle.com/nrl/"
seasons <- seq(2023, 2025, 1)
end_url <- "-player-stats/"

data_by_season <- tibble()

for (i in seasons) {
  
  Sys.sleep(2)
  
  url <- str_c(base_url, i, end_url)
  
  html <- read_html(url)
  
  tables <- html %>%
    html_nodes("table") %>%
    html_table()
  
  key_table <- tables[[1]] %>%
    clean_names() %>%
    mutate(year = i,
           .before = x)
  
  data_by_season <- bind_rows(data_by_season, key_table)
  
}

attack_by_season <- data_by_season %>%
  select(year, x, player, pos, mins, t, ta, lb, lba, er) %>%
  group_by(year, player, pos) %>%
  summarise(tot_mins = sum(mins),
            tot_t = sum(t),
            tot_ta = sum(ta),
            tot_lb = sum(lb),
            tot_lba = sum(lba),
            tot_er = sum(er)) %>%
  arrange(desc(tot_mins)) %>%
  filter(tot_mins >= (12 * 80)) %>%
  mutate(t_inv_per_80 = sum(tot_t, tot_ta) / tot_mins * 80,
         lb_inv_per_80 = sum(tot_lb, tot_lba) / tot_mins * 80,
         er_per_80 = tot_er / tot_mins * 80)

attack_summary <- data_by_season %>%
  select(year, x, player, pos, mins, t, ta, lb, lba, er) %>%
  group_by(player, pos) %>%
  summarise(tot_mins = sum(mins),
            tot_t = sum(t),
            tot_ta = sum(ta),
            tot_lb = sum(lb),
            tot_lba = sum(lba),
            tot_er = sum(er)) %>%
  arrange(desc(tot_mins)) %>%
  filter(tot_mins >= (20 * 80)) %>%
  mutate(t_inv_per_80 = sum(tot_t, tot_ta) / tot_mins * 80,
         lb_inv_per_80 = sum(tot_lb, tot_lba) / tot_mins * 80,
         er_per_80 = tot_er / tot_mins * 80)

bri_gold <- "#E6BB45"
pen_pink <- "#CCADBB"
background_grey <- "#C9C9C9"

rough_scatter <- ggplot(attack_summary, aes(y = t_inv_per_80, x = lb_inv_per_80)) +
  geom_point(size = 5,
             color = background_grey,
             alpha = 0.2) +
  geom_point(data = (attack_summary %>% filter(player == "DylanEdwards")),
             shape = 21,
             fill = pen_pink,
             color = ptb_dark_grey,
             alpha = 1,
             size = 5,
             stroke = 1) +
  geom_point(data = (attack_summary %>% filter(player == "ReeceWalsh")),
             shape = 21,
             fill = bri_gold,
             color = ptb_dark_grey,
             alpha = 1,
             size = 5,
             stroke = 1) +
  scale_x_continuous(limits = c(0, 3.7),
                     breaks = seq(0, 3, 1),
                     expand = c(0, 0)) +
  scale_y_continuous(limits = c(0, 2.7),
                     breaks = seq(1, 2, 1),
                     expand = c(0,0)) +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.ticks = element_blank()) +
  annotate(geom = "text",
           x = 0.1,
           y = 2.6,
           label = "Tries &\ntry assists\nper 80 mins",
           family = ptb_font,
           colour = ptb_dark_grey,
           fontface = "bold",
           hjust = 0,
           vjust = 1) +
  annotate(geom = "text",
           x = 3.6,
           y = 0.7,
           label = "Line breaks &\nline-break assists\nper 80 mins",
           family = ptb_font,
           colour = ptb_dark_grey,
           fontface = "bold",
           hjust = 1,
           vjust = 1)
rough_scatter

scatter_themed <- rough_scatter +
  theme_ptb()
scatter_themed

scatter_labelled <- scatter_themed +
  labs(title = "<b style='color:#CCAB52'>Reece Walsh</b> is the most productive<br>attacking player in the NRL — by far",
       subtitle = "<b>Try</b> and <b>line-break involvements</b> recorded per<br>80 minutes by all NRL players <b>since 2023</b>",
       caption = "<i>Players with <1,600 minutes since 2023 excluded</i><br><br>Data: Zero Tackle | Chart: Plot the Ball")
scatter_labelled
