library(tidyverse)
library(readxl)
library(janitor)
library(ggtext)
library(scales)

### DATA DOWNLOADED FROM https://www.baseball-reference.com/bio/Japan_born.shtml ON 17TH SEPT
batting_list <- read_csv("batting-list.csv") %>%
  clean_names()
pitching_list <- read_csv("pitching-list.csv") %>%
  clean_names()

batting_tidy <- batting_list %>%
  select(name, pa, war) %>%
  rename("overall_war" = "war")

pitching_tidy <- pitching_list %>%
  select(name, bf, war) %>%
  rename("pitching_war" = "war")

japan_all <- left_join(batting_tidy, pitching_tidy, by = c("name" = "name"))

japan_all$bf <- replace_na(japan_all$bf, 0)
japan_all$pitching_war <- replace_na(japan_all$pitching_war, 0)

japan_all <- japan_all %>%
  mutate(hitting_war = overall_war - pitching_war,
         pa_plus_bf = pa + bf)

japan_filtered <- japan_all %>%
  filter(overall_war >= 2)

ohtani_colour <- "#ff003c"
background_colour <- "#aea0a7"

rough_scatter <- ggplot(japan_filtered, aes(x = pa_plus_bf, y = overall_war)) +
  geom_point(color = case_when(japan_filtered$name == "Shohei Ohtani" ~ ohtani_colour,
                               TRUE ~ background_colour),
             alpha = case_when(japan_filtered$name == "Shohei Ohtani" ~ 1,
                               TRUE ~ 0.4),
             size = 5) +
  scale_y_continuous(name = NULL,
                     limits = c(0, 69),
                     breaks = seq(0, 60, 10),
                     expand = c(0,0)) +
  scale_x_continuous(name = NULL,
                     limits = c(0, 11500),
                     breaks = seq(0, 10000, 2500),
                     expand = c(0,0),
                     labels = comma) +
  theme(legend.position = "none") +
  annotate(geom = "text",
           x = 200,
           y = 67,
           label = "Career MLB\nWins Above\nReplacement",
           family = ptb_font,
           colour = ptb_dark_grey,
           fontface = "bold",
           hjust = 0,
           vjust = 1) +
  annotate(geom = "text",
           x = 11250,
           y = 15,
           label = "No. of career\nPlate Appearances\nand Batters Faced",
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
  labs(title = "<span style='color:#ff003c;'>Ohtani</span> is already the second-best<br>Japanese player in MLB history",
       subtitle = "Career <b>Wins Above Replacement</b> recorded in<br>Major League Baseball by players <b>born in Japan</b>",
       caption = "<i>Players with career WAR of <2.0 not plotted</i><br><br>Data: Baseball Reference | Chart: Plot the Ball")
labelled_scatter
