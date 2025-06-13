library(tidyverse)
library(janitor)
library(googlesheets4)
library(lemon)
library(ggtext)
library(scales)

### data downloaded from e.g. https://www.tennisabstract.com/cgi-bin/player-more.cgi?p=206173/Jannik-Sinner&table=winners-errors
w_e_data <- read_sheet(ss = "https://docs.google.com/spreadsheets/d/1lmC4uEXFt2X8cs0Kxu-eGig9oabvU4MzSpy4bZQ1HM0/edit?gid=1231592106#gid=1231592106",
                       sheet = "Filtered") %>%
  clean_names()

w_e_clean <- w_e_data %>%
  select(1, 2, 3, 4, 5, 7, 8)

sinner_alcaraz <- w_e_clean %>%
  filter(player == "Jannik Sinner" | player == "Carlos Alcaraz")

alcaraz_polygon <- tibble(ufe_pt = c(0.178, 0.223, 0.209, 0.124, 0.069, 0.093),
                          wnr_pt = c(0.1, 0.156, 0.243, 0.218, 0.185, 0.137))

sinner_polygon <- tibble(ufe_pt = c(0.173, 0.216, 0.13, 0.118, 0.1, 0.104),
                          wnr_pt = c(0.218, 0.153, 0.111, 0.125, 0.153, 0.187))

rough_scatter <- ggplot() +
  geom_polygon(data = alcaraz_polygon,
               aes(y = wnr_pt, x = ufe_pt),
               fill = "#CC937A20") +
  geom_point(data = sinner_alcaraz,
             aes(y = wnr_pt, x = ufe_pt, colour = player),
             size = 5,
             alpha = 0.8) +
  geom_polygon(data = sinner_polygon,
               aes(y = wnr_pt, x = ufe_pt),
               fill = "#26806C50") +
  scale_x_continuous(limits = c(0.06, 0.31),
                     breaks = seq(0.05, 0.3, 0.05),
                     labels = percent_format(scale = 100),
                     expand = c(0, 0)) +
  scale_y_continuous(limits = c(0.06, 0.29),
                     breaks = seq(0.05, 0.25, 0.05),
                     labels = percent_format(scale = 100),
                     expand = c(0, 0)) +
scale_colour_manual(values = c("#CC937A", "#26806C")) +
  theme(legend.position = "none")
rough_scatter

themed_scatter <- rough_scatter +
  theme_ptb() +
  theme(axis.title = element_blank(),
        axis.ticks = element_blank(),
        axis.line = element_blank()) +
  annotate(geom = "text",
           y = 0.28,
           x = 0.07,
           label = "Winner %",
           family = ptb_font,
           colour = ptb_dark_grey,
           fontface = "bold",
           hjust = 0,
           vjust = 1) +
  annotate(geom = "text",
           y = 0.09,
           x = 0.29,
           label = "UFE %",
           family = ptb_font,
           colour = ptb_dark_grey,
           fontface = "bold",
           hjust = 1,
           vjust = 1)
themed_scatter

labelled_scatter <- themed_scatter +
  labs(title = "<b style='color:#26806C'>Sinner</b> plays more consistently than<br><b style='color:#CC937A'>Alcaraz</b> — without the same highs",
       subtitle = "<b>Winner %</b> and <b>UFE %</b> recorded by <b style='color:#CC937A'>Carlos Alcaraz</b><br>and <b style='color:#26806C'>Jannik Sinner</b> in GS QFs, SFs and Finals",
       caption = "'UFE' = unforced error | 'GS' = Grand Slam | Excl. 2025 French Open<br><br>Data: Tennis Abstract | Chart: Plot the Ball")
labelled_scatter