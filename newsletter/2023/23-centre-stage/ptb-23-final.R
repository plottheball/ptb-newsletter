library(tidyverse)
library(googlesheets4)
library(janitor)
library(ggtext)
library(scales)

# Results imported via Google Sheets from https://www.lnb.fr/elite/stats-engine/?option=player&season=2022&competition=266&type=total
lnb_data_url <- "https://docs.google.com/spreadsheets/d/1A5ggaT1sPqsH7qNsoeziFdG5WWqlGxPWEZN24NBxf_k/edit#gid=768960038"

lnb_data <- read_sheet(lnb_data_url) %>%
  clean_names() %>%
  select(2, 3, 4, 5, 6, 7, 8)

lnb_data_filtered <- lnb_data %>%
  filter(height_category != "<200cm")

lnb_data_filtered$height_cm <- as.integer(lnb_data_filtered$height_cm)

mets_gold <- "#fbb813"
background_data <- "#C9C9C9"

scatter_rough <- ggplot(lnb_data_filtered, aes(x = blk_36, y = x3pa_36)) +
  geom_point(color = if_else(lnb_data_filtered$joueur == "Victor Wembanyama", mets_gold, background_data),
             alpha = if_else(lnb_data_filtered$joueur == "Victor Wembanyama", 1, 0.65),
             size = 5) +
  scale_x_continuous(limits = c(0, 4.1),
                     breaks = seq(0, 4, 1),
                     expand = c(0,0.2)) +
  scale_y_continuous(limits = c(0, 7.1),
                     breaks = seq(0, 7, 1),
                     expand = c(0,0.2))
scatter_rough

scatter_themed <- scatter_rough +
  theme_ptb() +
  theme(axis.title = element_blank(),
        axis.line = element_blank()) +
  annotate(geom = "text",
           x = 0.1,
           y = 6.9,
           label = "3PA per\n36 mins",
           family = ptb_font,
           colour = ptb_dark_grey,
           fontface = "bold",
           hjust = 0,
           vjust = 1) +
  annotate(geom = "text",
           x = 3.9,
           y = 0.1,
           label = "BLK per\n36 mins",
           family = ptb_font,
           colour = ptb_dark_grey,
           fontface = "bold",
           hjust = 1,
           vjust = 0) +
  geom_hline(yintercept = 0,
             colour = ptb_dark_grey) +
  geom_vline(xintercept = 0,
             colour = ptb_dark_grey)
scatter_themed

scatter_labelled <- scatter_themed +
  labs(title = "<b style='color:#fbb813'>Victor Wembanyama</b> is two players in one",
       subtitle = "Three-point attempts and blocks per 36 mins recorded by<br><b>players taller than 200cm</b> in LNB Pro A during 2022-23",
       caption = "<i>Players with <700 regular-season minutes excluded</i><br><br>Data: LNB | Chart: Plot the Ball")
scatter_labelled

scatter_2_rough <- ggplot(lnb_data_filtered, aes(x = ft_percent, y = height_cm)) +
  geom_jitter(color = if_else(lnb_data_filtered$joueur == "Victor Wembanyama", mets_gold, background_data),
             alpha = if_else(lnb_data_filtered$joueur == "Victor Wembanyama", 1, 0.65),
             size = 5,
             height = 0.15) +
  scale_x_continuous(limits = c(20, 105),
                     breaks = seq(25, 105, 25),
                     expand = c(0,0),
                     labels = percent_format(accuracy = 1, scale = 1)) +
  scale_y_continuous(limits = c(191, 229),
                     breaks = seq(200, 220, 10),
                     expand = c(0,0))
scatter_2_rough

scatter_2_themed <- scatter_2_rough +
  theme_ptb() +
  theme(axis.title = element_blank(),
        axis.line = element_blank()) +
  annotate(geom = "text",
           x = 20,
           y = 227,
           label = "Height\n(cm)",
           family = ptb_font,
           colour = ptb_dark_grey,
           fontface = "bold",
           hjust = 0,
           vjust = 1) +
  annotate(geom = "text",
           x = 98,
           y = 193,
           label = "FT%",
           family = ptb_font,
           colour = ptb_dark_grey,
           fontface = "bold",
           hjust = 1,
           vjust = 0) +
  geom_vline(xintercept = 100,
             colour = ptb_dark_grey)
scatter_2_themed

scatter_2_labelled <- scatter_2_themed +
  labs(title = "<b style='color:#fbb813'>Wembanyama</b> is a skilled free-throw shooter",
       subtitle = "Height (in cm) and free-throw % of <b>players taller than<br>200cm</b> in LNB Pro A during 2022-23",
       caption = "<i>Players with <700 regular-season minutes excluded</i><br><br>Data: LNB | Chart: Plot the Ball")
scatter_2_labelled
