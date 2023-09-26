library(tidyverse)
library(rvest)
library(janitor)
library(ggtext)

country_url <- "https://erubik.xyz/esm/nations.php?view=history-seasons"

country_html <- read_html(country_url)

country_tables <- country_html %>%
  html_nodes("table")

key_table <- country_tables[[1]] %>%
  html_table()

table_tidy <- key_table %>%
  select(-1, -3, -4)

new_names <- unlist(table_tidy[1,])

names(table_tidy) <- new_names

table_final <- table_tidy[2:nrow(table_tidy),] %>%
  clean_names()

table_final[,2:ncol(table_final)] <- sapply(table_final[,2:ncol(table_final)], as.integer)

table_final <- table_final %>%
  mutate_if(is.numeric, replace_na, 0) %>%
  arrange(x)

country_list <- unlist(table_final$x)
grouping_list <- c("03 South America", "05 Europe (Other)", "05 Europe (Other)", "05 Europe (Other)", "03 South America",
                   "04 Africa", "03 South America", "02 Other", "05 Europe (Other)", "05 Europe (Other)",
                   "04 Africa", "01 Europe (Big Five)", "05 Europe (Other)", "01 Europe (Big Five)", "01 Europe (Big Five)",
                   "01 Europe (Big Five)", "04 Africa", "02 Other", "04 Africa", "04 Africa",
                   "04 Africa", "05 Europe (Other)", "04 Africa", "05 Europe (Other)", "05 Europe (Other)",
                   "05 Europe (Other)", "05 Europe (Other)", "05 Europe (Other)", "04 Africa", "05 Europe (Other)",
                   "05 Europe (Other)", "05 Europe (Other)", "02 Other", "01 Europe (Big Five)", "05 Europe (Other)",
                   "05 Europe (Other)", "05 Europe (Other)", "03 South America", "05 Europe (Other)", "05 Europe (Other)")
country_table <- tibble(country_list, grouping_list)

category_list <- sort(unique(grouping_list))
category_index <- tibble(category_list, reference = seq(1, 5, 1))

table_final <- left_join(table_final, country_table, by = c("x" = "country_list"))

category_summary <- table_final %>%
  group_by(grouping_list) %>% 
  summarise(count_1996 = sum(x95_96),
            count_1997 = sum(x96_97),
            count_1998 = sum(x97_98),
            count_1999 = sum(x98_99),
            count_2000 = sum(x99_00),
            count_2001 = sum(x00_01),
            count_2002 = sum(x01_02),
            count_2003 = sum(x02_03),
            count_2004 = sum(x03_04),
            count_2005 = sum(x04_05),
            count_2006 = sum(x05_06),
            count_2007 = sum(x06_07),
            count_2008 = sum(x07_08),
            count_2009 = sum(x08_09),
            count_2010 = sum(x09_10),
            count_2011 = sum(x10_11),
            count_2012 = sum(x11_12),
            count_2013 = sum(x12_13),
            count_2014 = sum(x13_14),
            count_2015 = sum(x14_15),
            count_2016 = sum(x15_16),
            count_2017 = sum(x16_17),
            count_2018 = sum(x17_18),
            count_2019 = sum(x18_19),
            count_2020 = sum(x19_20),
            count_2021 = sum(x20_21),
            count_2022 = sum(x21_22),
            count_2023 = sum(x22_23))

category_rotated <- category_summary %>%
  pivot_longer(!grouping_list, names_to = "season", values_to = "player_count")

category_rotated$season <- as.integer(str_remove_all(category_rotated$season, "count_"))

category_rotated <- left_join(category_rotated, category_index, by = c("grouping_list" = "category_list"))

season_nos <- seq(1996, 2023, 1)

table_complete <- tibble()

for (i in season_nos) {
  
  table_for_plot <- tibble(season = i, index = seq(1, 11, 1))
  
  table_filtered <- category_rotated %>%
    filter(season == i)
  
  category_1 <- rep(table_filtered$grouping_list[1], table_filtered$player_count[1])
  category_2 <- rep(table_filtered$grouping_list[2], table_filtered$player_count[2])
  category_3 <- rep(table_filtered$grouping_list[3], table_filtered$player_count[3])
  category_4 <- rep(table_filtered$grouping_list[4], table_filtered$player_count[4])
  category_5 <- rep(table_filtered$grouping_list[5], table_filtered$player_count[5])
  
  season_list <- c(category_1, category_2, category_3, category_4, category_5)

  table_for_plot$category <- season_list
  
  table_complete <- bind_rows(table_complete, table_for_plot)
  
}

category_palette <- c("#37416E", "#E3DFE6", "#E3DFE6", "#E3DFE6", "#D61525")

waffle_rough <- ggplot(table_complete, aes(x = index, y = season, colour = category)) +
  geom_point(shape = 15, size = 2.75) +
  scale_colour_manual(values = category_palette, aesthetics = "colour") +
  scale_y_reverse(limits = c(2024, 1993),
                  breaks = seq(2020, 2000, -5),
                  labels = seq(2020, 2000, -5),
                  expand = c(0, 0)) +
  scale_x_continuous(limits = c(0, 17),
                     expand = c(0, 0))
waffle_rough

waffle_themed <- waffle_rough +
  theme_ptb() +
  theme(legend.position = "none",
        axis.title.y = element_blank(),
        axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.line.x =  element_blank(),
        axis.line.y = element_blank(),
        panel.grid.major = element_blank())
waffle_themed

waffle_labelled <- waffle_themed +
  labs(title = "The top talent in men's club football<br>comes from all over Europe",
       subtitle = "The <b>regions</b> which produced the 11 players picked<br>in each <b>ESM Team of the Season</b> since 1995-96",
       caption = "Data: European Sports Media | Chart: Plot the Ball")
waffle_labelled
