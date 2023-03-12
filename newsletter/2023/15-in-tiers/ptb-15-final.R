library(tidyverse)
library(rvest)
library(janitor)
library(lubridate)
library(RcppRoll)
library(lemon)
library(ggtext)

base_url <- "https://stats.espncricinfo.com/ci/engine/player/"
end_url <- ".html?class=1;template=results;type=batting;view=innings;wrappertype=print"

williamson_id <- "277906"
smith_id <- "267192"
root_id <- "303669"
kohli_id <- "253802"

player_ids <- tibble("name" = c("Williamson", "Smith", "Root", "Kohli"), "id" = c(williamson_id, smith_id, root_id, kohli_id))

records <- tibble()

for (i in player_ids$id) {
  
  url <- str_c(base_url, i, end_url)
  
  html <- read_html(url)
  
  tables <- html %>%
    html_nodes("table") %>%
    html_table()
  
  key_table <- tables[[4]] %>%
    clean_names()
  
  key_table$id <- i
  
  records <- rbind(records, key_table)
  
}

records_final <- left_join(records, player_ids)

records_final <- records_final %>%
  select(1, 3, 7, 8, 9, 11, 13, 14, 15)

records_final$runs <- str_replace(records_final$runs, fixed("*"), "")

records_final$runs <- as.integer(records_final$runs)
records_final$bf <- as.integer(records_final$bf)
records_final$pos <- as.integer(records_final$pos)
records_final$inns <- as.integer(records_final$inns)

records_final <- records_final %>%
  filter(!is.na(runs))

records_final$start_date <- as.Date(records_final$start_date, "%d %b %Y")

records_final <- records_final %>%
  relocate(start_date, name, id)

warner_id <- "219889"
pujara_id <- "32540"
latham_id <- "388802"
chandimal_id <- "300628"

player_ids_other <- tibble("name" = c("Warner", "Pujara", "Latham", "Chandimal"),
                           "id" = c(warner_id, pujara_id, latham_id, chandimal_id))

records_other <- tibble()

for (i in player_ids_other$id) {
  
  # build url
  url <- str_c(base_url, i, end_url)
  
  # get html
  html <- read_html(url)
  
  # grab tables
  tables <- html %>%
    html_nodes("table") %>%
    html_table()
  
  # select key table
  key_table <- tables[[4]] %>%
    clean_names()
  
  key_table$id <- i
  
  records_other <- rbind(records_other, key_table)
  
}

records_other_final <- left_join(records_other, player_ids_other)

records_other_final <- records_other_final %>%
  select(1, 3, 7, 8, 9, 11, 13, 14, 15)

records_other_final$runs <- str_replace(records_other_final$runs, fixed("*"), "")

records_other_final$runs <- as.integer(records_other_final$runs)
records_other_final$bf <- as.integer(records_other_final$bf)
records_other_final$pos <- as.integer(records_other_final$pos)
records_other_final$inns <- as.integer(records_other_final$inns)

records_other_final <- records_other_final %>%
  filter(!is.na(runs))

records_other_final$start_date <- as.Date(records_other_final$start_date, "%d %b %Y")

records_other_final <- records_other_final %>%
  relocate(start_date, name, id)

records_final <- arrange(records_final, start_date, inns)

records_final <- records_final %>%
  mutate(dismissed = case_when(dismissal == "not out" ~ 0,
                               dismissal == "retired notout" ~ 0,
                               TRUE ~ 1))

roll_ave_data <- tibble()
roll_no <- 50

for (i in player_ids$name) {
  
  data_filtered <- records_final %>%
    filter(name == i)
  
  data_filtered$runs_roll <- roll_sum(data_filtered$runs, n = roll_no, align = "right", fill = NA)
  data_filtered$dismissals_roll <- roll_sum(data_filtered$dismissed, n = roll_no, align = "right", fill = NA)
  data_filtered$average_roll <- data_filtered$runs_roll / data_filtered$dismissals_roll
  data_filtered <- rowid_to_column(data_filtered, var = "inns_no")
  
  final_data <- data_filtered %>%
    select(1, 3, 4, 14)
  
  roll_ave_data <- rbind(roll_ave_data, final_data)
  
}

roll_ave_final <- roll_ave_data %>%
  filter(!is.na(average_roll))

roll_for_plot <- roll_ave_final %>%
  select(-3) %>%
  pivot_wider(names_from = "name", values_from = average_roll)

background_data <- roll_ave_final %>%
  select(-name)

facet_labels <- c("Kohli" = "<b style='color:#5B85AA'>Virat Kohli</b>",
                  "Root" = "<b style='color:#E07A5F'>Joe Root</b>",
                  "Smith" = "<b style='color:#E6C35C'>Steve Smith</b>",
                  "Williamson" = "<b style='color:#141134'>Kane Williamson</b>")

rough_plot <- ggplot(roll_ave_final, aes(x = inns_no, y = average_roll)) +
  geom_path(data = background_data, aes(group = id), colour = ptb_light_grey, size = 1) +
  geom_path(size = 1, aes(colour = name)) +
  scale_y_continuous(limits = c(0, 100),
                     breaks = seq(0, 100, 25),
                     expand = c(0,0)) +
  scale_x_continuous(limits = c(0, 240),
                     breaks = seq(0, 200, 50),
                     expand = c(0,0)) +
  scale_colour_manual(values = c("Kohli" = "#5B85AA",
                                 "Root" = "#E07A5F",
                                 "Smith" = "#FED766",
                                 "Williamson" = "#141134")) +
  facet_rep_wrap(~name, nrow = 2,
                 repeat.tick.labels =  TRUE,
                 labeller = labeller(name = facet_labels)) +
  theme(legend.position = "none")
rough_plot

themed_plot <- rough_plot + theme_ptb()
themed_plot

labelled_plot <- themed_plot +
  labs(x = "Innings no.", 
       title = "Is the 'Big Four' really a 'Top Two'?",
       subtitle = "Rolling average number of <b>runs scored per dismissal</b><br>in men's test matches",
       caption = "<i>Average calculated on a 50-innings rolling basis</i><br><br>Data: ESPNCricinfo | Chart: Plot the Ball")
labelled_plot

records_other_final <- arrange(records_other_final, start_date, inns)

records_other_final <- records_other_final %>%
  mutate(dismissed = case_when(dismissal == "not out" ~ 0,
                               dismissal == "retired notout" ~ 0,
                               TRUE ~ 1))

roll_ave_data_other <- tibble()

for (i in player_ids_other$name) {
  
  data_filtered_other <- records_other_final %>%
    filter(name == i)
  
  data_filtered_other$runs_roll <- roll_sum(data_filtered_other$runs, n = roll_no, align = "right", fill = NA)
  data_filtered_other$dismissals_roll <- roll_sum(data_filtered_other$dismissed, n = roll_no, align = "right", fill = NA)
  data_filtered_other$average_roll <- data_filtered_other$runs_roll / data_filtered_other$dismissals_roll
  data_filtered_other <- rowid_to_column(data_filtered_other, var = "inns_no")
  
  final_data_other <- data_filtered_other %>%
    select(1, 3, 4, 14)
  
  roll_ave_data_other <- rbind(roll_ave_data_other, final_data_other)
  
}

roll_ave_final_other <- roll_ave_data_other %>%
  filter(!is.na(average_roll))

roll_for_plot_other <- roll_ave_final_other %>%
  select(-3) %>%
  pivot_wider(names_from = "name", values_from = average_roll)

background_data <- roll_ave_final %>%
  select(-name)

facet_labels_other <- c("Warner" = "<b style='color:#E6C35C'>David Warner</b>",
                        "Pujara" = "<b style='color:#5B85AA'>Cheteshwar Pujara</b>",
                        "Latham" = "<b style='color:#141134'>Tom Latham</b>",
                        "Chandimal" = "<b style='color:#df7400'>Dinesh Chandimal</b>")

rough_plot_other <- ggplot(roll_ave_final_other, aes(x = inns_no, y = average_roll)) +
  geom_path(data = background_data, aes(group = id), colour = ptb_light_grey, size = 1) +
  geom_path(size = 1, aes(colour = name)) +
  scale_y_continuous(limits = c(0, 100),
                     breaks = seq(0, 100, 25),
                     expand = c(0,0)) +
  scale_x_continuous(limits = c(0, 240),
                     breaks = seq(0, 200, 50),
                     expand = c(0,0)) +
  scale_colour_manual(values = c("Warner" = "#FED766",
                                 "Pujara" = "#5B85AA",
                                 "Latham" = "#141134",
                                 "Chandimal" = "#df7400")) +
  facet_rep_wrap(~name, nrow = 2,
                 repeat.tick.labels =  TRUE,
                 labeller = labeller(name = facet_labels_other)) +
  theme(legend.position = "none")
rough_plot_other

themed_plot_other <- rough_plot_other + theme_ptb()
themed_plot_other

labelled_plot_other <- themed_plot_other +
  labs(x = "Innings no.", 
       title = "The 'Next Four' vs. the <span style='color:#444F5A60'>'Big Four'</span>",
       subtitle = "Rolling average number of <b>runs scored per dismissal</b><br>in men's test matches",
       caption = "<i>Average calculated on a 50-innings rolling basis</i><br><br>Data: ESPNCricinfo | Chart: Plot the Ball")
labelled_plot_other
