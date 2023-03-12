library(tidyverse)
library(rvest)
library(lubridate)
library(janitor)
library(RcppRoll)
library(extrafont)
library(ggtext)
library(scales)
library(lemon)

url_base_eng_test <- "https://stats.espncricinfo.com/ci/engine/stats/index.html?class=1;filter=advanced;orderby=start;page="
first_page <- 1
url_end <- ";size=200;spanmin1=1+Jan+2000;spanval1=span;team=1;template=results;type=team;view=innings;wrappertype=print"
url_full_eng_test_first <- str_c(url_base_eng_test, first_page, url_end)

no_pages_eng_test_source <- read_html(url_full_eng_test_first) %>%
  html_node(xpath = "/html/body/div/div[3]/table[2]") %>%
  html_table()
no_pages_eng_test <- unlist(no_pages_eng_test_source[1,1]) %>%
  str_replace(., "Page 1 of ", "") %>%
  as.numeric()
rm(no_pages_eng_test_source)

rank_eng_test <- seq(first_page, no_pages_eng_test, 1)
links_eng_test <- str_c(url_base_eng_test, rank_eng_test, url_end)
data_eng_test <- tibble()

for (i in links_eng_test) {
  print(Sys.time())
  Sys.sleep(2)
  
  # read page in loop and select HTML table
  table_data <- read_html(i) %>%
    html_node(xpath = "/html/body/div/div[3]/table[3]") %>%
    html_table()
  
  # combine with existing record
  table_data$RPO <- as.character(table_data$RPO)
  data_eng_test <- bind_rows(data_eng_test, table_data)
}

eng_test_batting <- data_eng_test %>%
  clean_names() %>%
  filter(score != "DNB") %>%
  filter(overs != "0.0") %>%
  filter(rpo != "-") %>%
  select(-8)

eng_test_batting$rpo <- as.numeric(eng_test_batting$rpo)
eng_test_batting$inns <- as.integer(eng_test_batting$inns)
eng_test_batting$start_date <- dmy(eng_test_batting$start_date)

eng_test_batting <- eng_test_batting %>%
  separate(col = overs,
           into = c("completed_overs", "completed_balls"),
           sep = "[.]")
eng_test_batting <- eng_test_batting %>%
  separate(col = score,
           into = c("runs_scored", "wickets"),
           sep = "[/]")
eng_test_batting[2:5] <- sapply(eng_test_batting[2:5], as.integer)
eng_test_batting$completed_balls <- replace_na(eng_test_batting$completed_balls, 0)
eng_test_batting$wickets <- replace_na(eng_test_batting$wickets, 10)
eng_test_batting$total_balls <- eng_test_batting$completed_balls + (6 * eng_test_batting$completed_overs)

eng_test_batting <- eng_test_batting %>%
  arrange(start_date, inns)

roll_no <- 30

eng_test_batting$roll_runs <- (roll_sum(eng_test_batting$runs_scored, n = roll_no, align = "right", fill = NA))
eng_test_batting$roll_bf <- (roll_sum(eng_test_batting$total_balls, n = roll_no, align = "right", fill = NA))
eng_test_batting$roll_rpo <- eng_test_batting$roll_runs / eng_test_batting$roll_bf * 6
eng_test_batting$inns_index <- row_number(eng_test_batting$start_date)

rough_line_test <- ggplot(eng_test_batting, aes(x = inns_index, y = roll_rpo)) +
  geom_path(size = 1, colour = "#002f52") +
  scale_x_continuous(limits = c(-30, (max(eng_test_batting$inns_index) * 1.1)),
                     expand = c(0,0)) +
  scale_y_continuous(name = NULL,
                     limits = c(1.9, 5.1),
                     expand = c(0,0),
                     breaks = seq(2, 5, 1)) +
  theme(axis.line.x = element_blank(),
        axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        axis.ticks.x = element_blank(),
        panel.grid.major.x = element_blank()) +
  geom_vline(xintercept = 524,
             size = 0.75,
             color = "#ee393f",
             linetype = "dotted") +
  geom_vline(xintercept = 40,
             size = 0.5,
             color = "#e0e4ef",
             linetype = "dashed") +
  geom_vline(xintercept = 140,
             size = 0.5,
             color = "#e0e4ef",
             linetype = "dashed") +
  geom_vline(xintercept = 235,
             size = 0.5,
             color = "#e0e4ef",
             linetype = "dashed") +
  geom_vline(xintercept = 321,
             size = 0.5,
             color = "#e0e4ef",
             linetype = "dashed") +
  geom_vline(xintercept = 361,
             size = 0.5,
             color = "#e0e4ef",
             linetype = "dashed") +
  geom_vline(xintercept = 464,
             size = 0.5,
             color = "#e0e4ef",
             linetype = "dashed")
rough_line_test

themed_line_test <- rough_line_test + theme_ptb()
themed_line_test

labelled_line_test <- themed_line_test +
  labs(title = "McCullum's impact in tests, visualised",
       subtitle = "Rolling average no. of <b style='color:#002f52'>runs scored per over</b> by the<br>England men's cricket team in test matches since 2000",
       caption = "<i>Averages calculated on a 30-innings rolling basis</i><br><br>Data: ESPNCricinfo | Chart: Plot the Ball")
labelled_line_test
