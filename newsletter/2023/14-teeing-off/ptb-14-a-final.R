library(tidyverse)
library(rvest)
library(lubridate)
library(janitor)
library(RcppRoll)
library(extrafont)
library(ggtext)
library(scales)
library(lemon)

url_base_eng_odi <- "https://stats.espncricinfo.com/ci/engine/stats/index.html?class=2;filter=advanced;orderby=start;page="
first_page <- 1
url_end <- ";size=200;spanmin1=1+Jan+2000;spanval1=span;team=1;template=results;type=team;view=innings;wrappertype=print"
url_full_eng_odi_first <- str_c(url_base_eng_odi, first_page, url_end)

no_pages_eng_odi_source <- read_html(url_full_eng_odi_first) %>%
  html_node(xpath = "/html/body/div/div[3]/table[2]") %>%
  html_table()
no_pages_eng_odi <- unlist(no_pages_eng_odi_source[1,1]) %>%
  str_replace(., "Page 1 of ", "") %>%
  as.numeric()
rm(no_pages_eng_odi_source)

rank_eng_odi <- seq(first_page, no_pages_eng_odi, 1)
links_eng_odi <- str_c(url_base_eng_odi, rank_eng_odi, url_end)
data_eng_odi <- tibble()

for (i in links_eng_odi) {
  print(Sys.time())
  Sys.sleep(2)
  
  # read page in loop and select HTML table
  table_data <- read_html(i) %>%
    html_node(xpath = "/html/body/div/div[3]/table[3]") %>%
    html_table()
  
  # combine with existing record
  data_eng_odi <- bind_rows(data_eng_odi, table_data)
}

eng_odi_batting <- data_eng_odi %>%
  clean_names() %>%
  filter(score != "DNB") %>%
  select(-7)

eng_odi_batting$rpo <- as.numeric(eng_odi_batting$rpo)
eng_odi_batting$start_date <- dmy(eng_odi_batting$start_date)

eng_odi_batting <- eng_odi_batting %>%
  separate(col = overs,
           into = c("completed_overs", "completed_balls"),
           sep = "[.]")
eng_odi_batting <- eng_odi_batting %>%
  separate(col = score,
           into = c("runs_scored", "wickets"),
           sep = "[/]")
eng_odi_batting[2:5] <- sapply(eng_odi_batting[2:5], as.integer)
eng_odi_batting$completed_balls <- replace_na(eng_odi_batting$completed_balls, 0)
eng_odi_batting$wickets <- replace_na(eng_odi_batting$wickets, 10)
eng_odi_batting$total_balls <- eng_odi_batting$completed_balls + (6 * eng_odi_batting$completed_overs)

eng_odi_batting <- eng_odi_batting %>%
  arrange(start_date)

roll_no <- 30

eng_odi_batting$roll_runs <- (roll_sum(eng_odi_batting$runs_scored, n = roll_no, align = "right", fill = NA))
eng_odi_batting$roll_bf <- (roll_sum(eng_odi_batting$total_balls, n = roll_no, align = "right", fill = NA))
eng_odi_batting$roll_rpo <- eng_odi_batting$roll_runs / eng_odi_batting$roll_bf * 6
eng_odi_batting$match_index <- row_number(eng_odi_batting$start_date)

rough_line_odi <- ggplot(eng_odi_batting, aes(x = match_index, y = roll_rpo)) +
  geom_path(size = 1, color = "#62a5f1") +
  scale_x_continuous(limits = c(-30, (max(eng_odi_batting$match_index) * 1.1)),
                     expand = c(0,0)) +
  scale_y_continuous(name = NULL,
                     limits = c(3.9, 7.1),
                     expand = c(0,0),
                     breaks = seq(3, 7, 1)) +
  theme(axis.line.x = element_blank(),
        axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        axis.ticks.x = element_blank(),
        panel.grid.major.x = element_blank()) +
  geom_vline(xintercept = 331,
             size = 0.75,
             color = "#ee393f",
             linetype = "dotted") +
  geom_vline(xintercept = 70,
             size = 0.5,
             color = "#e0e4ef",
             linetype = "dashed") +
  geom_vline(xintercept = 165,
             size = 0.5,
             color = "#e0e4ef",
             linetype = "dashed") +
  geom_vline(xintercept = 253,
             size = 0.5,
             color = "#e0e4ef",
             linetype = "dashed") +
  geom_vline(xintercept = 341,
             size = 0.5,
             color = "#e0e4ef",
             linetype = "dashed") +
  geom_vline(xintercept = 437,
             size = 0.5,
             color = "#e0e4ef",
             linetype = "dashed")
rough_line_odi

themed_line_odi <- rough_line_odi + theme_ptb()
themed_line_odi

labelled_line_odi <- themed_line_odi +
  labs(title = "Morgan's impact in ODIs, visualised",
       subtitle = "Rolling average no. of <b style='color:#62a5f1'>runs scored per over</b> by the<br>England men's cricket team in ODIs since 2000",
       caption = "<i>Averages calculated on a 30-innings rolling basis</i><br><br>Data: ESPNCricinfo | Chart: Plot the Ball")
labelled_line_odi
