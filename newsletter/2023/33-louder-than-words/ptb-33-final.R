library(tidyverse)
library(rvest)
library(lubridate)
library(RcppRoll)
library(extrafont)
library(ggtext)
library(scales)
library(lemon)
library(janitor)

url_base <- "https://stats.espncricinfo.com/ci/engine/stats/index.html?batting_positionmax1=3;batting_positionmin1=1;batting_positionval1=batting_position;class=2;filter=advanced;orderby=innings;page="
first_page <- 1
url_end <- ";size=200;spanmin1=15+Jul+2019;spanval1=span;template=results;type=batting;wrappertype=print"
url_full_first <- str_c(url_base, first_page, url_end)

no_pages_source <- read_html(url_full_first) %>%
  html_node(xpath = "/html/body/div/div[3]/table[2]") %>%
  html_table()
no_pages <- unlist(no_pages_source[1,1]) %>%
  str_replace(., "Page 1 of ", "") %>%
  as.numeric()
rm(no_pages_source)

rank <- seq(first_page, no_pages, 1)

links <- str_c(url_base, rank, url_end)

batting_data <- tibble()

for (i in links) {
  print(Sys.time())
  Sys.sleep(2)
  
  table_data <- read_html(i) %>%
    html_node(xpath = "/html/body/div/div[3]/table[3]") %>%
    html_table()
  
  table_data <- table_data %>%
    filter(Inns != "-") %>%
    select(-2, -7, -8, -10, -13)
  
  table_data$Inns <- as.integer(table_data$Inns)
  table_data$NO <- as.integer(table_data$NO)
  table_data$Runs <- as.integer(table_data$Runs)
  table_data$BF <- as.integer(table_data$BF)
  table_data$`100` <- as.integer(table_data$`100`)
  table_data$`50` <- as.integer(table_data$`50`)
  table_data$`4s` <- as.integer(table_data$`4s`)
  table_data$`6s` <- as.integer(table_data$`6s`)
  
  batting_data <- bind_rows(batting_data, table_data)
}

data_final <- batting_data %>%
  clean_names() %>%
  filter(bf >= 600) %>%
  arrange(-bf)

data_final <- data_final %>%
  separate(col = player,
           into = c("batter_name", "batter_team"),
           sep =  "[(]")

data_final$batter_name <-  str_trim(data_final$batter_name)
data_final$batter_team <-  gsub(")", "", data_final$batter_team)

team_list <- unique(data_final$batter_team)
team_category <- c(T, T, F, T, T, T, F, F, T, F, F, T, F, F, T, T, T, F, T, T)
team_table <- data_frame(team = team_list, category = team_category)
test_teams <- team_table %>%
  filter(category == TRUE)

data_filtered <- data_final %>%
  filter(batter_team %in% test_teams$team)

data_filtered <- data_filtered %>%
  mutate(runs_per_dismissal = runs / (inns - no),
         strike_rate = runs / bf * 100,
         boundary_4_percentage = x4s / bf,
         boundary_6_percentage = x6s / bf,
         boundary_percentage = (x4s + x6s) / bf,
         eff_bound_percentage = (x4s + (1.5 * x6s)) / bf * 100,
         non_boundary_sr = (runs - (x4s * 4) - (x6s * 6)) / (bf - x4s - x6s) * 100)

gill_colour <- "#007fce"
background_colour <- "#031f5a"

rough_scatter_1 <- ggplot(data_filtered, aes(x = runs_per_dismissal, y = strike_rate)) +
  geom_point(color = case_when(data_filtered$batter_name == "Shubman Gill" ~ gill_colour,
                               TRUE ~ background_colour),
             alpha = case_when(data_filtered$batter_name == "Shubman Gill" ~ 1,
                               TRUE ~ 0.15),
             size = 5) +
  scale_y_continuous(name = NULL,
                     limits = c(61, 119),
                     breaks = seq(70, 110, 10),
                     expand = c(0,0)) +
  scale_x_continuous(name = NULL,
                     limits = c(21, 79),
                     breaks = seq(30, 70, 10),
                     expand = c(0,0)) +
  theme(legend.position = "none",
        axis.line.x = element_blank(),
        axis.line.y = element_blank()) +
  annotate(geom = "text",
           x = 25,
           y = 115,
           label = "Runs scored\nper 100 balls",
           family = ptb_font,
           colour = ptb_dark_grey,
           fontface = "bold",
           hjust = 0,
           vjust = 1) +
  annotate(geom = "text",
           x = 75,
           y = 69,
           label = "Runs scored\nper dismissal",
           family = ptb_font,
           colour = ptb_dark_grey,
           fontface = "bold",
           hjust = 1,
           vjust = 1)
rough_scatter_1

themed_scatter_1 <- rough_scatter_1 +
  theme_ptb()
themed_scatter_1

labelled_scatter_1 <- themed_scatter_1 +
  labs(title = "<span style='color:#007fce;'>Shubman Gill</span> has been the standout<br>ODI batter of this World Cup cycle",
       subtitle = "Runs scored <b>per 1OO balls</b> and <b>per dismissal</b> by<br>top-order batters in men's ODIs since Aug 2019",
       caption = "<i>Batters with <600 balls faced excluded</i><br><br>Data: ESPNCricinfo | Chart: Plot the Ball")
labelled_scatter_1

rough_scatter_2 <- ggplot(data_filtered, aes(y = non_boundary_sr, x = eff_bound_percentage)) +
  geom_point(color = case_when(data_filtered$batter_name == "Shubman Gill" ~ gill_colour,
                               TRUE ~ background_colour),
             alpha = case_when(data_filtered$batter_name == "Shubman Gill" ~ 1,
                               TRUE ~ 0.15),
             size = 5) +
  scale_x_continuous(name = NULL,
                     limits = c(4, 21),
                     breaks = seq(5, 20, 5),
                     expand = c(0,0),
                     labels = percent_format(accuracy = 1, scale = 1)) +
  scale_y_continuous(name = NULL,
                     limits = c(21, 79),
                     breaks = seq(30, 70, 10),
                     expand = c(0,0)) +
  theme(legend.position = "none",
        axis.line.x = element_blank(),
        axis.line.y = element_blank()) +
  annotate(geom = "text",
           x = 5.25,
           y = 77.5,
           label = "Non-boundary\nstrike rate",
           family = ptb_font,
           colour = ptb_dark_grey,
           fontface = "bold",
           hjust = 0,
           vjust = 1) +
  annotate(geom = "text",
           x = 19.5,
           y = 29,
           label = "Effective\nboundary %",
           family = ptb_font,
           colour = ptb_dark_grey,
           fontface = "bold",
           hjust = 1,
           vjust = 1)
rough_scatter_2

themed_scatter_2 <- rough_scatter_2 +
  theme_ptb()
themed_scatter_2

labelled_scatter_2 <- themed_scatter_2 +
  labs(title = "<span style='color:#007fce;'>Gill</span> blends boundaries with rotation",
       subtitle = "<b>Non-boundary strike rate</b> and <b>effective<br>boundary %</b> recorded by top-order batters in<br>men's ODIs since Aug 2019",
       caption = "<i>Batters with <600 balls faced excluded</i><br><br>Data: ESPNCricinfo | Chart: Plot the Ball")
labelled_scatter_2
