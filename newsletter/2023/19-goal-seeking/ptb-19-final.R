library(tidyverse)
library(httr)
library(rvest)
library(janitor)
library(ggtext)

clubelo_html <- read_html("http://clubelo.com/")
clubelo_tables <- clubelo_html %>%
  html_table()
top25_table <- clubelo_tables[[2]]

current_top25 <- unlist(top25_table$Club) %>%
  str_remove_all("[:digit:]") %>%
  str_remove_all(fixed(" ")) %>%
  str_replace(fixed("Atlético"), "Atletico") %>%
  str_squish() %>%
  tolower()

base_url <- "api.clubelo.com/"

all_data <- tibble()

for (i in current_top25) {
  
  final_url <- str_c(base_url, i)
  
  get_url <- GET(url = final_url)
  
  elo_data <- content(get_url, "parsed")
  
  all_data <- rbind(all_data, elo_data)
  
  Sys.sleep(2)
  
}

data_filtered <- all_data %>%
  filter(From >= '2011-07-01') %>%
  clean_names()

top25_list <- data_filtered$club %>%
  unique() %>%
  unlist()

focus_colour <- "#005daa"
background_colour <- "#444F5A25"

rough_line <- ggplot(data_filtered, aes(x = from, y = elo, colour = club)) +
  geom_line(linewidth = 1) +
  scale_y_continuous(limits = c(1400, 2250),
                     labels = seq(1400, 2200, 200)) +
  scale_color_manual(values = c("Man City" = background_colour,
                                "Bayern" = background_colour,
                                "Real Madrid" = background_colour,
                                "Arsenal" = background_colour,
                                "Liverpool" = background_colour,
                                "Napoli" = background_colour,
                                "Barcelona" = background_colour,
                                "Newcastle" = background_colour,
                                "Man United" = background_colour,
                                "Milan" = background_colour,
                                "Paris SG" = background_colour,
                                "Atletico" = background_colour,
                                "Inter" = background_colour,
                                "Tottenham" = background_colour,
                                "Juventus" = background_colour,
                                "Brighton" = focus_colour,
                                "Roma" = background_colour,
                                "Dortmund" = background_colour,
                                "Porto" = background_colour,
                                "Aston Villa" = background_colour,
                                "Chelsea" = background_colour,
                                "Benfica" = background_colour,
                                "RB Leipzig" = background_colour,
                                "Lazio" = background_colour,
                                "Leverkusen" = background_colour)) +
  theme(legend.position = "none",
        axis.title.x = element_blank(),
        axis.line.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.line.y = element_blank())
rough_line

themed_line <- rough_line + 
  theme_ptb()
themed_line

labelled_line <- themed_line +
  labs(title = "<b style='color:#005daa'>Brighton</b>'s steady climb towards the top of<br>European football is a unique path",
       subtitle = "Change in <b>Elo ratings</b> of the current top 25 men's club<br>football teams in Europe since July 2011",
       caption = "<i>Data as at 23 Apr 2023</i><br><br>Data: ClubElo.com | Chart: Plot the Ball")
labelled_line
