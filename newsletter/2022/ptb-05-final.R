# load packages...
library(tidyverse)
# ...for scraping,...
library(httr)
library(rvest)
library(RSelenium)
# ...for analysis...
library(janitor)
# ...and for visualisation
library(ggtext)

### OBTAIN, CLEAN & ANALYSE DATA

## GRAB LIST OF MATCH URLS

# set up Selenium browser
rD <- rsDriver(browser = "firefox", port = 4445L)
remDr <- rD[["client"]]
remDr$open()
# remDr$close()

# create list of draw URLs
nrl_draw_url <- "https://www.nrl.com/draw/?competition=111&round=1&season=2022"
draw_url_base <- "https://www.nrl.com/draw/?competition="
comp_id <- 111
url_round <- "&round="
round_nos <- seq(1, 30, 1)
url_season <- "&season="
season_nos <- seq(2013, 2022, 1)

draw_urls_final <- vector()

for (i in season_nos) {
  draw_urls <- str_c(draw_url_base, comp_id, url_round, round_nos, url_season, i)
  
  draw_urls_final <- c(draw_urls_final, draw_urls)
}

# grab match URLs from each round
match_urls_final <- vector()

for (i in draw_urls_final) {
  
  remDr$navigate(i)
  
  draw_html <- remDr$getPageSource()[1]
  draw_html <- unlist(draw_html)
  
  draw_html_clean <- read_html(draw_html)
  
  all_urls <- draw_html_clean %>%
    html_nodes("a")
  
  urls_tidy <- all_urls %>%
    as.character() %>%
    as_tibble()
  
  urls_filtered <- urls_tidy %>%
    filter(str_detect(value, "draw/nrl-premiership"))
  
  urls_filtered <- urls_filtered %>%
    separate(col = value, into = c("element", "url", "class"), sep = " ") %>%
    select(-element, -class) %>%
    separate(col = url, into = c("element", "slug"), sep = "=") %>%
    select(-element)
  
  round_urls_final <- pull(urls_filtered, slug)
  
  match_urls_final <- c(match_urls_final, round_urls_final)
  
}

match_urls_final <- str_remove_all(match_urls_final, "\"")
match_urls_final <- unique(match_urls_final)
match_urls_final <- str_c("https://www.nrl.com", match_urls_final)

## GRAB TABLES OF PLAYER DATA FROM EACH MATCH URL

# set up empty dataframe to hold player data
player_data <- tibble()

# set up for loop for all matches in list (up to end of R12 2022: index 1873)
for (i in match_urls_final[1:1873]) {
  
  # navigate to page, grab page source and clean
  remDr$navigate(i)
  match_html <- remDr$getPageSource()[1]
  match_html <- unlist(match_html)
  html_clean <- read_html(match_html)
  
  # grab tables for home and away team...
  all_tables <- html_clean %>%
    html_nodes("table") %>%
    html_table()
  table_home <- all_tables %>%
    pluck(1) %>%
    clean_names()
  table_away <- all_tables %>%
    pluck(3) %>%
    clean_names()
  # ...and filter out unnecessary columns
  home_tidy <- table_home %>%
    select(c(2, 3, 4, 6, 8, 25, 26, 27, 36, 37, 38, 39, 49, 51, 52, 53, 56, 60))
  away_tidy <- table_away %>%
    select(c(2, 3, 4, 6, 8, 25, 26, 27, 36, 37, 38, 39, 49, 51, 52, 53, 56, 60))
  
  # rename columns using names from inside dataframe
  new_names_home <- as_vector(home_tidy[1, 1:18])
  new_names_away <- as_vector(away_tidy[1, 1:18])
  home_final <- home_tidy %>%
    filter(! row_number() %in% 1) %>%
    set_names(new_names_home) %>%
    clean_names()
  away_final <- away_tidy %>%
    filter(! row_number() %in% 1) %>%
    set_names(new_names_away) %>%
    clean_names()
  
  # clean data and combine into single match dataframe
  match_final <- rbind(home_final, away_final)
  match_final$player <- str_replace_all(match_final$player, "\n                \n                  ", " ")
  match_final <- match_final %>%
    separate(mins_played, c("mins", "seconds"))
  match_final[4:18] <- sapply(match_final[4:18], as.integer)
  match_final <- match_final %>%
    replace(is.na(.), 0)
  
  # add URL reference
  match_final$url <- i
  
  # select only important columns
  match_final <- match_final %>%
    select(player, number, position, mins, seconds, try_assists, passes, receipts, url)
  
  # bind to existing data
  player_data <- rbind(player_data, match_final)
  
}

# create final dataframe,...
player_data_final <- player_data
# ...tidy URL information into separate usable columns...
player_data_final$url <- player_data_final$url %>%
  str_remove(., "https://www.nrl.com/draw/nrl-premiership/")
player_data_final <- player_data_final %>%
  separate(., col = url, into = c("year", "round", "match"), sep = "/")
# ...and add playerID column to avoid errors due to specific player names occurring twice
player_data_final <- player_data_final %>%
  mutate(playerID = case_when(player == "Sione Katoa" & position == "Hooker" ~ "Sione Katoa (HK)",
                              player == "Sione Katoa" & position == "Interchange" ~ "Sione Katoa (HK)",
                              player == "Sione Katoa" & position == "Winger" ~ "Sione Katoa (WI)",
                              TRUE ~ player))

## CONVERT TO DATAFRAMES FOR VISUALISATION

# summarise data for each season by player position...
by_position_season <- player_data_final %>%
  group_by(position, year) %>%
  summarise(total_apps = n(),
            total_mins = sum(mins),
            total_secs = sum(seconds),
            total_tas = sum(try_assists),
            total_passes = sum(passes),
            total_receipts = sum(receipts)) %>%
  mutate(tas_per_80 = total_tas / total_mins * 80)
# ...and filter out data for replacement players
for_position_season_chart <- by_position_season %>%
  select(position, year, tas_per_80) %>%
  filter(!position %in% c("Reserve", "Replacement", "Interchange"))

# summarise data for each player by position,...
for_player_position_chart <- player_data_final %>%
  group_by(playerID, position) %>%
  summarise(no_apps = n(),
            no_mins = sum(mins),
            no_tas = sum(try_assists)) %>%
  mutate(tas_per_80 = no_tas / no_mins * 80) %>%
  # filtering out players with less than 1,000 minutes in a given position...
  filter(no_mins >= 1000) %>%
  # ...and filtering out substitute appearances
  filter(!position %in% c("Reserve", "Replacement", "Interchange"))

# create list of players active during 2022 season...
active_check <- player_data_final %>%
  filter(year == 2022) %>%
  select(playerID) %>%
  unique()
active_check$t_or_f <- TRUE
# ...and combine with summary by player
for_player_position_chart <- left_join(x = for_player_position_chart, y = active_check, by = c('playerID' = 'playerID'))
for_player_position_chart$t_or_f <- for_player_position_chart$t_or_f %>%
  replace_na(., FALSE)

### VISUALISE

## SET UP CUSTOM COLOURS

focus_colour <- "#008039"
focus_colour_2 <- "#005225"
background_colour <- "#EEF0F2"
complementary_colour <- "#CCC9B4"

custom_palette <- c(complementary_colour, focus_colour_2)

## PLOT 1:HEAT MAP

# create custom labelled heat map (with custom ordering of y-axis by position)
rough_plot <- ggplot(for_position_season_chart, aes(x = year, y = factor(position, level = c("Lock", "2nd Row", "Prop", "Hooker", "Halfback", "Five-Eighth", "Centre", "Winger", "Fullback")), fill = tas_per_80)) +
  geom_tile(color = "white",
            lwd = 1,
            linetype = 1) +
  scale_fill_gradient(low = background_colour, high = focus_colour) +
  geom_text(aes(label = round(tas_per_80, digits = 1)), color = background_colour, size = 3.5, fontface = "bold", family = ptb_font) +
  theme(legend.position = "none",
        axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        axis.line.x =  element_blank(),
        axis.line.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank())
rough_plot

# add custom theme to plot
themed_plot <- rough_plot + theme_ptb()
themed_plot

# add labelling to plot
labelled_plot <- themed_plot +
  labs(title = "Where do the NRL's playmakers play?",
       subtitle = "Average number of try asssists provided per 80 minutes<br> by NRL players starting in each position since 2013",
       caption = "<i>Figures for 2022 season based on data for Round 1 to Round 12</i><br><br>Data: NRL.com | Chart: Plot the Ball")
labelled_plot

final_plot <- labelled_plot

## PLOT 2: SCATTER WITH JITTER

# create custom scatter plot (with custom ordering of y-axis by position)
rough_plot_2 <- ggplot(for_player_position_chart, aes(x = tas_per_80,
                                                      y = factor(position, level = c("Lock", "2nd Row", "Prop", "Hooker", "Halfback", "Five-Eighth", "Centre", "Winger", "Fullback")))) +
  geom_jitter(width = 0, height = 0.15, size = 1.75, alpha = 0.75, aes(color = t_or_f)) +
  scale_color_manual(values = custom_palette) +
  scale_x_continuous(name = NULL,
                     limits = c(0, 1.2),
                     breaks = seq(0, 1.2, 0.5))+
  theme(legend.position = "none",
        axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        axis.line.y = element_blank(),
        axis.ticks.y = element_blank())
rough_plot_2

# add custom theme to plot
themed_plot_2 <- rough_plot_2 +
  theme_ptb()
themed_plot_2

# add labelling to plot
labelled_plot_2 <- themed_plot_2 +
  labs(title = "The best playmakers at each position",
       subtitle = "Try asssists provided per 80 minutes by NRL players with<br>at least 1,000 minutes played at each position since 2013,<br> with <b style='color:#005225'>players active during the 2022 season</b> highlighted",
       caption = "<i>Figures based on data up to Round 12 of 2022 season</i><br><br>Data: NRL.com | Chart: Plot the Ball")
labelled_plot_2

final_plot_2 <- labelled_plot_2

#### NOTES
#### code for custom theme elements not included above
