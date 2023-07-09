# load packages...
library(tidyverse)
# ...for scraping,...
library(rvest)
# ...for analysis...
library(janitor)
# ...and for visualisation
library(lemon)
library(ggtext)
library(scales)

### OBTAIN, CLEAN & ANALYSE DATA

## GRAB CURRENT TENNIS ABSTRACT ELO RANKINGS FOR BOTH ATP AND WTA TOURS

# define URLs,...
atp_elo_url <- "https://tennisabstract.com/reports/atp_elo_ratings.html"
wta_elo_url <- "https://tennisabstract.com/reports/wta_elo_ratings.html"
# ...read in page HTML,...
atp_elo_html <- read_html(atp_elo_url)
wta_elo_html <- read_html(wta_elo_url)
# ...scrape all tables....
atp_tables <- atp_elo_html %>%
  html_nodes("table") %>% 
  html_table()
wta_tables <- wta_elo_html %>%
  html_nodes("table") %>% 
  html_table()
# ...and grab relevant data for both tours
atp_elo_current <- atp_tables[5]
atp_elo_current <- atp_elo_current[[1]] %>%
  select(-5, -9, -13)
wta_elo_current <- wta_tables[5]
wta_elo_current <- wta_elo_current[[1]] %>%
  select(-5, -9, -13)

## GRAB PLAYER PAGE URLS FOR REFERENCE

# grab ATP player URLs from rankings page
atp_player_urls <- atp_elo_html %>%
  html_nodes("table") %>%
  html_nodes("td") %>% 
  html_nodes("a")
atp_urls_tidy <- atp_player_urls %>%
  as.character() %>%
  as_tibble() %>%
  rename(player_url = value)
atp_urls_tidy <- atp_urls_tidy %>%
  separate(col = player_url,
           into = c("player_url", "name_check"),
           sep = ">")

# grab WTA player URLs from rankings page
wta_player_urls <- wta_elo_html %>%
  html_nodes("table") %>%
  html_nodes("td") %>% 
  html_nodes("a")
wta_urls_tidy <- wta_player_urls %>%
  as.character() %>%
  as_tibble() %>%
  rename(player_url = value)
wta_urls_tidy <- wta_urls_tidy %>%
  separate(col = player_url,
           into = c("player_url", "name_check"),
           sep = ">")

# clean the URLs (and name column - to cross-reference to main dataset) into a usable format
atp_urls_tidy$player_url <- str_remove_all(string = atp_urls_tidy$player_url, pattern = "<a href=")
atp_urls_tidy$name_check <- str_remove_all(string = atp_urls_tidy$name_check, pattern = "</a")
atp_urls_tidy$player_url <- str_remove_all(string = atp_urls_tidy$player_url, pattern = "\"")
wta_urls_tidy$player_url <- str_remove_all(string = wta_urls_tidy$player_url, pattern = "<a href=")
wta_urls_tidy$name_check <- str_remove_all(string = wta_urls_tidy$name_check, pattern = "</a")
wta_urls_tidy$player_url <- str_remove_all(string = wta_urls_tidy$player_url, pattern = "\"")


# combine with Elo record
atp_elo_current <- left_join(x = atp_elo_current, y = atp_urls_tidy, by = c("Player" = "name_check"))
atp_final <- atp_elo_current %>%
  clean_names() %>%
  mutate(tour = "ATP")
wta_elo_current <- left_join(x = wta_elo_current, y = wta_urls_tidy, by = c("Player" = "name_check"))
wta_final <- wta_elo_current %>%
  clean_names() %>%
  mutate(tour = "WTA")

## CONVERT TO FINAL DATAFRAMES FOR VISUALISATION

# combine ATP and WTA records
all_players <- rbind(atp_final, wta_final)

# filter to players at least 12 months past their peak
analysis_all <- all_players %>%
  mutate(time_since_peak = age - peak_age)
analysis_filtered <- analysis_all %>%
  filter(time_since_peak > 1)

# filter to current top 100 players on each tour, with 'elite under-21' players (i.e. Elo of > 2000) denoted separately
analysis_top_100 <- analysis_all %>%
  filter(rank <= 100) %>%
  mutate(category = case_when(
    age <= 21 & elo >= 2000 & tour == "ATP" ~ "ATP focus",
    age <= 21 & elo >= 2000 & tour == "WTA" ~ "WTA focus",
    TRUE ~ "Other"))

### VISUALISE

## SET UP CUSTOM COLOURS

atp_colour <- "#A4CCDB"
wta_colour <- "#490C99"
custom_palette <- c(atp_colour, wta_colour)
atp_colour_focus <- "#0387c8"
background_colour <- "#DCCCA3"
custom_palette_2 <- c(atp_colour_focus, background_colour, wta_colour)

## PLOT 1: FREQUENCY POLYGON

# create frequency polygon (of share of observations rather than count) using geom_area not geom_freqpoly (to allow for fill below lines)
peak_freqpoly <- ggplot(analysis_filtered, aes(peak_age, after_stat(density), colour = tour, fill = tour)) +
  geom_area(stat = "bin", binwidth = 1, alpha = 0.25, position = "identity", size = 1.5) +
  scale_y_continuous(limits = c(0, 0.21),
                     breaks = seq(0,0.2,0.05),
                     expand = c(0,0),
                     labels = NULL) +
  scale_color_manual(values = custom_palette, aesthetics = c("colour", "fill")) +
  theme(legend.position = "none",
        axis.title.x = element_blank(),
        axis.line.y = element_blank(),
        axis.ticks.y = element_blank())
peak_freqpoly

# add custom theme to plot
themed_freqpoly <- peak_freqpoly + 
  theme_ptb()
themed_freqpoly

# add labelling to plot
labelled_freqpoly <- themed_freqpoly +
  labs(title = "Players peak earlier on the <span style='color:#490C99;'>WTA Tour</span>",
       subtitle = "Distribution of current <b style='color:#A4CCDB;'>ATP</b> and <b style='color:#490C99;'>WTA</b> players who reached<br>their peak Tennis Abstract Elo rating at each age",
       caption = "<i>excluding players currently within 12 months of their peak</i><br><br>Data: Tennis Abstract | Chart: Plot the Ball")
labelled_freqpoly

final_plot <- labelled_freqpoly

## PLOT 2: SCATTER

# create small multiple scatter
current_scatter <- ggplot(analysis_top_100, aes(x = age, y = elo, colour = category)) +
  geom_point(alpha = 0.75, size = 2.5) +
  scale_color_manual(values = custom_palette_2, aesthetics = "colour") +
  theme(legend.position = "none",
        axis.title.x = element_blank(),
        strip.text = element_blank()) +
  scale_y_continuous(breaks = seq(1800,2200,200)) +
  facet_rep_wrap(~tour, nrow = 2, repeat.tick.labels =  TRUE)
current_scatter

# add custom theme to plot
themed_scatter <- current_scatter +
  theme_ptb()
themed_scatter

# add labelling to plot
labelled_scatter <- themed_scatter +
  labs(title = "Each tour has two elite under-21 players",
       subtitle = "Age and current Tennis Abstract Elo rating of players<br>ranked in the top 100* on the <b style='color:#0387c8;'>ATP</b> and <b style='color:#490C99;'>WTA</b> Tours",
       caption = "<i>* according to Tennis Abstract's Elo rankings at 11 July 2022</i><br><br>Data: Tennis Abstract | Chart: Plot the Ball")
labelled_scatter

final_plot_2 <- labelled_scatter

#### NOTES
#### code for custom theme elements not included above
