# load packages...
library(tidyverse)
# ...for scraping,...
library(rvest)
# ...for analysis...
library(janitor)
# ...and for visualisation
library(ggtext)
library(lemon)

### OBTAIN, CLEAN & ANALYSE DATA

## GRAB DATA FROM PRO FOOTBALL REFERENCE

# set up base URL and other URL components for relevant years
base_url <- "https://www.pro-football-reference.com/years/"
years <- seq(2000,2022, by = 1)
end_url <- "/draft.htm"

# set up blank dataframe to house final data
all_qbs <- tibble()

# set up web scrape for each draft
for (i in years) {
  
  # create URL
  draft_url <- str_c(base_url, i, end_url)
  
  # read in page HTML
  html <- read_html(draft_url)
  
  # grab all tables...
  tables <- html %>%
    html_nodes("table") %>%
    html_table()
  
  # ...and select table of each drafted player's career statistics
  key_table <- tables[[1]]
  
  # rename columns...
  new_names <- unlist(key_table[1,])
  old_names <- names(key_table)
  new_names_final <- str_c(old_names, " ", new_names)
  names(key_table) <- new_names_final
  # ...and select only important columns
  key_table <- key_table %>%
    clean_names() %>%
    select(1:5,
           13:21)
  
  # remove irrelevant rows
  key_table <- key_table %>%
    filter(key_table$rnd != "Rnd") %>%
    mutate(year = i,
           .before = rnd)
  
  # reformat numeric columns
  key_table[1:3] <- sapply(key_table[1:3], as.integer)
  key_table[7:15] <- sapply(key_table[7:15], as.integer)
  
  # filter only to drafted quarterbacks
  qb_table <- key_table %>%
    filter(pos == "QB")
  
  # bind to complete listing
  all_qbs <- rbind(all_qbs, qb_table)
  
}

## TIDY AND SUMMARISE

# filter out players with no career games played
all_qbs <- all_qbs %>%
  replace(is.na(.), 0)

# summarise statistics by draft year...
summary_by_year <- all_qbs %>%
  group_by(year) %>%
  summarise(no_qbs = n(),
            tot_g = sum(g),
            tot_passing_cmp = sum(passing_cmp),
            tot_passing_att = sum(passing_att),
            tot_passing_yds = sum(passing_yds),
            tot_passing_td = sum(passing_td),
            tot_passing_int = sum(passing_int),
            tot_rushing_att = sum(rushing_att),
            tot_rushing_yds = sum(rushing_yds),
            tot_rushing_td = sum(rushing_td))
# ...and calculate key metrics for analysis
summary_by_year <- summary_by_year %>%
  mutate(pass_yards_per_pass_att = tot_passing_yds / tot_passing_att,
         rush_yards_per_rush_att = tot_rushing_yds / tot_rushing_att,
         rush_yards_per_pass_att = tot_rushing_yds / tot_passing_att,
         tot_att = tot_passing_att + tot_rushing_att)

## FILTER TO RELEVANT DATA FOR GRAPHIC 1

# select key metrics columns only and filter out years with less data
data_for_plot <- summary_by_year %>%
  select(year,
         pass_yards_per_pass_att,
         rush_yards_per_rush_att,
         tot_att) %>%
  filter(year != 2021 & year != 2022)

# reformat to make suitable for small multiple bar charts...
for_plot_final <- data_for_plot %>%
  pivot_longer(cols = c(pass_yards_per_pass_att, rush_yards_per_rush_att),
               names_to = "metric",
               values_to = "value")
# ...and add category for colour encoding
for_plot_final <- for_plot_final %>%
  mutate(category = if_else(year > 2016, "Focus", "Other"))

## FILTER TO RELEVANT DATA FOR GRAPHIC 2

# calculate key metrics for each player, filter out recent years and filter out players with less career playing time
qbs_for_plot <- all_qbs %>%
  filter(year != 2021 & year != 2022) %>%
  mutate(pass_yards_per_pass_att = passing_yds / passing_att,
         rush_yards_per_rush_att = rushing_yds / rushing_att,
         rush_yards_per_pass_att = rushing_yds / passing_att,
         tot_att = passing_att + rushing_att) %>%
  filter(tot_att >= 500)

### VISUALISE

# set up custom palette of league colours
nfl_navy <- "#003069"
nfl_navy_desaturated <- "#405C80"
nfl_navy_light <- "#CFD9E6"
nfl_red <- "#d60303"
nfl_red_desaturated <- "#E68A8A"
nfl_red_light <- "#F2C2C2"

## GRAPHIC 1: SMALL MULTIPLE BARS

# customise labels for each facet
facet_labels <- c("Yards per pass attempt", "Yards per rush attempt")
names(facet_labels) <- c("pass_yards_per_pass_att", "rush_yards_per_rush_att")

# create basic small multiple bar charts
rough_bars <- ggplot(data = for_plot_final, aes(x = year, y = value, fill = category)) +
  geom_bar(stat = "identity", width = 0.8) +
  scale_fill_manual(values = c(`Other` = nfl_navy_light, `Focus` = nfl_red)) +
  facet_rep_wrap(~metric, nrow = 2, ncol = 1, labeller = labeller(metric = facet_labels), repeat.tick.labels = TRUE)
rough_bars

# add custom theme to plot
themed_bars <- rough_bars +
  theme_ptb() +
  scale_y_continuous(limits = c(0, 9.9),
                     breaks = seq(0,8,2),
                     expand = c(0,0)) +
  scale_x_continuous(name = "Year drafted",
                     limits = c(1999,2021),
                     breaks = seq(2000,2020,5),
                     expand = c(0,0)) +
  theme(legend.position = "none")
themed_bars

# add labelling to plot
labelled_bars <- themed_bars +
  labs(title = "<span style='color:#d60303'>Recently drafted</span> QBs run the ball better",
       subtitle = "Career yards per pass attempt and yards per rush attempt<br>recorded by NFL QBs drafted in each year since 2000",
       caption = "Regular season only | 2021 and 2022 QBs excluded | As at 26 Oct 2022<br><br>Data: Pro Football Reference | Chart: Plot the Ball")
labelled_bars

final_bars <- labelled_bars

## GRAPHIC 2: SCATTER PLOT

# create basic scatter plot
rough_scatter <- ggplot(qbs_for_plot, aes(x = rush_yards_per_rush_att, y = pass_yards_per_pass_att)) + 
  geom_point(size = 3,
             color = if_else(qbs_for_plot$year>=2017, nfl_red, nfl_navy_light)) +
  scale_x_continuous(limits = c(0.5, 7.5),
                     breaks = seq(1,7,1),
                     expand = c(0,0),
                     name = NULL) +
  scale_y_continuous(limits = c(3.9, 8.9),
                     breaks = seq(4,8,1),
                     expand = c(0,0),
                     name = NULL) +
  annotate(geom = "text",
           x = 0.6,
           y = 8.7,
           label = "Yards per pass attempt",
           family = ptb_font,
           colour = ptb_dark_grey,
           fontface = "bold",
           hjust = 0,
           vjust = 0) +
  annotate(geom = "text",
           x = 6.9,
           y = 4.1,
           label = "Yards per rush attempt",
           family = ptb_font,
           colour = ptb_dark_grey,
           fontface = "bold",
           hjust = 1,
           vjust = 0) +
  theme(legend.position = "none")
rough_scatter

# add custom theme to plot
themed_scatter <- rough_scatter +
  theme_ptb() +
  theme(axis.line.x = element_blank(),
        axis.line.y = element_blank())
themed_scatter

# add labelling to plot
labelled_scatter <- themed_scatter +
  labs(title = "<span style='color:#d60303'>Recently drafted</span> QBs run the ball better",
       subtitle = "Career yards per pass attempt and yards per rush attempt<br>recorded by NFL QBs drafted between 2000 and 2020",
       caption = "Regular season only | QBs with <500 attempts excluded | As at 26 Oct 2022<br><br>Data: Pro Football Reference | Chart: Plot the Ball")
labelled_scatter

final_scatter <- labelled_scatter

#### NOTES
#### code for custom theme elements not included above
