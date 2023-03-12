# load packages...
library(tidyverse)
# ...for scraping,...
library(rvest)
# ...for analysis...
library(janitor)
# ...and for visualisation
library(gt)
library(gtExtras)
library(svglite)
library(scales)
library(ggtext)

### OBTAIN, CLEAN & ANALYSE DATA

## GRAB DATA FROM ESPNCRICINFO

# set up base season URL and other URL components
base_fixtures_url <- "https://www.espncricinfo.com/series/"
end_fixtures_url <- "/match-schedule-fixtures-and-results"
scorecard_indicator <- "/full-scorecard"

# set up list of unique season IDs
id_2022 <- "wbbl-2022-23-1323553"
id_2021 <- "wbbl-2021-22-1269053"
id_2020 <- "wbbl-2020-21-1226776"
id_2019 <- "wbbl-2019-20-1188381"
id_2018 <- "wbbl-2018-19-1152722"
id_2017 <- "wbbl-2017-18-1118470"
id_2016 <- "women-s-big-bash-league-2016-17-1023541"
id_2015 <- "women-s-big-bash-league-2015-16-896429"
season_ids <- c(id_2015, id_2016, id_2017, id_2018, id_2019, id_2020, id_2021, id_2022)

# set up blank dataframe to house fixture list
fixture_urls <- tibble()

# set up web scrape for each season's fixture list
for (i in season_ids) {
  
  # create URL
  season_url <- str_c(base_fixtures_url, i, end_fixtures_url)
  
  # read in page HTML
  html <- read_html(season_url)
  
  # grab all URLs from links...
  match_urls <- html %>%
    html_nodes("a") %>%
    html_attr("href")
  #...and tidy into tibble
  urls_tidy <- match_urls %>%
    as.tibble()
  
  # filter only to match scorecards,...
  urls_filtered <- urls_tidy %>%
    filter(grepl(i, value) & grepl(scorecard_indicator, value))
  #...add season as variable to each observation,...
  urls_filtered$season <- i
  #...bind to full dataset...
  fixture_urls <- rbind(fixture_urls, urls_filtered)
  #...and check for duplicates
  fixture_urls <- unique(fixture_urls)
  
}

# set up base match URL
base_match_url <- "https://www.espncricinfo.com"

# create all match URLs
all_match_urls <- str_c(base_match_url, fixture_urls$value)

# set up blank dataframe to house batting and bowling records
batting_records <- tibble()
bowling_records <- tibble()

# set up web scrape for each match scorecard
for (i in all_match_urls) {
  
  print(paste0("Getting match: ", i))
  
  # read in page HTML
  html <- read_html(i)
  
  # grab all tables
  all_tables <- html %>%
    html_nodes("table") %>%
    html_table()
  
  # filter out incomplete matches
  if(length(all_tables) < 5) {
    next
  }
  
  # isolate tables containing batting and bowling data from each innings
  first_inns_bat <- all_tables[[1]] %>%
    clean_names()
  
  if(names(first_inns_bat[1]) != "batting") {
    next
  }
  
  first_inns_bowl <- all_tables[[2]] %>%
    clean_names()
  
  if(names(first_inns_bowl[1]) != "bowling") {
    next
  }
  
  second_inns_bat <- all_tables[[3]] %>%
    clean_names()
  
  if(names(second_inns_bat[1]) != "batting") {
    next
  }
  
  second_inns_bowl <- all_tables[[4]] %>%
    clean_names()
  
  if(names(second_inns_bowl[1]) != "bowling") {
    next
  }
  
  # add identifying characteristics to batting data as other variables and tidy
  first_inns_bat$inns_no <- 1
  first_inns_bat$order <- row(first_inns_bat[1])
  first_inns_bat$url <- i
  first_inns_bat <- first_inns_bat %>%
    filter(!grepl("Extras", batting) & !grepl("TOTAL", batting) & !grepl("Did not bat:", batting) & !grepl("Fall of wickets:", batting))
  first_inns_bat <- first_inns_bat %>%
    select(-sr, -x_2, -x_3)
  second_inns_bat$inns_no <- 2
  second_inns_bat$order <- row(second_inns_bat[1])
  second_inns_bat$url <- i
  second_inns_bat <- second_inns_bat %>%
    filter(!grepl("Extras", batting) & !grepl("TOTAL", batting) & !grepl("Did not bat:", batting) & !grepl("Fall of wickets:", batting))
  second_inns_bat <- second_inns_bat %>%
    select(-sr, -x_2, -x_3)
  
  # add identifying characteristics to bowling data as other variables and tidy
  first_inns_bowl$inns_no <- 1
  first_inns_bowl$order <- row(first_inns_bowl[1])
  first_inns_bowl$url <- i
  first_inns_bowl <- first_inns_bowl %>%
    select(-econ)
  second_inns_bowl$inns_no <- 2
  second_inns_bowl$order <- row(second_inns_bowl[1])
  second_inns_bowl$url <- i
  second_inns_bowl <- second_inns_bowl %>%
    select(-econ)
  
  # combine data for each innings together...
  all_bat <- rbind(first_inns_bat, second_inns_bat)
  all_bowl <- rbind(first_inns_bowl, second_inns_bowl)
  #...and bind to overall dataset
  batting_records <- rbind(batting_records, all_bat)
  bowling_records <- rbind(bowling_records, all_bowl)
  
}

## TIDY AND SUMMARISE

# remove captain and wicketkeeper symbols from batting data, as well as leading/trailing white space
batting_records_final <- batting_records %>%
  mutate(across(batting, str_replace_all, fixed("(c)"), "")) %>%
  mutate(across(batting, str_replace_all, fixed("†"), "")) %>%
  mutate(across(batting, str_squish))

# fix column names,...
old_names <- names(batting_records_final)
old_names[1] <- "player"
old_names[2] <- "dismissal"
old_names[9] <- "position"
new_names <- old_names
names(batting_records_final) <- new_names
batting_records_final$position <- batting_records_final$position[,1]
#...format certain columns as integers... 
batting_records_final[3:9] <- sapply(batting_records_final[3:9], as.integer)
#...and remove entries where no balls are faced by a batter in a given match
batting_records_filtered <- batting_records_final %>%
  filter(!is.na(b))

# group batting data by match to calculate match scoring rates
match_run_rates <- batting_records_filtered %>%
  group_by(url) %>%
  summarise(n_balls = sum(b),
            n_runs = sum(r))

# combine with dataset to compare match scoring rates to player scoring rates
runs_vs_exp <- left_join(x = batting_records_filtered, y = match_run_rates, by = c("url" = "url"))
runs_vs_exp <- runs_vs_exp %>%
  mutate(other_balls = n_balls - b,
         other_runs = n_runs - r,
         other_sr = other_runs / other_balls) %>%
  select(-2, -5, -6, -7, -8, -9) %>%
  relocate(url, .before = player)
runs_vs_exp <- runs_vs_exp %>%
  mutate(batter_sr = r / b,
         sr_diff = batter_sr - other_sr,
         vs_exp = b * sr_diff)
runs_vs_exp <- runs_vs_exp %>%
  filter(b > 0)

## FILTER TO RELEVANT DATA FOR GRAPHIC 1

# group data by each player across all seasons and summarise
batters_vs_exp <- runs_vs_exp %>%
  group_by(player) %>%
  summarise(total_inns = n(),
            total_balls = sum(b),
            total_runs = sum(r),
            balls_per_inns = total_balls / total_inns,
            sr = total_runs / total_balls,
            vs_exp_total = sum(vs_exp),
            vs_exp_inns = vs_exp_total / total_inns,
            vs_exp_ball = vs_exp_total / total_balls)

# filter to players with more than 1,700 balls faced
batters_vs_exp_filtered <- arrange(batters_vs_exp, desc(total_balls)) %>%
  filter(total_balls >= 1700)

# reduce table to key metrics...
batters_for_table <- batters_vs_exp_filtered %>%
  mutate(runs_per_inns = total_runs / total_inns) %>%
  select(player,
         total_inns,
         balls_per_inns,
         runs_per_inns,
         vs_exp_inns)
#...and filter out retired and overseas players
exclude_list <- c("Sophie Devine", "Suzie Bates", "Rachel Priest", "Mignon du Preez", "Alex Blackwell", "Heather Knight")
batters_for_table$category <- if_else(batters_for_table$player %in% exclude_list, "Overseas", "Australia")
batters_for_table <- batters_for_table %>%
  filter(category == "Australia")
batters_for_table$category <- NULL

# arrange by vs. expectation metric for table
batters_for_table <- batters_for_table %>%
  arrange(desc(vs_exp_inns))


## FILTER TO RELEVANT DATA FOR GRAPHIC 2

# filter to players with more than 700 balls faced...
batters_for_scatter <- batters_vs_exp_filtered %>%
  filter(total_balls >= 700) %>%
  mutate(runs_per_inns = total_runs / total_inns) %>%
  select(player,
         total_inns,
         balls_per_inns,
         runs_per_inns,
         vs_exp_inns)
# ...and add categorisation (i.e. whether or not player appears in graphic 1)
batters_for_scatter$category <- if_else(batters_for_scatter$player %in% batters_for_table$player, "Focus", "Other")

### VISUALISE

# set up custom palette of league colours
wbbl_yellow <- "#dbe02e"
wbbl_yellow_dark <- "#B3B827"
wbbl_grey <- "#575253"
category_palette <- c(wbbl_yellow_dark, ptb_mid_grey)

## GRAPHIC 1: TABLE WITH MINI BAR

# set up basic table
display_tbl <- batters_for_table %>%
  gt(rowname_col = "player")
display_tbl

# add mini bar
display_tbl <- display_tbl %>%
  gt_plt_bar(column = vs_exp_inns, keep_column = TRUE, width = 25, color = wbbl_yellow)
display_tbl

# add formatting, headers and notes
tbl_formatted <- display_tbl %>%
  tab_header(title = html("<b>Alyssa Healy's efficiency is unmatched</b>"),
             subtitle = html("Performance of current Australian batters in the WBBL since 2015<br><br><i style='font-size:90%'>Note: '+/-' measures runs scored per innings compared to expectation, which is calculated in each innings based on a batter's scoring rate relative to the scoring rate of other players in that game</i><br>")) %>%
  tab_source_note(source_note = html("Players with <1,700 balls faced in completed WBBL matches excluded<br><br>Data: ESPNCricinfo | Table: Plot the Ball")) %>%
  tab_stubhead(label = "Player") %>%
  tab_spanner(label = md("— PER INNINGS —"),
              columns = c(balls_per_inns, runs_per_inns, vs_exp_inns)) %>%
  fmt_number(columns = c(balls_per_inns, runs_per_inns), decimals = 1) %>%
  fmt_number(columns = vs_exp_inns, decimals = 1, force_sign = TRUE) %>%
  cols_label(total_inns = "Innings",
             balls_per_inns = "Balls",
             runs_per_inns = "Runs",
             vs_exp_inns = "+/-",
             DUPE_COLUMN_PLT = "") %>%
  cols_align(align = "right",
             columns = everything()) %>%
  opt_table_font(font = ptb_font) %>%
  tab_options(table.border.top.width = px(0),
              table.border.bottom.width = px(0),
              heading.border.bottom.width = px(0),
              column_labels.border.bottom.width = px(0),
              column_labels.border.top.width = px(0),
              table_body.border.top.width = px(0),
              table_body.border.bottom.width = px(0),
              stub.border.width = px(2),
              stub.border.color = ptb_light_grey) %>%
  # format title
  tab_style(locations = cells_title(groups = "title"),
            style = list(cell_text(align = "left",
                                   color = ptb_dark_grey,
                                   size = px(20)))) %>%
  # format subtitle
  tab_style(locations = cells_title(groups = "subtitle"),
            style = list(cell_text(align = "left",
                                   color = ptb_dark_grey,
                                   size = px(15)))) %>%
  # format stubhead, column spanners and labels
  tab_style(locations = list(cells_column_spanners(),
                             cells_column_labels(),
                             cells_stubhead()),
            style = list(cell_text(color = "#FFFFFF",
                                   weight = "bold"),
                         cell_fill(color = wbbl_grey))) %>%
  # format stub
  tab_style(locations = cells_stub(rows = TRUE),
            style =  list(cell_text(align = "left",
                                    color = ptb_dark_grey,
                                    size = "small"),
                          cell_borders(sides = c("top", "bottom"),
                                       weight = px(1),
                                       color = ptb_light_grey))) %>%
  # format body
  tab_style(locations = cells_body(),
            style = list(cell_text(color = ptb_dark_grey,
                                   size = "small"),
                         cell_borders(sides = c("top", "bottom"),
                                      weight = px(1),
                                      color = ptb_light_grey))) %>%
  # format source notes
  tab_style(locations = cells_source_notes(),
            style = list(cell_text(align = "right",
                                   color = ptb_mid_grey)))  %>%
  # format row stubs
  tab_style(locations =  list(cells_stub(rows = everything())),
            style = list(cell_text(weight = "bold"))) %>%
  # format key column
  tab_style(locations = list(cells_body(columns = vs_exp_inns)),
            style = list(cell_text(weight = "bold"))) %>%
  # format other columns
  tab_style(locations = list(cells_body(columns = c(balls_per_inns, runs_per_inns))),
            style = list(cell_text(color = ptb_mid_grey)))
tbl_formatted


## GRAPHIC 2: SCATTER PLOT

# set up basic scatter plot
scatter_of_batters <- ggplot(batters_for_scatter, aes(x = balls_per_inns, y = vs_exp_inns, colour = category)) + 
  geom_point(size = 3, alpha = case_when(batters_for_scatter$category == "Focus" ~ .75,
                                         TRUE ~ 0.25)) +
  scale_x_continuous(name = NULL,
                     limits = c(0, 39),
                     breaks = seq(0, 30, 10),
                     expand = c(0,0),
                     labels = comma) +
  scale_y_continuous(name = NULL,
                     limits = c(-3.9, 5.9),
                     breaks = seq(-2, 4, 2),
                     expand = c(0,0),
                     labels = label_number(style_positive = "plus")) +
  scale_colour_manual(values = category_palette, aesthetics = "colour") +
  annotate(geom = "text",
           x = 1,
           y = 5.8,
           label = "Runs vs.\nexpectation\nper innings",
           family = ptb_font,
           colour = ptb_dark_grey,
           fontface = "bold",
           hjust = 0,
           vjust = 1) +
  annotate(geom = "text",
           x = 38,
           y = -2.8,
           label = "Balls per\n innings",
           family = ptb_font,
           colour = ptb_dark_grey,
           fontface = "bold",
           hjust = 1,
           vjust = 1) +
  theme(legend.position = "none")
scatter_of_batters

# add custom theme to plot...
scatter_themed <- scatter_of_batters +
  theme_ptb() +
  theme(axis.line.x = element_blank()) +
  # ...with intercept line at y = 0
  geom_hline(yintercept = 0,
             colour = ptb_dark_grey)
scatter_themed

# add labelling to plot
scatter_labelled <- scatter_themed +
  labs(title = "Alyssa Healy's efficiency is unmatched",
       subtitle = "Performance of batters in the WBBL since 2015",
       caption = "<i>Runs vs. expectation is calculated in each innings based on a batter's<br>scoring rate relative to the scoring rate of other players in that game</i><br><br>Players with <700 balls faced in completed WBBL matches excluded<br><br>Data: ESPNCricinfo | Chart: Plot the Ball")
scatter_labelled

#### NOTES
#### code for custom theme elements not included above
