# load packages...
library(tidyverse)
# ...for scraping,...
library(rvest)
# ...for analysis...
library(janitor)
library(lubridate)
# ...and for visualisation
library(gt)
library(ggtext)

### OBTAIN, CLEAN & ANALYSE DATA

## IDENTIFY PLAYERS MEETING CRITERIA

## 16YOs to have played for Canada in the men's World Juniors:
## C Bedard, C McDavid, S Crosby, J Bouwmeester, J Spezza, E Lindros, B Campbell, W Gretzky
## SOURCE: https://www.iihf.com/en/events/2022/wm20/news/31427/canada_s_16-year-old_scorers

## U16s to have been granted exceptional player status in the CHL:
## M Misa, C Bedard, S Wright, J Veleno, S Day, C McDavid, A Ekblad, J Tavares
## SOURCE: https://www.sportsnet.ca/juniors/article/michael-misa-granted-exceptional-player-status-eligible-for-ohl-draft/
## SOURCE: https://chl.ca/article/connor-bedard-granted-exceptional-player-status

## GRAB PLAYER STATISTICS FROM PROFILE PAGES ON HOCKEYDB.COM

# set up base URL
hdb_base <- "https://www.hockeydb.com/ihdb/stats/pdisplay.php?pid="

# assign player IDs to variables
bedard_hdb <- "240679"
wright_hdb <- "227271"
veleno_hdb <- "187148"
day_hdb <- "170115"
mcdavid_hdb <- "160293"
ekblad_hdb <- "145037"
tavares_hdb <- "89823"
crosby_hdb <- "73288"
bouwmeester_hdb <- "46419"
spezza_hdb <- "42311"
lindros_hdb <- "3158"
campbell_hdb <- "7289"
gretzky_hdb <- "2035"

# create list of IDs
player_ids <- c(bedard_hdb, wright_hdb, veleno_hdb, day_hdb, mcdavid_hdb, ekblad_hdb, tavares_hdb, crosby_hdb, bouwmeester_hdb, spezza_hdb, lindros_hdb, campbell_hdb, gretzky_hdb)

# set up blank dataframes to house final data
final_table <- tibble()
final_vitals <- tibble()

# set up web scrape for each player
for (i in player_ids) {
  
  # create URL
  player_url <- str_c(hdb_base, i)
  
  # grab all tables...
  all_tables <- read_html(player_url) %>%
    html_nodes("table") %>%
    html_table()
  # ...and select table of player's domestic statistics
  key_table <- all_tables[[1]] %>%
    clean_names()
  
  # split between regular season and playoff statistics
  data_regular <- key_table[1:7]
  data_playoffs <- key_table %>%
    select(-c(4, 5, 6, 7, 8, 9, 14))
  
  # rename columns
  col_names <- unlist(data_regular[1,])
  names(data_regular) <- col_names
  names(data_playoffs) <- col_names
  
  # add category detail to each table
  data_regular <- data_regular %>%
    mutate(category = "Regular season",
           .after = Season)
  data_playoffs <- data_playoffs %>%
    mutate(category = "Playoffs",
           .after = Season)
  
  # recombine into single record
  data_all <- rbind(data_regular, data_playoffs) %>%
    clean_names() %>%
    filter(season != "Season") %>%
    filter(team != "NHL Totals") %>%
    mutate(player_id = i,
           .before = season)
  
  # bind to complete listing
  final_table <- rbind(final_table, data_all)
  
  # grab player's vital info
  vitals <- read_html(player_url) %>%
    html_node(".v1") %>%
    html_text()
  
  # tidy from text string into tabular format
  vitals_tidy <- str_split_fixed(string = vitals, pattern = "\n", n = Inf)
  vitals_tidy <- c(vitals_tidy[2], vitals_tidy[4])
  vitals_tidy <- str_split_fixed(string = vitals_tidy, pattern = " -- ", n = Inf)
  vitals_tidy <- c(vitals_tidy[1], vitals_tidy[2], vitals_tidy[4], vitals_tidy[8])
  vitals_tidy <- str_split_fixed(string = vitals_tidy, pattern = "Born", n = Inf)
  vitals_tidy <- c(vitals_tidy[1], vitals_tidy[2], vitals_tidy[3], vitals_tidy[4], vitals_tidy[7])
  vitals_tidy <- str_split_fixed(string = vitals_tidy, pattern = "shoots ", n = Inf)
  vitals_tidy <- c(vitals_tidy[1], vitals_tidy[2], vitals_tidy[4], vitals_tidy[5], vitals_tidy[8])
  vitals_tidy <- str_split_fixed(string = vitals_tidy, pattern = "\\[", n = Inf)
  vitals_tidy <- c(vitals_tidy[1], vitals_tidy[2], vitals_tidy[4], vitals_tidy[5], vitals_tidy[8])
  vitals_tidy <- str_split_fixed(string = vitals_tidy, pattern = "/", n = Inf)
  vitals_tidy <- c(vitals_tidy[1], vitals_tidy[2], vitals_tidy[3], vitals_tidy[4], vitals_tidy[5] , vitals_tidy[10])
  vitals_tidy <- str_remove_all(string = vitals_tidy, pattern = "\\]")
  vitals_tidy <- str_squish(vitals_tidy)
  vitals_table <- tibble("Player ID" = i,
                         "Name" = vitals_tidy[1],
                         "Position" = vitals_tidy[2],
                         "Date of birth" = vitals_tidy[3],
                         "Shoots" = vitals_tidy[4],
                         "Height" = vitals_tidy[5],
                         "Weight" = vitals_tidy[6]) %>%
    clean_names()
  
  # bind to complete listing
  final_vitals <- rbind(final_vitals, vitals_table)
  
}

# identify players with vitals formatted differently
id_issues <- c(lindros_hdb, gretzky_hdb)

# set up blank tibble
vitals_fix <- tibble()

# fix vitals data for Lindros and Gretzky
for (i in id_issues) {
  
  # create URL
  player_url <- str_c(hdb_base, i)
  
  # grab player's vital info
  vitals <- read_html(player_url) %>%
    html_node(".v1") %>%
    html_text()
  
  # tidy from text string into tabular format
  vitals_tidy <- str_split_fixed(string = vitals, pattern = "\n", n = Inf)
  vitals_tidy <- c(vitals_tidy[2], vitals_tidy[4])
  vitals_tidy <- str_split_fixed(string = vitals_tidy, pattern = " -- ", n = Inf)
  vitals_tidy <- c(vitals_tidy[1], vitals_tidy[2], vitals_tidy[4], vitals_tidy[6])
  vitals_tidy <- str_split_fixed(string = vitals_tidy, pattern = "Born", n = Inf)
  vitals_tidy <- c(vitals_tidy[1], vitals_tidy[2], vitals_tidy[4], vitals_tidy[6])
  vitals_tidy <- str_split_fixed(string = vitals_tidy, pattern = "\\[", n = Inf)
  vitals_tidy <- c(vitals_tidy[1], vitals_tidy[2], vitals_tidy[4], vitals_tidy[7])
  vitals_tidy <- str_split_fixed(string = vitals_tidy, pattern = "/", n = Inf)
  vitals_tidy <- c(vitals_tidy[1], vitals_tidy[2], vitals_tidy[3], vitals_tidy[4], vitals_tidy[8])
  vitals_tidy <- str_remove_all(string = vitals_tidy, pattern = "\\]")
  vitals_tidy <- str_squish(vitals_tidy)
  vitals_table <- tibble("Player ID" = i,
                         "Name" = vitals_tidy[1],
                         "Position" = vitals_tidy[2],
                         "Date of birth" = vitals_tidy[3],
                         "Shoots" = case_when(i == lindros_hdb ~ "R",
                                              i == gretzky_hdb ~ "L"),
                         "Height" = vitals_tidy[4],
                         "Weight" = vitals_tidy[5]) %>%
    clean_names()
  
  # bind to complete listing
  vitals_fix <- rbind(vitals_fix, vitals_table)
  
}

# add corrected data into dataframe
final_vitals <- final_vitals %>%
  filter(!player_id %in% vitals_fix$player_id)
final_vitals <- rbind(final_vitals, vitals_fix)

# reformat date of birth info
final_vitals$date_of_birth <- mdy(final_vitals$date_of_birth)

# join vital info and statistics
data_complete <- left_join(x = final_table, y = final_vitals, by = c("player_id" = "player_id"))
data_complete <- data_complete %>%
  relocate(c(10:15), .after =  1) %>%
  select(-c(5:7))

# reformat statistics columns
data_complete[9:12] <- sapply(data_complete[9:12], as.integer)

# remove blank seasons
data_complete <- data_complete %>%
  filter(!is.na(gp))

# obtain list of leagues and specify list for filter
leagues_list <- unique(data_complete$lge)
selected_leagues <- c("WHL", "OHL", "QMJHL", "NHL")

# filter to selected leagues
data_complete <- data_complete %>%
  filter(lge %in% selected_leagues)

# add category info
data_complete <- data_complete %>%
  mutate(level = case_when(lge == "NHL" ~ "Senior",
                           TRUE ~ "Junior"),
         .before = lge)

# add age info
data_complete <- data_complete %>%
  mutate(season_age = as.integer(substr(season, 1, 4)) - as.integer(year(date_of_birth)),
         .after = season)

## FILTER TO RELEVANT DATA FOR GRAPHIC 1

# filter to data for forwards only
data_forwards <- data_complete %>%
  filter(position != "Defense")

# filter to seasons at age 16 or younger
forwards_u16 <- data_forwards %>%
  filter(season_age <= 16)

# summarise U16 statistics for each player...
forwards_u16_summary <- forwards_u16 %>%
  group_by(name) %>%
  summarise(tot_gp = sum(gp),
            tot_g = sum(g),
            tot_a = sum(a),
            tot_pts = sum(pts),
            g_game = tot_g / tot_gp,
            a_game = tot_a / tot_gp,
            pts_game = tot_pts / tot_gp) %>%
  arrange(desc(pts_game))
# ...and create final table to be visualised
display_table_data <- forwards_u16_summary %>%
  select(-tot_g, -tot_a, -tot_pts)

## FILTER TO RELEVANT DATA FOR GRAPHIC 2

# filter to NHL regular season statistics only...
nhl_forwards <- data_forwards %>%
  filter(level == "Senior") %>%
  filter(category == "Regular season") %>%
  mutate(season_start = as.integer(substr(x = season, start = 1, stop = 4)),
         .after = season)
# ...and adjust points figures for seasons which were not 82 games in length
nhl_lockout_adj <- nhl_forwards %>%
  mutate(tot_games = case_when(season_start <= 1991 ~ 80,
                               season_start <= 1993 ~ 84,
                               season_start == 1994 ~ 48,
                               season_start <= 2011 ~ 82,
                               season_start == 2012 ~ 48,
                               season_start <= 2018 ~ 82,
                               season_start == 2019 ~ 71,
                               season_start == 2020 ~ 56,
                               TRUE ~ 82),
         adj_pts = round(pts * 82 / tot_games, 0),
         adj_val = adj_pts - pts) %>%
  arrange(season_start, desc(name))

### VISUALISE

## GRAPHIC 1: TABLE

# set up custom palette of league colours
chl_palette <- c("#522f91", "#522f9120", "#414141")

# set up basic table
display_tbl <- display_table_data %>%
  gt(rowname_col = "name")
display_tbl

# add formatting, headers and notes
tbl_formatted <- display_tbl %>%
  tab_header(title = html("<b><span style='color:#522f91'>Bedard</span>'s production puts him in elite company</b>"),
             subtitle = html("CHL performance of forwards granted exceptional status by league or selected for World Juniors at 16 in their age-15 and age-16 seasons<br><br>")) %>%
  tab_source_note(source_note = html("<br>Data: hockeyDB.com | Table: Plot the Ball")) %>%
  tab_stubhead(label = "Player") %>%
  tab_spanner(label = md("PER-GAME STATISTICS"),
              columns = c(g_game, a_game, pts_game)) %>%
  fmt_number(columns = c(g_game, a_game, pts_game), decimals = 1) %>%
  cols_label(tot_gp = "Games",
             g_game = "Goals",
             a_game = "Assists",
             pts_game = "Points") %>%
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
                         cell_fill(color = chl_palette[3]))) %>%
  # format stub
  tab_style(locations = cells_stub(rows = TRUE),
            style =  list(cell_text(align = "left",
                                    color = ptb_dark_grey),
                          cell_borders(sides = c("top", "bottom"),
                                       weight = px(1),
                                       color = ptb_light_grey))) %>%
  # format body
  tab_style(locations = cells_body(),
            style = list(cell_text(color = ptb_dark_grey),
                         cell_borders(sides = c("top", "bottom"),
                                      weight = px(1),
                                      color = ptb_light_grey))) %>%
  # format source notes
  tab_style(locations = cells_source_notes(),
            style = list(cell_text(align = "right",
                                   color = ptb_mid_grey))) %>%
  # format key row
  tab_style(locations =  list(cells_body(columns = everything(),
                                         rows = name == "Connor Bedard"),
                              cells_stub(rows = name == "Connor Bedard")),
            style = list(cell_text(color = chl_palette[1],
                                   weight = "bold"),
                         cell_fill(color = chl_palette[2]))) %>%
  # format all other rows
  tab_style(locations =  list(cells_body(columns = c(g_game, a_game),
                                         rows = name != "Connor Bedard")),
            style = list(cell_text(color = ptb_mid_grey))) %>%
  # format all other row stubs
  tab_style(locations =  list(cells_stub(rows = name != "Connor Bedard")),
            style = list(cell_text(weight = "bold"))) %>%
  # format key column
  tab_style(locations = list(cells_body(columns = pts_game)),
            style = list(cell_text(weight = "bold")))
tbl_formatted

## GRAPHIC 2: BAR CHART

# set up custom palette of league colours
nhl_palette <- c("#f29c39", "#00000070")

# create basic stacked bar chart
rough_bar <- ggplot(data = nhl_lockout_adj, mapping = aes(x = season_start, y = adj_pts, fill = name)) +
  geom_bar(position = "stack", stat = "identity", width = 1, colour = "#FFFFFF", fill = if_else(nhl_lockout_adj$player_id == "2035", nhl_palette[1], nhl_palette[2])) +
  scale_y_continuous(name = NULL,
                     limits = c(0, 410),
                     expand = c(0,0),
                     breaks = seq(0, 400, 100)) +
  scale_x_continuous(name = NULL,
                     limits = c(1978, 2022),
                     expand = c(0,0),
                     breaks = seq(1980, 2020, 10))
rough_bar

# add custom theme to plot
themed_bar <- rough_bar + 
  theme_ptb() +
  theme(legend.position = "none",
        panel.grid.major.x = element_blank())
themed_bar

# add labelling to plot
final_bar <- themed_bar + 
  labs(title = "<span style='color:#f29c39;'>Gretzky</span>'s contributions are unmatched",
       subtitle = "Combined NHL points* recorded by forwards granted CHL<br>exceptional status or selected for World Juniors at age 16",
       caption = "<i><span style='color:#444F5A60;'>*Regular season only; points recorded in seasons shorter (or longer) than 82<br>games adjusted to 82-game pace</i></span><br><br>Data: hockeyDB.com | Chart: Plot the Ball")
final_bar

#### NOTES
#### code for custom theme elements not included above
