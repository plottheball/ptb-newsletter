# load packages...
library(tidyverse)
# ...for scraping,...
library(rvest)
# ...for analysis...
library(lubridate)
library(RcppRoll)
# ...and for visualisation
library(extrafont)
library(ggtext)
library(scales)
library(lemon)

### SCRAPE

## SET UP URL LISTS

# define basic url structures for right-arm pace bowlers,...
url_base_right_pace <- "https://stats.espncricinfo.com/ci/engine/stats/index.html?bowling_hand=1;bowling_pacespin=1;class=1;filter=advanced;orderby=start;page="
first_page <- 1
url_end <- ";size=200;spanmin1=1+Jan+1946;spanval1=span;template=results;type=bowling;view=innings;wrappertype=print"
url_full_right_pace_first <- str_c(url_base_right_pace, first_page, url_end)
# ...left-arm pace bowlers,
url_base_left_pace <- "https://stats.espncricinfo.com/ci/engine/stats/index.html?bowling_hand=2;bowling_pacespin=1;class=1;filter=advanced;orderby=start;page="
first_page <- 1
url_full_left_pace_first <- str_c(url_base_left_pace, first_page, url_end)
# ...right-arm spin bowlers...
url_base_right_spin <- "https://stats.espncricinfo.com/ci/engine/stats/index.html?bowling_hand=1;bowling_pacespin=2;class=1;filter=advanced;orderby=start;page="
first_page <- 1
url_end <- ";size=200;spanmin1=1+Jan+1946;spanval1=span;template=results;type=bowling;view=innings;wrappertype=print"
url_full_right_spin_first <- str_c(url_base_right_spin, first_page, url_end)
# ...and left-arm spin bowlers
url_base_left_spin <- "https://stats.espncricinfo.com/ci/engine/stats/index.html?bowling_hand=2;bowling_pacespin=2;class=1;filter=advanced;orderby=start;page="
first_page <- 1
url_full_left_spin_first <- str_c(url_base_left_spin, first_page, url_end)

# grab total no. of pages of data for right-arm pace bowlers,...
no_pages_right_pace_source <- read_html(url_full_right_pace_first) %>%
  html_node(xpath = "/html/body/div/div[3]/table[2]") %>%
  html_table()
no_pages_right_pace <- unlist(no_pages_right_pace_source[1,1]) %>%
  str_replace(., "Page 1 of ", "") %>%
  as.numeric()
rm(no_pages_right_pace_source)
# ...left-arm pace bowlers,...
no_pages_left_pace_source <- read_html(url_full_left_pace_first) %>%
  html_node(xpath = "/html/body/div/div[3]/table[2]") %>%
  html_table()
no_pages_left_pace <- unlist(no_pages_left_pace_source[1,1]) %>%
  str_replace(., "Page 1 of ", "") %>%
  as.numeric()
rm(no_pages_left_pace_source)
# ...right-arm spin bowlers,...
no_pages_right_spin_source <- read_html(url_full_right_spin_first) %>%
  html_node(xpath = "/html/body/div/div[3]/table[2]") %>%
  html_table()
no_pages_right_spin <- unlist(no_pages_right_spin_source[1,1]) %>%
  str_replace(., "Page 1 of ", "") %>%
  as.numeric()
rm(no_pages_right_spin_source)
# ...and left-arm spin bowlers
no_pages_left_spin_source <- read_html(url_full_left_spin_first) %>%
  html_node(xpath = "/html/body/div/div[3]/table[2]") %>%
  html_table()
no_pages_left_spin <- unlist(no_pages_left_spin_source[1,1]) %>%
  str_replace(., "Page 1 of ", "") %>%
  as.numeric()
rm(no_pages_left_spin_source)

# create sequence for pages of each category of data
rank_right_pace <- seq(first_page, no_pages_right_pace, 1)
rank_left_pace <- seq(first_page, no_pages_left_pace, 1)
rank_right_spin <- seq(first_page, no_pages_right_spin, 1)
rank_left_spin <- seq(first_page, no_pages_left_spin, 1)

# build URLs for all pages
links_right_pace <- str_c(url_base_right_pace, rank_right_pace, url_end)
links_left_pace <- str_c(url_base_left_pace, rank_left_pace, url_end)
links_right_spin <- str_c(url_base_right_spin, rank_right_spin, url_end)
links_left_spin <- str_c(url_base_left_spin, rank_left_spin, url_end)

## SET UP SCRAPE

# empty tibbles to store data
bowling_data_right_pace <- tibble()
bowling_data_left_pace <- tibble()
bowling_data_right_spin <- tibble()
bowling_data_left_spin <- tibble()

# scrape 1: right-arm pace
for (i in links_right_pace) {
  print(Sys.time())
  Sys.sleep(2)
  
  # read page in loop and select HTML table
  table_data <- read_html(i) %>%
    html_node(xpath = "/html/body/div/div[3]/table[3]") %>%
    html_table()
  
  # combine with existing record (with re-classifications to prevent 'Can't combine...' errors)
  table_data$Inns <- as.character(table_data$Inns)
  table_data$Overs <- as.character(table_data$Overs)
  table_data$Mdns <- as.character(table_data$Mdns)
  table_data$Runs <- as.character(table_data$Runs)
  table_data$Wkts <- as.character(table_data$Wkts)
  table_data$Econ <- as.character(table_data$Econ)
  bowling_data_right_pace <- bind_rows(bowling_data_right_pace, table_data)
}

# scrape 2: left-arm pace
for (i in links_left_pace) {
  print(Sys.time())
  Sys.sleep(2)
  
  # read page in loop and select HTML table
  table_data <- read_html(i) %>%
    html_node(xpath = "/html/body/div/div[3]/table[3]") %>%
    html_table()
  
  # combine with existing record (with re-classifications to prevent 'Can't combine...' errors)
  table_data$Inns <- as.character(table_data$Inns)
  table_data$Overs <- as.character(table_data$Overs)
  table_data$Mdns <- as.character(table_data$Mdns)
  table_data$Runs <- as.character(table_data$Runs)
  table_data$Wkts <- as.character(table_data$Wkts)
  table_data$Econ <- as.character(table_data$Econ)
  bowling_data_left_pace <- bind_rows(bowling_data_left_pace, table_data)
}

# scrape 3: right-arm spin
for (i in links_right_spin) {
  print(Sys.time())
  Sys.sleep(2)
  
  # read page in loop and select HTML table
  table_data <- read_html(i) %>%
    html_node(xpath = "/html/body/div/div[3]/table[3]") %>%
    html_table()
  
  # combine with existing record (with re-classifications to prevent 'Can't combine...' errors)
  table_data$Inns <- as.character(table_data$Inns)
  table_data$Overs <- as.character(table_data$Overs)
  table_data$Mdns <- as.character(table_data$Mdns)
  table_data$Runs <- as.character(table_data$Runs)
  table_data$Wkts <- as.character(table_data$Wkts)
  table_data$Econ <- as.character(table_data$Econ)
  bowling_data_right_spin <- bind_rows(bowling_data_right_spin, table_data)
}

# scrape 4: left-arm spin
for (i in links_left_spin) {
  print(Sys.time())
  # how long to sleep for
  Sys.sleep(2)
  
  # read page in our loop and grab data table
  table_data <- read_html(i) %>%
    html_node(xpath = "/html/body/div/div[3]/table[3]") %>%
    html_table()
  
  # combine with existing record (with re-classifications to prevent 'Can't combine...' errors)
  table_data$Inns <- as.character(table_data$Inns)
  table_data$Overs <- as.character(table_data$Overs)
  table_data$Mdns <- as.character(table_data$Mdns)
  table_data$Runs <- as.character(table_data$Runs)
  table_data$Wkts <- as.character(table_data$Wkts)
  table_data$Econ <- as.character(table_data$Econ)
  bowling_data_left_spin <- bind_rows(bowling_data_left_spin, table_data)
}

### CLEAN

# add left-arm and right-arm pace/spin labels, and tidy column names
left_arm_pacers <- bowling_data_left_pace %>%
  mutate(bowler_name = Player,
         bowler_type = 'left_pace',
         match_date = `Start Date`,
         venue = Ground,
         opponent = Opposition,
         balls_per_over = BPO,
         match_inns = Inns,
         overs_bowled = Overs,
         maidens_bowled = Mdns,
         wickets_taken = Wkts,
         runs_conceded = Runs,
         .keep = "none")
right_arm_pacers <- bowling_data_right_pace %>%
  mutate(bowler_name = Player,
         bowler_type = 'right_pace',
         match_date = `Start Date`,
         venue = Ground,
         opponent = Opposition,
         balls_per_over = BPO,
         match_inns = Inns,
         overs_bowled = Overs,
         maidens_bowled = Mdns,
         wickets_taken = Wkts,
         runs_conceded = Runs,
         .keep = "none")
left_arm_spinners <- bowling_data_left_spin %>%
  mutate(bowler_name = Player,
         bowler_type = 'left_spin',
         match_date = `Start Date`,
         venue = Ground,
         opponent = Opposition,
         balls_per_over = BPO,
         match_inns = Inns,
         overs_bowled = Overs,
         maidens_bowled = Mdns,
         wickets_taken = Wkts,
         runs_conceded = Runs,
         .keep = "none")
right_arm_spinners <- bowling_data_right_spin %>%
  mutate(bowler_name = Player,
         bowler_type = 'right_spin',
         match_date = `Start Date`,
         venue = Ground,
         opponent = Opposition,
         balls_per_over = BPO,
         match_inns = Inns,
         overs_bowled = Overs,
         maidens_bowled = Mdns,
         wickets_taken = Wkts,
         runs_conceded = Runs,
         .keep = "none")

# combine into one record per type
pace_bowlers_all <- rbind(left_arm_pacers, right_arm_pacers)
spin_bowlers_all <- rbind(left_arm_spinners, right_arm_spinners)

# fix naming convention to preempt error
spin_bowlers_all$bowler_name <- str_replace_all(spin_bowlers_all$bowler_name,fixed("(3)"), "III")

# remove non-bowling appearances
pace_bowlers_filtered <- pace_bowlers_all %>%
  filter(overs_bowled != "DNB") %>%
  filter(overs_bowled != "TDNB") %>%
  filter(overs_bowled != "sub")
spin_bowlers_filtered <- spin_bowlers_all %>%
  filter(overs_bowled != "DNB") %>%
  filter(overs_bowled != "TDNB") %>%
  filter(overs_bowled != "sub")

# tidy text columns: 
# (i) split up bowler/team names 
pace_bowlers_filtered <- pace_bowlers_filtered %>%
  separate(col = bowler_name,
           into = c("bowler_name", "bowler_team"),
           sep =  "[(]")
spin_bowlers_filtered <- spin_bowlers_filtered %>%
  separate(col = bowler_name,
           into = c("bowler_name", "bowler_team"),
           sep =  "[(]")
# (ii) tidy bowler/team name text
pace_bowlers_filtered$bowler_name <-  str_trim(pace_bowlers_filtered$bowler_name)
pace_bowlers_filtered$bowler_team <-  gsub(")", "", pace_bowlers_filtered$bowler_team)
spin_bowlers_filtered$bowler_name <-  str_trim(spin_bowlers_filtered$bowler_name)
spin_bowlers_filtered$bowler_team <-  gsub(")", "", spin_bowlers_filtered$bowler_team)
# (iii) tidy opponent name text
pace_bowlers_filtered$opponent <-  gsub("v ", "", pace_bowlers_filtered$opponent)
spin_bowlers_filtered$opponent <-  gsub("v ", "", spin_bowlers_filtered$opponent)
# (iv) adjust team names for consistency with opponent column
team_names <- tibble(short = sort(unique(pace_bowlers_filtered$bowler_team)),
                     long = sort(unique(pace_bowlers_filtered$opponent)))
pace_bowlers_filtered <- left_join(x = pace_bowlers_filtered,
                                   y = team_names,
                                   by = c("bowler_team" = "short"))
pace_bowlers_filtered <- pace_bowlers_filtered %>%
  mutate(bowler_team = long,
         long = NULL)
spin_bowlers_filtered <- left_join(x = spin_bowlers_filtered,
                                   y = team_names,
                                   by = c("bowler_team" = "short"))
spin_bowlers_filtered <- spin_bowlers_filtered %>%
  mutate(bowler_team = long,
         long = NULL)

# tidy data columns:
# (i) split overs in completed overs/balls bowled
pace_bowlers_filtered <- pace_bowlers_filtered %>%
  separate(col = overs_bowled,
           into = c("completed_overs", "completed_balls"),
           sep = "[.]")
pace_bowlers_filtered$completed_balls <- replace_na(pace_bowlers_filtered$completed_balls, 0)
spin_bowlers_filtered <- spin_bowlers_filtered %>%
  separate(col = overs_bowled,
           into = c("completed_overs", "completed_balls"),
           sep = "[.]")
spin_bowlers_filtered$completed_balls <- replace_na(spin_bowlers_filtered$completed_balls, 0)
# (ii) reclass data columns to integer
pace_bowlers_filtered[7:13] <- sapply(pace_bowlers_filtered[7:13], as.integer)
spin_bowlers_filtered[7:13] <- sapply(spin_bowlers_filtered[7:13], as.integer)
# (iii) reclass date column to date format and re-sort
pace_bowlers_filtered$match_date <- as.Date(pace_bowlers_filtered$match_date, "%d %b %Y")
pace_bowlers_filtered <- arrange(pace_bowlers_filtered, match_date, match_inns, bowler_name)
pace_bowlers_filtered <- pace_bowlers_filtered %>%
  mutate(match_year = year(match_date),
         .after = bowler_type)
spin_bowlers_filtered$match_date <- as.Date(spin_bowlers_filtered$match_date, "%d %b %Y")
spin_bowlers_filtered <- arrange(spin_bowlers_filtered, match_date, match_inns, bowler_name)
spin_bowlers_filtered <- spin_bowlers_filtered %>%
  mutate(match_year = year(match_date),
         .after = bowler_type)
# (iv) add unique match_ID
pace_bowlers_filtered <- pace_bowlers_filtered %>%
  mutate(match_ID = str_c(venue, match_date),
         .after = venue)
pace_bowlers_filtered$match_ID <- gsub("-","", pace_bowlers_filtered$match_ID)
pace_bowlers_filtered$match_ID <- gsub(" ","", pace_bowlers_filtered$match_ID)
pace_bowlers_filtered$match_ID <- gsub("'","", pace_bowlers_filtered$match_ID)
pace_bowlers_filtered$match_ID <- gsub(",","", pace_bowlers_filtered$match_ID)
pace_bowlers_filtered$match_ID <- gsub("[(]","", pace_bowlers_filtered$match_ID)
pace_bowlers_filtered$match_ID <- gsub("[)]","", pace_bowlers_filtered$match_ID)
spin_bowlers_filtered <- spin_bowlers_filtered %>%
  mutate(match_ID = str_c(venue, match_date),
         .after = venue)
spin_bowlers_filtered$match_ID <- gsub("-","", spin_bowlers_filtered$match_ID)
spin_bowlers_filtered$match_ID <- gsub(" ","", spin_bowlers_filtered$match_ID)
spin_bowlers_filtered$match_ID <- gsub("'","", spin_bowlers_filtered$match_ID)
spin_bowlers_filtered$match_ID <- gsub(",","", spin_bowlers_filtered$match_ID)
spin_bowlers_filtered$match_ID <- gsub("[(]","", spin_bowlers_filtered$match_ID)
spin_bowlers_filtered$match_ID <- gsub("[)]","", spin_bowlers_filtered$match_ID)

# create final dataframes for analysis
pace_bowlers_final <- pace_bowlers_filtered %>%
  mutate(balls_bowled = ((balls_per_over * completed_overs) + completed_balls),
         .after = match_inns,
         balls_per_over = NULL,
         completed_overs = NULL,
         completed_balls = NULL,
         maidens_bowled = NULL)
spin_bowlers_final <- spin_bowlers_filtered %>%
  mutate(balls_bowled = ((balls_per_over * completed_overs) + completed_balls),
         .after = match_inns,
         balls_per_over = NULL,
         completed_overs = NULL,
         completed_balls = NULL,
         maidens_bowled = NULL)

### ANALYSE

## PLOT 1: PACE VS. SPIN SINCE 1946

# summarise by match
pace_by_match <- pace_bowlers_final %>%
  group_by(match_ID, match_year, match_date) %>%
  summarise(balls_bowled = sum(balls_bowled),
            wickets_taken = sum(wickets_taken),
            runs_conceded = sum(runs_conceded),
            runs_per_wkt = runs_conceded/wickets_taken,
            balls_per_wkt = balls_bowled/wickets_taken,
            runs_per_ball = runs_conceded/balls_bowled)
spin_by_match <- spin_bowlers_final %>%
  group_by(match_ID, match_year, match_date) %>%
  summarise(balls_bowled = sum(balls_bowled),
            wickets_taken = sum(wickets_taken),
            runs_conceded = sum(runs_conceded),
            runs_per_wkt = runs_conceded/wickets_taken,
            balls_per_wkt = balls_bowled/wickets_taken,
            runs_per_ball = runs_conceded/balls_bowled)

# join spin and pace records
pace_for_join <- pace_by_match %>%
  rename(balls_bowled_pace = balls_bowled,
         wickets_taken_pace = wickets_taken,
         runs_conceded_pace = runs_conceded) %>%
  mutate(runs_per_wkt = NULL,
         balls_per_wkt = NULL,
         runs_per_ball = NULL)
spin_for_join <- spin_by_match %>%
  rename(balls_bowled_spin = balls_bowled,
         wickets_taken_spin = wickets_taken,
         runs_conceded_spin = runs_conceded) %>%
  mutate(runs_per_wkt = NULL,
         balls_per_wkt = NULL,
         runs_per_ball = NULL)
all_by_match <- full_join(pace_for_join, spin_for_join, by = "match_ID")
all_by_match <- all_by_match %>%
  rename(match_year = match_year.x,
         match_date = match_date.x) %>%
  replace_na(list(balls_bowled_spin = 0, wickets_taken_spin = 0, runs_conceded_spin = 0)) %>%
  mutate(match_year.y = NULL,
         match_date.y = NULL,
         balls_bowled_total = balls_bowled_pace + balls_bowled_spin,
         wickets_taken_total = wickets_taken_pace + wickets_taken_spin,
         runs_conceded_total = runs_conceded_pace + runs_conceded_spin)

# calculate rolling averages
roll_no <- 100
roll_ave_final <- arrange(all_by_match, match_date, match_ID)
roll_ave_final$bb_pace_roll <- roll_sum(roll_ave_final$balls_bowled_pace, n = roll_no, align = "right", fill = NA)
roll_ave_final$wt_pace_roll <- roll_sum(roll_ave_final$wickets_taken_pace, n = roll_no, align = "right", fill = NA)
roll_ave_final$rc_pace_roll <- roll_sum(roll_ave_final$runs_conceded_pace, n = roll_no, align = "right", fill = NA)
roll_ave_final$bb_spin_roll <- roll_sum(roll_ave_final$balls_bowled_spin, n = roll_no, align = "right", fill = NA)
roll_ave_final$wt_spin_roll <- roll_sum(roll_ave_final$wickets_taken_spin, n = roll_no, align = "right", fill = NA)
roll_ave_final$rc_spin_roll <- roll_sum(roll_ave_final$runs_conceded_spin, n = roll_no, align = "right", fill = NA)
roll_ave_final <- roll_ave_final %>%
  mutate(roll_runs_per_wkt_pace = rc_pace_roll / wt_pace_roll,
         roll_runs_per_wkt_spin = rc_spin_roll / wt_spin_roll)

## PLOT 2: PACE BY TEAM SINCE 1946

# summarise by match and by team (pace only)
pace_by_team_match <- pace_bowlers_final %>%
  group_by(bowler_team, match_ID, match_year, match_date) %>%
  summarise(balls_bowled = sum(balls_bowled),
            wickets_taken = sum(wickets_taken),
            runs_conceded = sum(runs_conceded),
            runs_per_wkt = runs_conceded/wickets_taken,
            balls_per_wkt = balls_bowled/wickets_taken,
            runs_per_ball = runs_conceded/balls_bowled)
pace_by_team_match <- arrange(pace_by_team_match, match_date, match_ID, bowler_team)

#split by team
aus_pace_by_match <- pace_by_team_match %>%
  filter(bowler_team == "Australia")
eng_pace_by_match <- pace_by_team_match %>%
  filter(bowler_team == "England")
ind_pace_by_match <- pace_by_team_match %>%
  filter(bowler_team == "India")
rsa_pace_by_match <- pace_by_team_match %>%
  filter(bowler_team == "South Africa")

# calculate rolling averages for (i) Australia...
roll_no_by_team <- 30
roll_ave_by_match_aus_pace <- arrange(aus_pace_by_match, match_date, match_ID)
roll_ave_by_match_aus_pace$bb_roll <- roll_sum(roll_ave_by_match_aus_pace$balls_bowled, n = roll_no_by_team, align = "right", fill = NA)
roll_ave_by_match_aus_pace$wt_roll <- roll_sum(roll_ave_by_match_aus_pace$wickets_taken, n = roll_no_by_team, align = "right", fill = NA)
roll_ave_by_match_aus_pace$rc_roll <- roll_sum(roll_ave_by_match_aus_pace$runs_conceded, n = roll_no_by_team, align = "right", fill = NA)
roll_ave_by_match_aus_pace <- roll_ave_by_match_aus_pace %>%
  mutate(roll_runs_per_wkt = rc_roll / wt_roll,
         roll_balls_per_wkt = bb_roll / wt_roll)
# ...(ii) England...
roll_ave_by_match_eng_pace <- arrange(eng_pace_by_match, match_date, match_ID)
roll_ave_by_match_eng_pace$bb_roll <- roll_sum(roll_ave_by_match_eng_pace$balls_bowled, n = roll_no_by_team, align = "right", fill = NA)
roll_ave_by_match_eng_pace$wt_roll <- roll_sum(roll_ave_by_match_eng_pace$wickets_taken, n = roll_no_by_team, align = "right", fill = NA)
roll_ave_by_match_eng_pace$rc_roll <- roll_sum(roll_ave_by_match_eng_pace$runs_conceded, n = roll_no_by_team, align = "right", fill = NA)
roll_ave_by_match_eng_pace <- roll_ave_by_match_eng_pace %>%
  mutate(roll_runs_per_wkt = rc_roll / wt_roll,
         roll_balls_per_wkt = bb_roll / wt_roll)
# ...(iii) India...
roll_ave_by_match_ind_pace <- arrange(ind_pace_by_match, match_date, match_ID)
roll_ave_by_match_ind_pace$bb_roll <- roll_sum(roll_ave_by_match_ind_pace$balls_bowled, n = roll_no_by_team, align = "right", fill = NA)
roll_ave_by_match_ind_pace$wt_roll <- roll_sum(roll_ave_by_match_ind_pace$wickets_taken, n = roll_no_by_team, align = "right", fill = NA)
roll_ave_by_match_ind_pace$rc_roll <- roll_sum(roll_ave_by_match_ind_pace$runs_conceded, n = roll_no_by_team, align = "right", fill = NA)
roll_ave_by_match_ind_pace <- roll_ave_by_match_ind_pace %>%
  mutate(roll_runs_per_wkt = rc_roll / wt_roll,
         roll_balls_per_wkt = bb_roll / wt_roll)
# ...(iv) South Africa (first spell - prior to ICC ban)...
roll_ave_by_match_rsa_pace_1 <- arrange(rsa_pace_by_match, match_date, match_ID)
roll_ave_by_match_rsa_pace_1 <- roll_ave_by_match_rsa_pace_1 %>%
  filter(match_date <= as.Date("1970-03-05"))
roll_ave_by_match_rsa_pace_1$bb_roll <- roll_sum(roll_ave_by_match_rsa_pace_1$balls_bowled, n = roll_no_by_team, align = "right", fill = NA)
roll_ave_by_match_rsa_pace_1$wt_roll <- roll_sum(roll_ave_by_match_rsa_pace_1$wickets_taken, n = roll_no_by_team, align = "right", fill = NA)
roll_ave_by_match_rsa_pace_1$rc_roll <- roll_sum(roll_ave_by_match_rsa_pace_1$runs_conceded, n = roll_no_by_team, align = "right", fill = NA)
roll_ave_by_match_rsa_pace_1 <- roll_ave_by_match_rsa_pace_1 %>%
  mutate(roll_runs_per_wkt = rc_roll / wt_roll,
         roll_balls_per_wkt = bb_roll / wt_roll)
# and (v) South Africa (second spell - after ICC ban)
roll_ave_by_match_rsa_pace_2 <- arrange(rsa_pace_by_match, match_date, match_ID)
roll_ave_by_match_rsa_pace_2 <- roll_ave_by_match_rsa_pace_2 %>%
  filter(match_date > as.Date("1970-03-05"))
roll_ave_by_match_rsa_pace_2$bb_roll <- roll_sum(roll_ave_by_match_rsa_pace_2$balls_bowled, n = roll_no_by_team, align = "right", fill = NA)
roll_ave_by_match_rsa_pace_2$wt_roll <- roll_sum(roll_ave_by_match_rsa_pace_2$wickets_taken, n = roll_no_by_team, align = "right", fill = NA)
roll_ave_by_match_rsa_pace_2$rc_roll <- roll_sum(roll_ave_by_match_rsa_pace_2$runs_conceded, n = roll_no_by_team, align = "right", fill = NA)
roll_ave_by_match_rsa_pace_2 <- roll_ave_by_match_rsa_pace_2 %>%
  mutate(roll_runs_per_wkt = rc_roll / wt_roll,
         roll_balls_per_wkt = bb_roll / wt_roll)

# simplify and combine for small multiples
multiples_data_aus <- roll_ave_by_match_aus_pace %>%
  select(bowler_team, match_ID, match_year, match_date, roll_runs_per_wkt)
multiples_data_eng <- roll_ave_by_match_eng_pace %>%
  select(bowler_team, match_ID, match_year, match_date, roll_runs_per_wkt)
multiples_data_ind <- roll_ave_by_match_ind_pace %>%
  select(bowler_team, match_ID, match_year, match_date, roll_runs_per_wkt)
multiples_data_rsa_1 <- roll_ave_by_match_rsa_pace_1 %>%
  select(bowler_team, match_ID, match_year, match_date, roll_runs_per_wkt)
multiples_data_rsa_2 <- roll_ave_by_match_rsa_pace_2 %>%
  select(bowler_team, match_ID, match_year, match_date, roll_runs_per_wkt)
multiples_data <- rbind(multiples_data_aus,
                        multiples_data_eng,
                        multiples_data_ind,
                        multiples_data_rsa_1,
                        multiples_data_rsa_2)
multiples_data <- arrange(multiples_data, match_date, match_ID)

### VISUALISE

## PLOT 1: PACE VS. SPIN SINCE 1946

# create rough plot...
rough_plot_1 <- ggplot(roll_ave_final, aes(x = as.Date(match_date))) +
  geom_path(aes(y = roll_runs_per_wkt_spin),
            color = ptb_light_pink,
            size = 1) +
  geom_path(aes(y = roll_runs_per_wkt_pace),
            color = ptb_green,
            size = 1) +
  scale_x_date(name = NULL,
               date_labels = "%Y",
               date_breaks = "10 years",
               limits = c(as.Date("1950-01-01"), as.Date("2022-12-31")),
               expand = c(0,0)) +
  scale_y_continuous(name = NULL,
                     limits = c(20, 50),
                     expand = c(0,0),
                     breaks = seq(20, 50, 5)) +
# ...including rotated and moved y-axis label
  annotate(geom = "text",
           x = as.Date("1951-01-01"),
           y = 49.5,
           label = "Runs per\nwicket",
           family = ptb_font,
           colour = ptb_dark_grey,
           fontface = "bold",
           hjust = 0,
           vjust = 1)

# add custom theme to plot
themed_plot_1 <- rough_plot_1 + theme_ptb()

# add labelling to plot
final_plot_1 <- themed_plot_1 + labs(title = "Facing <span style='color:#0EA47A;'>pace</span> is harder than ever",
                                    subtitle = "Runs scored per wicket* against <b><span style='color:#0EA47A;'>pace</span></b> and <b><span style='color:#EA9AB2;'>spin</span></b> bowling<br>in men's tests since 1946",
                                    caption = "<i><span style='color:#444F5A;'>*100-game rolling average</i></span><br><br>Data: ESPNCricinfo | Chart: Plot the Ball")

## PLOT 2: PACE BY TEAM SINCE 1946

# create custom palette by team
team_palette <- c("#FED766", "#E07A5F", "#5B85AA", "#293F14")

# create dataframe with custom annotation for each facet
annotation_by_facet <- data.frame(
  label = c("", "", "", "BANNED\nBY ICC"),
  bowler_team   = c("Australia", "England", "India", "South Africa"))

# create rough plot
rough_plot_2 <- ggplot(multiples_data, aes(x = as.Date(match_date), y = roll_runs_per_wkt, color = bowler_team)) +
  geom_path(size = 1) +
  scale_x_date(name = NULL,
               date_labels = "%Y",
               date_breaks = "20 years",
               limits = c(as.Date("1950-01-01"), as.Date("2022-12-31")),
               expand = c(0,0)) +
  scale_y_continuous(name = NULL,
                     limits = c(20, 50),
                     expand = c(0,0),
                     breaks = seq(20, 50, 5)) +
  theme(legend.position = "none") +
  scale_colour_manual(values = team_palette, aesthetics = c("colour", "fill")) +
  facet_rep_wrap(~bowler_team, nrow = 2, repeat.tick.labels =  TRUE) +
  theme(strip.text = element_blank(),
        strip.background = element_blank()) +
  geom_text(data = annotation_by_facet,
            mapping = aes(label = label, x = as.Date("1983-06-01"), y = 27, family = ptb_font, fontface = "bold.italic", alpha = 0.7))

# add custom theme to plot
themed_plot_2 <- rough_plot_2 + theme_ptb()

# add labelling to plot
final_plot_2 <- themed_plot_2 + labs(title = "<span style='color:#5B85AA;'>India</span>'s seamers are better than ever",
                                        subtitle = "Runs scored per wicket* against pace in men's tests since<br>1946 vs. <b><span style='color:#FED766;'>Australia</span></b>, <b><span style='color:#E07A5F;'>England</span></b>, <b><span style='color:#5B85AA;'>India</span></b> and <b><span style='color:#293F14;'>South Africa</span></b>",
                                        caption = "<i><span style='color:#444F5A;'>*30-game rolling average</i></span><br><br>Data: ESPNCricinfo | Chart: Plot the Ball")

#### NOTES
#### code for custom theme elements not included above
