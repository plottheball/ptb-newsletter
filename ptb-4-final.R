# load packages...
library(tidyverse)
# ...for scraping,...
library(rvest)
library(httr)
# ...for analysis...
library(janitor)
# ...and for visualisation
library(ggtext)
library(scales)

### OBTAIN, CLEAN & ANALYSE DATA

## SCRAPE & TIDY

# specify range of URLs for scraping...
base_url <- "https://www.basketball-reference.com/wnba/years/"
end_url <- "_advanced.html"
years <- seq(1997, 2021, by = 1)
# ...and xpath of table to be extracted
table_xpath <- "/html/body/div[3]/div[5]/div[3]/div[2]/table"

# set up blank dataframe for scraped data
data_final <- tibble()

# set up for loop to obtain player data from Basketball Reference
for (i in years) {
  print(paste0("Getting data for season: ", i))
  
  # grab on-court data from key table...
  url <- str_c(base_url, i, end_url)
  page_html <- read_html(url)
  page_tables <- page_html %>%
    html_nodes("table") %>%
    html_table()
  page_table <- page_tables %>%
    pluck(1) %>%
    clean_names() %>%
    distinct()
  page_table <- page_table[!grepl("Team", page_table$team),]
  page_table <- page_table %>%
    select(-c(g_2, mp_2, x))
  page_table$player <- str_remove_all(page_table$player, "\\*")
  #...and the URL for each player page (to obtain college info)
  player_urls <- page_html %>%
    html_nodes("table") %>%
    html_nodes("th") %>% 
    html_nodes("a")
  player_urls_tidy <- player_urls %>%
    as.character() %>%
    as_tibble() %>%
    rename(player_url = value)
  player_urls_tidy <- player_urls_tidy %>%
    separate(col = player_url,
             into = c("player_url", "name_check"),
             sep = ">")
  
  # clean the URLs (and name column - to cross-reference to main dataset) into a usable format
  player_urls_tidy$player_url <- str_remove_all(string = player_urls_tidy$player_url, pattern = "<a href=")
  player_urls_tidy$name_check <- str_remove_all(string = player_urls_tidy$name_check, pattern = "</a")
  
  # combine the main dataset with the URLs, add year of season and tidy
  page_table_final <- left_join(x = page_table, y = player_urls_tidy, by = c("player" = "name_check"))
  page_table_final <- page_table_final[!grepl("TOT", page_table_final$team),]
  page_table_final$season <- as.character(i)
  page_table_final[4:23] <- sapply(page_table_final[4:23], as.numeric)
  page_table_final <- page_table_final %>%
    distinct()
  
  data_final <- bind_rows(data_final, page_table_final)
}

# convert each URL into usable web address
data_final$player_url <- gsub('"', '', data_final$player_url)
data_final <- data_final %>%
  relocate(c("player_url", "season"), .after = player) %>%
  mutate(complete_url = str_c("https://www.basketball-reference.com", player_url),
         .after = player_url)

# generate list of unique player URLs to scrape college info
player_list <- data_final %>%
  select(player, complete_url) %>%
  distinct()
college_urls <- player_list$complete_url

# set up blank dataframe for scraped data
college_tibble <- tibble()

# set up for loop to obtain college data
for (i in college_urls) {
  print(paste0("Getting data for link: ", i))
  status_check <- httr::GET(i) %>%
    status_code()
  if (status_check == 404) {
    next
  }

  # grab text from key table, convert to usable string and add to dataframe
  college_html <- read_html(i)
  p_elements <- college_html %>%
    html_nodes("p") %>%
    html_text() %>%
    {if(length(.) == 0) NA else .}
  college <- toString(p_elements)
  df <- tibble_row(i, college)
  college_tibble <- bind_rows(college_tibble, df)
  
}

# extract college info from other scraped text in string
college_tibble_clean <- college_tibble %>%
  separate(col = college, into = c("discard", "keep"), sep = "College: ")
college_tibble_clean <- college_tibble_clean %>% 
  select(-discard)
college_tibble_final <- college_tibble_clean %>%
  separate(col = keep, into = c("college", "discard"), sep = ",", extra = "drop")
college_tibble_final <- college_tibble_final %>%
  select(-discard)

# combine main dataset with the college info to create final dataset
data_final_w_col <- left_join(x = data_final, y = college_tibble_final, by = c("complete_url" = "i"))
data_final_w_col <- data_final_w_col %>%
  relocate(college, .after = player)

## ANALYSE

# calculate total minutes played by attendees of each college and by season
data_by_college <- data_final_w_col %>%
  group_by(college, season) %>%
  summarise(total_mins = sum(mp))

# calculate total minutes played by attendees of each college for all seasons
by_college_all_seasons <- data_final_w_col %>%
  group_by(college) %>%
  summarise(total_mins = sum(mp))

# split minutes played data between minutes played by UConn attendees and all other minutes 
data_by_college$category <- if_else(data_by_college$college == "UConn", "UConn", "Other")
data_by_college$category <- replace_na(data_by_college$category, "Other")

# summarise split of minutes between UConn and other colleges in each season
uconn_season_summary <- data_by_college %>%
  group_by(season, category) %>%
  summarise(mins = sum(total_mins))
season_mins_share <- uconn_season_summary %>% 
  pivot_wider(names_from = category, values_from = mins)
season_mins_share <- season_mins_share %>%
  mutate(total = Other + UConn,
         uconn_share = UConn / total,
         other_share = Other / total)

# finalise simple dataset for plot 1
by_season_final <- season_mins_share %>%
  select(season, uconn_share)
by_season_final$season <- as.integer(by_season_final$season)

# finalise simple dataset for plot 2,...
by_college_final <- by_college_all_seasons %>%
  filter(!is.na(college)) %>%
  arrange(desc(total_mins)) %>%
  head(n = 10L)
# ...including shortening names and...
by_college_final$college <- str_replace(by_college_final$college, "Rutgers University", "Rutgers")
# ...placing data in descending order
by_college_final <- by_college_final %>%
  mutate(college = fct_reorder(college, total_mins))


### VISUALISE

# set up custom colours
uconn_navy <- "#000E2F"
uconn_red <- "#E4002B"
contrast_blue <- "#4D6399"
contrast_grey <- "#D3D8DB"

## PLOT 1:BAR CHART

# create custom bar chart
rough_plot <- ggplot(data = by_college_final, aes(x = college, y = total_mins, fill = custom)) +
  geom_bar(stat = "identity",
           fill = if_else(by_college_final$college=="UConn", contrast_blue, contrast_grey),
           width = 0.7) +
  scale_y_continuous(name = NULL,
                     limits = c(0,220000),
                     expand = c(0,0),
                     breaks = seq(0, 220000, 100000),
                     labels = comma) +
  coord_flip()
rough_plot

# add custom theme to plot
themed_plot <- rough_plot + theme_ptb()
themed_plot

# add labelling to plot
final_plot <- themed_plot +
  labs(title = "<b style='color:#4D6399;'>UConn</b> is a major WNBA talent source",
       subtitle = "Total WNBA minutes played by attendees of each college",
       caption = "<i>Top 10 colleges shown; data for all seasons between 1997 and 2021</i><br><br>Data: Basketball Reference | Chart: Plot the Ball")
final_plot

## PLOT 2: AREA CHART

# create custom area chart
rough_plot_2 <- ggplot(data = by_season_final, aes(x = season, y = uconn_share)) +
  geom_path(colour = uconn_navy,
            size = 1) +
  geom_area(fill = contrast_blue,
            alpha = 0.5) +
  scale_x_continuous(name = NULL,
                     limits = c(1997, 2021),
                     expand = c(0,0)) +
  scale_y_continuous(name = NULL,
                     limits = c(0, 0.45),
                     expand = c(0, 0),
                     breaks = seq(0, 0.45, 0.1),
                     labels = label_percent(accuracy = 1L))
rough_plot_2

# add custom theme to plot
themed_plot_2 <- rough_plot_2 + theme_ptb()
themed_plot_2

# add labelling to plot
final_plot_2 <- themed_plot_2 +
  labs(title = "<b style='color:#4D6399;'>UConn</b>'s influence peaked in 2018",
       subtitle = "Share of total WNBA minutes played by <b style='color:#4D6399;'>UConn</b> attendees<br> in each season between 1997 and 2021",
       caption = "Data: Basketball Reference | Chart: Plot the Ball")
final_plot_2

#### NOTES
#### code for custom theme elements not included above
