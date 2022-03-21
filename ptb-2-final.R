# load packages...
library(tidyverse)
# ...for obtaining data,...
library(worldfootballR)
# ...for analysis...
library(janitor)
library(lubridate)
# ...and for visualisation
library(ggalt)
library(ggtext)
library(lemon)
library(forcats)
library(scales)

### OBTAIN, CLEAN & ANALYSE DATA

## PLOT 1: EREDIVISIE AVERAGE AGE (2000-PRESENT)

# grab Eredivisie playing time data from FBref (using 'worldfootballR' package)
eredivisie_pt_summary <- get_season_team_stats(country = "NED", gender = "M", season_end_year = seq(from = 2001, to = 2022, by = 1), tier = "1st", stat_type = "playing_time")
eredivisie_tidy <- select(eredivisie_pt_summary, -c(seq(9,24, 1)))

# filter to Ajax only...
dumbbell_data_ajax <- eredivisie_tidy %>%
  filter(Squad == "Ajax") %>%
# ...and adjust Eredivisie current season age data (based on current date) to align with prior year data (calculated as at 1 August)
  mutate(Age_Adj = ifelse(Season_End_Year == 2022, (as_date("2021-08-01") - today(tzone = "UTC"))/as.numeric((as_date("2022-08-01") - as_date("2021-08-01"))), 0),
         Age_Final_Ajax = round(Age + Age_Adj, digits = 1),
         Team_or_Opponent = NULL,
         Num_Players = NULL)

# repeat for Ajax's opponents
dumbbell_data_opp <- eredivisie_tidy %>%
  filter(Squad == "vs Ajax") %>%
  mutate(Age_Adj = ifelse(Season_End_Year == 2022, (as_date("2021-08-01") - today(tzone = "UTC"))/as.numeric((as_date("2022-08-01") - as_date("2021-08-01"))), 0),
         Age_Final_Opp = round(Age + Age_Adj, digits = 1),
         Team_or_Opponent = NULL,
         Num_Players = NULL)

# combine into single dataframe
dumbbell_data <- left_join(x = dumbbell_data_ajax, y = dumbbell_data_opp, by = "Season_End_Year", suffix = c("_Ajax", "_Opp"), keep = FALSE)
dumbbell_data_final <- dumbbell_data %>%
  mutate(Season = paste0(Season_End_Year -1, "-", str_sub(as.character(Season_End_Year), -2)),
         Age_Final_Ajax = Age_Final_Ajax,
         Age_Final_Opp = Age_Final_Opp,
         .keep = "none")
dumbbell_data_final <- dumbbell_data_final %>%
  arrange(Season)

## PLOT 2: PRIOR EXPERIENCE OF CL SQUADS ('18-19 VS. '21-22)

# grab url for each relevant CL season from fbref.com
cl_22_url <- "https://fbref.com/en/squads/19c3f8c4/2021-2022/s11323/Ajax-Stats-Champions-League"
cl_19_url <- "https://fbref.com/en/squads/19c3f8c4/2018-2019/s2102/Ajax-Stats-Champions-League"

# grab CL playing time data
cl_22_pt <- fb_team_player_stats(cl_22_url, "playing_time")
cl_19_pt <- fb_team_player_stats(cl_19_url, "playing_time")

# filter CL playing time data to players with at least a quarter of current-season CL minutes played
cl_22_max_mins <- cl_22_pt[1,9] / (cl_22_pt[1,11]/100)
cl_22_pt <- cl_22_pt %>%
  filter(Min_Playing_Time >= (cl_22_max_mins/4))
cl_19_max_mins <- cl_19_pt[1,9] / (cl_19_pt[1,11]/100)
cl_19_pt <- cl_19_pt %>%
  filter(Min_Playing_Time >= (cl_19_max_mins/4))

# grab URLs for pages of those players,...
squad_cl_19_urls <- cl_19_pt$PlayerURL
squad_cl_22_urls <- cl_22_pt$PlayerURL
# ...obtain their playing time history,...
squad_cl_19_history <- fb_player_season_stats(squad_cl_19_urls, "playing_time")
squad_cl_22_history <- fb_player_season_stats(squad_cl_22_urls, "playing_time")
# ...filter for top-division league seasons prior to the CL season being analysed...
squad_cl_19_filtered <- squad_cl_19_history %>%
  filter(Season != "2021-2022" & Season != "2020-2021" & Season != "2019-2020" & Season != "2018-2019") %>%
  filter(Country != "") %>%
  filter(str_detect(Comp, "1.")) %>%
  filter(MP_Time != 0)
squad_cl_22_filtered <- squad_cl_22_history %>%
  filter(Season != "2021-2022") %>%
  filter(Country != "") %>%
  filter(str_detect(Comp, "1.")) %>%
  filter(MP_Time != 0)
# ...and remove columns irrelevant to analysis
squad_cl_19_filtered <- squad_cl_19_filtered %>%
  select(-c(seq(10, 29, 1)))
squad_cl_22_filtered <- squad_cl_22_filtered %>%
  select(-c(seq(10, 29, 1)))

# create a list of countries that players have appeared in...
countries_19 <- unique(squad_cl_19_filtered$Country)
countries_22 <- unique(squad_cl_22_filtered$Country)
countries <- unique(c(countries_19, countries_22))
# ...and categorise them (based on 'Big 5' status or otherwise)
countries_tiers <- tibble(countries = countries,
                          category = c("Home", "Big Five", "Other", "Other", "Other", "Other", "Big Five", "Other"))

# add country info to CL datasets
squad_cl_19_filtered <- left_join(x = squad_cl_19_filtered, y = countries_tiers, by = c("Country" = "countries"))
squad_cl_22_filtered <- left_join(x = squad_cl_22_filtered, y = countries_tiers, by = c("Country" = "countries"))

# create summaries of players' playing time in each category of league...
squad_cl_19_by_player <- squad_cl_19_filtered %>%
  group_by(player_name, category) %>%
  summarise(mins = sum(Min_Time))
squad_cl_22_by_player <- squad_cl_22_filtered %>%
  group_by(player_name, category) %>%
  summarise(mins = sum(Min_Time))
# ...and specifically at Ajax
squad_cl_19_by_player_ajax <- squad_cl_19_filtered %>%
  group_by(player_name, Squad) %>%
  summarise(mins = sum(Min_Time)) %>%
  filter(Squad == "Ajax") %>%
  mutate(ajax = mins,
         .keep = "none")
squad_cl_22_by_player_ajax <- squad_cl_22_filtered %>%
  group_by(player_name, Squad) %>%
  summarise(mins = sum(Min_Time)) %>%
  filter(Squad == "Ajax") %>%
  mutate(ajax = mins,
         .keep = "none")

# reshape 2018-19 dataframe to provide necessary columns for 'Ajax vs. Big 5 vs. other' analysis...
squad_cl_19_by_player_reshaped <- squad_cl_19_by_player %>%
  pivot_wider(names_from = category, values_from = mins) %>%
  clean_names()
squad_cl_19_by_player_reshaped <- left_join(x = squad_cl_19_by_player_reshaped, y = squad_cl_19_by_player_ajax, by = c("player_name" = "player_name"))
squad_cl_19_by_player_reshaped <- replace_na(data = squad_cl_19_by_player_reshaped, replace = list(home = 0, big_five = 0, other = 0, ajax = 0))
squad_cl_19_by_player_reshaped <- squad_cl_19_by_player_reshaped %>%
  mutate(other_mins = (home + other - ajax),
         home = NULL,
         other = NULL)
squad_cl_19_by_player_reshaped <- squad_cl_19_by_player_reshaped %>%
  pivot_longer(!player_name, names_to = "grouping", values_to = "minutes") %>%
  mutate(season = "2018-19 squad")
# ...and do same for 2021-22 dataframe
squad_cl_22_by_player_reshaped <- squad_cl_22_by_player %>%
  pivot_wider(names_from = category, values_from = mins) %>%
  clean_names()
squad_cl_22_by_player_reshaped <- left_join(x = squad_cl_22_by_player_reshaped, y = squad_cl_22_by_player_ajax, by = c("player_name" = "player_name"))
squad_cl_22_by_player_reshaped <- replace_na(data = squad_cl_22_by_player_reshaped, replace = list(home = 0, big_five = 0, other = 0, ajax = 0))
squad_cl_22_by_player_reshaped <- squad_cl_22_by_player_reshaped %>%
  mutate(other_mins = (home + other - ajax),
         home = NULL,
         other = NULL)
squad_cl_22_by_player_reshaped <- squad_cl_22_by_player_reshaped %>%
  pivot_longer(!player_name, names_to = "grouping", values_to = "minutes") %>%
  mutate(season = "2021-22 squad")

# combine two seasons of data for visualisation
player_mins_data_final <- rbind(squad_cl_19_by_player_reshaped, squad_cl_22_by_player_reshaped)

### VISUALISE

## PLOT 1: EREDIVISIE AVERAGE AGE (2000-PRESENT)

# create custom palette
ajax_palette <- c("#D2122E", "#E6DFDF", "#4C4D54")

# create rough plot
rough_dumbbell <- ggplot(dumbbell_data_final, aes(x = Age_Final_Ajax, xend = Age_Final_Opp, y = Season)) + 
  geom_dumbbell(colour = ajax_palette[2],
                size = 3,
                colour_x = ajax_palette[1],
                colour_xend = ajax_palette[3]) +
  scale_x_continuous(name = NULL,
                     limits = c(22, 28),
                     breaks = seq(22, 28, 2)) +
  scale_y_discrete(limits = factor(dumbbell_data_final$Season),
                   labels = c("","'01-02","","","","","'06-07","","","", "","'11-12","","","","","'16-17","","","","", "'21-22")) +
  coord_flip()
rough_dumbbell

# add custom theme to plot
themed_dumbbell <- rough_dumbbell +
  theme_ptb() +
  theme(panel.grid.major.x = element_blank())
themed_dumbbell

# add labelling to plot
final_dumbbell <- themed_dumbbell +
  labs(title = "<b style='color:#D2122E;'>Ajax</b>'s 2021-22 squad is a clear outlier",
       subtitle = "Average age* of <b style='color:#D2122E;'>Ajax</b> and <b style='color:#4C4D54;'>their domestic opponents</b> in <br>each Eredivisie season since 2000-01",
       caption = "<i><span style='color:#444F5A;'>*at 1 August in year of season start, weighted by minutes played</i></span><br><br>Data: FBref | Chart: Plot the Ball")
final_dumbbell

## PLOT 2: PRIOR EXPERIENCE OF CL SQUADS ('18-19 VS. '21-22)

# create custom palette...
ajax_palette_2 <- c("#733F47", "#D17381")
# ...and custom labels for each facet
facet_labels <- c(ajax = "CAREER LEAGUE MINUTES FOR AJAX", big_five = "CAREER LEAGUE MINUTES FOR CLUBS IN 'BIG 5' LEAGUES", other_mins = "CAREER LEAGUE MINUTES FOR OTHER CLUBS")

# create rough plot
rough_bar <- ggplot(player_mins_data_final, aes(x = season, y = minutes, fill = season)) + 
  geom_bar(position = "stack", stat = "identity", width = 0.5, colour = "#FFFFFF") +
  facet_rep_wrap(~grouping, nrow = 3, labeller = labeller(grouping = facet_labels), repeat.tick.labels = TRUE) +
  scale_colour_manual(values = ajax_palette_2, aesthetics = "fill") +
  scale_y_continuous(name = NULL,
                     limits = c(0, 90000),
                     expand = c(0,0),
                     breaks = seq(0, 90000, 25000),
                     labels = comma) +
  coord_flip()
rough_bar

# add custom theme to plot
themed_bar <- rough_bar + 
  theme_ptb() +
  theme(legend.position = "none",
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank())
themed_bar

# add labelling to plot
final_bar <- themed_bar + 
  labs(title = "This team has more 'Big 5' experience",
       subtitle = "Cumulative previous experience of members of Ajax's<br><b style='color:#D17381;'>2021-22</b> and <b style='color:#733F47;'>2018-19</b> Champions League squads*",
       caption = "<i><span style='color:#444F5A;'>*Players who played at least 25% of available CL minutes only</i></span><br><br>Data: FBref | Chart: Plot the Ball")
final_bar

#### NOTES
#### code for custom theme elements not included above
