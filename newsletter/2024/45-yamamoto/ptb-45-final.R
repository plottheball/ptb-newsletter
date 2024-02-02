library(tidyverse)
library(readxl)
library(janitor)
library(gt)
library(gtExtras)
library(ggbeeswarm)
library(ggtext)
library(scales)

### NPB data downloaded from: https://www.baseball-reference.com/register/
### MLB data downloaded from: https://www.baseball-reference.com/leagues/

years <- seq(2019, 2023, 1)

all_data <- tibble()

for (i in years) {
  
  pacific_data <- read.csv(str_c("jppl_", i, ".csv")) %>% 
    clean_names()
  
  pacific_data$year <- i
  pacific_data$league <- "Pacific"
  
  all_data <- bind_rows(all_data, pacific_data)
  
  central_data <- read.csv(str_c("jpcl_", i, ".csv")) %>% 
    clean_names()
  
  central_data$year <- i
  central_data$league <- "Central"
  
  all_data <- bind_rows(all_data, central_data)
  
}

data_clean <- all_data %>%
  select(1, 5, 8, 28, 29)

data_summary <- data_clean %>%
  group_by(league, year) %>%
  summarise(no_games = sum(g),
            total_runs = sum(r),
            runs_per_game = total_runs / no_games) %>%
  arrange(year)

npb_summary <- data_clean %>%
  group_by(year) %>%
  summarise(no_games = sum(g),
            total_runs = sum(r),
            runs_per_game = total_runs / no_games) %>%
  arrange(year)

al_data <- read_csv("al_summary.csv") %>%
  clean_names() %>%
  filter(year > 2018) %>%
  select(1, 5) %>%
  mutate(league = "American")

nl_data <- read_csv("nl_summary.csv") %>%
  clean_names() %>%
  filter(year > 2018) %>%
  select(1, 5) %>%
  mutate(league = "National")

mlb_summary <- left_join(x = al_data, y = nl_data, by = ("year" = "year")) %>%
  clean_names()

mlb_summary$runs_per_game <- (mlb_summary$r_g_x + mlb_summary$r_g_y)/2

npb_final <- npb_summary %>%
  select(1, 4) %>%
  mutate(league = "NPB")

mlb_final <- mlb_summary %>%
  select(1, 6) %>%
  mutate(league = "MLB")

final_comparison <- bind_rows(npb_final, mlb_final) %>%
  pivot_wider(names_from = league, names_prefix = "runs_per_game_", values_from = runs_per_game) %>%
  clean_names() %>%
  mutate(percent_difference = (runs_per_game_mlb - runs_per_game_npb)/runs_per_game_mlb,
         percent_difference_plt = (runs_per_game_mlb - runs_per_game_npb)/runs_per_game_mlb * 100)

mlb_blue <- "#041e42"
mlb_red <- "#bf0d3e"

display_tbl <- final_comparison %>%
  gt(rowname_col = "year")
display_tbl

display_tbl <- display_tbl %>%
  gt_plt_bar_pct(column = percent_difference_plt,
                 width = 100,
                 fill = mlb_red,
                 scaled = TRUE)
display_tbl

tbl_formatted <- display_tbl %>%
  tab_header(title = html("<b>The run-scoring gap between<br>MLB and NPB has widened</b>"),
             subtitle = html("Average number of runs scored per team per<br>game in each NPB and MLB season since 2017<br><br>")) %>%
  tab_source_note(source_note = html("<br><br>Data: Baseball Reference | Table: Plot the Ball<br>")) %>%
  tab_stubhead(label = "Season") %>%
  tab_spanner(label = md("— RUNS SCORED PER GAME —"),
              columns = c(runs_per_game_npb, runs_per_game_mlb, percent_difference)) %>%
  fmt_number(columns = c(runs_per_game_npb, runs_per_game_mlb), decimals = 1) %>%
  fmt_percent(columns = percent_difference, decimals = 0, force_sign = TRUE) %>%
  cols_label(runs_per_game_npb = "NPB",
             runs_per_game_mlb = "MLB",
             percent_difference = "+/-",
             percent_difference_plt = "") %>%
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
  tab_style(locations = cells_title(groups = "title"),
            style = list(cell_text(align = "left",
                                   color = ptb_dark_grey,
                                   size = px(30)))) %>%
  tab_style(locations = cells_title(groups = "subtitle"),
            style = list(cell_text(align = "left",
                                   color = ptb_dark_grey,
                                   size = px(20)))) %>%
  tab_style(locations = list(cells_column_spanners(),
                             cells_column_labels(),
                             cells_stubhead()),
            style = list(cell_text(color = "#FFFFFF",
                                   weight = "bold",
                                   size = "large"),
                         cell_fill(color = mlb_blue))) %>%
  tab_style(locations = cells_stub(rows = TRUE),
            style =  list(cell_text(align = "left",
                                    color = ptb_dark_grey,
                                    size = "large"),
                          cell_borders(sides = c("top", "bottom"),
                                       weight = px(1),
                                       color = ptb_light_grey))) %>%
  tab_style(locations = cells_body(),
            style = list(cell_text(color = ptb_dark_grey,
                                   size = "large"),
                         cell_borders(sides = c("top", "bottom"),
                                      weight = px(1),
                                      color = ptb_light_grey))) %>%
  tab_style(locations = cells_source_notes(),
            style = list(cell_text(align = "right",
                                   color = ptb_mid_grey,
                                   size = "medium")))  %>%
  tab_style(locations =  list(cells_stub(rows = everything())),
            style = list(cell_text(weight = "bold"))) %>%
  tab_style(locations = list(cells_body(columns = percent_difference)),
            style = list(cell_text(weight = "bold",
                                   color = mlb_red))) %>%
  tab_style(locations = list(cells_body(columns = c(runs_per_game_npb, runs_per_game_mlb))),
            style = list(cell_text(color = ptb_mid_grey)))
tbl_formatted

pitching_data <- tibble()

for (i in years) {
  
  pacific_pitching_data <- read.csv(str_c("jppl_pitching_", i, ".csv")) %>% 
    clean_names()
  
  pacific_pitching_data$year <- as.character(i)
  pacific_pitching_data$league <- "Pacific"
  
  pitching_data <- bind_rows(pitching_data, pacific_pitching_data)
  
  central_pitching_data <- read.csv(str_c("jpcl_pitching_", i, ".csv")) %>% 
    clean_names()
  
  central_pitching_data$year <- as.character(i)
  central_pitching_data$league <- "Central"
  
  pitching_data <- bind_rows(pitching_data, central_pitching_data)
  
}

pitching_clean <- pitching_data %>%
  select(2, 3, 4, 10, 12, 18, 30, 32, 34, 35, 36, 37) %>%
  filter(ip >= 100 & (ip / g) >= 4) %>%
  mutate(year = fct_relevel(year,
                           "2023", "2022", "2021", "2020", "2019"),
         custom_color = if_else(name == "Yoshinobu Yamamoto", "Focus", "Other"),
         custom_alpha = if_else(name == "Yoshinobu Yamamoto", 1, 0.2))

dodger_blue <- "#005A9C"

rough_beeswarm <- ggplot(pitching_clean, aes(era, year, alpha = custom_alpha)) +
  geom_quasirandom(varwidth = TRUE,
                   size = 3.5,
                   color = "black",
                   fill = dodger_blue,
                   shape = "circle filled") +
  theme(legend.position = "none",
        axis.line.x = element_blank(),
        panel.grid.major.y = element_blank()) +
  scale_x_continuous(name = NULL,
                     limits = c(0, 5.9),
                     breaks = seq(0, 5, 1),
                     expand = c(0,0))
rough_beeswarm

themed_beeswarm <- rough_beeswarm +
  theme_ptb()
themed_beeswarm

labelled_beeswarm <- themed_beeswarm +
  labs(title = "<span style='color:#005A9C;'>Yamamoto</span> has led Japanese pitchers<br>in ERA in four of the last five seasons",
       subtitle = "Distribution of <b>ERAs</b> recorded by starting pitchers<br>in each NPB season since 2019",
       caption = "<i>Players with <100 innings pitched excluded</i><br><br>Data: Baseball Reference | Chart: Plot the Ball")
labelled_beeswarm
