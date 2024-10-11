library(tidyverse)
library(janitor)
library(worldfootballR)
library(RcppRoll)
library(ggtext)
library(scales)
library(gt)
library(gtExtras)

league_urls <- fb_league_urls(country = "ESP", gender = "M", season_end_year = seq(1989, 2025))

team_urls_all <- c()

for (i in league_urls) {
  
  team_urls <- fb_teams_urls(i, time_pause = 3)
  
  team_urls_all <- c(team_urls_all, team_urls)
  
}

barcelona_urls <- team_urls_all[grepl(fixed("Barcelona"), x = team_urls_all)]
madrid_urls <- team_urls_all[grepl(fixed("Real-Madrid"), x = team_urls_all)]

final_list <- c(barcelona_urls, madrid_urls)

playing_time_data <- fb_team_player_stats(team_urls = final_list,
                            stat_type = "playing_time")

playing_time_clean <- playing_time_data %>%
  select(1, 2, 3, 4, 7, 9, 11) %>%
  clean_names() %>%
  filter(!is.na(min_playing_time))

playing_time_historic <- playing_time_clean %>%
  filter(season != "2024-2025") %>%
  mutate(age_final = as.integer(age),
         .after = player) %>%
  select(-age, -comp)
  
playing_time_current <- playing_time_clean %>%
  filter(season == "2024-2025") %>%
  separate(col = age,
           into = c("age_years", "age_days"),
           sep = "-") %>%
  mutate(age_years = as.integer(age_years),
         age_days = as.integer(age_days),
         day_adj = as.integer(as_date("2024-09-03") - as_date("2024-08-01")),
         age_final = if_else(day_adj > age_days, age_years - 1, age_years),
         .after = age_days) %>%
  select(-comp, -age_years, -age_days, -day_adj)

pt_final <- bind_rows(playing_time_historic, playing_time_current) %>%
  mutate(season_start_year = as.integer(str_sub(season, 0, 4)),
         .after = season) %>%
  select(-season)

pt_filtered <- pt_final %>%
  filter(season_start_year > 2002)

pt_summary_age <- pt_filtered %>%
  group_by(squad, age_final) %>%
  summarise(total_mins = sum(min_playing_time))

pt_summary <- pt_filtered %>%
  group_by(squad) %>%
  summarise(total_mins = sum(min_playing_time))

pt_comp <- left_join(x = pt_summary_age, y = pt_summary, by = c("squad" = "squad"), suffix = c("_age", "_team")) %>%
  mutate(age_mins_share = total_mins_age / total_mins_team * 100) %>%
  select(-total_mins_age, -total_mins_team) %>%
  pivot_wider(names_from = squad,
              values_from = age_mins_share) %>%
  clean_names() %>%
  replace_na(list(barcelona = 0, real_madrid = 0)) %>%
  mutate(difference = barcelona - real_madrid,
         category = barcelona > real_madrid)

rough_bars <- ggplot(pt_comp, aes(x = age_final, y = difference, fill = category)) +
  geom_bar(stat = "identity",
           width = 0.55) +
  scale_fill_manual(values = c("#003566", "#CC2941")) +
  geom_hline(yintercept = 0) +
  scale_y_continuous(limits = c(-2.4, 3.4),
                     breaks = seq(-3, 3, 1),
                     labels = percent_format(scale = 1, style_positive = "plus"),
                     expand = c(0, 0)) +
  scale_x_continuous(limits = c(14, 39),
                     breaks = seq(16, 40, 3),
                     expand = c(0, 0)) +
  theme(legend.position = "none",
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.ticks = element_blank(),
        panel.grid.major.x = element_blank(),
        axis.line.x = element_blank(),
        axis.line.y = element_blank())
rough_bars

themed_bars <- rough_bars +
  theme_ptb()
themed_bars

labelled_bars <- themed_bars +
  labs(title = "<b style='color:#CC2941'>Barcelona</b> trust youth — and <b style='color:#004F99'>Madrid</b><br>use players early in their peak",
       subtitle = "Difference in share of La Liga minutes played<br>by players of each age for <b style='color:#CC2941'>Barcelona</b> and <b style='color:#004F99'>Real<br>Madrid</b> since 2003-04 (in percentage points)",
       caption = "<i>Data as at 9 Oct 2024</i><br><br>Data: FBref | Chart: Plot the Ball")
labelled_bars

teenage_minutes <- pt_final %>%
  filter(age_final < 20 & season_start_year > 2002 & squad == "Barcelona") %>%
  select(-min_percent_playing_time)

gavi_update <- tibble(season_start_year = 2024,
                      squad = "Barcelona",
                      player = "Gavi",
                      age_final = 19,
                      min_playing_time = 0)

teenage_minutes <- bind_rows(teenage_minutes, gavi_update)

teenage_summary <- teenage_minutes %>%
  group_by(player) %>%
  summarise(tot_pt = sum(min_playing_time)) %>%
  arrange(-tot_pt) %>%
  head(10)

teenage_filtered <- teenage_minutes %>%
  arrange(age_final) %>%
  filter(player %in% teenage_summary$player) %>%
  select(-season_start_year, -squad) %>%
  pivot_wider(names_from = "age_final",
              values_from = "min_playing_time")

teenage_final <- left_join(teenage_filtered, teenage_summary, by = c("player" = "player")) %>%
  arrange(-tot_pt) %>%
  clean_names()

display_tbl <- teenage_final %>%
  gt(rowname_col = "player",
     groupname_col = NA) %>%
  sub_missing(missing_text = "") %>%
  cols_width(starts_with("x") ~ px(60))
display_tbl

display_tbl <- display_tbl %>%
  gt_plt_bar(column = tot_pt, keep_column = TRUE, width = 30, color = "#9DA6A4")
display_tbl

tbl_formatted <- display_tbl %>%
  tab_header(title = html("<b><b style='color:#CC2941'>Yamal</b> and <b style='color:#CC2941'>Cubarsí</b> have each already<br>played more than 2,000 La Liga minutes<br></b>"),
             subtitle = html("Most minutes played for Barcelona in La Liga since 2003-04<br>by players in their age-15 to age-19 seasons<br><br>")) %>%
  tab_source_note(source_note = html("<br><i>Data as at 9 Oct 2024</i><br><br>Data: FBref | Table: Plot the Ball<br>")) %>%
  tab_stubhead(label = "Player") %>%
  tab_spanner(label = md("— LA LIGA MINUTES AT AGE: —"),
              columns = c(x15, x16, x17, x18, x19)) %>%
  fmt_number(columns = c(x15, x16, x17, x18, x19, tot_pt), use_seps = TRUE, decimals = 0) %>%
  cols_label(x15 = "15",
             x16 = "16",
             x17 = "17",
             x18 = "18",
             x19 = "19",
             tot_pt = "Total",
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
                         cell_fill(color = "#79807E"))) %>%
  tab_style(locations = cells_stub(rows = TRUE),
            style =  list(cell_text(align = "left",
                                    color = ptb_mid_grey_v2,
                                    size = "large"),
                          cell_borders(sides = c("top", "bottom"),
                                       weight = px(1),
                                       color = ptb_light_grey))) %>%
  tab_style(locations = cells_body(),
            style = list(cell_text(color = ptb_mid_grey_v2,
                                   size = "large"),
                         cell_borders(sides = c("top", "bottom"),
                                      weight = px(1),
                                      color = ptb_light_grey))) %>%
  tab_style(locations = cells_source_notes(),
            style = list(cell_text(align = "right",
                                   color = ptb_mid_grey,
                                   size = "medium")))  %>%
  tab_style(locations =  list(cells_stub(rows = player == "Lamine Yamal")),
            style = list(cell_text(weight = "bold",
                                   color = "#CC2941"),
                         cell_fill(color = "#CC294110"))) %>%
  tab_style(locations =  list(cells_body(rows = player == "Lamine Yamal")),
            style = list(cell_text(color = "#CC2941"),
                         cell_fill(color = "#CC294110"))) %>%
  tab_style(locations =  list(cells_body(rows = player == "Lamine Yamal",
                                         columns = c("x18", "x19"))),
            style = list(cell_text(color = "#CC2941"),
                         cell_fill(color = "#d5d5d560"))) %>%
  tab_style(locations =  list(cells_stub(rows = player == "Pau Cubarsí")),
            style = list(cell_text(weight = "bold",
                                   color = "#CC2941"),
                         cell_fill(color = "#CC294110"))) %>%
  tab_style(locations =  list(cells_body(rows = player == "Pau Cubarsí")),
            style = list(cell_text(color = "#CC2941"),
                         cell_fill(color = "#CC294110"))) %>%
  tab_style(locations =  list(cells_body(rows = player == "Pau Cubarsí",
                                         columns = c("x18", "x19"))),
            style = list(cell_text(color = "#CC2941"),
                         cell_fill(color = "#d5d5d560"))) %>%
  tab_style(locations =  list(cells_body(rows = player == "Pau Cubarsí",
                                         columns = "x17")),
            style = list(cell_text(color = "#CC294180"))) %>%
  tab_style(locations =  list(cells_body(rows = player == "Lamine Yamal",
                                         columns = "x17")),
            style = list(cell_text(color = "#CC294180"))) %>%
  tab_style(locations =  list(cells_body(rows = player == "Gavi",
                                         columns = "x19")),
            style = list(cell_text(color = ptb_light_grey_v2))) %>%
  tab_style(locations = list(cells_body(columns = tot_pt)),
            style = list(cell_borders(sides = c("left"),
                                      weight = px(2),
                                      color = ptb_light_grey))) %>%
  tab_style(locations = list(cells_body(rows = player == "Pau Cubarsí",
                                        columns = tot_pt)),
            style = list(cell_text(weight = "bold"))) %>%
  tab_style(locations = list(cells_body(rows = player == "Lamine Yamal",
                                        columns = tot_pt)),
            style = list(cell_text(weight = "bold")))
tbl_formatted
