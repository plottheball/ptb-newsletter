library(tidyverse)
library(googlesheets4)
library(rvest)
library(janitor)
library(worldfootballR)
library(gt)
library(gtUtils)
library(gtExtras)

gk_list <- read_sheet("https://docs.google.com/spreadsheets/d/1MStuD37jY38Hrc8iSSdvdlWS1rZWIbCGC8FsVkQGsj8/edit?gid=0#gid=0",
                      sheet = "Euro 2025 squads") %>%
  clean_names()

all_data <- tibble()

#### NWSL 2023-25

nwsl_2023 <- fb_league_stats(country = "USA", gender = "F", tier = "1st", season_end_year = 2023, stat_type = "keepers_adv", team_or_player = "player") %>%
  clean_names() %>%
  select(-age) %>%
  mutate(year = 2023)
nwsl_2024 <- fb_league_stats(country = "USA", gender = "F", tier = "1st", season_end_year = 2024, stat_type = "keepers_adv", team_or_player = "player") %>%
  clean_names() %>%
  select(-age) %>%
  mutate(year = 2024)
nwsl_2025 <- fb_league_stats(country = "USA", gender = "F", tier = "1st", season_end_year = 2025, stat_type = "keepers_adv", team_or_player = "player") %>%
  clean_names() %>%
  select(-age) %>%
  mutate(year = 2025)
nwsl_all <- bind_rows(nwsl_2023, nwsl_2024, nwsl_2025) %>%
  select(36, 2, 4, 7, 9, 13, 14, 16, 3)

nwsl_2023_basic <- fb_league_stats(country = "USA", gender = "F", tier = "1st", season_end_year = 2023, stat_type = "keepers", team_or_player = "player") %>%
  clean_names() %>%
  select(-age) %>%
  mutate(year = 2023)
nwsl_2024_basic <- fb_league_stats(country = "USA", gender = "F", tier = "1st", season_end_year = 2024, stat_type = "keepers", team_or_player = "player") %>%
  clean_names() %>%
  select(-age) %>%
  mutate(year = 2024)
nwsl_2025_basic <- fb_league_stats(country = "USA", gender = "F", tier = "1st", season_end_year = 2025, stat_type = "keepers", team_or_player = "player") %>%
  clean_names() %>%
  select(-age) %>%
  mutate(year = 2025)
nwsl_all_basic <- bind_rows(nwsl_2023_basic, nwsl_2024_basic, nwsl_2025_basic) %>%
  select(29, 2, 4, 7, 14, 3)

nwsl_final <- full_join(nwsl_all_basic, nwsl_all, by = c("player", "nation", "born", "player_href", "year")) %>%
  relocate(so_ta, .after = "player_href")

all_data <- bind_rows(all_data, nwsl_final)

#### Bundesliga 2023-25

buli_2023 <- fb_league_stats(country = "GER", gender = "F", tier = "1st", season_end_year = 2023, stat_type = "keepers_adv", team_or_player = "player") %>%
  clean_names() %>%
  select(-age) %>%
  mutate(year = 2023)
buli_2024 <- fb_league_stats(country = "GER", gender = "F", tier = "1st", season_end_year = 2024, stat_type = "keepers_adv", team_or_player = "player") %>%
  clean_names() %>%
  select(-age) %>%
  mutate(year = 2024)
buli_2025 <- fb_league_stats(country = "GER", gender = "F", tier = "1st", season_end_year = 2025, stat_type = "keepers_adv", team_or_player = "player") %>%
  clean_names() %>%
  select(-age) %>%
  mutate(year = 2025)
buli_all <- bind_rows(buli_2023, buli_2024, buli_2025) %>%
  select(36, 2, 4, 7, 9, 13, 14, 16, 3)

buli_2023_basic <- fb_league_stats(country = "GER", gender = "F", tier = "1st", season_end_year = 2023, stat_type = "keepers", team_or_player = "player") %>%
  clean_names() %>%
  select(-age) %>%
  mutate(year = 2023)
buli_2024_basic <- fb_league_stats(country = "GER", gender = "F", tier = "1st", season_end_year = 2024, stat_type = "keepers", team_or_player = "player") %>%
  clean_names() %>%
  select(-age) %>%
  mutate(year = 2024)
buli_2025_basic <- fb_league_stats(country = "GER", gender = "F", tier = "1st", season_end_year = 2025, stat_type = "keepers", team_or_player = "player") %>%
  clean_names() %>%
  select(-age) %>%
  mutate(year = 2025)
buli_all_basic <- bind_rows(buli_2023_basic, buli_2024_basic, buli_2025_basic) %>%
  select(29, 2, 4, 7, 14, 3)

buli_final <- full_join(buli_all_basic, buli_all, by = c("player", "nation", "born", "player_href", "year")) %>%
  relocate(so_ta, .after = "player_href")

all_data <- bind_rows(all_data, buli_final)

#### WSL 2023-25

ewsl_2023 <- fb_league_stats(country = "ENG", gender = "F", tier = "1st", season_end_year = 2023, stat_type = "keepers_adv", team_or_player = "player") %>%
  clean_names() %>%
  select(-age) %>%
  mutate(year = 2023)
ewsl_2024 <- fb_league_stats(country = "ENG", gender = "F", tier = "1st", season_end_year = 2024, stat_type = "keepers_adv", team_or_player = "player") %>%
  clean_names() %>%
  select(-age) %>%
  mutate(year = 2024)
ewsl_2025 <- fb_league_stats(country = "ENG", gender = "F", tier = "1st", season_end_year = 2025, stat_type = "keepers_adv", team_or_player = "player") %>%
  clean_names() %>%
  select(-age) %>%
  mutate(year = 2025)
ewsl_all <- bind_rows(ewsl_2023, ewsl_2024, ewsl_2025) %>%
  select(36, 2, 4, 7, 9, 13, 14, 16, 3)

ewsl_2023_basic <- fb_league_stats(country = "ENG", gender = "F", tier = "1st", season_end_year = 2023, stat_type = "keepers", team_or_player = "player") %>%
  clean_names() %>%
  select(-age) %>%
  mutate(year = 2023)
ewsl_2024_basic <- fb_league_stats(country = "ENG", gender = "F", tier = "1st", season_end_year = 2024, stat_type = "keepers", team_or_player = "player") %>%
  clean_names() %>%
  select(-age) %>%
  mutate(year = 2024)
ewsl_2025_basic <- fb_league_stats(country = "ENG", gender = "F", tier = "1st", season_end_year = 2025, stat_type = "keepers", team_or_player = "player") %>%
  clean_names() %>%
  select(-age) %>%
  mutate(year = 2025)
ewsl_all_basic <- bind_rows(ewsl_2023_basic, ewsl_2024_basic, ewsl_2025_basic) %>%
  select(29, 2, 4, 7, 14, 3)

ewsl_final <- full_join(ewsl_all_basic, ewsl_all, by = c("player", "nation", "born", "player_href", "year"), relationship = "many-to-many") %>%
  relocate(so_ta, .after = "player_href") %>%
  ### LIZE KOP 2025 FIX
  filter(ga_goals < so_ta & (p_sx_g_expected / so_ta) > 0.07)

all_data <- bind_rows(all_data, ewsl_final)

#### Liga F 2023-25

ligf_2023 <- fb_league_stats(country = "ESP", gender = "F", tier = "1st", season_end_year = 2023, stat_type = "keepers_adv", team_or_player = "player") %>%
  clean_names() %>%
  select(-age) %>%
  mutate(year = 2023)
ligf_2024 <- fb_league_stats(country = "ESP", gender = "F", tier = "1st", season_end_year = 2024, stat_type = "keepers_adv", team_or_player = "player") %>%
  clean_names() %>%
  select(-age) %>%
  mutate(year = 2024)
ligf_2025 <- fb_league_stats(country = "ESP", gender = "F", tier = "1st", season_end_year = 2025, stat_type = "keepers_adv", team_or_player = "player") %>%
  clean_names() %>%
  select(-age) %>%
  mutate(year = 2025)
ligf_all <- bind_rows(ligf_2023, ligf_2024, ligf_2025) %>%
  select(36, 2, 4, 7, 9, 13, 14, 16, 3)

ligf_2023_basic <- fb_league_stats(country = "ESP", gender = "F", tier = "1st", season_end_year = 2023, stat_type = "keepers", team_or_player = "player") %>%
  clean_names() %>%
  select(-age) %>%
  mutate(year = 2023)
ligf_2024_basic <- fb_league_stats(country = "ESP", gender = "F", tier = "1st", season_end_year = 2024, stat_type = "keepers", team_or_player = "player") %>%
  clean_names() %>%
  select(-age) %>%
  mutate(year = 2024)
ligf_2025_basic <- fb_league_stats(country = "ESP", gender = "F", tier = "1st", season_end_year = 2025, stat_type = "keepers", team_or_player = "player") %>%
  clean_names() %>%
  select(-age) %>%
  mutate(year = 2025)
ligf_all_basic <- bind_rows(ligf_2023_basic, ligf_2024_basic, ligf_2025_basic) %>%
  select(29, 2, 4, 7, 14, 3)

ligf_final <- full_join(ligf_all_basic, ligf_all, by = c("player", "nation", "born", "player_href", "year"), relationship = "many-to-many") %>%
  relocate(so_ta, .after = "player_href")

all_data <- bind_rows(all_data, ligf_final)

#### Serie A 2023-25

sera_2023 <- fb_league_stats(country = "ITA", gender = "F", tier = "1st", season_end_year = 2023, stat_type = "keepers_adv", team_or_player = "player") %>%
  clean_names() %>%
  select(-age) %>%
  mutate(year = 2023)
sera_2024 <- fb_league_stats(country = "ITA", gender = "F", tier = "1st", season_end_year = 2024, stat_type = "keepers_adv", team_or_player = "player") %>%
  clean_names() %>%
  select(-age) %>%
  mutate(year = 2024)
sera_2025 <- fb_league_stats(country = "ITA", gender = "F", tier = "1st", season_end_year = 2025, stat_type = "keepers_adv", team_or_player = "player") %>%
  clean_names() %>%
  select(-age) %>%
  mutate(year = 2025)
sera_all <- bind_rows(sera_2023, sera_2024, sera_2025) %>%
  select(36, 2, 4, 7, 9, 13, 14, 16, 3)

sera_2023_basic <- fb_league_stats(country = "ITA", gender = "F", tier = "1st", season_end_year = 2023, stat_type = "keepers", team_or_player = "player") %>%
  clean_names() %>%
  select(-age) %>%
  mutate(year = 2023)
sera_2024_basic <- fb_league_stats(country = "ITA", gender = "F", tier = "1st", season_end_year = 2024, stat_type = "keepers", team_or_player = "player") %>%
  clean_names() %>%
  select(-age) %>%
  mutate(year = 2024)
sera_2025_basic <- fb_league_stats(country = "ITA", gender = "F", tier = "1st", season_end_year = 2025, stat_type = "keepers", team_or_player = "player") %>%
  clean_names() %>%
  select(-age) %>%
  mutate(year = 2025)
sera_all_basic <- bind_rows(sera_2023_basic, sera_2024_basic, sera_2025_basic) %>%
  select(29, 2, 4, 7, 14, 3)

sera_final <- full_join(sera_all_basic, sera_all, by = c("player", "nation", "born", "player_href", "year"), relationship = "many-to-many") %>%
  relocate(so_ta, .after = "player_href")
  ### NB ROBERTA APRILE 2025 NEEDS FIXING

all_data <- bind_rows(all_data, sera_final) 

#### Ligue 1 2023-25

ligu_2023 <- fb_league_stats(country = "FRA", gender = "F", tier = "1st", season_end_year = 2023, stat_type = "keepers_adv", team_or_player = "player") %>%
  clean_names() %>%
  select(-age) %>%
  mutate(year = 2023)
ligu_2024 <- fb_league_stats(country = "FRA", gender = "F", tier = "1st", season_end_year = 2024, stat_type = "keepers_adv", team_or_player = "player") %>%
  clean_names() %>%
  select(-age) %>%
  mutate(year = 2024)
ligu_2025 <- fb_league_stats(country = "FRA", gender = "F", tier = "1st", season_end_year = 2025, stat_type = "keepers_adv", team_or_player = "player") %>%
  clean_names() %>%
  select(-age) %>%
  mutate(year = 2025)
ligu_all <- bind_rows(ligu_2023, ligu_2024, ligu_2025) %>%
  select(36, 2, 4, 7, 9, 13, 14, 16, 3)

ligu_2023_basic <- fb_league_stats(country = "FRA", gender = "F", tier = "1st", season_end_year = 2023, stat_type = "keepers", team_or_player = "player") %>%
  clean_names() %>%
  select(-age) %>%
  mutate(year = 2023)
ligu_2024_basic <- fb_league_stats(country = "FRA", gender = "F", tier = "1st", season_end_year = 2024, stat_type = "keepers", team_or_player = "player") %>%
  clean_names() %>%
  select(-age) %>%
  mutate(year = 2024)
ligu_2025_basic <- fb_league_stats(country = "FRA", gender = "F", tier = "1st", season_end_year = 2025, stat_type = "keepers", team_or_player = "player") %>%
  clean_names() %>%
  select(-age) %>%
  mutate(year = 2025)
ligu_all_basic <- bind_rows(ligu_2023_basic, ligu_2024_basic, ligu_2025_basic) %>%
  select(29, 2, 4, 7, 14, 3)

ligu_final <- full_join(ligu_all_basic, ligu_all, by = c("player", "nation", "born", "player_href", "year"), relationship = "many-to-many") %>%
  relocate(so_ta, .after = "player_href")

all_data <- bind_rows(all_data, ligu_final) 

#### UWCL 2023-25

uwcl_2023 <- fb_league_stats(country = NA_character_, gender = "F", tier = NA_character_, non_dom_league_url = "https://fbref.com/en/comps/181/history/Champions-League-Seasons", season_end_year = 2023, stat_type = "keepers_adv", team_or_player = "player") %>%
  clean_names() %>%
  select(-age) %>%
  mutate(year = 2023)
uwcl_2024 <- fb_league_stats(country = NA_character_, gender = "F", tier = NA_character_, non_dom_league_url = "https://fbref.com/en/comps/181/history/Champions-League-Seasons", season_end_year = 2024, stat_type = "keepers_adv", team_or_player = "player") %>%
  clean_names() %>%
  select(-age) %>%
  mutate(year = 2024)
uwcl_2025 <- fb_league_stats(country = NA_character_, gender = "F", tier = NA_character_, non_dom_league_url = "https://fbref.com/en/comps/181/history/Champions-League-Seasons", season_end_year = 2025, stat_type = "keepers_adv", team_or_player = "player") %>%
  clean_names() %>%
  select(-age) %>%
  mutate(year = 2025)
uwcl_all <- bind_rows(uwcl_2023, uwcl_2024, uwcl_2025) %>%
  select(36, 2, 4, 7, 9, 13, 14, 16, 3)

uwcl_2023_basic <- fb_league_stats(country = NA_character_, gender = "F", tier = NA_character_, non_dom_league_url = "https://fbref.com/en/comps/181/history/Champions-League-Seasons", season_end_year = 2023, stat_type = "keepers", team_or_player = "player") %>%
  clean_names() %>%
  select(-age) %>%
  mutate(year = 2023)
uwcl_2024_basic <- fb_league_stats(country = NA_character_, gender = "F", tier = NA_character_, non_dom_league_url = "https://fbref.com/en/comps/181/history/Champions-League-Seasons", season_end_year = 2024, stat_type = "keepers", team_or_player = "player") %>%
  clean_names() %>%
  select(-age) %>%
  mutate(year = 2024)
uwcl_2025_basic <- fb_league_stats(country = NA_character_, gender = "F", tier = NA_character_, non_dom_league_url = "https://fbref.com/en/comps/181/history/Champions-League-Seasons", season_end_year = 2025, stat_type = "keepers", team_or_player = "player") %>%
  clean_names() %>%
  select(-age) %>%
  mutate(year = 2025)
uwcl_all_basic <- bind_rows(uwcl_2023_basic, uwcl_2024_basic, uwcl_2025_basic) %>%
  select(29, 2, 4, 7, 14, 3)

uwcl_final <- full_join(uwcl_all_basic, uwcl_all, by = c("player", "nation", "born", "player_href", "year"), relationship = "many-to-many") %>%
  relocate(so_ta, .after = "player_href")

all_data <- bind_rows(all_data, uwcl_final) %>%
  unique()

### World Cup 2023

fwwc_2023 <- read_sheet("https://docs.google.com/spreadsheets/d/1MStuD37jY38Hrc8iSSdvdlWS1rZWIbCGC8FsVkQGsj8/edit?gid=0#gid=0",
                        sheet = "WWC 2023") %>%
  clean_names()

all_data <- bind_rows(all_data, fwwc_2023) %>%
  unique()


### TIDY DATA AND FILTER

filtered_data <- all_data %>%
  filter(player_href %in% gk_list$link)

summary_data <- filtered_data %>%
  group_by(player_href, player, nation, born) %>%
  summarise(tot_sota = sum(so_ta),
            tot_ga = sum(ga_goals),
            tot_oga = sum(og_goals),
            tot_psxga = sum(p_sx_g_expected)) %>%
  arrange(desc(tot_sota)) %>%
  filter(tot_sota >= 100) %>%
  mutate(psxg_vs_g = tot_psxga - tot_ga + tot_oga,
         per_100_sota = psxg_vs_g / tot_sota * 100) %>%
  arrange(desc(per_100_sota))

all_countries <- unique(unlist(summary_data$nation))

top_gks <- tibble()

for (i in all_countries) {
  
  filtered_data <- summary_data %>%
    filter(nation == i) %>%
    head(1)
  
  top_gks <- bind_rows(top_gks, filtered_data)
  
}

for_table <- top_gks %>%
  head(10) %>%
  select(2, 3, 5, 9, 10)

for_table$nation <- str_replace_all(for_table$nation, "is ISL", "Iceland")
for_table$nation <- str_replace_all(for_table$nation, "pt POR", "Portugal")
for_table$nation <- str_replace_all(for_table$nation, "eng ENG", "England")
for_table$nation <- str_replace_all(for_table$nation, "de GER", "Germany")
for_table$nation <- str_replace_all(for_table$nation, "it ITA", "Italy")
for_table$nation <- str_replace_all(for_table$nation, "es ESP", "Spain")
for_table$nation <- str_replace_all(for_table$nation, "se SWE", "Sweden")
for_table$nation <- str_replace_all(for_table$nation, "pl POL", "Poland")
for_table$nation <- str_replace_all(for_table$nation, "nl NED", "Netherlands")
for_table$nation <- str_replace_all(for_table$nation, "no NOR", "Norway")

display_table <- for_table %>%
  gt(rowname_col = "player") %>%
  cols_width(player ~ px(235),
             nation ~ px(115),
             tot_sota ~ px(65),
             psxg_vs_g ~ px(65),
             per_100_sota ~ px(150))
display_table

header_colour <- "#79807E"
text_focus <- "#02529C"
fill_focus <- str_c(text_focus, "10")

tbl_formatted <- display_table %>%
  tab_header(title = html("In <b style='color:#02529C'>Cecilía Rán Rúnarsdóttir</b>, Iceland might have the best goalie at Euro 2025"),
             subtitle = html("Top <b>goalkeepers</b> at <b>Euro 2025</b>, based on performances in the 'big five' women's leagues, NWSL, UWCL and World Cup <b>since 2022-23</b><br><br>")) %>%
  tab_source_note(source_note = html("<br>'SOTA' = shots on target against | 'goals saved' = post-shot xG less goals conceded<br><br><i>Players with < 100 SOTA excluded; only one player per competing country included</i><br><br>Data: FBref | Table: Plot the Ball<br>")) %>%
  tab_stubhead(label = "") %>%
  tab_spanner(label = md("— GOALS SAVED —"),
              columns = c(psxg_vs_g, per_100_sota)) %>%
  fmt_number(columns = c(psxg_vs_g, per_100_sota), decimals = 1, force_sign = TRUE) %>%
  fmt_number(columns = c(tot_sota), use_seps = TRUE, decimals = 0, force_sign = FALSE) %>%
  cols_label(nation = "Nation",
             tot_sota = "SOTA",
             psxg_vs_g = "Total",
             per_100_sota = "▼ Per 100 SOTA") %>%
  cols_align(align = "right",
             columns = everything()) %>%
  cols_align(align = "left",
             columns = c(player, nation)) %>%
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
                                   weight = "bold",
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
                         cell_fill(color = header_colour))) %>%
  tab_style(locations = cells_stub(rows = TRUE),
            style =  list(cell_text(align = "left",
                                    color = ptb_mid_grey,
                                    size = "large"),
                          cell_borders(sides = c("top", "bottom"),
                                       weight = px(1),
                                       color = ptb_light_grey))) %>%
  tab_style(locations = cells_body(),
            style = list(cell_text(color = ptb_mid_grey,
                                   size = "large"),
                         cell_borders(sides = c("top", "bottom"),
                                      weight = px(1),
                                      color = ptb_light_grey))) %>%
  tab_style(locations = cells_source_notes(),
            style = list(cell_text(align = "right",
                                   color = ptb_mid_grey,
                                   size = "medium"))) %>%
  tab_style(locations = cells_body(columns = c(psxg_vs_g)),
            style = list(cell_borders(sides = c("left"),
                                      weight = px(2.5),
                                      color = ptb_light_grey))) %>%
  tab_style(locations = cells_body(columns = c(tot_sota, per_100_sota)),
            style = list(cell_borders(sides = c("left"),
                                      weight = px(1.5),
                                      color = ptb_light_grey))) %>%
  tab_style(locations = cells_stub(rows = nation == "Iceland"),
            style = list(cell_text(color = text_focus,
                                   weight = "bold"),
                         cell_fill(color = fill_focus))) %>%
  tab_style(locations = cells_body(rows = nation == "Iceland"),
            style = list(cell_text(color = text_focus,
                                   weight = "bold"),
                         cell_fill(color = fill_focus)))
tbl_formatted
