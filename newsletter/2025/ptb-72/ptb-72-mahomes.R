library(tidyverse)
library(rvest)
library(janitor)
library(ggtext)
library(scales)
library(gt)
library(gtExtras)
library(gtUtils)

rushing_urls <- str_c("https://www.pro-football-reference.com/years/", seq(2018, 2024, 1), "/rushing_advanced.htm")

all_years <- seq(2018, 2024, 1)

all_passing <- tibble()

for (i in all_years) {
  
  Sys.sleep(1)
  
  html <- read_html(str_c("https://www.pro-football-reference.com/years/", i, "/passing_advanced.htm"))
  
  tables <- html %>%
    html_nodes("table") %>%
    html_table()
  
  table_final <- tables[[1]] %>%
    clean_names()
  
  names(table_final) <- unlist(table_final[1,])
  
  table_final <- table_final %>%
    clean_names() %>%
    filter(player != "Player" & player != "League Average") %>%
    select(player, age, team, pos, g, gs, cmp, att, iay, cay, yac, scrm) %>%
    mutate(season = i,
           .before = team)
  
  all_passing <- bind_rows(all_passing, table_final)
  
}

all_rushing <- tibble()

for (i in all_years) {
  
  Sys.sleep(1)
  
  html <- read_html(str_c("https://www.pro-football-reference.com/years/", i, "/rushing_advanced.htm"))
  
  tables <- html %>%
    html_nodes("table") %>%
    html_table()
  
  table_final <- tables[[1]] %>%
    clean_names()
  
  names(table_final) <- unlist(table_final[1,])
  
  table_final <- table_final %>%
    clean_names() %>%
    filter(player != "Player" & player != "League Average") %>%
    select(player, age, team, pos, g, gs, att, yds, ybc, yac) %>%
    mutate(season = i,
           .before = team)
  
  all_rushing <- bind_rows(all_rushing, table_final)
  
}

passing_qbs <- all_passing %>%
  filter(pos == "QB") %>%
  mutate(g = as.integer(g),
         gs = as.integer(gs),
         cmp = as.integer(cmp),
         att = as.integer(att),
         iay = as.integer(iay),
         cay = as.integer(cay),
         yac = as.integer(yac)) %>%
  select(-scrm)

rushing_qbs <- all_rushing %>%
  filter(pos == "QB") %>%
  mutate(g = as.integer(g),
         gs = as.integer(gs),
         r_att = as.integer(att),
         r_yds = as.integer(yds),
         r_ybc = as.integer(ybc),
         r_yac = as.integer(yac)) %>%
  select(-att, -yds, -ybc, -yac)

qbs_all <- full_join(x = passing_qbs, y = rushing_qbs, by = c("player", "age", "season", "team", "pos", "g", "gs"))

qbs_filtered <- qbs_all %>%
  filter(gs > 9 & (g == gs)) %>%
  mutate(tot_att = att + r_att,
         tot_yds = cay + yac + r_yds,
         tot_att_g = tot_att / g,
         tot_yds_g = tot_yds / g,
         p_att_g = att / g,
         r_att_g = r_att / g,
         air_yards_cmp = cay / cmp,
         yac_cmp = yac / cmp,
         r_ybc_att = r_ybc / r_att,
         r_yac_att = r_yac / r_att) %>%
  arrange(desc(tot_att_g))

year_summary <- qbs_all %>%
  group_by(season) %>%
  summarise(tot_cmp = sum(cmp, na.rm = TRUE),
            tot_att = sum(att, na.rm = TRUE),
            tot_icmp = tot_att - tot_cmp,
            tot_cay = sum(cay, na.rm = TRUE),
            tot_iay = sum(iay, na.rm = TRUE),
            tot_icay = tot_iay - tot_cay) %>%
  mutate(lg_air_yards_per_comp = tot_cay / tot_cmp,
         lg_air_yards_per_incomp = tot_icay / tot_icmp) %>%
  select(season, lg_air_yards_per_comp, lg_air_yards_per_incomp)

year_qb_summary <- qbs_all %>%
  group_by(player, season) %>%
  summarise(tot_cmp = sum(cmp, na.rm = TRUE),
            tot_att = sum(att, na.rm = TRUE),
            tot_icmp = tot_att - tot_cmp,
            tot_cay = sum(cay, na.rm = TRUE),
            tot_iay = sum(iay, na.rm = TRUE),
            tot_icay = tot_iay - tot_cay) %>%
  mutate(air_yards_per_comp = tot_cay / tot_cmp,
         air_yards_per_incomp = tot_icay / tot_icmp)

mahomes_summary <- year_qb_summary %>%
  filter(player == "Patrick Mahomes") %>%
  select(player, season, air_yards_per_comp, air_yards_per_incomp)

mahomes_final <- left_join(mahomes_summary, year_summary, by = ("season")) %>%
  relocate(lg_air_yards_per_comp, .after = air_yards_per_comp)

col_1 <- unlist(mahomes_final$season)
col_2 <- unlist(mahomes_final$air_yards_per_comp)
col_3 <- unlist(mahomes_final$lg_air_yards_per_comp)
col_4 <- unlist(mahomes_final$air_yards_per_incomp)
col_5 <- unlist(mahomes_final$lg_air_yards_per_incomp)

mahomes_final <- tibble("season" = col_1,
                        "air_yards_per_comp" = col_2,
                        "lg_air_yards_per_comp" = col_3,
                        "air_yards_per_incomp" = col_4,
                        "lg_air_yards_per_incomp" = col_5)

nfl_navy_dark <- "#062040"
nfl_navy <- "#003069"
nfl_navy_desaturated <- "#405C80"
nfl_navy_light <- "#ADBBCC"
nfl_navy_fill <- "#ADBBCC20"
nfl_red <- "#d60303"
nfl_red_desaturated <- "#E68A8A"
nfl_red_light <- "#CC9999"
mid_grey <- "#E1E2E680"
light_grey <- "#F0F1F580"

display_tbl <- mahomes_final %>%
  gt(rowname_col = "season",
     row_group_as_column = TRUE) %>%
  cols_width(season ~ px(90),
             lg_air_yards_per_comp ~ px(140),
             air_yards_per_comp ~ px(140),
             lg_air_yards_per_incomp ~ px(140),
             air_yards_per_incomp ~ px(140))
display_tbl

### UPDATE TITLE

tbl_formatted <- display_tbl %>%
  tab_header(title = html("<b>Mahomes no longer throws as far downfield — on either <b style='color:#086249'>completions</b> or <b style='color:#952548'>incompletions</b><br></b>"),
             subtitle = html("<b style='color:#086249'>Actual</b> air yards per <b style='color:#086249'>complete</b> pass and <b style='color:#952548'>intended</b> air yards per <b style='color:#952548'>incomplete</b> pass recorded by <b>Patrick Mahomes</b> since <b>2018</b><br><br>")) %>%
  tab_source_note(source_note = html("<br><br><br><i>Data as at 8 Jan 2024</i><br><br>Data: Pro Football Reference | Table: Plot the Ball<br>")) %>%
  tab_stubhead(label = "Season") %>%
  tab_spanner(label = md("— AIR YDS PER COMP. —"),
              columns = c(air_yards_per_comp, lg_air_yards_per_comp)) %>%
  tab_spanner(label = md("— AIR YDS PER INCOMP. —"),
              columns = c(air_yards_per_incomp, lg_air_yards_per_incomp)) %>%
  fmt_number(columns = c(air_yards_per_comp, lg_air_yards_per_comp, air_yards_per_incomp, lg_air_yards_per_incomp), use_seps = TRUE, decimals = 1) %>%
  cols_label(air_yards_per_comp = "Mahomes",
             lg_air_yards_per_comp = "NFL ave.",
             air_yards_per_incomp = "Mahomes",
             lg_air_yards_per_incomp = "NFL ave.") %>%
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
                         cell_fill(color = nfl_navy_dark))) %>%
  tab_style(locations = cells_stub(rows = TRUE),
            style =  list(cell_text(align = "left",
                                    color = ptb_dark_grey,
                                    size = "large"),
                          cell_borders(sides = c("top", "bottom"),
                                       weight = px(1),
                                       color = ptb_light_grey))) %>%
  tab_style(locations = cells_column_labels(columns = c(air_yards_per_comp, lg_air_yards_per_comp, air_yards_per_incomp, lg_air_yards_per_incomp)),
            style = list(cell_text(align = "center"))) %>%
  tab_style(locations = cells_body(columns = c(air_yards_per_comp, lg_air_yards_per_comp, air_yards_per_incomp, lg_air_yards_per_incomp)),
            style = list(cell_text(color = ptb_dark_grey,
                                   size = "large",
                                   align = "center"),
                         cell_borders(sides = c("top", "bottom"),
                                      weight = px(1),
                                      color = ptb_light_grey))) %>%
  tab_style(locations = cells_body(rows = season == 2024,
                                   columns = c(air_yards_per_comp, air_yards_per_incomp)),
            style = list(cell_text(weight = "bold"),
                         cell_fill(color = light_grey))) %>%
  tab_style(locations = cells_body(rows = season == 2024),
            style = list(cell_fill(color = nfl_navy_fill))) %>%
  tab_style(locations = cells_stub(rows = season == 2024),
            style = list(cell_text(weight = "bold"),
                         cell_fill(color = nfl_navy_fill))) %>%
  tab_style(locations = cells_body(columns = c(air_yards_per_comp, air_yards_per_incomp)),
            cell_borders(sides = c("left"),
                         weight = px(1.5),
                         color = ptb_light_grey)) %>%
  tab_style(locations = cells_source_notes(),
            style = list(cell_text(align = "right",
                                   color = ptb_mid_grey,
                                   size = "medium"))) %>%
  gt_color_pills(columns = air_yards_per_comp,
                 digits = 1,
                 pill_height = 25,
                 palette = c("#A3DCCC", "#053D2E"),
                 domain = c(min(mahomes_final$air_yards_per_comp) - 0.1, max(mahomes_final$air_yards_per_comp) + 0.1)) %>%
  gt_color_pills(columns = lg_air_yards_per_comp,
                 digits = 1,
                 pill_height = 25,
                 palette = c("#A3DCCC", "#053D2E"),
                 domain = c(min(mahomes_final$air_yards_per_comp) - 0.1, max(mahomes_final$air_yards_per_comp) + 0.1)) %>%
  gt_color_pills(columns = air_yards_per_incomp,
                 digits = 1,
                 pill_height = 25,
                 palette = c("#F0C4D1", "#4D1325"),
                 domain = c(min(mahomes_final$air_yards_per_incomp) - 0.1, max(mahomes_final$air_yards_per_incomp) + 0.1)) %>%
  gt_color_pills(columns = lg_air_yards_per_incomp,
                 digits = 1,
                 pill_height = 25,
                 palette = c("#F0C4D1", "#4D1325"),
                 domain = c(min(mahomes_final$air_yards_per_incomp) - 0.1, max(mahomes_final$air_yards_per_incomp) + 0.1))
tbl_formatted
