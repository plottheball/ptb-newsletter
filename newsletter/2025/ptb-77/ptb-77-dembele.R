library(tidyverse)
library(janitor)
library(rvest)
library(gt)

league_url <- "https://fbref.com/en/players/b19db005/Ousmane-Dembele"
league_html <- read_html(league_url)
league_tables <- league_html %>%
  html_nodes("table") %>%
  html_table()

summary_table_league <- league_tables[[6]] %>%
  clean_names()

new_names <- unlist(summary_table_league[1,])
names(summary_table_league) <- new_names

summary_table_league <- summary_table_league %>%
  clean_names() %>%
  mutate(age = as.integer(age),
         prog_actions = as.integer(prg_c) + as.integer(prg_p) + as.integer(prg_r)) %>%
  filter(!is.na(age)) %>%
  select(season, age, squad, comp, x90s, npx_g_x_ag, prog_actions) %>%
  mutate(x90s = as.numeric(x90s),
         npx_g_x_ag = as.numeric(npx_g_x_ag))

europe_url <- "https://fbref.com/en/players/b19db005/all_comps/Ousmane-Dembele-Stats---All-Competitions"

europe_html <- read_html(europe_url)
europe_tables <- europe_html %>%
  html_nodes("table") %>%
  html_table()

summary_table_europe <- europe_tables[[6]] %>%
  clean_names()

new_names_europe <- unlist(summary_table_europe[1,])
names(summary_table_europe) <- new_names_europe

summary_table_europe <- summary_table_europe %>%
  clean_names() %>%
  mutate(age = as.integer(age),
         prog_actions = as.integer(prg_c) + as.integer(prg_p) + as.integer(prg_r)) %>%
  filter(!is.na(age)) %>%
  select(season, age, squad, comp, x90s, npx_g_x_ag, prog_actions) %>%
  mutate(x90s = as.numeric(x90s),
         npx_g_x_ag = as.numeric(npx_g_x_ag))

summary_data <- bind_rows(summary_table_europe, summary_table_league)

summary_final <- summary_data %>%
  group_by(season, age, squad) %>%
  summarise(tot_x90s = sum(x90s),
            tot_npx_g_x_ag = sum(npx_g_x_ag),
            tot_prog_actions = sum(prog_actions))

pt_table_league <- league_tables[[13]] %>%
  clean_names()

new_names_pt_league <- unlist(pt_table_league[1,])
names(pt_table_league) <- new_names_pt_league

pt_table_league <- pt_table_league %>%
  clean_names() %>%
  mutate(age = as.integer(age)) %>%
  filter(!is.na(age)) %>%
  select(season, age, squad, comp, min, min_percent) %>%
  mutate(min = as.integer(str_remove_all(min, fixed(","))),
         min_percent = as.numeric(min_percent) / 100,
         available_min = min / min_percent,
         avail_final = round((available_min / 90), digits = 0) * 90) %>%
  select(season, age, squad, comp, avail_final)

pt_table_europe <- europe_tables[[48]] %>%
  clean_names()

new_names_pt_europe <- unlist(pt_table_europe[1,])
names(pt_table_europe) <- new_names_pt_europe

pt_table_europe <- pt_table_europe %>%
  clean_names() %>%
  mutate(age = as.integer(age)) %>%
  filter(!is.na(age)) %>%
  select(season, age, squad, comp, min, min_percent) %>%
  mutate(min = as.integer(str_remove_all(min, fixed(","))),
         min_percent = as.numeric(min_percent) / 100,
         available_min = min / min_percent,
         avail_final = round((available_min / 90), digits = 0) * 90) %>%
  select(season, age, squad, comp, avail_final)

pt_data <- bind_rows(pt_table_europe, pt_table_league)

pt_final <- pt_data %>%
  group_by(season, age, squad) %>%
  summarise(tot_avail_x90s = sum(avail_final) / 90)

final_table <- full_join(x = summary_final, y = pt_final) %>%
  mutate(mins_share = tot_x90s / tot_avail_x90s * 100,
         chance_creation = tot_npx_g_x_ag / tot_x90s,
         prog_contribution = tot_prog_actions / tot_x90s) %>%
  mutate(season = str_replace_all(season, fixed("-20"), "-")) %>%
  select(season, age, squad, mins_share, chance_creation, prog_contribution)

header_colour <- "#79807E"
barca_red <- "#CC7A87"
psg_blue <- "#01416f"
rennes_black <- "#4D3D3D"
dortmund_yellow <- "#BFB786"
background_data <- "#C9C9C9"
highlight_psg <- str_c(psg_blue, "08")
na_fill <- str_c(header_colour, "08")

display_tbl <- final_table %>%
  gt(rowname_col = "season",
     groupname_col = NA) %>%
  sub_missing(missing_text = "n/a") %>%
  cols_width(season ~ px(80),
             age ~ px(50),
             squad ~ px(110),
             mins_share ~ px(110),
             chance_creation ~ px(125),
             prog_contribution ~ px(125))
display_tbl

tbl_formatted <- display_tbl %>%
  tab_header(title = html("Dembélé has played more in each season at <b style='color:#004680'>PSG</b> than he ever did at <b style='color:#CC7A87'>Barça</b>"),
             subtitle = html("Share of available <b>minutes</b> played by <b>Ousmane Dembélé</b> in league and continental competition since 2015-16, and his contribution to <b>creation</b> and <b>progression</b> in each season<br><br>")) %>%
  tab_source_note(source_note = html("<br>'NPxG + xAG' = non-penalty expected goals + expected assisted goals<br>'Prog. actions' = progressive carries, passes and receptions<br><br><br>Data: FBref | Table: Plot the Ball<br>")) %>%
  tab_stubhead(label = "") %>%
  tab_spanner(label = md("— PER 90 MINUTES: —"),
              columns = c(chance_creation, prog_contribution)) %>%
  fmt_number(columns = c(prog_contribution), use_seps = TRUE, decimals = 1) %>%
  fmt_number(columns = c(chance_creation), use_seps = TRUE, decimals = 2) %>%
  fmt_percent(columns = c(mins_share), use_seps = TRUE, decimals = 0, scale_values = FALSE) %>%
  cols_label(age = "Age",
             squad = "Team",
             mins_share = "% of mins",
             chance_creation = "NPxG + xAG",
             prog_contribution = "Prog. actions") %>%
  cols_align(align = "right",
             columns = everything()) %>%
  cols_align(align = "left",
             columns = c(squad)) %>%
  cols_align(align = "center",
             columns = c(age, mins_share)) %>%
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
  tab_style(locations = cells_body(columns = c(mins_share, chance_creation)),
            style = list(cell_borders(sides = c("left"),
                                      weight = px(1.5),
                                      color = ptb_light_grey))) %>%
  tab_style(locations = cells_body(rows = squad == "Barcelona"),
            style = list(cell_text(color = barca_red))) %>%
  tab_style(locations = cells_stub(rows = squad == "Barcelona"),
            style = list(cell_text(color = barca_red))) %>%
  tab_style(locations = cells_body(rows = squad == "Rennes",
                                   columns = c(age, squad, mins_share)),
            style = list(cell_text(color = rennes_black))) %>%
  tab_style(locations = cells_stub(rows = squad == "Rennes"),
            style = list(cell_text(color = rennes_black))) %>%
  tab_style(locations = cells_body(rows = squad == "Dortmund",
                                   columns = c(age, squad, mins_share)),
            style = list(cell_text(color = dortmund_yellow))) %>%
  tab_style(locations = cells_stub(rows = squad == "Dortmund"),
            style = list(cell_text(color = dortmund_yellow))) %>%
  tab_style(locations = cells_body(rows = squad == "Paris S-G"),
            style = list(cell_text(color = psg_blue,
                                   weight = "bold"),
                         cell_fill(color = highlight_psg))) %>%
  tab_style(locations = cells_stub(rows = squad == "Paris S-G"),
            style = list(cell_text(color = psg_blue,
                                   weight = "bold"),
                         cell_fill(color = highlight_psg))) %>%
  tab_style(locations = cells_body(rows = squad == "Rennes",
                                   columns = c(chance_creation, prog_contribution)),
            style = list(cell_fill(color = na_fill))) %>%
  tab_style(locations = cells_body(rows = squad == "Dortmund",
                                   columns = c(chance_creation, prog_contribution)),
            style = list(cell_fill(color = na_fill)))
tbl_formatted
