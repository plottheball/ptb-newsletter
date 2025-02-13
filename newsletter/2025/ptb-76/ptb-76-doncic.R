library(tidyverse)
library(rvest)
library(janitor)
library(gt)

all_nba_players <- c("a/antetgi01.html",
                     "d/doncilu01.html",
                     "g/gilgesh01.html",
                     "j/jokicni01.html",
                     "t/tatumja01.html")

all_mp_data <- tibble()

for (i in all_nba_players) {
  
  Sys.sleep(1)
  
  html <- read_html(str_c("https://www.basketball-reference.com/players/", i))
  
  name <- html %>%
    html_nodes("h1") %>%
    html_text()
  
  tables <- html %>%
    html_nodes("table") %>%
    html_table()
  
  key_table <- tables[[5]] %>%
    clean_names() %>%
    mutate(player = name,
           age = as.integer(age),
           mp = as.integer(mp)) %>%
    filter(lg == "NBA" & team != "2TM") %>%
    select(player, age, lg, team, mp)
  
  key_table_post <- tables[[6]] %>%
    clean_names() %>%
    mutate(player = name,
           age = as.integer(age),
           mp = as.integer(mp)) %>%
    filter(lg == "NBA" & team != "2TM") %>%
    select(player, age, lg, team, mp)
  
  key_table <- bind_rows(key_table, key_table_post)
  
  all_mp_data <- bind_rows(all_mp_data, key_table)

}

all_mp_summary <- all_mp_data %>%
  group_by(player, age) %>%
  summarise(total_mp = sum(mp)) %>%
  arrange(desc(total_mp))

all_nba_overseas <- c("luka-doncic-1.html")

overseas_mp_data <- tibble()

for (i in all_nba_overseas) {
  
  Sys.sleep(1)
  
  html <- read_html(str_c("https://www.basketball-reference.com/international/players/", i))
  
  name <- html %>%
    html_nodes("h1") %>%
    html_text() %>%
    str_remove(., fixed(" International Stats"))
  
  tables <- html %>%
    html_nodes("table") %>%
    html_table()
  
  key_table <- tables[[4]] %>%
    clean_names() %>%
    mutate(player = name,
           mp = as.integer(mp)) %>%
    filter(league == "Euroleague" | league == "EuroLeague") %>%
    select(player, season, league, team, mp)
  
  key_table_post <- tables[[16]] %>%
    clean_names() %>%
    mutate(player = name,
           mp = as.integer(mp)) %>%
    filter(league == "Euroleague" | league == "EuroLeague") %>%
    select(player, season, league, team, mp)
  
  key_table <- bind_rows(key_table, key_table_post)
  
  overseas_mp_data <- bind_rows(overseas_mp_data, key_table)
  
}

overseas_mp_final <- overseas_mp_data %>%
  mutate(age = case_when(player == "\n\t\tLuka Dončić\n\t\t\t" & season == "2017-18" ~ 18,
                         player == "\n\t\tLuka Dončić\n\t\t\t" & season == "2016-17" ~ 17,
                         player == "\n\t\tLuka Dončić\n\t\t\t" & season == "2015-16" ~ 16)) 

overseas_mp_summary <- overseas_mp_final %>%
  group_by(player, age) %>%
  summarise(total_mp = sum(mp)) %>%
  arrange(desc(total_mp))

all_euroleague_nba_mp <- bind_rows(all_mp_summary, overseas_mp_summary) %>%
  arrange(player, age)

mp_at_24 <- all_euroleague_nba_mp %>%
  filter(age < 25) %>%
  arrange(age) %>%
  mutate(equiv_gp = total_mp / 36)

mp_at_24_top_5 <- mp_at_24 %>%
  group_by(player) %>%
  summarise(total = sum(equiv_gp)) %>%
  arrange(desc(total)) %>%
  head(5)

mp_at_24_final <- mp_at_24 %>%
  filter(player %in% mp_at_24_top_5$player) %>%
  select(-total_mp) %>%
  pivot_wider(names_from = "age",
              values_from = "equiv_gp") %>%
  clean_names() %>%
  left_join(., y = mp_at_24_top_5, by = "player") %>%
  arrange(desc(total)) %>%
  mutate(player = str_replace(player, fixed("Giannis Antetokounmpo"), "Giannis A.")) %>%
  mutate(player = str_replace(player, fixed("Shai Gilgeous-Alexander"), "Shai Gilgeous-A."))

display_tbl <- mp_at_24_final %>%
  gt(rowname_col = "player",
     groupname_col = NA) %>%
  sub_missing(missing_text = "") %>%
  cols_width(player ~ px(145),
             starts_with("x") ~ px(50),
             total ~ px(55))
display_tbl

tbl_formatted <- display_tbl %>%
  tab_header(title = html("New Laker <b style='color:#CC9C33'>Luka Dončić</b> began his pro career much younger than his peers"),
             subtitle = html("<b>True games played</b> by each 2023-24 <b>All-NBA First Team</b> selection in <b>top club competitions</b> through their <b>age-24</b> season<br><br>")) %>%
  tab_source_note(source_note = html("<br><i>36 mins played = 1 'true game played'; EuroLeague and NBA only (incl. playoffs)</i><br><br>Data: Basketball Reference | Table: Plot the Ball<br>")) %>%
  tab_stubhead(label = "Player") %>%
  tab_spanner(label = md("— TRUE GAMES PLAYED AT AGE: —"),
              columns = c(x16, x17, x18, x19, x20, x21, x22, x23, x24)) %>%
  fmt_number(columns = c(x16, x17, x18, x19, x20, x21, x22, x23, x24, total), use_seps = TRUE, decimals = 1) %>%
  cols_label(x16 = "16",
             x17 = "17",
             x18 = "18",
             x19 = "19",
             x20 = "20",
             x21 = "21",
             x22 = "22",
             x23 = "23",
             x24 = "24",
             total = "Total") %>%
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
                         cell_fill(color = "#482966"))) %>%
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
  tab_style(locations = cells_body(columns = c(total)),
            list(cell_borders(sides = c("left"),
                         weight = px(2.5),
                         color = ptb_light_grey))) %>%
  tab_style(locations = cells_body(columns = c(x17, x18, x19, x20, x21, x22, x23, x24)),
            list(cell_borders(sides = c("left"),
                              weight = px(1.5),
                              color = ptb_light_grey_v4))) %>%
  tab_style(locations =  list(cells_stub(rows = player == "\n\t\tLuka Dončić\n\t\t\t")),
            style = list(cell_text(color = "#CC9C33"),
                         cell_fill(color = "#CC9C3310"))) %>%
  tab_style(locations =  list(cells_body(rows = player == "\n\t\tLuka Dončić\n\t\t\t")),
            style = list(cell_text(color = "#CC9C33"),
                         cell_fill(color = "#CC9C3310"))) %>%
  tab_style(locations = cells_source_notes(),
            style = list(cell_text(align = "right",
                                   color = ptb_mid_grey,
                                   size = "medium"))) %>%
  tab_style(locations =  list(cells_body(rows = c("\n\t\tJayson Tatum\n\t\t\t",
                                                  "\n\t\tGiannis A.\n\t\t\t",
                                                  "\n\t\tNikola Jokić\n\t\t\t",
                                                  "\n\t\tShai Gilgeous-A.\n\t\t\t"),
                                         columns = c("x16", "x17", "x18"))),
            style = list(cell_fill(color = ptb_light_grey_v4))) %>%
  tab_style(locations =  list(cells_body(rows = c("\n\t\tNikola Jokić\n\t\t\t",
                                                  "\n\t\tShai Gilgeous-A.\n\t\t\t"),
                                         columns = c("x19"))),
            style = list(cell_fill(color = ptb_light_grey_v4))) %>%
  tab_style(locations =  list(cells_body(rows = c("\n\t\tLuka Dončić\n\t\t\t"),
                                         columns = c("x16", "x17", "x18"))),
            style = list(cell_text(weight = "bold",
                                   color = "#B3882D"))) %>%
  tab_style(locations =  list(cells_body(rows = c("\n\t\tJayson Tatum\n\t\t\t"),
                                         columns = c("x19", "x20", "x21", "x23", "x24", "total"))),
            style = list(cell_text(weight = "bold",
                                   color = ptb_mid_grey_v2))) %>%
  tab_style(locations =  list(cells_body(rows = c("\n\t\tGiannis A.\n\t\t\t"),
                                         columns = c("x22"))),
            style = list(cell_text(weight = "bold",
                                   color = ptb_mid_grey_v2)))

tbl_formatted
