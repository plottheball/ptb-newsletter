library(tidyverse)
library(janitor)
library(rvest)
library(gt)
library(gtExtras)
library(gtUtils)

league_table_url <- "https://www.eurobasket.com/France/basketball-League-LFB.aspx?women=1"

team_urls <- read_html(league_table_url) %>%
  html_nodes("a") %>%
  html_attr("href")

team_urls <- team_urls[grepl("/team/", team_urls)]

team_urls <- str_replace_all(team_urls, fixed("?Women=1"), "/Stats?Women=1")

all_data <- tibble()

for (i in team_urls) {
  
  Sys.sleep(3)
  
  html <- read_html(i)
  
  tables <- html %>%
    html_nodes("table") %>%
    html_table()
  
  table_1 <- tables[[15]] %>%
    clean_names()
  
  table_1 <- table_1 %>%
    select(number, name, g, min, ftm_a) %>%
    separate(col = ftm_a,
             into = c("ftm", "fta")) %>%
    mutate(min_adj = round(min * g),
           ftm = as.integer(ftm),
           fta = as.integer(fta),
           comp = "comp_1",
           team_id = str_remove_all(str_remove_all(i, "https://basketball.eurobasket.com/team/"), "/Stats?Women=1"))
  
  all_data <- bind_rows(all_data, table_1)
  
  if (length(tables) < 17) {
    
    next
    
  }
  
  table_2 <- tables[[18]] %>%
    clean_names()
  
  table_2 <- table_2 %>%
    select(number, name, g, min, ftm_a) %>%
    separate(col = ftm_a,
             into = c("ftm", "fta")) %>%
    mutate(min_adj = round(min * g),
           ftm = as.integer(ftm),
           fta = as.integer(fta),
           comp = "comp_2",
           team_id = str_remove_all(str_remove_all(i, "https://basketball.eurobasket.com/team/"), "/Stats?Women=1"))
  
  all_data <- bind_rows(all_data, table_2)
  
}

player_summary <- all_data %>%
  group_by(team_id, name) %>%
  summarise(tot_gp = sum(g),
            tot_mp = sum(min_adj),
            tot_ftm = sum(ftm),
            tot_fta = sum(fta)) %>%
  arrange(desc(tot_fta)) %>%
  filter(tot_fta >= 50)

over_190 <- tibble(name = c("Malonga, Dominique",
                            "Diarisso, Oumou",
                            "Geiselsoeder L.",
                            "Mendjiadeu, Dulcy",
                            "Milapie, Marie-Michelle",
                            "Akhator, Evelyn",
                            "Diaby, Kariata",
                            "Zodia, Jess-Mine",
                            "Gueye, Aminata",
                            "Peters, Haley"),
                   height = c(1.98,
                              1.90,
                              1.92,
                              1.90,
                              1.92,
                              1.90,
                              1.93,
                              1.90,
                              1.93,
                              1.90),
                   dob = c(dmy("16-11-2005"),
                           dmy("01-05-2003"),
                           dmy("10-02-2000"),
                           dmy("26-07-1999"),
                           dmy("06-02-1996"),
                           dmy("03-02-1995"),
                           dmy("29-06-1995"),
                           dmy("22-12-2004"),
                           dmy("10-07-2002"),
                           dmy("17-09-1992"))) %>%
  mutate(current_age = as.integer(today() - dob) / 365.25,
         .before = "height")

final_table <- left_join(over_190, player_summary, by = "name") %>%
  select(-dob, -team_id) %>%
  mutate(fta_per_36 = tot_fta / tot_mp * 36,
         ft_percent = tot_ftm / tot_fta * 100) %>%
  arrange(desc(ft_percent)) %>%
  select(name, current_age, height, tot_gp, fta_per_36, ft_percent) %>%
  separate(name,
           into = c("last_name", "first_name"),
           sep = ", ") %>%
  mutate(final_name = str_c(first_name, " ", last_name),
         .before = last_name) %>%
  select(-last_name, -first_name, -tot_gp)

final_table$final_name <- replace_na(final_table$final_name, "Luisa Geiselsöder")

display_tbl <- final_table %>%
  gt(rowname_col = "final_name") %>%
  cols_width(final_name ~ px(200),
             current_age ~ px(90),
             height ~ px(110),
             fta_per_36 ~ px(100),
             ft_percent ~ px(100))
display_tbl

header_colour <- "#79807E"
asvel_colour <- "#CC6688"
highlight_asvel <- str_c(asvel_colour, "10")

tbl_formatted <- display_tbl %>%
  tab_header(title = html("<b style='color:#CC6688'>Malonga</b> is a weaker free-throw shooter than most other tall LFB players"),
             subtitle = html("<b>Free-throw success rate</b> of LFB players listed at <b>1.90m or taller</b> in domestic and continental competition during 2024-25<br><br>")) %>%
  tab_source_note(source_note = html("<br>'LFB' = Ligue Féminine de Basketball | 'FTA/36' = FT attempts per 36 minutes<br><br><i>Players with <50 free throws attempted across all competitions excluded</i><br><br>Data: Eurobasket.com | Table: Plot the Ball<br>")) %>%
  tab_stubhead(label = "") %>%
  fmt_number(columns = c(height), use_seps = TRUE, decimals = 2) %>%
  fmt_number(columns = c(current_age, fta_per_36), use_seps = TRUE, decimals = 1) %>%
  fmt_percent(columns = c(ft_percent), force_sign = FALSE, use_seps = TRUE, decimals = 1, scale_values = FALSE) %>%
  cols_label(current_age = "Age (yrs)",
             height = "Height (m)",
             fta_per_36 = "FTA/36",
             ft_percent = "▼ FT %") %>%
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
  tab_style(locations = cells_body(columns = c(fta_per_36)),
            style = list(cell_borders(sides = c("left"),
                                      weight = px(2),
                                      color = ptb_light_grey))) %>%
  tab_style(locations = cells_stub(rows = final_name == "Dominique Malonga"),
            style = list(cell_text(color = asvel_colour,
                                   weight = "bold"),
                         cell_fill(color = highlight_asvel))) %>%
  tab_style(locations = cells_body(rows = final_name == "Dominique Malonga"),
            style = list(cell_text(color = asvel_colour,
                                   weight = "bold"),
                         cell_fill(color = highlight_asvel)))
tbl_formatted
