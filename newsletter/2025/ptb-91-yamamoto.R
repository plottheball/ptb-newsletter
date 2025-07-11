library(tidyverse)
library(janitor)
library(gt)
library(gtUtils)
library(gtExtras)

arsenal_stats <- read.csv("pitch-arsenal-stats.csv") %>%
  clean_names()

arsenal_clean <- arsenal_stats %>%
  select(1:9, 14, 20) %>%
  separate(last_name_first_name,
           sep = "[,]",
           into = c("last_name", "first_name"),
           remove = FALSE) %>%
  mutate(name = str_c(str_squish(first_name), " ", str_squish(last_name)),
         .before = last_name)

### https://www.baseball-reference.com/bio/Japan_born.shtml
jp_list <- c("Maeda, Kenta", "Darvish, Yu", "Senga, Kodai", "Kikuchi, Yusei",
          "Imanaga, Shota", "Yamamoto, Yoshinobu", "Ohtani, Shohei", "Sasaki, Roki",
          "Matsui, Yuki", "Sugano, Tomoyuki", "Ogasawara, Shinnosuke")

split_usage_jp <- arsenal_clean %>%
  select(1, 2, 8, 11) %>%
  filter(last_name_first_name %in% jp_list) %>%
  group_by(pitch_name) %>%
  summarise(tot_pitches = sum(pitches))

split_usage_non_jp <- arsenal_clean %>%
  select(1, 2, 8, 11) %>%
  filter(!last_name_first_name %in% jp_list) %>%
  group_by(pitch_name) %>%
  summarise(tot_pitches = sum(pitches))

arsenal_jp <- arsenal_clean %>%
  filter(last_name_first_name %in% jp_list)

total_pitches <- arsenal_jp %>%
  group_by(name) %>%
  summarise(tot_pitches = sum(pitches)) %>%
  arrange(desc(tot_pitches))

jp_list_filtered <- total_pitches %>%
  filter(tot_pitches > 500) %>%
  select(name) %>%
  unlist()

arsenal_jp_filtered <- arsenal_clean %>%
  filter(name %in% jp_list_filtered)

fb_jp <- arsenal_jp_filtered %>%
  filter(pitch_name == "4-Seam Fastball") %>%
  select(2, 12, 13)

split_jp <- arsenal_jp_filtered %>%
  filter(pitch_name == "Split-Finger") %>%
  select(2, 12, 13)

top_split <- arsenal_clean %>%
  select(name, pitch_name, pitches, whiff_percent) %>%
  filter(pitch_name == "Split-Finger") %>%
  arrange(desc(pitches)) %>%
  filter(pitches > 300)

split_bbe_stats <- read.csv("batted-ball.csv") %>%
  clean_names() %>%
  separate(name,
           sep = "[,]",
           into = c("last_name", "first_name"),
           remove = FALSE) %>%
  mutate(full_name = str_c(str_squish(first_name), " ", str_squish(last_name)),
         .before = last_name) %>%
  select(full_name, bbe, gb_rate)

splitters_final <- left_join(x = top_split, y = split_bbe_stats, by = c("name" = "full_name")) %>%
  filter(!is.na(bbe)) %>%
  select(-pitch_name) %>%
  arrange(desc(gb_rate)) %>%
  mutate(gb_rate = gb_rate * 100)

display_table <- splitters_final %>%
  gt(rowname_col = "name") %>%
  cols_width(name ~ px(200),
             pitches ~ px(105),
             whiff_percent ~ px(85),
             bbe ~ px(105),
             gb_rate ~ px(85))
display_table

header_colour <- "#79807E"
text_focus <- "#005A9C"
fill_focus <- str_c(text_focus, "10")
other_focus <- "#B36B7C"
other_fill <- str_c(other_focus, "05")

tbl_formatted <- display_table %>%
  tab_header(title = html("MLB hitters find it difficult to make good contact with <b style='color:#005A9C'>Yamamoto</b>'s splitter"),
             subtitle = html("Performance of pitchers who have thrown more than 300 split-finger fastballs during the 2025 MLB season<br><br>")) %>%
  tab_source_note(source_note = html("<br>'Whiff' = swing and miss | 'GB' = ground ball<br><br>Data: MLB Statcast | Table: Plot the Ball<br>")) %>%
  tab_stubhead(label = "") %>%
  fmt_number(columns = c(pitches, bbe), use_seps = TRUE, decimals = 0, force_sign = FALSE) %>%
  fmt_percent(columns = c(whiff_percent, gb_rate), decimals = 1, force_sign = FALSE, scale_values = FALSE) %>%
  cols_label(pitches = "Splitters",
             whiff_percent = "Whiff %",
             bbe = "Hit in play",
             gb_rate = "▼ GB %") %>%
  cols_align(align = "right",
             columns = everything()) %>%
  cols_align(align = "left",
             columns = c(name)) %>%
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
  tab_style(locations = cells_body(columns = c(pitches, bbe)),
            style = list(cell_borders(sides = c("left"),
                                      weight = px(2),
                                      color = ptb_light_grey))) %>%
  tab_style(locations = cells_body(columns = c(whiff_percent, gb_rate)),
            style = list(cell_borders(sides = c("left"),
                                      weight = px(1),
                                      color = ptb_light_grey))) %>%
  tab_style(locations = cells_stub(rows = name == "Yoshinobu Yamamoto"),
            style = list(cell_text(color = text_focus,
                                   weight = "bold"),
                         cell_fill(color = fill_focus))) %>%
  tab_style(locations = cells_body(rows = name == "Yoshinobu Yamamoto"),
            style = list(cell_text(color = text_focus,
                                   weight = "bold"),
                         cell_fill(color = fill_focus))) %>%
  tab_style(locations = cells_stub(rows = name == "Tomoyuki Sugano"),
            style = list(cell_text(color = other_focus),
                         cell_fill(color = other_fill))) %>%
  tab_style(locations = cells_body(rows = name == "Tomoyuki Sugano"),
            style = list(cell_text(color = other_focus),
                         cell_fill(color = other_fill))) %>%
  tab_style(locations = cells_stub(rows = name == "Shota Imanaga"),
            style = list(cell_text(color = other_focus),
                         cell_fill(color = other_fill))) %>%
  tab_style(locations = cells_body(rows = name == "Shota Imanaga"),
            style = list(cell_text(color = other_focus),
                         cell_fill(color = other_fill)))
tbl_formatted