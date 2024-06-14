library(tidyverse)
library(worldfootballR)
library(janitor)
library(ggtext)
library(scales)
library(gt)
library(gtExtras)

shooting_data <- fb_big5_advanced_season_stats(season_end_year = 2024,
                                               stat_type = "shooting",
                                               team_or_player = "player") %>%
  clean_names()

passing_data <- fb_big5_advanced_season_stats(season_end_year = 2024,
                                              stat_type = "passing",
                                              team_or_player = "player") %>%
  clean_names()

possession_data <- fb_big5_advanced_season_stats(season_end_year = 2024,
                                                 stat_type = "possession",
                                                 team_or_player = "player") %>%
  clean_names()

playing_time_data <- fb_big5_advanced_season_stats(season_end_year = 2024,
                                                   stat_type = "playing_time",
                                                   team_or_player = "player") %>%
  clean_names()

playing_time_filtered <- playing_time_data %>%
  group_by(player, url, pos) %>%
  summarise(total_mp = sum(min_playing_time)) %>%
  filter(!is.na(total_mp))

big5_fw <- playing_time_filtered %>%
  filter(pos == "FW" | pos == "FW,MF" | pos == "FW,DF") %>%
  select(-3, -4) %>%
  unique()

playing_time_fw <- playing_time_data %>%
  filter(url %in% big5_fw$url)

pt_grouped_fw <- playing_time_fw %>%
  group_by(player, url) %>%
  summarise(total_mp = sum(min_playing_time))

passing_filtered_fw <- passing_data %>%
  select(2, 3, 4, 5, 25, 26, 32, 33) %>%
  filter(url %in% big5_fw$url)

passing_grouped_fw <- passing_filtered_fw %>%
  group_by(player, url) %>%
  summarise(total_xag = sum(x_ag),
            total_xa = sum(x_a_expected),
            total_prog_passes = sum(prg_p))

shooting_filtered_fw <- shooting_data %>%
  select(2, 3, 4, 5, 23, 27) %>%
  filter(url %in% big5_fw$url)

shooting_grouped_fw <- shooting_filtered_fw %>%
  group_by(player, url) %>%
  summarise(total_npxg = sum(npx_g_expected))

possession_filtered_fw <- possession_data %>%
  select(2, 3, 4, 5, 25, 31, 32) %>%
  filter(url %in% big5_fw$url)

possession_grouped_fw <- possession_filtered_fw %>%
  group_by(player, url) %>%
  summarise(total_prog_carries = sum(prg_c_carries),
            total_prog_receptions = sum(prg_r_receiving))

big5_fw_final <- left_join(pt_grouped_fw, passing_grouped_fw, by = c("player", "url")) %>%
  left_join(., shooting_grouped_fw, by = c("player", "url")) %>%
  left_join(., possession_grouped_fw, by = c("player", "url"))

big5_fw_filtered <- big5_fw_final %>%
  mutate(prog_p_r_c = total_prog_passes + total_prog_receptions + total_prog_carries,
         npxg_xag = total_npxg + total_xag,
         prog_p_r_c_p90 = prog_p_r_c / total_mp * 90,
         npxg_xag_p90 = npxg_xag / total_mp * 90) %>%
  filter(total_mp >= 2000)

spain_gold <- "#FFB619"
england_red <- "#D92B40"
france_blue <- "#17548b"
germany_pink <- "#BD428E"
portugal_green <- "#0a4030"
background_data <- "#C9C9C9"

fw_plot_rough <- ggplot(big5_fw_filtered, aes(x = prog_p_r_c_p90, y = npxg_xag_p90)) +
  geom_point(color = case_when(big5_fw_filtered$player == "Lamine Yamal" ~  spain_gold,
                               big5_fw_filtered$player == "Bukayo Saka" ~ england_red,
                               big5_fw_filtered$player == "Kylian Mbappé" ~ france_blue,
                               big5_fw_filtered$player == "Leroy Sané" ~ germany_pink,
                               big5_fw_filtered$player == "Rafael Leão" ~ portugal_green,
                               TRUE ~ background_data),
             alpha = case_when(big5_fw_filtered$player == "Lamine Yamal" ~  1,
                               big5_fw_filtered$player == "Bukayo Saka" ~ 1,
                               big5_fw_filtered$player == "Kylian Mbappé" ~ 1,
                               big5_fw_filtered$player == "Leroy Sané" ~ 1,
                               big5_fw_filtered$player == "Rafael Leão" ~ 1,
                               TRUE ~ 0.35),
             size = 5) +
  scale_x_continuous(limits = c(0, 31),
                     breaks = seq(0, 30, 10),
                     expand = c(0,0)) +
  scale_y_continuous(limits = c(0, 1.24),
                     breaks = seq(0, 1, 0.25),
                     expand = c(0,0))
fw_plot_rough

fw_plot_themed <- fw_plot_rough +
  theme_ptb() +
  theme(axis.title = element_blank()) +
  annotate(geom = "text",
           x = 0.5,
           y = 1.23,
           label = "Non-penalty xG\nand xAG per 90 mins",
           family = ptb_font,
           colour = ptb_dark_grey,
           fontface = "bold",
           hjust = 0,
           vjust = 1) +
  annotate(geom = "text",
           x = 29.5,
           y = 0.22,
           label = "Progressive carries, passes\nand receptions per 90 mins",
           family = ptb_font,
           colour = ptb_dark_grey,
           fontface = "bold",
           hjust = 1,
           vjust = 1)
fw_plot_themed

fw_plot_labelled <- fw_plot_themed +
  labs(title = "<b style='color:#17548b'>Mbappé</b> and <b style='color:#BD428E'>Sané</b> create more shots<br>than other elite ball progressors",
       subtitle = "Forwards' contribution to chance <b>creation</b> and ball<br><b>progression</b> in the 'Big Five' leagues in 2023-24",
       caption = "<i>Players with <2,000 total minutes excluded</i><br><br>Data: FBref | Chart: Plot the Ball")
fw_plot_labelled

wide_forward_shortlist <- big5_fw_filtered %>%
  filter(prog_p_r_c_p90 >= 20)

shooting_data_extended <- fb_big5_advanced_season_stats(season_end_year = seq(2018, 2024, 1),
                                               stat_type = "shooting",
                                               team_or_player = "player") %>%
  clean_names()

wide_forward_shooting <- shooting_data_extended %>%
  filter(url %in% wide_forward_shortlist$url) %>%
  group_by(player, url) %>%
  summarise(total_np_shots = sum(sh_standard) - sum(p_katt_standard),
            total_npxg = sum(npx_g_expected),
            total_npg = sum(gls_standard) - sum(pk_standard),
            npg_less_npxg = total_npg - total_npxg) %>%
  arrange(desc(npg_less_npxg)) %>%
  select(-url)

bar_colour <- "#919997"
header_colour <- "#494D4B"
highlight_mbappe <- str_c(france_blue, "20")
highlight_sane <- str_c(germany_pink, "20")
highlight_saka <- str_c(england_red, "20")
highlight_yamal <- str_c(spain_gold, "20")
highlight_leao <- str_c(portugal_green, "20")

display_tbl <- wide_forward_shooting %>%
  gt(rowname_col = "player",
     groupname_col = NA)
display_tbl

display_tbl <- display_tbl %>%
  gt_plt_bar(column = npg_less_npxg, keep_column = TRUE, width = 30, color = bar_colour)
display_tbl

tbl_formatted <- display_tbl %>%
  tab_header(title = html("<b><b style='color:#17548b'>Mbappé</b>'s finishing also sets him<br>apart from other wide forwards</b>"),
             subtitle = html("Career* <b>finishing performance</b> of forwards with:<br><br>- at least 20 progressive actions per 90, and<br>- at least 2,000 total minutes played<br><br> in a 'Big Five' league during the 2023-24 season<br><br>")) %>%
  tab_source_note(source_note = html("<br><i>* Data available for 'Big Five' leagues since the 2017-18 season</i><br><br>Data: FBref | Table: Plot the Ball<br>")) %>%
  tab_stubhead(label = "Player") %>%
  tab_spanner(label = md("— NON-PENALTY: —"),
              columns = c(total_np_shots, total_npxg, total_npg)) %>%
  fmt_number(columns = c(total_np_shots, total_npg), decimals = 0) %>%
  fmt_number(columns = c(total_npxg), decimals = 1) %>%
  fmt_number(columns = c(npg_less_npxg), decimals = 1, force_sign = TRUE) %>%
  cols_label(total_np_shots = "Shots",
             total_npxg = "xG",
             total_npg = "Goals",
             npg_less_npxg = "+/-",
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
                         cell_fill(color = header_colour))) %>%
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
  tab_style(locations =  list(cells_stub(rows = player == "Kylian Mbappé")),
            style = list(cell_text(weight = "bold",
                                   color = france_blue),
                         cell_fill(color = highlight_mbappe),
                         cell_borders(sides = c("bottom", "right"),
                                      weight = px(1.5),
                                      color = ptb_mid_grey))) %>%
  tab_style(locations =  list(cells_body(rows = player == "Kylian Mbappé")),
            style = list(cell_fill(color = highlight_mbappe),
                         cell_borders(sides = c("bottom"),
                                      weight = px(1.5),
                                      color = ptb_mid_grey))) %>%
  tab_style(locations =  list(cells_stub(rows = player == "Rafael Leão")),
            style = list(cell_text(weight = "bold",
                                   color = portugal_green),
                         cell_fill(color = highlight_leao),
                         cell_borders(sides = c("top", "right"),
                                      weight = px(1.5),
                                      color = ptb_mid_grey))) %>%
  tab_style(locations =  list(cells_body(rows = player == "Rafael Leão")),
            style = list(cell_fill(color = highlight_leao),
                         cell_borders(sides = c("top"),
                                      weight = px(1.5),
                                      color = ptb_mid_grey))) %>%
  tab_style(locations =  list(cells_stub(rows = player == "Leroy Sané")),
            style = list(cell_text(weight = "bold",
                                   color = germany_pink),
                         cell_fill(color = highlight_sane),
                         cell_borders(sides = c("top", "bottom", "right"),
                                      weight = px(1.5),
                                      color = ptb_mid_grey))) %>%
  tab_style(locations =  list(cells_body(rows = player == "Leroy Sané")),
            style = list(cell_fill(color = highlight_sane),
                         cell_borders(sides = c("top", "bottom"),
                                      weight = px(1.5),
                                      color = ptb_mid_grey))) %>%
  tab_style(locations =  list(cells_stub(rows = player == "Bukayo Saka")),
            style = list(cell_text(weight = "bold",
                                   color = england_red),
                         cell_fill(color = highlight_saka),
                         cell_borders(sides = c("bottom", "right"),
                                      weight = px(1.5),
                                      color = ptb_mid_grey))) %>%
  tab_style(locations =  list(cells_body(rows = player == "Bukayo Saka")),
            style = list(cell_fill(color = highlight_saka),
                         cell_borders(sides = c("bottom"),
                                      weight = px(1.5),
                                      color = ptb_mid_grey))) %>%
  tab_style(locations =  list(cells_stub(rows = player == "Lamine Yamal")),
            style = list(cell_text(weight = "bold",
                                   color = spain_gold),
                         cell_fill(color = highlight_yamal),
                         cell_borders(sides = c("top", "right"),
                                      weight = px(1.5),
                                      color = ptb_mid_grey))) %>%
  tab_style(locations =  list(cells_body(rows = player == "Lamine Yamal")),
            style = list(cell_fill(color = highlight_yamal),
                         cell_borders(sides = c("top"),
                                      weight = px(1.5),
                                      color = ptb_mid_grey))) %>%
  tab_style(locations = list(cells_body(columns = npg_less_npxg)),
            style = list(cell_text(weight = "bold",
                                   color = bar_colour))) %>%
  tab_style(locations = list(cells_body(columns = c(total_np_shots, total_npxg))),
            style = list(cell_text(color = ptb_mid_grey)))
tbl_formatted
