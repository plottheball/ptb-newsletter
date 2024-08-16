library(tidyverse)
library(googlesheets4)
library(janitor)
library(ggtext)
library(scales)
library(gt)
library(gtExtras)

# Data imported via Google Sheets from https://olympics.com/en/paris-2024/medals
alt_medals <- read_sheet(ss = "https://docs.google.com/spreadsheets/d/1oqLsq5dokU6hyoxt2k3Ol1ISYL5TG14E6uIussCbyo0/edit?usp=sharing",
                         sheet = "Data") %>%
  clean_names()

sport_summary <- alt_medals %>%
  group_by(sport) %>%
  summarise(available_golds = sum(gold_count))

alt_medals_adj <- left_join(x = alt_medals,
                              y = sport_summary,
                              by = c("sport" = "sport")) %>%
  mutate(adj_gold = gold_count / available_golds * 2,
         adj_silver = silver_count / available_golds * 2,
         adj_bronze = bronze_count / available_golds * 2)

alt_medal_table <- alt_medals_adj %>%
  group_by(country) %>%
  summarise(tot_adj_gold = sum(adj_gold),
            tot_adj_silver = sum(adj_silver),
            tot_adj_bronze = sum(adj_bronze)) %>%
  mutate(no_total = tot_adj_gold + tot_adj_silver + tot_adj_bronze,
         gold_weighted = tot_adj_gold * 3,
         silver_weighted = tot_adj_silver * 2,
         bronze_weighted = tot_adj_bronze * 1,
         weighted_score = gold_weighted + silver_weighted + bronze_weighted) %>%
  arrange(desc(weighted_score))

final_table <- alt_medal_table %>%
  select(1, 2, 3, 4, 9) %>%
  head(10)

bar_colour <- "#0a868c"
header_colour <- "#025b6d"
highlight_host <- str_c(header_colour, "10")

display_tbl <- final_table %>%
  gt(rowname_col = "country",
     groupname_col = NA)
display_tbl

display_tbl <- display_tbl %>%
  gt_plt_bar(column = weighted_score, keep_column = TRUE, width = 30, color = bar_colour)
display_tbl

tbl_formatted <- display_tbl %>%
  tab_header(title = html("<b>What would the Paris 2024 table look<br>like if the only events were ball sports?</b>"),
             subtitle = html("Weighted adjusted number of medals won by countries<br>competing in <b>ball sports</b> at the <b>2024 Summer Olympic Games</b><br><br>")) %>%
  tab_source_note(source_note = html("<br><i>Number of gold medals available in each sport equalised at 2;<br>3x weighting applied to gold medals, 2x to silver and 1x to bronze</i><br><br>Data: International Olympic Committee | Table: Plot the Ball<br>")) %>%
  tab_stubhead(label = "Country") %>%
  tab_spanner(label = md("— ADJUSTED MEDALS —"),
              columns = c(tot_adj_gold, tot_adj_silver, tot_adj_bronze)) %>%
  fmt_number(columns = c(tot_adj_gold, tot_adj_silver, tot_adj_bronze, weighted_score), decimals = 1) %>%
  cols_label(tot_adj_gold = "Gold",
             tot_adj_silver = "Silver",
             tot_adj_bronze = "Bronze",
             weighted_score = "Weighted",
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
  tab_style(locations =  list(cells_stub(rows = country == "France")),
            style = list(cell_text(weight = "bold",
                                   color = bar_colour),
                         cell_fill(color = highlight_host))) %>%
  tab_style(locations =  list(cells_body(rows = country == "France")),
            style = list(cell_text(color = header_colour),
                         cell_fill(color = highlight_host))) %>%
  tab_style(locations = list(cells_body(columns = weighted_score)),
            style = list(cell_text(weight = "bold",
                                   color = bar_colour),
                         cell_borders(sides = c("left"),
                                      weight = px(2),
                                      color = ptb_light_grey))) %>%
  tab_style(locations = list(cells_body(columns = c(tot_adj_gold, tot_adj_silver, tot_adj_bronze))),
            style = list(cell_text(color = ptb_mid_grey)))
tbl_formatted
