library(googlesheets4)
library(tidyverse)
library(janitor)
library(gt)
library(gtExtras)
library(gtUtils)

link <- "https://docs.google.com/spreadsheets/d/1A02PO63HCqLEXCTRohYbNgU2HW6CPI85bvj8YLyZS9M/edit?usp=sharing"

table_attack <- read_sheet(ss = link, sheet = "Table - attack") %>%
  arrange(desc(Rating)) %>%
  select(-Rank)

display_tbl_attack <- table_attack %>%
  gt(rowname_col = "Team") %>%
  sub_missing(missing_text = "") %>%
  cols_width(Team ~ px(115),
             Rating ~ px(75),
             everything() ~ px(41))
display_tbl_attack

range_min <- -25
range_max <- -range_min
home_adv <- 2

world_rugby_blue <- "#061C3B"
text_positive_min <- "#8CBBAE"
text_positive <- "#096F52"
text_positive_max <- "#053B2B"
fill_positive <- ptb_green
text_negative_min <- "#C897A7"
text_negative <- "#8A2243"
text_negative_max <- "#491223"
fill_negative <- ptb_dark_pink
text_mid <- ptb_mid_grey
fill_mid <- "#F2F2F2"
fill_other <- "#FCFCFC"

tbl_formatted_attack <- display_tbl_attack %>%
  tab_header(title = html("<span style='color:#346099'>France</span> score around five more points per game than the average 'Tier 1' men's side"),
             subtitle = html("<b>'Attack rating'</b> of each Tier 1 nation in men's rugby union, expressed in points per game <b>better or worse than average</b><br><br>")) %>%
  tab_source_note(source_note = html("<br><i>Data as at 1 Dec 2024</i><br><br>'Attack' reflects a team's four most recent results against each other nation, adjusted for home advantage; more weight is given to more recent games<br><br><br>Data: Plot the Ball | Table: Plot the Ball<br>")) %>%
  tab_stubhead(label = "Team") %>%
  tab_spanner(label = md("— ADJUSTED POINTS SCORED VS. —"),
              ### MANUALLY RE-ORDER COLUMNS TO MATCH RANKING
              columns = c(ITA, WLS, ENG, AUS, SCT, ARG, IRL, FRA, ZAF, NZL)) %>%
  fmt_number(columns = c(Rating, ARG, AUS, ENG, FRA, IRL, ITA, NZL, SCT, ZAF, WLS),
             force_sign = TRUE,
             decimals = 1) %>%
  cols_align(align = "center",
             columns = everything()) %>%
  opt_table_font(font = ptb_font) %>%
  tab_options(table.border.top.width = px(0),
              table.border.bottom.width = px(0),
              heading.border.bottom.width = px(0),
              column_labels.border.bottom.width = px(0),
              column_labels.border.top.width = px(0),
              table_body.border.top.width = px(0),
              table_body.border.bottom.width = px(0),
              stub.border.width = px(2.5),
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
                                   size = "medium"),
                         cell_fill(color = world_rugby_blue))) %>%
  tab_style(locations = cells_stub(rows = TRUE),
            style =  list(cell_text(align = "left",
                                    color = ptb_mid_grey,
                                    size = "medium"),
                          cell_borders(sides = c("top", "bottom"),
                                       weight = px(1),
                                       color = ptb_light_grey))) %>%
  tab_style(locations = cells_body(rows = TRUE),
            style = list(cell_text(color = ptb_dark_grey,
                                   size = "medium",
                                   align = "center"),
                         cell_borders(sides = c("top", "bottom"),
                                      weight = px(1),
                                      color = ptb_light_grey))) %>%
  tab_style(locations = cells_body(columns = everything()),
            style = list(cell_text(size = "small"),
                         cell_borders(sides = c("right"),
                                      weight = px(0.5),
                                      color = ptb_light_grey),
                         cell_fill(color = fill_other))) %>%
  tab_style(locations = cells_body(columns = c(Rating)),
            style = list(cell_text(size = "medium",
                                   weight = "bold"),
                         cell_borders(sides = c("right"),
                                      weight = px(2.5),
                                      color = ptb_light_grey))) %>%
  tab_style(locations = cells_source_notes(),
            style = list(cell_text(align = "right",
                                   color = ptb_mid_grey,
                                   size = "medium"))) %>%
  tab_style(locations = cells_body(columns = c(Rating),
                                   rows = Rating > 0),
            style = cell_text(color = text_positive,
                              size = "medium")) %>%
  tab_style(locations = cells_body(columns = c(Rating),
                                   rows = Rating < 0),
            style = cell_text(color = text_negative,
                              size = "medium")) %>%
  ###
  tab_style(locations = cells_body(columns = c(ZAF),
                                   rows = ZAF > 0),
            style = cell_text(color = text_positive)) %>%
  tab_style(locations = cells_body(columns = c(ZAF),
                                   rows = ZAF <  0),
            style = cell_text(color = text_negative)) %>%
  ###
  tab_style(locations = cells_body(columns = c(NZL),
                                   rows = NZL > 0),
            style = cell_text(color = text_positive)) %>%
  tab_style(locations = cells_body(columns = c(NZL),
                                   rows = NZL < 0),
            style = cell_text(color = text_negative)) %>%
  ###
  tab_style(locations = cells_body(columns = c(IRL),
                                   rows = IRL > 0),
            style = cell_text(color = text_positive)) %>%
  tab_style(locations = cells_body(columns = c(IRL),
                                   rows = IRL < 0),
            style = cell_text(color = text_negative)) %>%
  ###
  tab_style(locations = cells_body(columns = c(FRA),
                                   rows = FRA > 0),
            style = cell_text(color = text_positive)) %>%
  tab_style(locations = cells_body(columns = c(FRA),
                                   rows = FRA < 0),
            style = cell_text(color = text_negative)) %>%
  ###
  tab_style(locations = cells_body(columns = c(SCT),
                                   rows = SCT > 0),
            style = cell_text(color = text_positive)) %>%
  tab_style(locations = cells_body(columns = c(SCT),
                                   rows = SCT < 0),
            style = cell_text(color = text_negative)) %>%
  ###
  tab_style(locations = cells_body(columns = c(ENG),
                                   rows = ENG > 0),
            style = cell_text(color = text_positive)) %>%
  tab_style(locations = cells_body(columns = c(ENG),
                                   rows = ENG < 0),
            style = cell_text(color = text_negative)) %>%
  ###
  tab_style(locations = cells_body(columns = c(ARG),
                                   rows = ARG > 0),
            style = cell_text(color = text_positive)) %>%
  tab_style(locations = cells_body(columns = c(ARG),
                                   rows = ARG < 0),
            style = cell_text(color = text_negative)) %>%
  ###
  tab_style(locations = cells_body(columns = c(AUS),
                                   rows = AUS > 0),
            style = cell_text(color = text_positive)) %>%
  tab_style(locations = cells_body(columns = c(AUS),
                                   rows = AUS < 0),
            style = cell_text(color = text_negative)) %>%
  ###
  tab_style(locations = cells_body(columns = c(WLS),
                                   rows = WLS > 0),
            style = cell_text(color = text_positive)) %>%
  tab_style(locations = cells_body(columns = c(WLS),
                                   rows = WLS < 0),
            style = cell_text(color = text_negative)) %>%
  ###
  tab_style(locations = cells_body(columns = c(ITA),
                                   rows = ITA > 0),
            style = cell_text(color = text_positive)) %>%
  tab_style(locations = cells_body(columns = c(ITA),
                                   rows = ITA < 0),
            style = cell_text(color = text_negative)) %>%
  cols_label(Rating = "Attack",
             ARG = "AR",
             AUS = "AU",
             ENG = "EN",
             FRA = "FR",
             IRL = "IE",
             ITA = "IT",
             NZL = "NZ",
             SCT = "SC",
             ZAF = "ZA",
             WLS = "WA") %>%
  tab_style(locations = cells_stub(rows = "France"),
            style =  list(cell_text(align = "left",
                                    weight = "bold",
                                    color = "#346099",
                                    size = "medium"),
                          cell_fill(color = "#34609925"))) %>%
  tab_style(locations = cells_stub(rows = c("Ireland", "Scotland", "England", "Wales", "Italy")),
            style =  list(cell_text(align = "left",
                                    weight = "bold",
                                    color = ptb_dark_grey,
                                    size = "medium"),
                          cell_fill(color = ptb_light_grey_v4))) %>%
  tab_style(locations = cells_body(rows = c("New Zealand", "South Africa", "Argentina", "Australia")),
            style =  list(cell_text(color = ptb_mid_grey))) %>%
  tab_style(locations = cells_body(rows = c("Ireland", "Scotland", "England", "Wales", "Italy")),
            style =  list(cell_fill(color = ptb_light_grey_v4))) %>%
  tab_style(locations = cells_body(rows = "France"),
            style =  list(cell_fill(color = "#34609925"))) %>%
  data_color(columns = Rating,
             rows = c("France", "Ireland", "Scotland", "England", "Wales", "Italy"),
             autocolor_text = FALSE,
             method = "numeric",
             palette = c(fill_negative, fill_mid, fill_positive),
             domain = c(range_min, range_max)) %>%
  tab_style(locations = cells_body(rows = "France",
                                   columns = "FRA"),
            style =  list(cell_fill(color = fill_other))) %>%
  tab_style(locations = cells_body(rows = "Ireland",
                                   columns = "IRL"),
            style =  list(cell_fill(color = fill_other))) %>%
  tab_style(locations = cells_body(rows = "Scotland",
                                   columns = "SCT"),
            style =  list(cell_fill(color = fill_other))) %>%
  tab_style(locations = cells_body(rows = "England",
                                   columns = "ENG"),
            style =  list(cell_fill(color = fill_other))) %>%
  tab_style(locations = cells_body(rows = "Wales",
                                   columns = "WLS"),
            style =  list(cell_fill(color = fill_other))) %>%
  tab_style(locations = cells_body(rows = "Italy",
                                   columns = "ITA"),
            style =  list(cell_fill(color = fill_other)))
tbl_formatted_attack
