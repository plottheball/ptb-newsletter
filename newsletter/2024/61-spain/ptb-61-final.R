library(tidyverse)
library(janitor)
library(rvest)
library(RSelenium)
library(xml2)
library(ggtext)
library(googlesheets4)
library(scales)

base_url <- "https://www.fifa.com/fifa-world-ranking/women?dateId=ranking_"

dates <- c("20230609", "20230324",
           "20221209", "20221013", "20220805", "20220617", "20220325",
           "20211210", "20210820", "20210625", "20210416",
           "20201218", "20200814", "20200626", "20200327",
           "20191213", "20190927", "20190712", "20190329", 
           "20181207", "20180928", "20180622", "20180323",
           "20171215", "20170901", "20170623", "20170324",
           "20161223", "20160826", "20160624", "20160325",
           "20151218", "20150925", "20150710", "20150327",
           "20141219")

rD <- rsDriver(browser = "firefox", port = 4445L)
remDr <- rD[["client"]]
remDr$open()

all_rankings <- tibble()

for (i in dates) {
  
  print(paste0("Getting rankings for: ", i))
  
  url_latest <- str_c(base_url, i)
  
  remDr$navigate(url_latest)
  
  Sys.sleep(2)
  
  html <- remDr$getPageSource()
  html <- unlist(html)
  
  html_clean <- read_html(html)
  
  all_tables <- html_clean %>%
    html_nodes("table")
  
  ranking_table <- all_tables[[1]] %>%
    html_table() %>%
    clean_names() %>%
    select(1, 3, 4) %>%
    mutate(date = i,
           .before = rk)
  
  all_rankings <- rbind(all_rankings, ranking_table)
  
}

rankings_tidy <- all_rankings

### CODE ABOVE RUN FOR PREVIOUS EDITION OF NEWSLETTER; LATER DATA ADDED MANUALLY (SCRAPE ISSUES) AND IMPORTED VIA GOOGLE SHEETS
data_update <- read_sheet(ss = "https://docs.google.com/spreadsheets/d/1Vv9gshyxJl97xqFfKDY2pbHFPsfVcjHihriy1Rt6-gI/edit?gid=0#gid=0",
                          sheet = "filtered") %>%
  clean_names()

rankings_final <- bind_rows(rankings_tidy, data_update)

rankings_final$rk <- as.integer(rankings_final$rk)
rankings_final$total_points_pts <- as.numeric(rankings_final$total_points_pts)
rankings_final$date <- ymd(rankings_final$date)

current_top20 <- rankings_final %>%
  filter(date == "2024-06-14" & rk < 21)

current_top20 <- unique(current_top20$team)

rankings_filtered <- rankings_final %>%
  filter(team %in% current_top20) %>%
  arrange(desc(date))

focus_colour_esp <- "#E6173D"
background_colour <- "#C9C9C960"

rough_line <- ggplot(rankings_filtered, aes(x = date, y = total_points_pts, colour = team)) +
  geom_line(linewidth = 1.5) +
  scale_y_continuous(limits = c(1525, 2275),
                     breaks = seq(1600, 2200, 200)) +
  scale_x_date(expand = c(0.1, 0)) +
  scale_color_manual(values = c("USAUSA"= background_colour,
                                "GermanyGER"= background_colour,
                                "SwedenSWE"= background_colour,
                                "EnglandENG"= background_colour,
                                "FranceFRA"= background_colour,
                                "SpainESP"= focus_colour_esp,
                                "CanadaCAN"= background_colour,
                                "BrazilBRA"= background_colour,
                                "NetherlandsNED"= background_colour,
                                "AustraliaAUS"= background_colour,
                                "JapanJPN"= background_colour,
                                "NorwayNOR"= background_colour,
                                "DenmarkDEN"= background_colour,
                                "China PRCHN"= background_colour,
                                "IcelandISL"= background_colour,
                                "ItalyITA"= background_colour,
                                "Korea RepublicKOR"= background_colour,
                                "Korea DPRPRK"= background_colour,
                                "AustriaAUT"= background_colour,       
                                "BelgiumBEL"= background_colour,
                                "SwitzerlandSUI" = background_colour)) +
  theme(legend.position = "none",
        axis.title.x = element_blank(),
        axis.line.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.title.y = element_blank(),
        axis.line.y = element_blank())
rough_line

themed_line <- rough_line +
  theme_ptb()
themed_line

labelled_line <- themed_line +
  labs(title = "<b style='color:#E6173D'>Spain</b> have shot up the FIFA rankings",
       subtitle = "Change in <b>rating points</b> of the current top 20<br>women's international football teams since 2015",
       caption = "<i>Data as at 14 Jun 2024</i><br><br>Data: FIFA | Chart: Plot the Ball")
labelled_line

# data imported via Google Sheets from rsssf.org
# U17 WC: https://www.rsssf.org/tablesw/wc-wom-u17.html
# U20 WC: https://www.rsssf.org/tablesw/wc-wom-u19.html
# U17 EUR: https://www.rsssf.org/tablese/eur-women-u17.html
# U19 EUR: https://www.rsssf.org/tablese/eur-women-u18.html (NOT INCLUDING ONGOING MATCHES)
age_group_results <- read_sheet(ss = "https://docs.google.com/spreadsheets/d/1CgD8sqxn3MLEASnCPbb4mcuOdTZCDC42O8M9M4xInvU/edit?gid=1955711446#gid=1955711446",
                                sheet = "All-time") %>%
  clean_names()

age_group_summary <- age_group_results %>%
  group_by(country) %>%
  summarise(tot_gp = sum(g),
            tot_w = sum(w),
            tot_gf = sum(g_for),
            tot_ga = sum(g_against)) %>%
  mutate(win_share = tot_w / tot_gp,
         goal_share = tot_gf / (tot_gf + tot_ga),
         category = if_else(country == "Spain", "Focus", "Other")) %>%
  filter(tot_gp >= 50)

palette_category <- c(focus_colour_esp, background_colour)

rough_scatter <- ggplot(age_group_summary, aes(y = goal_share, x = win_share, colour = category)) +
  geom_point(size = 5.5, alpha = case_when(age_group_summary$country == "Spain" ~ 1,
                                           TRUE ~ 0.5)) +
  scale_x_continuous(limits = c(0.075, 0.925),
                     breaks = seq(0.25, 0.75, 0.25),
                     expand = c(0, 0),
                     label = percent_format(scale = 100)) +
  scale_y_continuous(limits = c(0.15, 0.825),
                     breaks = seq(0.25, 0.75, 0.25),
                     expand = c(0, 0),
                     label = percent_format(scale = 100)) +
  scale_colour_manual(values = palette_category, aesthetics = "colour") +
  theme(legend.position = "none")
rough_scatter

themed_scatter <- rough_scatter +
  theme_ptb() +
  theme(axis.title = element_blank(),
        axis.ticks = element_blank(),
        axis.line = element_blank()) +
  annotate(geom = "text",
           x = 0.875,
           y = 0.18,
           label = "Win %",
           family = ptb_font,
           colour = ptb_dark_grey,
           fontface = "bold",
           hjust = 1,
           vjust = 0) +
  annotate(geom = "text",
           x = 0.10,
           y = 0.75,
           label = "% of\n goals",
           family = ptb_font,
           colour = ptb_dark_grey,
           fontface = "bold",
           hjust = 0,
           vjust = 0.5)
themed_scatter

labelled_scatter <- themed_scatter +
  labs(title = "<b style='color:#E6173D'>Spain</b> have historically excelled in<br>women's age-group football",
       subtitle = "Historic <b>win %</b> of women's international age-group<br>teams in FIFA and UEFA events, compared to the<br><b>share of goals</b> they scored in those fixtures",
       caption = "<i>Teams with <50 games played excluded</i><br><br>Data: RSSSF | Chart: Plot the Ball")
labelled_scatter
