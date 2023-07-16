library(tidyverse)
library(janitor)
library(rvest)
library(RSelenium)
library(xml2)
library(ggtext)

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

rankings_tidy$rk <- as.integer(rankings_tidy$rk)
rankings_tidy$total_points_pts <- as.numeric(rankings_tidy$total_points_pts)
rankings_tidy$date <- ymd(rankings_tidy$date)

current_top20 <- rankings_tidy %>%
  filter(date == "2023-06-09" & rk < 21)

current_top20 <- unlist(current_top20$team)

rankings_filtered <- rankings_tidy %>%
  filter(team %in% current_top20)

focus_colour_eng <- "#e50b16"
focus_colour_usa <- "#212844"
background_colour <- "#444F5A25"


rough_line <- ggplot(rankings_filtered, aes(x = date, y = total_points_pts, colour = team)) +
  geom_line(linewidth = 1.5) +
  scale_y_continuous(limits = c(1525, 2275),
                     breaks = seq(1600, 2200, 200)) +
  scale_x_date(expand = c(0.06, 0)) +
  scale_color_manual(values = c("USAUSA"= focus_colour_usa,
                                "GermanyGER"= background_colour,
                                "SwedenSWE"= background_colour,
                                "EnglandENG"= focus_colour_eng,
                                "FranceFRA"= background_colour,
                                "SpainESP"= background_colour,
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
  labs(title = "<b style='color:#e50b16'>England</b> aren't far off the <b style='color:#212844'>USWNT</b>",
       subtitle = "Change in <b>rating points</b> of the current top 20<br>women's international football teams since 2015",
       caption = "<i>Data as at 9 Jun 2023</i><br><br>Data: FIFA | Chart: Plot the Ball")
labelled_line
