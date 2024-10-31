library(tidyverse)
library(rvest)
library(janitor)
library(RcppRoll)
library(ggtext)
library(xml2)
library(scales)

base_url <- "http://stats.allblacks.com/asp/testrecords.asp?team1=NZ&sdate=1884&edate=2024&ground=&country=&tourn=Tests&icount="
sequence <- seq(0, 640, 20)

all_matches <- tibble()

for (i in sequence) {
  
  Sys.sleep(0.5)
  
  full_url <- str_c(base_url, i)
  
  html <- read_html(full_url)
  
  tables <- html %>%
    html_nodes("table") %>%
    html_table()
  
  key_table <- tables[[1]]
  
  all_matches <- bind_rows(all_matches, key_table)
  
}

names(all_matches) <- unlist(all_matches[1,])

matches_clean <- all_matches %>%
  clean_names() %>%
  mutate(date = dmy(date)) %>%
  filter(!is.na(date)) %>%
  separate(col = result,
           into = c("points_for", "points_against"),
           sep = " - ") %>%
  mutate(points_for = as.integer(points_for),
         points_against = as.integer(points_against)) %>%
  arrange(date) %>%
  mutate(match_index = row_number(),
         .before = date) %>%
  select(-city, -country)

test_opponents <- sort(unique(unlist(matches_clean$team)))

opponents_category <- tibble(team = test_opponents,
                             category = c("Other Tier 1", "Tri-Nations", "Five Nations", "Tier 2",
                                          "Five Nations", "Tier 2", "Five Nations", "Tier 2",
                                          "Five Nations", "Other Tier 1", "Tier 2", "Tier 2",
                                          "Tier 2", "Tier 2", "Tier 2", "Tier 2",
                                          "Five Nations", "Tri-Nations", "Tier 2", "Tier 2",
                                          "Tier 2", "Five Nations", "Other Tier 1"))

matches_clean <- left_join(matches_clean, opponents_category, by = c("team" = "team"))

modern_era <- matches_clean %>%
  filter(date >= "1992-07-04") %>%
  mutate(match_index = 0 - nrow(.) + row_number())

category_count <- modern_era %>%
  group_by(category) %>%
  summarise(no_matches = n(),
            total_for = sum(points_for),
            total_against = sum(points_against),
            points_share = total_for / (total_for + total_against))

tier_one_tests <- modern_era %>%
  filter(category == "Tri-Nations" | category == "Five Nations" | category == "Other Tier 1") %>%
  mutate(match_index = 0 - nrow(.) + row_number())

roll_no <- 30

tier_one_tests <- tier_one_tests %>%
  mutate(roll_for = roll_sum(x = points_for, n = roll_no, align = "right", fill = NA) / roll_no,
         roll_against = roll_sum(x = points_against, n = roll_no, align = "right", fill = NA) / roll_no,
         roll_margin = roll_for - roll_against)

nz_black <- "#0B0A1A"
nz_blue <- "#148FCC"
nz_grey <- "#e0e4ef"

rough_line <- ggplot(tier_one_tests, aes(x = match_index)) +
  scale_y_continuous(name = NULL,
                     limits = c(1, 49),
                     expand = c(0,0),
                     breaks = seq(10, 40, 10)) +
  scale_x_continuous(limits = c(-236, 0),
                     expand = c(0.2, 0.1)) +
  geom_vline(xintercept = -8,
             size = 0.7,
             color = nz_grey,
             linetype = "dashed") +
  geom_vline(xintercept = -47,
             size = 0.7,
             color = nz_grey,
             linetype = "dashed") +
  geom_vline(xintercept = -143,
             size = 0.7,
             color = nz_grey,
             linetype = "dashed") +
  geom_vline(xintercept = -236,
             size = 0.7,
             color = nz_grey,
             linetype = "dashed") +
  geom_ribbon(aes(ymin = roll_against, ymax = roll_for), fill = nz_grey, alpha = 1) +
  geom_line(aes(y = roll_for), colour = nz_black, linewidth = 1.5, alpha = 1) +
  geom_line(aes(y = roll_against), colour = nz_blue, linewidth = 1.5, alpha = 1)
rough_line

themed_line <- rough_line +
  theme_ptb() +
  theme(axis.line = element_blank(),
        axis.ticks = element_blank(),
        axis.text.x = element_blank(),
        panel.grid.major.x = element_blank(),
        axis.title.x = element_blank())
themed_line

labelled_line <- themed_line +
  labs(title = "The <b style='color:#148FCC'>defensive performance</b> of the All<br>Blacks has deteriorated since 2019",
       subtitle = "Average scoreline in test matches between <b style='color:#0B0A1A'>New<br>Zealand</b> and their <b style='color:#148FCC'>Tier 1 opponents</b> since 2004",
       caption = "<i>Averages calculated on a 30-game rolling basis</i><br><br>Data: allblacks.com | Chart: Plot the Ball")
labelled_line

all_links <- c()

for (i in sequence) {
  
  Sys.sleep(0.5)
  
  full_url <- str_c(base_url, i)
  
  html <- read_html(full_url)
  
  links <- html %>%
    html_nodes("a") %>%
    html_attr("href")
  
  all_links <- c(all_links, links)
  
}

match_links <- all_links %>%
  str_subset(pattern = fixed("/asp/teamsheet.asp?MT_ID=")) %>%
  rev()

matches_clean$id <- clean_lineups$match

modern_era <- matches_clean %>%
  filter(date >= "1992-07-04") %>%
  mutate(match_index = 0 - nrow(.) + row_number())

links_final <- unlist(modern_era$id)

all_lineups <- tibble()

for (i in links_final) {
  
  Sys.sleep(0.25)
  
  print(str_c("Getting match: ", i))
  
  full_url <- str_c("http://stats.allblacks.com", i)
  
  html <- read_html(full_url)
  
  html_clean <- html
  
  xml_find_all(html_clean, ".//br") %>% xml_add_sibling("p", "\n")
  
  xml_find_all(html_clean, ".//br") %>% xml_remove()
  
  tables <- html_clean %>%
    html_nodes("table") %>%
    html_table()
  
  key_table <- tables[[3]]
  
  lineup <- key_table[3, 1] %>%
    clean_names() %>%
    mutate(match = i)
  
  all_lineups <- bind_rows(all_lineups, lineup)
  
}

clean_lineups <- all_lineups

clean_lineups$x1 <- gsub("\\s*\\([^\\)]+\\)", "", all_lineups$x1)

lineups_final <- left_join(modern_era, clean_lineups, by = c("id" = "match"))

lineups_final <- lineups_final %>%
  separate(col = x1,
           into = c("starter_1", "starter_2", "starter_3", "starter_4", "starter_5", "starter_6", "starter_7", "starter_8",
                    "starter_9", "starter_10", "starter_11", "starter_12", "starter_13", "starter_14", "starter_15",
                    "reserve_marker", "reserve_16", "reserve_17", "reserve_18", "reserve_19", "reserve_20", "reserve_21", "reserve_22", "reserve_23"),
           sep = "\n")

lineups_t1 <- lineups_final %>%
  filter(category == "Tri-Nations" | category == "Five Nations" | category == "Other Tier 1") %>%
  mutate(match_index = 0 - nrow(.) + row_number()) %>%
  select(-points_for, -points_against, -category, -reserve_marker, -reserve_16, -reserve_17, -reserve_18, -reserve_19, -reserve_20, -reserve_21, -reserve_22, -reserve_23) %>%
  pivot_longer(cols = c("starter_1", "starter_2", "starter_3", "starter_4", "starter_5", "starter_6", "starter_7", "starter_8",
                        "starter_9", "starter_10", "starter_11", "starter_12", "starter_13", "starter_14", "starter_15"),
               names_to = "position",
               values_to = "player") %>%
  mutate(player = str_squish(str_remove_all(player, fixed(")"))),
         year = year(date))

summary_by_player <- lineups_t1 %>%
  group_by(player) %>%
  summarise(count = n())

match_list <- unique(unlist(lineups_t1$match_index))

similarity_data <- tibble()

for (i in match_list) {
  
  anchor_ref <- i - roll_no
  
  match_lineup <- lineups_t1 %>%
    filter(match_index == i)
  
  prev_30_lineups <- lineups_t1 %>%
    filter(match_index < i & match_index >= anchor_ref)
  
  prev_30_summary <- prev_30_lineups %>%
    group_by(position, player) %>%
    summarise(prev_30_starts = n())
  
  lineup_context <- left_join(x = match_lineup, y = prev_30_summary, by = c("position" = "position", "player" = "player"))
  
  similarity_score <- sum(lineup_context$prev_30_starts, na.rm = TRUE)
  
  match_data <- tibble("match_index" = i, "similarity_score" = similarity_score)
  
  similarity_data <- bind_rows(similarity_data, match_data)
  
}

similarity_final <- similarity_data %>%
  filter(match_index >= (min(match_index) + 60)) %>%
  mutate(roll_ave_score = roll_sum(x = similarity_score, n = roll_no, align = "right", fill = NA) / roll_no,
         roll_ave_rebased = roll_ave_score / (roll_no * 15) * 100)

rough_lineup <- ggplot(similarity_final, aes(x = match_index)) +
  scale_y_continuous(name = NULL,
                     limits = c(0, 100),
                     expand = c(0, 0),
                     breaks = seq(0, 100, 25),
                     labels = percent_format(scale = 1)) +
  scale_x_continuous(limits = c(-236, 0),
                     expand = c(0.1, 0.1)) +
  geom_vline(xintercept = -8,
             size = 0.7,
             color = nz_grey,
             linetype = "dashed") +
  geom_vline(xintercept = -47,
             size = 0.7,
             color = nz_grey,
             linetype = "dashed") +
  geom_vline(xintercept = -143,
             size = 0.7,
             color = nz_grey,
             linetype = "dashed") +
  geom_vline(xintercept = -236,
             size = 0.7,
             color = nz_grey,
             linetype = "dashed") +
  geom_ribbon(aes(ymin = 0, ymax = roll_ave_rebased), fill = nz_grey, alpha = 1) +
  geom_line(aes(y = roll_ave_rebased), colour = nz_black, linewidth = 1.5, alpha = 1) +
  geom_hline(yintercept = 100,
             size = 1,
             color = ptb_dark_grey,
             linetype = "solid") +
  geom_hline(yintercept = 0,
             size = 1,
             color = ptb_dark_grey,
             linetype = "solid")
rough_lineup

themed_lineup <- rough_lineup +
  theme_ptb() +
  theme(axis.line = element_blank(),
        axis.ticks = element_blank(),
        axis.text.x = element_blank(),
        panel.grid.major.x = element_blank(),
        axis.title.x = element_blank())
themed_lineup

labelled_lineup <- themed_lineup +
  labs(title = "The All Blacks have selected their<br>line-ups more consistently since 2022",
       subtitle = "Average similarity score of New Zealand's starting<br>line-ups in Tier 1 tests since 2004",
       caption = "<i>Calculated on a 30-game rolling basis</i><br><br>Data: allblacks.com | Chart: Plot the Ball")
labelled_lineup
