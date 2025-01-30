library(tidyverse)
library(rvest)
library(janitor)
library(googlesheets4)
library(ggtext)
library(scales)

### "https://www.baseball-reference.com/bio/Japan_born.shtml"
player_ids <- c("darvis001yu-", "maeda-001ken", "kikuch000yus",
                "otani-000sho", "imanag000sho", "senga-000kod",
                "yamamo004yos", "sasaki000rok")

all_data <- tibble()

for (i in player_ids) {
  
  Sys.sleep(1)
  
  if(i == "otani-000sho") {
    next
  } 
  
  url <- str_c("https://www.baseball-reference.com/register/player.fcgi?id=", i)
  
  html <- read_html(url)
  
  tables <- html %>%
    html_nodes("table") %>%
    html_table()
  
  key_table <- tables[[1]] %>%
    clean_names() %>%
    mutate(id = i,
           .before = "year")
  
  key_table <- key_table %>%
    select(id, year, age, lg, g, ip, bb, so) %>%
    filter(lg == "JPPL" | lg == "JPCL" | lg == "AL" | lg == "NL") %>%
    mutate(across(everything(), as.character))
  
  all_data <- bind_rows(all_data, key_table)
  
}

ohtani_data <- read_sheet(ss = "https://docs.google.com/spreadsheets/d/18fWBZJCWOC3rjA2pjyn0_mM6mSLMh1LvPX_jzqyRas0/edit?gid=78139247#gid=78139247",
                          sheet = "Pitching")

ohtani_data <- ohtani_data %>%
  clean_names() %>%
  mutate(id = "otani-000sho",
         .before = "year") %>%
  select(id, year, age, lg, g, ip, bb, so) %>%
  filter(lg == "JPPL" | lg == "JPCL" | lg == "AL" | lg == "NL") %>%
  mutate(across(everything(), as.character))

final_data <- bind_rows(all_data, ohtani_data) %>%
  unique() %>%
  mutate(year = as.integer(year),
         age = as.integer(age),
         g = as.integer(g),
         bb = as.integer(bb),
         so = as.integer(so)) %>%
  separate(col = ip,
           into = c("inns_complete", "inns_incomplete"),
           sep = "[.]") %>%
  mutate(inns_complete = as.integer(inns_complete),
         inns_incomplete = replace_na(as.integer(inns_incomplete), 0),
         inns_total = inns_complete + (inns_incomplete / 3))

npb_data <- final_data %>%
  filter(lg == "JPPL" | lg == "JPCL")

npb_summary <- npb_data %>%
  group_by(id) %>%
  summarise(n_inns = sum(inns_total)) %>%
  arrange(desc(n_inns)) %>%
  mutate(category = case_when(id == "sasaki000rok" ~ "Focus",
                              id == "otani-000sho" | id == "yamamo004yos" ~ "Other",
                              TRUE ~ "Context"),
         name = case_when(id == "darvis001yu-" ~ "<span style='color:#B3B3B3'>Yu Darvish</span>",
                          id == "maeda-001ken" ~ "<span style='color:#B3B3B3'>Kenta Maeda</span>",
                          id == "kikuch000yus" ~ "<span style='color:#B3B3B3'>Yusei Kikuchi</span>",
                          id == "imanag000sho" ~ "<span style='color:#B3B3B3'>Shota Imanaga</span>",
                          id == "senga-000kod" ~ "<span style='color:#B3B3B3'>Kodai Senga</span>",
                          id == "yamamo004yos" ~ "<span style='color:#8FA4B3'>Yoshinobu Yamamoto</span>",
                          id == "otani-000sho" ~ "<span style='color:#8FA4B3'>Shohei Ohtani</span>",
                          id == "sasaki000rok" ~ "<b style='color:#005A9C'>Roki Sasaki</b>",
                          TRUE ~ "!"),
         .before = id)

rough_bars <- ggplot(npb_summary, aes(x = fct_reorder(name, n_inns), y = n_inns, fill = category)) +
  geom_bar(stat = "identity",
           width = 0.6) +
  scale_fill_manual(values = c("#C9C9C9", "#005A9C", "#A3BBCC")) +
  geom_hline(yintercept = 0) +
  scale_y_continuous(limits = c(0, 1950),
                     breaks = seq(0, 1500, 500),
                     labels = label_comma(),
                     expand = c(0, 0)) +
  theme(legend.position = "none",
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.ticks = element_blank(),
        panel.grid.major.y = element_blank(),
        axis.line.x = element_blank(),
        axis.line.y = element_blank()) +
  coord_flip()
rough_bars

themed_bars <- rough_bars +
  theme_ptb()
themed_bars

labelled_bars <- themed_bars +
  labs(title = "<b style='color:#005A9C'>Sasaki</b> has much less NPB experience<br>than other pitchers who left Japan",
       subtitle = "Career <b>NPB innings</b> of Japanese pitchers active<br>in MLB in 2024, compared to <b style='color:#005A9C'>Roki Sasaki</b>'s total",
       caption = "<br><i>Starting pitchers (GS > 75% of G; at least 10 career MLB GS) only</i><br><br>Data: Baseball Reference | Chart: Plot the Ball")
labelled_bars
