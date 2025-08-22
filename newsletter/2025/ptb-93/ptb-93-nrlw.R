library(tidyverse)
library(janitor)
library(rvest)
library(googlesheets4)
library(scales)
library(ggtext)

players <- read_html("https://www.rugbyleagueproject.org/seasons/nrlw-2025/players.html") %>%
  html_nodes("table") %>%
  html_table()

players_clean <- players[[1]] %>%
  clean_names() %>%
  filter(x1 != "To view matches in which a player appeared, click the List button.")

names <- unlist(players_clean[1,])

names(players_clean) <- names

players_final <- players_clean %>%
  clean_names() %>%
  filter(age != "Age") %>%
  select(1:7) %>%
  arrange(team_s) %>%
  mutate(age = as.integer(age),
         app = replace_na(as.integer(app), 0),
         int = replace_na(as.integer(int), 0),
         tot = replace_na(as.integer(tot), 0))

sheet_write(data = players_final,
            ss = "https://docs.google.com/spreadsheets/d/1UJCIGziRm32UBp9ZjlSqB72TjKncvXNa8U9hyAZoo_Q/edit",
            sheet = "Raw")

### data manually added and re-imported
mins_data <- read_sheet(ss = "https://docs.google.com/spreadsheets/d/1UJCIGziRm32UBp9ZjlSqB72TjKncvXNa8U9hyAZoo_Q/edit",
                        sheet = "Mins by team") %>%
  clean_names()

mins_final <- mins_data %>%
  select(2, 3, 4, 5) %>%
  pivot_longer(cols = c("c_nz_int_share", "b_nz_dom_share", "a_other_share"),
               names_to = "category",
               values_to = "value") %>%
  mutate(sort_value = if_else(category == "a_other_share", value, NA))

rough_bars <- ggplot(mins_final, aes(fill = category, y = value, x = fct_reorder(name, desc(sort_value)))) +
  geom_bar(position = "stack", stat = "identity", width = 0.8) +
  geom_hline(yintercept = 0) +
  geom_hline(yintercept = 1,
             color = "grey") +
  scale_fill_manual(values = c("#ffffff00", "#A3CCC0", "#6D5C99")) +
  scale_y_continuous(labels = percent_format()) +
  coord_flip() +
  theme(legend.position = "none",
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.ticks.y = element_blank(),
        panel.grid.major.y = element_blank(),
        axis.line.x = element_blank(),
        axis.line.y = element_blank(),
        axis.ticks = element_blank(),
        axis.text.y = element_text(hjust = 0))
rough_bars

themed_bars <- rough_bars +
  theme_ptb()
themed_bars

labelled_bars <- themed_bars +
  labs(title = "<b style='color:#5A468C'>Kiwi rugby union internationals</b> have<br>played 6% of NRLW minutes in 2025",
       subtitle = "Share of each NRLW team's 2025 minutes played<br>by players who have played <b style='color:#5A468C'>international</b> or<br><b style='color:#63A692'>domestic</b> rugby union in New Zealand",
       caption = "<br>Data: Rugby League Project; Rugby Database | Chart: Plot the Ball")
labelled_bars
