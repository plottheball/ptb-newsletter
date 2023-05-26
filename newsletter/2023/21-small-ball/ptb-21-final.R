library(tidyverse)
library(janitor)
library(googlesheets4)
library(RcppRoll)
library(scales)
library(ggtext)
library(ggborderline)

# Results imported via Google Sheets from https://www.world.rugby/sevens-series/standings/womens
results_url <- "https://docs.google.com/spreadsheets/d/1fWSaK0ChpL5ZpGrcuEoGOnj1XFye3nUUobhPufXgpRE/edit?usp=sharing"

results_data <- read_sheet(results_url) %>%
  clean_names()

roll_no <- 7

roll_results <- results_data
roll_results$nzl_roll <- roll_sum(roll_results$new_zealand_7s, n = roll_no, align = "right", fill = NA) / (roll_no * 20) * 100
roll_results$aus_roll <- roll_sum(roll_results$australia_7s, n = roll_no, align = "right", fill = NA) / (roll_no * 20) * 100
roll_results$can_roll <- roll_sum(roll_results$canada_7s, n = roll_no, align = "right", fill = NA) / (roll_no * 20) * 100
roll_results$usa_roll <- roll_sum(roll_results$usa_7s, n = roll_no, align = "right", fill = NA) / (roll_no * 20) * 100
roll_results$esp_roll <- roll_sum(roll_results$spain_7s, n = roll_no, align = "right", fill = NA) / (roll_no * 20) * 100
roll_results$fra_roll <- roll_sum(roll_results$france_7s, n = roll_no, align = "right", fill = NA) / (roll_no * 20) * 100

nzl_black <- "#141134"
background_data <- "#D9C3C3"
ref_line_colour <- "#e0e4ef"

rough_borderline <- ggplot(roll_results, aes(x = index)) +
  geom_borderpath(aes(y = nzl_roll),
            color = nzl_black,
            linewidth = 1.5) +
  geom_borderpath(aes(y = aus_roll),
            color = background_data,
            linewidth = 1.5) +
  geom_borderpath(aes(y = can_roll),
            color = background_data,
            linewidth = 1.5) +
  geom_borderpath(aes(y = usa_roll),
            color = background_data,
            linewidth = 1.5) +
  geom_borderpath(aes(y = esp_roll),
            color = background_data,
            linewidth = 1.5) +
  geom_borderpath(aes(y = fra_roll),
            color = background_data,
            linewidth = 1.5) +
  scale_x_continuous(limits = c(0, 55),
                     expand = c(0,0)) +
  scale_y_continuous(limits = c(0, 100),
                     labels = percent_format(accuracy = 1, scale = 1),
                     breaks = seq(0, 100, 20),
                     expand = c(0,0)) +
  theme(axis.line.x = element_blank(),
        axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.ticks.x = element_blank(),
        panel.grid.major.x = element_blank(),
        legend.position = "none") +
  geom_hline(yintercept = 0,
             colour = ptb_dark_grey,
             size = 1) +
  geom_hline(yintercept = 100,
             colour = ptb_dark_grey,
             size = 1)
rough_borderline

themed_borderline <- rough_borderline +
  theme_ptb() +
  geom_vline(xintercept = 9,
             size = 0.5,
             color = ref_line_colour,
             linetype = "dashed") +
  geom_vline(xintercept = 15,
             size = 0.5,
             color = ref_line_colour,
             linetype = "dashed") +
  geom_vline(xintercept = 20,
             size = 0.5,
             color = ref_line_colour,
             linetype = "dashed") +
  geom_vline(xintercept = 26,
             size = 0.5,
             color = ref_line_colour,
             linetype = "dashed") +
  geom_vline(xintercept = 31,
             size = 0.5,
             color = ref_line_colour,
             linetype = "dashed") +
  geom_vline(xintercept = 37,
             size = 0.5,
             color = ref_line_colour,
             linetype = "dashed") +
  geom_vline(xintercept = 42,
             size = 0.5,
             color = ref_line_colour,
             linetype = "dashed") +
  geom_vline(xintercept = 44,
             size = 0.5,
             color = ref_line_colour,
             linetype = "dashed") +
  geom_vline(xintercept = 51,
             size = 0.5,
             color = ref_line_colour,
             linetype = "dashed")
themed_borderline

labelled_borderline <- themed_borderline +
  labs(title = "<span style='color:#141134'>New Zealand</span> achieved 138 of 140 possible<br>points during the 2023 Sevens World Series",
       subtitle = "Rolling average share of available standings points won by<br>selected teams on the World Rugby Women's Sevens Series",
       caption = "<b style='color:#D9C3C3'>Also plotted: Australia, Canada, USA, France & Spain</b><br><br><i>Averages calculated on a seven-event rolling basis; Covid-affected events excluded</i><br><br>Data: World Rugby | Chart: Plot the Ball")
labelled_borderline
