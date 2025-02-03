library(tidyverse)
library(tidytuesdayR)
library(hrbrthemes)
library(scales)
library(lubridate)
library(ggrepel)
library(patchwork)
library(here)
library(camcorder)
library(ragg)
library(ggtext)
library(ggbump)
library(ggplot2)
library(dplyr)
library(wesanderson)
library(countrycode)
library(gganimate)
library(quarto)
# For reproducible randomness
set.seed(123)
theme_set(theme_ipsum() + theme(legend.position = "bottom"))

# 1) Create a vector of 20 employer names
employers <- c(
  "Apple", "Google", "Amazon", "Netflix", "Airbnb", 
  "Oracle", "Meta", "Salesforce", "IBM", "Intel",
  "HP", "Cisco", "Uber", "Lyft", "Dropbox", 
  "Snap", "Tesla", "Microsoft", "PayPal", "Adobe"
)

# 2) Years we want
years <- c(2023, 2024)

# 3) Create a data frame of all combinations of employers and years
df <- expand_grid(year = years, employer = employers) %>%
  # 4) Add a synthetic total_hires count for each row
  mutate(total_hires = sample(50:500, n(), replace = TRUE))

# Inspect the resulting data
df

rankings <- df %>%
  group_by(year) %>%
  mutate(rank = dense_rank(-total_hires)) %>%
  ungroup()
  
hires_cumulative <- rankings %>%
  arrange(employer, year) %>%  
  group_by(employer) %>% 
  mutate(cumulative_hires = cumsum(total_hires)) %>%
  ungroup() 

top_companies <- hires_cumulative %>%
  group_by(employer) %>%
  slice_max(order_by = year, n = 1) %>%  
  ungroup() %>%
  slice_max(order_by = cumulative_hires, n = 4) %>%
  pull(employer)

color_palette <- c(
  "Oracle" = "#072C8F",  
  "Intel" = "#009E73",                  
  "Adobe" = "#FF0000",                  
  "Snap" = "#E69F00",
  "Other" = "#A9A9A9")

names(color_palette) <- c(top_companies, "Other")

overall_data <- hires_cumulative %>%
  group_by(employer) %>%
  slice_max(year, n = 1) %>%
  ungroup() %>%
  slice_max(cumulative_hires, n = 1) %>%
  mutate(
    total_companies = n_distinct(hires_cumulative$employer),
    top_employer = employer,
    top_hires = cumulative_hires,
    second_employer = top_companies[2],
    second_hires = filter(hires_cumulative, employer == second_employer & year == max(year))$cumulative_hires) %>%
  slice(1) %>%
  mutate(subtitle = str_wrap(glue::glue("AEXTA has hired from {total_companies} distinct companies over the past two years. With the top 10 previous employers accounting for 20% of *all* external hires. {top_employer} has been the #1 previous employer with {top_hires} hires, while {second_employer} has also been consistent YoY."), 60))

overall <- ggplot(hires_cumulative,
                  aes(x = year, y = cumulative_hires, group = employer)) +
  geom_line(aes(
    color = ifelse(employer %in% top_companies, employer, "Other"),
    alpha = ifelse(employer %in% top_companies, 1, 0.3)
  ), size = 1.2) +
  geom_point(
    data = . %>% slice_max(year, n = 1, by = employer) %>% filter(employer %in% top_companies),
    size = 3,
    aes(color = employer)
  ) +
  geom_label_repel(
    data = hires_cumulative %>% 
      group_by(employer) %>% 
      slice_max(year, n = 1) %>% 
      filter(employer %in% top_companies),
    aes(
      label = str_wrap(
        glue::glue("{employer} ({cumulative_hires})"), 
        width = 16
      ),
      color = employer
    ),
    hjust = "right",  
    fill = "white",
    direction = "y",
    label.size = NA,
    box.padding = 0.5,
    nudge_x = 1,
    nudge_y = 1.5,
    fontface = "bold",
    size = 5.5
  ) +
  scale_y_continuous(
    breaks = scales::pretty_breaks(5)
  ) +
  scale_x_continuous(breaks = seq(2023, 2024, by = 1)) +
  theme_minimal() +
  labs(y = "", x = "",  
       title = str_wrap("Total AEXTA Hires by Previous Employer", 40),
       subtitle = overall_data$subtitle) +
  scale_color_manual(values = color_palette) +
  scale_alpha_identity() +
  theme(
    plot.title = element_text(size = 45),
    plot.subtitle = element_text(size = 34),
    panel.grid.minor = element_blank(),
    plot.margin = margin(-15.5, 120, 5.5, 5.5),
    panel.grid.major.x = element_blank(),
    axis.text = element_text(size = 18),
    panel.grid.major.y = element_line(color = "gray90")  
  ) +
  theme_ipsum_pub(grid = "xy", axis_text_size = 18, plot_title_size = 28, subtitle_size = 20) + 
  theme(legend.position="none")

trends <- rankings %>%
  group_by(employer) %>%
  arrange(year) %>%
  mutate(
    rank_change = first(rank) - lag(rank),
    total_change = last(rank) - first(rank),
    variability = sd(rank, na.rm = TRUE),
    iso2c = TRUE,
  ) %>%
  ungroup() %>%
  mutate(year = ymd(paste0(year, "-12-01")))

create_bump_chart <- function(data,
                              highlight_companies,
                              title,
                              highlight_color = "red",
                              n_interesting_points = 1) {
  data <- data %>%
    mutate(highlight = ifelse(employer %in% highlight_companies, employer, "Other"))
  
  last_year <- data %>% 
    filter(employer %in% highlight_companies) %>% 
    summarize(max_year = max(year, na.rm = TRUE)) %>%
    pull(max_year)
  
  first_year_data <- data %>%
    filter(highlight != "Other") %>%
    group_by(employer) %>%
    slice_min(order_by = year, n = 1) %>%
    ungroup()
  
  ggplot(data, aes(
    x = year,
    y = rank,
    color = highlight,
    group = iso2c
  )) +
    geom_bump(
      data = filter(data, highlight == "Other"),
      alpha = 0.2,
      size = 0.1
    ) +
    geom_point(
      data = filter(data, highlight == "Other"),
      size = 3,
      alpha = 0.3
    ) +
    geom_bump(data = filter(data, highlight != "Other"), size = 1) +
    geom_point(data = filter(data, highlight != "Other"), size = 3) +
    # Last year labels
    geom_text(
      data = data %>% filter(highlight != "Other" & year == last_year),
      aes(label = glue::glue("{str_wrap(employer, width = 30)} ({rank})")),
      hjust = -0.2,
      vjust = 0.5,
      label.color = NA,
      fontface = "bold",
      size = 5,
      show.legend = FALSE
    ) +
    geom_point(
      data = first_year_data,
      size = 2
    ) +
    geom_label_repel(
      data = first_year_data,
      aes(label = rank),
      hjust = "left",
      label.size = NA,
      label.color = NA,
      fontface = "bold",
      size = 5,
      show.legend = FALSE
    ) +
    scale_y_reverse() +
    scale_size(range = c(10)) +
    scale_color_manual(values = c("Other" = "gray", setNames(
      c(highlight_color), highlight_companies
    ))) +
    theme_ipsum_pub(grid = "xy",
                    plot_title_size = 24,
                    axis_text_size = 18,
                    axis_title_size = 18) + 
    labs(x = "", y = "Rank", title = title) +
    coord_cartesian(clip = "off") +
    theme(legend.position="none",
          axis.text.x = element_blank()) + 
    coord_cartesian(clip = "off")
}
# 1. Biggest rise
biggest_rise <- trends %>%
  group_by(employer) %>%
  summarize(total_change = first(rank) - last(rank)) %>%
  arrange(desc(total_change)) %>%
  slice_head(n = 1) %>%
  pull(employer)

p1 <- create_bump_chart(trends, biggest_rise, "Biggest Rise in Rank", 
                        highlight_color = "#FB9701", 
)

# 2. Biggest fall
biggest_fall <- trends %>%
  group_by(employer) %>%
  summarize(total_change = first(rank) - last(rank)) %>%
  arrange(total_change) %>%
  slice_head(n = 1) %>%
  pull(employer)


p2 <- create_bump_chart(trends, biggest_fall, "Biggest Fall in Rank", 
                        highlight_color = "#F70020")

# 3. Most steady
most_steady <- trends %>%
  group_by(employer) %>%
  summarize(variability = mean(variability)) %>%
  arrange(variability) %>%
  slice_head(n = 1) %>%
  pull(employer)

p3 <- create_bump_chart(trends, most_steady, "Most Steady Ranks", highlight_color = "#072C8F")

# 4. Least steady
least_steady <- trends %>%
  group_by(employer) %>%
  summarize(variability = mean(variability)) %>%
  arrange(desc(variability)) %>%
  slice_head(n = 1) %>%
  pull(employer)

p4 <- create_bump_chart(trends, least_steady, "Least Steady Ranks", highlight_color = "purple")

(
  free(overall) |
    ( p1  /
        p2 /
        p3 
    ) + 
    plot_layout(axes = "collect")
) +
  plot_layout(ncol = 2, widths = c(1.5, 1.8)) +
  plot_annotation(
    caption = "Source: Workday Req002A - Filled Requisition Detail Report | Viz by Channing Griffin - Sr. Talent Partner, Digital Media",
    theme = theme(plot.caption = element_text(size = 16),
                  plot.title = element_text(size = 28))
  )



