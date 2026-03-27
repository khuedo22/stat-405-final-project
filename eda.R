library(f1dataR)
library(dplyr)
library(tidyverse)

# combine all results from 2019-2024 in one data frame

# full_data <- map_dfr(2019:2024, function(year) {
#   schedule <- load_schedule(season = year)
#   map_dfr(seq_len(nrow(schedule)), function(round) {
#     load_results(season = year, round = round) %>%
#       mutate(
#         season = year,
#         round = round,
#         race_name = schedule$race_name[round],
#         circuit = schedule$circuit_id[round],
#         top_speed_kph = as.numeric(top_speed_kph),
#         fastest_rank = as.integer(fastest_rank)  # force consistent type
#       )
#   })
# })
# 
# write_csv(full_data, "full_data.csv")

full_data <- read.csv("full_data.csv")

# grid vs gap across all circuits
full_data %>%
  filter(
    !is.na(gap),           # exclude DNFs
    gap != "",
    status == "Finished",       # only finishers
    !is.na(grid),
    grid > 0                    # exclude pit lane starts (grid = 0)
  ) %>%
  mutate(
    gap_seconds = as.numeric(gap),
    grid = as.integer(grid)
  ) %>%
  filter(!is.na(gap_seconds)) %>%
  ggplot(aes(x = factor(grid), y = gap_seconds)) +
  geom_boxplot(outlier.alpha = 0.3, fill = "#e10600", color = "white") +
  scale_y_continuous(labels = scales::label_number(suffix = "s")) +
  labs(
    title = "Gap to Race Winner by Grid Position (2019–2024)",
    x = "Grid Position",
    y = "Gap to Winner (seconds)"
  ) +
  theme_minimal()


# grid vs gap across all circuits scatterplot
full_data %>%
  filter(
    status == "Finished",
    !is.na(grid),
    grid > 0,
    !is.na(gap),
    gap != ""
  ) %>%
  mutate(
    gap_seconds = as.numeric(gap),
    grid = as.integer(grid)
  ) %>%
  filter(!is.na(gap_seconds)) %>%
  ggplot(aes(x = grid, y = gap_seconds)) +
  geom_point(alpha = 0.5, size = 1.5) +
  scale_x_continuous(breaks = 1:20) +
  scale_y_continuous(labels = scales::label_number(suffix = "s")) +
  scale_color_viridis_d(option = "turbo") +  # 20+ distinct colors
  labs(
    title = "Grid Position vs Gap to Winner (2019–2024)",
    x = "Grid Position",
    y = "Gap to Winner (seconds)"
  ) +
  theme_minimal()


# average gap of each grid by circuit
full_data %>%
  filter(
    status == "Finished",
    !is.na(grid),
    grid > 0,
    !is.na(gap),
    gap != ""
  ) %>%
  mutate(
    gap_seconds = as.numeric(gap),
    grid = as.integer(grid)
  ) %>%
  filter(!is.na(gap_seconds)) %>%
  group_by(circuit, grid) %>%
  summarise(avg_gap = mean(gap_seconds), .groups = "drop") %>%
  ggplot(aes(x = grid, y = circuit, fill = avg_gap)) +
  geom_tile(color = "#1a1a2e", linewidth = 0.3) +
  scale_x_continuous(breaks = 1:20) +
  scale_fill_gradientn(
    colors = c("#00d2ff", "#ffffff", "#e10600"),
    values = scales::rescale(c(0, 30, 120)),  # adjust midpoint if needed
    labels = scales::label_number(suffix = "s"),
    name = "Avg Gap"
  ) +
  labs(
    title = "Average Gap to Winner by Grid Position & Circuit (2019–2024)",
    x = "Grid Position",
    y = NULL
  ) +
  theme_minimal()


# grid vs gap for each circuit
full_data %>%
  filter(
    status == "Finished",
    !is.na(grid),
    grid > 0,
    !is.na(gap),
    gap != ""
  ) %>%
  mutate(
    gap_seconds = as.numeric(gap),
    grid = as.integer(grid)
  ) %>%
  filter(!is.na(gap_seconds)) %>%
  ggplot(aes(x = grid, y = gap_seconds)) +
  geom_point(alpha = 0.4, size = 0.8, color = "#e10600") +
  facet_wrap(~ circuit, ncol = 4) +
  scale_x_continuous(breaks = c(1, 10, 20)) +
  scale_y_continuous(labels = scales::label_number(suffix = "s")) +
  labs(
    title = "Grid Position vs Gap to Winner by Circuit (2019–2024)",
    x = "Grid Position",
    y = "Gap to Winner (seconds)"
  ) +
  theme_minimal() 

# grid vs gap by season
full_data %>%
  filter(
    status == "Finished",
    !is.na(grid),
    grid > 0,
    !is.na(gap),
    gap != ""
  ) %>%
  mutate(
    gap_seconds = as.numeric(gap),
    grid = as.integer(grid)
  ) %>%
  filter(!is.na(gap_seconds)) %>%
  ggplot(aes(x = grid, y = gap_seconds)) +
  geom_point(alpha = 0.4, size = 0.8, color = "#e10600") +
  facet_wrap(~ season, ncol = 4) +
  scale_x_continuous(breaks = c(1, 10, 20)) +
  scale_y_continuous(labels = scales::label_number(suffix = "s")) +
  labs(
    title = "Grid Position vs Gap to Winner by Season",
    x = "Grid Position",
    y = "Gap to Winner (seconds)"
  ) +
  theme_minimal() 

# grid vs gap by round
full_data %>%
  filter(
    status == "Finished",
    !is.na(grid),
    grid > 0,
    !is.na(gap),
    gap != ""
  ) %>%
  mutate(
    gap_seconds = as.numeric(gap),
    grid = as.integer(grid)
  ) %>%
  filter(!is.na(gap_seconds)) %>%
  ggplot(aes(x = grid, y = gap_seconds)) +
  geom_point(alpha = 0.4, size = 0.8, color = "#e10600") +
  facet_wrap(~ round, ncol = 4) +
  scale_x_continuous(breaks = c(1, 10, 20)) +
  scale_y_continuous(labels = scales::label_number(suffix = "s")) +
  labs(
    title = "Grid Position vs Gap to Winner by Round",
    x = "Grid Position",
    y = "Gap to Winner (seconds)"
  ) +
  theme_minimal() 
