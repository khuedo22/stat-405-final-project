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

convert_to_seconds <- function(x) {
  if (is.numeric(x)) return(x)
  
  x <- as.character(x)
  
  sapply(x, function(val) {
    # Plain number (no colon)
    if (grepl("^[+-]?\\d+\\.?\\d*$", val)) {
      return(as.numeric(val))
    }
    
    # No sign prefix and contains a colon = gap of 0
    if (!grepl("^[+-]", val) && grepl(":", val)) {
      return(0)
    }
    
    # Extract sign
    sign <- ifelse(startsWith(val, "-"), -1, 1)
    val <- gsub("^[+-]", "", val)
    
    parts <- strsplit(val, ":")[[1]]
    
    if (length(parts) == 2) {
      minutes <- as.numeric(parts[1])
      seconds <- as.numeric(parts[2])
      return(sign * (minutes * 60 + seconds))
    } else if (length(parts) == 3) {
      hours   <- as.numeric(parts[1])
      minutes <- as.numeric(parts[2])
      seconds <- as.numeric(parts[3])
      return(sign * (hours * 3600 + minutes * 60 + seconds))
    } else {
      return(NA)
    }
  }, USE.NAMES = FALSE)
}
full_data <- full_data %>%
  mutate(gap_seconds = convert_to_seconds(gap), 
         grid = as.integer(grid)) %>%
  filter(
    status == "Finished",       # only finishers
    !is.na(grid),
    grid > 0,                    # exclude pit lane starts (grid = 0)
    !is.na(gap_seconds))
  
# grid vs gap across all circuits
full_data %>%
  ggplot(aes(x = factor(grid), y = gap_seconds)) +
  geom_boxplot(outlier.alpha = 0.3, fill = "#e10600", color = "white") +
  scale_y_continuous(labels = scales::label_number(suffix = "s")) +
  labs(
    title = "Gap to Race Winner by Grid Position (2019–2024)",
    x = "Grid Position",
    y = "Gap to Winner (seconds)"
  ) +
  theme_minimal()

# grid vs gap across all circuits log transformed
full_data %>%
  ggplot(aes(x = factor(grid), y = log(1+gap_seconds))) +
  geom_boxplot(outlier.alpha = 0.3, fill = "#e10600", color = "white") +
  scale_y_continuous() +
  labs(
    title = "Log Gap to Race Winner by Grid Position (2019–2024)",
    x = "Grid Position",
    y = "Log(Gap to Winner + 1)"
  ) +
  theme_minimal()

# grid vs gap across all circuits scatterplot
full_data %>%
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

# grid vs log gap for each circuit
full_data %>%
  ggplot(aes(x = grid, y = log(gap_seconds+1))) +
  geom_point(alpha = 0.4, size = 0.8, color = "#e10600") +
  facet_wrap(~ circuit, ncol = 4) +
  scale_x_continuous(breaks = c(1, 10, 20)) +
  scale_y_continuous() +
  labs(
    title = "Grid Position vs Gap to Winner by Circuit (2019–2024)",
    x = "Grid Position",
    y = "Log(Gap to Winner + 1)"
  ) +
  theme_minimal() 

# grid vs gap by season
full_data %>%
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

 full_data %>%
  select(grid, gap_seconds) %>%
  summary()

hist(full_data$gap_seconds)
qqplot(full_data$grid,full_data$gap_seconds)
qqline(log(full_data$gap_seconds+1))
qqnorm(log(full_data$gap_seconds+1))

hist(filter(full_data, grid == 4)$gap_seconds)




# Example coefficients


# Generate example data (replace with your actual data)
set.seed(42)


# Set your own parameter values
alpha <- 0.5   # intercept
beta  <- 0.1   # slope

# Step 1: Compute rate for each grid value from the linear predictor
params <- tibble(grid = 1:20) |>
  mutate(rate = alpha + beta * grid)

# Step 2: Build overlay curves
overlay <- params |>
  rowwise() |>
  mutate(
    x    = list(seq(0, quantile(full_data$gap_seconds[full_data$grid == grid], 0.995), 
                    length.out = 300)),
    dens = list(dexp(unlist(x), rate = rate))
  ) |>
  unnest(c(x, dens))

ggplot(full_data, aes(x = gap_seconds)) +
  geom_histogram(
    aes(y = after_stat(density)),
    bins      = 30,
    fill      = "blue",
    colour    = "white",
    linewidth = 0.3
  ) +
  geom_line(
    data      = overlay,
    aes(x = x, y = dens),
    colour    = "red",
    alpha=0.1,
    linewidth = 0.8
  ) +
  facet_wrap(~ grid, scales = "free", ncol = 4,
             labeller = label_bquote(grid == .(grid))) +
  labs(
    x        = "gap_seconds",
    y        = "density",
    title    = "gap_seconds by grid position",
    subtitle = sprintf("Exp(rate = %.2f + %.2f × grid) overlay", alpha, beta)
  ) +
  theme_minimal(base_size = 11) +
  theme(
    strip.text       = element_text(size = 9, face = "bold"),
    panel.grid.minor = element_blank(),
    plot.title       = element_text(face = "bold")
  )
hist(log(full_data$gap_seconds+1))
