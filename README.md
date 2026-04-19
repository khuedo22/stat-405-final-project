# STAT 405 - Bayesian Statistics Project

## Reading the data

The dataset `full_data.csv` is included in the repository. If you need to regenerate 
it from scratch using the `f1dataR` package, use the following code (note this takes a long time to run):

```r
# Load the pre-compiled dataset containing all seasons (2019–2024)
# full_data <- map_dfr(2019:2024, function(year) {
#   schedule <- load_schedule(season = year)
#   map_dfr(seq_len(nrow(schedule)), function(round) {
#     tryCatch(
#       load_results(season = year, round = round) %>%
#         mutate(
#           season = year,
#           round = round,
#           race_name = schedule$race_name[round],
#           circuit = schedule$circuit_id[round],
#           top_speed_kph = as.numeric(top_speed_kph),
#           fastest_rank = as.integer(fastest_rank)
#         ),
#       error = function(e) {
#         message("Skipping season ", year, " round ", round, ": ", e$message)
#         NULL
#       }
#     )
#   })
# })

# write_csv(full_data, "full_data.csv")
```