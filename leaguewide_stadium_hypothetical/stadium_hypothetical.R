library(readxl)
library(tidyverse)
library(purrr)

# Read in park dimension data from an Excel file
park_dimensions <- read_excel("park_dimensions.xlsx")

# Filter out teams with unique stadium designs (Giants, Red Sox, Astros)
pd2 <- park_dimensions |>
  filter(team != "Giants" & team != "Red Sox" & team != "Astros")

# Define columns for distance and height measurements
dist_cols <- c("lf_line_dist", "lf_gap_dist", "cf_dist", "rf_gap_dist", "rf_line_dist")
ht_cols   <- c("lf_line_ht", "lf_gap_ht", "cf_ht", "rf_gap_ht", "rf_line_ht")

# Calculate total absolute differences for each stadium relative to all others
comparison <- pd2 |>
  mutate(
    # Total distance-related change (sum of absolute differences across all other stadiums)
    dist_change = map_dbl(1:n(), function(i) {
      stadium_i <- pd2[i, dist_cols]
      others <- pd2[-i, dist_cols]
      diffs <- abs(sweep(others, 2, unlist(stadium_i), "-"))
      sum(colSums(diffs))
    }),
    
    # Total height-related change (sum of absolute differences)
    ht_change = map_dbl(1:n(), function(i) {
      stadium_i <- pd2[i, ht_cols]
      others <- pd2[-i, ht_cols]
      diffs <- abs(sweep(others, 2, unlist(stadium_i), "-"))
      sum(colSums(diffs))
    }),
    
    # Combined total change (distance + height)
    total_change = dist_change + ht_change
  ) |>
  arrange(total_change)  # sort to see stadiums with smallest total change first

comparison  # display results

# Calculate directional (net) differences instead of absolute
comparison2 <- pd2 |>
  mutate(
    # Net distance-related change (sum of differences without abs)
    dist_change = map_dbl(1:n(), function(i) {
      stadium_i <- pd2[i, dist_cols]
      others <- pd2[-i, dist_cols]
      diffs <- sweep(others, 2, unlist(stadium_i), "-")  # directional differences
      sum(colSums(diffs))  # sum directional differences
    }),
    
    # Net height-related change
    ht_change = map_dbl(1:n(), function(i) {
      stadium_i <- pd2[i, ht_cols]
      others <- pd2[-i, ht_cols]
      diffs <- sweep(others, 2, unlist(stadium_i), "-")  # directional differences
      sum(colSums(diffs))
    }),
    
    # Combined net change
    total_change = dist_change + ht_change
  ) |>
  arrange(total_change)

# Compare averages for reference
mean(comparison$total_change) # mean total absolute change for filtered stadiums
