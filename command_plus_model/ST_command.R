library(tidyverse)
library(purrr)
library(MASS) 
library(lubridate)
library(ggplot2)
library(viridis)
library(imager)
library(pracma)
library(KernSmooth)
library(parallel)
library(readr)

all_pbp <- readRDS("count_bases.rds")

#### st1 Data ####
years <- 2015:2022

st1 <- map_dfr(years, ~ {
  readRDS(paste0("all_pbp_", .x, ".rds")) |>
    filter(details.type.code == "ST") |>
    dplyr::select(
      game_pk, game_date, pitchNumber, details.description, 
      count.balls.start, count.strikes.start, count.outs.start, 
      last.pitch.of.ab, pitchData.strikeZoneTop, pitchData.strikeZoneBottom, 
      atBatIndex, result.event, result.eventType, result.description, 
      result.awayScore, result.homeScore, about.atBatIndex, 
      about.halfInning, about.inning, matchup.batter.id, 
      matchup.batter.fullName, matchup.batSide.code, matchup.pitcher.id, 
      matchup.pitcher.fullName, matchup.pitchHand.code, matchup.splits.batter, 
      matchup.splits.pitcher, home_team, away_team, batting_team, fielding_team, details.type.code,
      details.type.description, pitchData.coordinates.pX, pitchData.coordinates.pZ,
      matchup.postOnFirst.fullName, matchup.postOnSecond.fullName, matchup.postOnThird.fullName,
      hitData.trajectory, hitData.hardness, hitData.launchSpeed, hitData.launchAngle
    )
})

#fix counts
st1 <- left_join(st1, all_pbp)
st1 <- distinct(st1)

st1 <- st1 |>
  arrange(game_date, game_pk, about.atBatIndex, pitchNumber) |>
  mutate(
    last.pitch.of.ab = case_when(
      !is.na(last.pitch.of.ab) ~ last.pitch.of.ab,  # keep existing non-NA
      grepl("In play", details.description) ~ "true",
      count.strikes.start == 3 ~ "true",
      count.balls.start == 4 ~ "true"
    )
  ) |>
  mutate(last.pitch.of.ab = ifelse(is.na(last.pitch.of.ab), "false", last.pitch.of.ab)) |>
  mutate(result.event = ifelse(last.pitch.of.ab == "false", NA, result.event))

st1 <- st1 |>
  mutate(
    pitch_result = case_when(
      grepl("In play", details.description) ~ result.event,
      details.description == "Ball" & result.event == "Walk" ~ "Walk",
      grepl("Strike", details.description) & result.event == "Strikeout" ~ 
        paste0(details.description, "out"),
      TRUE ~ details.description
    )
  )

st1 <- st1 |>
  mutate(pitch_result = ifelse(pitch_result == "Swinging Strike (Blocked)out",
                               "Swinging Strikeout (Blocked)",
                               pitch_result))

st1 <- st1 |> 
  mutate(OnFirst = ifelse(!is.na(OnFirst), 1, 0),
         OnSecond = ifelse(!is.na(OnSecond), 1, 0),
         OnThird = ifelse(!is.na(OnThird), 1, 0),
         OnBase = ifelse(OnFirst + OnSecond + OnThird >= 1, 1, 0) 
  ) |> filter(pitch_result != "Intent Ball", pitch_result != "Pitchout",
              pitch_result != "Foul Pitchout")

#### st2 Data ####
# Load and prepare data
years <- 2023:2025

st2 <- map_dfr(years, ~ {
  readRDS(paste0("all_pbp_", .x, ".rds")) |>
    filter(details.type.code == "ST") |>
    dplyr::select(
      game_pk, game_date, pitchNumber, details.description, 
      count.balls.start, count.strikes.start, count.outs.start, 
      last.pitch.of.ab, pitchData.strikeZoneTop, pitchData.strikeZoneBottom, 
      atBatIndex, result.event, result.eventType, result.description, 
      result.awayScore, result.homeScore, about.atBatIndex, 
      about.halfInning, about.inning, matchup.batter.id, 
      matchup.batter.fullName, matchup.batSide.code, matchup.pitcher.id, 
      matchup.pitcher.fullName, matchup.pitchHand.code, matchup.splits.batter, 
      matchup.splits.pitcher, home_team, away_team, batting_team, fielding_team, details.type.code,
      details.type.description, pitchData.coordinates.pX, pitchData.coordinates.pZ,
      matchup.postOnFirst.fullName, matchup.postOnSecond.fullName, matchup.postOnThird.fullName,
      hitData.trajectory, hitData.hardness, hitData.launchSpeed, hitData.launchAngle
    )
})

st2 <- left_join(st2, all_pbp)
st2 <- distinct(st2)

st2 <- st2 |>
  arrange(game_date, game_pk, about.atBatIndex, pitchNumber) |>
  mutate(
    last.pitch.of.ab = case_when(
      !is.na(last.pitch.of.ab) ~ last.pitch.of.ab,  # keep existing non-NA
      grepl("In play", details.description) ~ "true",
      count.strikes.start == 3 ~ "true",
      count.balls.start == 4 ~ "true"
    )
  ) |>
  mutate(last.pitch.of.ab = ifelse(is.na(last.pitch.of.ab), "false", last.pitch.of.ab)) |>
  mutate(result.event = ifelse(last.pitch.of.ab == "false", NA, result.event))

st2 <- st2 |>
  mutate(
    pitch_result = case_when(
      grepl("In play", details.description) ~ result.event,
      details.description == "Ball" & result.event == "Walk" ~ "Walk",
      grepl("Strike", details.description) & result.event == "Strikeout" ~ 
        paste0(details.description, "out"),
      TRUE ~ details.description
    )
  )

st2 <- st2 |>
  mutate(pitch_result = ifelse(pitch_result == "Swinging Strike (Blocked)out",
                               "Swinging Strikeout (Blocked)",
                               pitch_result))

st2 <- st2 |> 
  mutate(OnFirst = ifelse(!is.na(OnFirst), 1, 0),
         OnSecond = ifelse(!is.na(OnSecond), 1, 0),
         OnThird = ifelse(!is.na(OnThird), 1, 0),
         OnBase = ifelse(OnFirst + OnSecond + OnThird >= 1, 1, 0) 
  ) |> filter(pitch_result != "Intent Ball", pitch_result != "Pitchout",
              pitch_result != "Foul Pitchout")

ST <- bind_rows(st1, st2)

saveRDS(ST, "base_ST.rds")
# ST <- readRDS("base_ST.rds")

ST <- ST |>
  mutate(game_date = as.Date(game_date),       # replace with your column name
         year = year(game_date)) 

ST <- ST |>
  rename(px = pitchData.coordinates.pX,
         pz = pitchData.coordinates.pZ,
         pitcher = matchup.pitcher.fullName)

ST <- ST |>
  dplyr::select(
    pitcher, matchup.pitcher.id, year, pitchData.strikeZoneTop, pitchData.strikeZoneBottom,
    pitch_result, count_before, OnBase, matchup.batSide.code, px, pz
  )

#create home plate (scaled to pitch location coords)
home_plate <- data.frame(
  x = c(0, -.8, -.7083, .7083, .8, 0),
  y = c(0, 0.3, 0.6, 0.6, 0.3, 0)
)

# Function to classify outcomes
classify_outcomes <- function(df) {
  df %>%
    mutate(
      outcome_type = case_when(
        # Any pitch within ±13.3 inches from center and within height bounds is automatically good
        (abs(px) <= 13.3/12) &
          (
            (pz >= (pitchData.strikeZoneBottom - 6/12) & pz <= pitchData.strikeZoneBottom) | 
            (pz >= pitchData.strikeZoneTop & pz <= (pitchData.strikeZoneTop + 3/12))   
      ) ~ "good",
        
        # Good outcomes
        pitch_result %in% c("Called Strike", "Swinging Strike", "Swinging Strike (Blocked)",
                            "Called Strikeout", "Swinging Strikeout", "Swinging Strikeout (Blocked)",
                            "Pop Out", "Forceout", "Flyout", "Groundout",
                            "Fielders Choice", "Lineout", "Field Error", "Fielders Choice Out",
                            "Grounded Into DP", "Bunt Lineout", "Bunt Groundout", "Bunt Pop Out",
                            "Double Play", "Foul Tip", "Missed Bunt", "Sac Bunt", "Sac Fly", 
                            "Sac Fly Double Play", "Triple Play", "Missed Bunt",
                            "Foul", "Foul Bunt") ~ "good",
        
        # Bad outcomes  
        pitch_result %in% c("Single", "Double", "Triple", "Home Run", 
                            "Hit By Pitch", "Ball", "Ball In Dirt", "Walk") ~ "bad",
        
        TRUE ~ "neutral"
      ),
      
      # Context classification
      count_type = case_when(
        count_before %in% c("1-0", "2-0", "3-0", "2-1", "3-1")  ~ "behind",
        count_before %in% c("1-1", "2-2", "0-0") ~ "even",
        count_before %in% c("0-1", "0-2", "1-2") ~ "ahead",
        count_before %in% c("3-2") ~ "full",
        TRUE ~ "other"
      ),
      
      # Base state
      base_state = case_when(
        OnBase == 0 ~ "empty",
        OnBase == 1 ~ "runners_on"
      )
    )
}

# Vectorized adaptive bandwidth calculation
calculate_adaptive_bandwidth_fast <- function(x, y, pilot_bandwidth = NULL, alpha = 0.5) {
  n <- length(x)
  
  # Step 1: Calculate pilot bandwidth if not provided
  if(is.null(pilot_bandwidth)) {
    pilot_h_x <- 1.06 * sd(x) * n^(-1/5)
    pilot_h_y <- 1.06 * sd(y) * n^(-1/5)
    pilot_bandwidth <- c(pilot_h_x, pilot_h_y)
  }
  
  # Step 2: Vectorized pilot density calculation using outer products
  x_matrix <- outer(x, x, "-") / pilot_bandwidth[1]
  y_matrix <- outer(y, y, "-") / pilot_bandwidth[2]
  
  # Vectorized Gaussian kernel calculation
  kernel_matrix <- exp(-(x_matrix^2 + y_matrix^2) / 2) / (2 * pi)
  pilot_densities <- rowMeans(kernel_matrix) / (pilot_bandwidth[1] * pilot_bandwidth[2])
  
  # Step 3: Calculate geometric mean (handle zeros)
  valid_densities <- pilot_densities[pilot_densities > 0]
  if(length(valid_densities) == 0) {
    geom_mean_density <- 1e-10
  } else {
    geom_mean_density <- exp(mean(log(valid_densities)))
  }
  
  # Step 4: Calculate adaptive factors
  adaptive_factors <- pmax((pilot_densities / geom_mean_density)^(-alpha), 0.1)
  
  # Step 5: Calculate adaptive bandwidths
  adaptive_h_x <- pilot_bandwidth[1] * adaptive_factors
  adaptive_h_y <- pilot_bandwidth[2] * adaptive_factors
  
  return(list(
    h_x = adaptive_h_x,
    h_y = adaptive_h_y,
    pilot_bandwidth = pilot_bandwidth,
    pilot_densities = pilot_densities,
    adaptive_factors = adaptive_factors
  ))
}

# Modified peak finding function - remove xlim/ylim constraints
find_adaptive_kde_peaks_fast <- function(kde_result, 
                                         min_height_pct = 0.4, 
                                         min_distance = 0.6, 
                                         max_peaks = 4) {
  
  if(is.null(kde_result)) return(tibble())
  
  x_grid <- kde_result$x
  y_grid <- kde_result$y  
  density_matrix <- kde_result$z
  
  max_density <- max(density_matrix, na.rm = TRUE)
  min_height <- max_density * min_height_pct
  
  # Use ALL grid indices (no xlim/ylim filtering)
  x_idx <- 2:(length(x_grid) - 1)  # Skip boundary indices
  y_idx <- 2:(length(y_grid) - 1)  # Skip boundary indices
  
  # Pre-allocate for speed
  peak_candidates <- vector("list", length(x_idx) * length(y_idx))
  peak_count <- 0
  
  # Vectorized local maxima finding
  for(i in y_idx) {
    for(j in x_idx) {
      current_density <- density_matrix[i, j]
      
      if(is.finite(current_density) && current_density >= min_height) {
        # Check 8-neighborhood efficiently
        neighborhood <- density_matrix[(i-1):(i+1), (j-1):(j+1)]
        
        if(current_density == max(neighborhood, na.rm = TRUE)) {
          peak_count <- peak_count + 1
          peak_candidates[[peak_count]] <- list(
            x = x_grid[j],
            z = y_grid[i],  # This should be y_grid[i], not z
            density = current_density
          )
        }
      }
    }
  }
  
  # Convert to tibble
  if(peak_count == 0) return(tibble(x = numeric(0), z = numeric(0), density = numeric(0)))
  
  peak_candidates <- peak_candidates[1:peak_count]
  peaks <- tibble(
    x = map_dbl(peak_candidates, "x"),
    z = map_dbl(peak_candidates, "z"),
    density = map_dbl(peak_candidates, "density")
  ) %>%
    arrange(desc(density)) %>%
    slice_head(n = min(max_peaks * 3, nrow(.)))
  
  if(nrow(peaks) <= 1) return(peaks)
  
  # Distance matrix approach for filtering
  distances <- as.matrix(dist(peaks[, c("x", "z")]))
  selected_idx <- c(1)  # Always keep the highest peak
  
  for(i in 2:nrow(peaks)) {
    min_dist_to_selected <- min(distances[i, selected_idx])
    if(min_dist_to_selected >= min_distance && length(selected_idx) < max_peaks) {
      selected_idx <- c(selected_idx, i)
    }
  }
  
  return(peaks[selected_idx, ])
}

# Modified adaptive KDE function - remove default xlim/ylim
create_adaptive_kde_fast <- function(x, y, grid_size = 50, xlim = NULL, ylim = NULL, alpha = 0.5) {
  
  # Remove NA values
  valid_idx <- !is.na(x) & !is.na(y)
  x <- x[valid_idx]
  y <- y[valid_idx]
  
  if(length(x) < 5) return(NULL)
  
  # Set grid limits with padding based on actual data range
  if(is.null(xlim)) xlim <- range(x) + c(-0.1, 0.1) * diff(range(x))
  if(is.null(ylim)) ylim <- range(y) + c(-0.1, 0.1) * diff(range(y))
  
  # Create evaluation grid
  x_grid <- seq(xlim[1], xlim[2], length.out = grid_size)
  y_grid <- seq(ylim[1], ylim[2], length.out = grid_size)
  
  # Calculate adaptive bandwidths once
  adaptive_bw <- calculate_adaptive_bandwidth_fast(x, y, alpha = alpha)
  
  # More memory-efficient approach: process grid in chunks
  chunk_size <- min(1000, grid_size * grid_size)
  grid_points <- expand.grid(x = x_grid, y = y_grid)
  n_chunks <- ceiling(nrow(grid_points) / chunk_size)
  
  density_vector <- numeric(nrow(grid_points))
  
  for(chunk in 1:n_chunks) {
    start_idx <- (chunk - 1) * chunk_size + 1
    end_idx <- min(chunk * chunk_size, nrow(grid_points))
    chunk_indices <- start_idx:end_idx
    
    # Process this chunk
    chunk_points <- grid_points[chunk_indices, ]
    
    # Vectorized distance calculations for this chunk
    x_diff <- outer(chunk_points$x, x, "-")
    y_diff <- outer(chunk_points$y, y, "-")
    
    # Normalize by adaptive bandwidths
    x_norm <- sweep(x_diff, 2, adaptive_bw$h_x, "/")
    y_norm <- sweep(y_diff, 2, adaptive_bw$h_y, "/")
    
    # Gaussian kernel
    kernel_vals <- exp(-(x_norm^2 + y_norm^2) / 2) / (2 * pi)
    
    # Normalize by bandwidths and average
    bandwidth_products <- outer(rep(1, nrow(chunk_points)), adaptive_bw$h_x * adaptive_bw$h_y)
    kernel_normalized <- kernel_vals / bandwidth_products
    
    density_vector[chunk_indices] <- rowMeans(kernel_normalized)
  }
  
  # Reshape to matrix
  density_matrix <- matrix(density_vector, nrow = grid_size, ncol = grid_size, byrow = TRUE)
  
  return(list(
    x = x_grid,
    y = y_grid,
    z = density_matrix,
    adaptive_info = adaptive_bw
  ))
}

# Fixed parallel processing version of main function
calculate_command_metrics_adaptive_kde_fast <- function(df, alpha = 0.5, min_pitches = 10, 
                                                        grid_size = 50, n_cores = 7) {
  
  df_classified <- classify_outcomes(df) %>%
    filter(!is.na(px), !is.na(pz), count_type != "other")
  
  # Step 1: Prepare grouped data
  good_outcomes <- df_classified %>%
    filter(outcome_type == "good") %>%
    group_by(pitcher, year, matchup.batSide.code, count_type) %>%
    nest() %>%
    filter(map_int(data, nrow) >= min_pitches)
  
  cat("Processing", nrow(good_outcomes), "pitcher-context combinations...\n")
  
  # Step 2: Parallel KDE computation
  if(n_cores > 1 && nrow(good_outcomes) > 10) {
    cl <- makeCluster(n_cores)
    
    clusterEvalQ(cl, {
      library(tidyverse)
    })
    
    clusterExport(cl, c("create_adaptive_kde_fast", "calculate_adaptive_bandwidth_fast",
                        "find_adaptive_kde_peaks_fast"), envir = environment())
    
    kde_results_list <- parLapply(cl, good_outcomes$data, function(coords_data) {
      coords <- coords_data %>% dplyr::select(px, pz)
      
      # No xlim/ylim constraints - use all data
      kde_result <- create_adaptive_kde_fast(coords$px, coords$pz, 
                                             grid_size = grid_size,
                                             alpha = alpha)
      
      if(is.null(kde_result)) return(list(kde = NULL, peaks = NULL))
      
      peaks <- find_adaptive_kde_peaks_fast(kde_result, min_height_pct = 0.4, 
                                            min_distance = 0.5, max_peaks = 5)
      
      list(kde = kde_result, peaks = peaks)
    })
    
    stopCluster(cl)
  } else {
    # Sequential processing
    pb <- progress_bar$new(total = nrow(good_outcomes))
    kde_results_list <- map(good_outcomes$data, function(coords_data) {
      pb$tick()
      coords <- coords_data %>% dplyr::select(px, pz)
      
      # No xlim/ylim constraints - use all data
      kde_result <- create_adaptive_kde_fast(coords$px, coords$pz, 
                                             grid_size = grid_size,
                                             alpha = alpha)
      
      if(is.null(kde_result)) return(list(kde = NULL, peaks = NULL))
      
      peaks <- find_adaptive_kde_peaks_fast(kde_result, min_height_pct = 0.4, 
                                            min_distance = 0.5, max_peaks = 5)
      
      list(kde = kde_result, peaks = peaks)
    })
  }
  
  # Rest of the function remains the same...
  if(length(kde_results_list) != nrow(good_outcomes)) {
    stop("Mismatch between number of groups and KDE results")
  }
  
  good_outcomes$kde <- map(kde_results_list, "kde")
  good_outcomes$peaks <- map(kde_results_list, "peaks")
  
  kde_list <- good_outcomes %>%
    filter(!map_lgl(kde, is.null), 
           !map_lgl(peaks, ~ is.null(.x) || nrow(.x) == 0))
  
  cat("Successfully processed", nrow(kde_list), "contexts\n")
  
  # Step 4: Vectorized distance calculation
  df_dist <- df_classified %>%
    left_join(kde_list %>% dplyr::select(-data, -kde), 
              by = c("pitcher", "year", "matchup.batSide.code", "count_type")) %>%
    filter(!map_lgl(peaks, ~ is.null(.x) || nrow(.x) == 0)) %>%
    rowwise() %>%
    mutate(
      distance_to_intent = {
        if(nrow(peaks) > 0) {
          distances <- sqrt((px - peaks$x)^2 + (pz - peaks$z)^2)
          min(distances, na.rm = TRUE)
        } else NA_real_
      }
    ) %>%
    ungroup() %>%
    filter(!is.na(distance_to_intent), is.finite(distance_to_intent))
  
  # Steps 5-6: Context and pitcher metrics
  context_metrics <- df_dist %>%
    group_by(pitcher, year, matchup.batSide.code, count_type) %>%
    summarise(
      total_pitches = n(),
      avg_distance = mean(distance_to_intent, na.rm = TRUE),
      distance_sd = sd(distance_to_intent, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(
      distance_sd = ifelse(is.na(distance_sd), 0, distance_sd),
      distance_command = 1 / (1 + avg_distance),
      consistency = 1 / (1 + distance_sd)
    )
  
  pitcher_metrics <- context_metrics %>%
    mutate(
      context_weight = case_when(
        count_type == "ahead" ~ 1.0,
        count_type == "even" ~ 0.7,
        count_type == "behind" ~ 0.4,
        count_type == "full" ~ 0.3,
        TRUE ~ 0.5
      )
    ) %>%
    group_by(pitcher, year) %>%
    summarise(
      total_pitches = sum(total_pitches),
      avg_distance_command = sum(distance_command * context_weight * total_pitches) / 
        sum(context_weight * total_pitches),
      avg_consistency = sum(consistency * context_weight * total_pitches) / 
        sum(context_weight * total_pitches),
      raw_command = 0.7 * avg_distance_command + 0.3 * avg_consistency,
      .groups = "drop"
    )
  
  return(list(
    pitcher_metrics = pitcher_metrics,
    kde_list = kde_list,
    context_metrics = context_metrics
  ))
}

# Visualization function to show multiple peaks
visualize_pitcher_multiPeak <- function(kde_results, df_classified, command_plus,
                                        pitcher_name, year_filter, context_type = "ahead") {
  
  # Get the pitcher's data
  pitcher_data <- df_classified %>%
    filter(pitcher == pitcher_name, year == year_filter, count_type == context_type)
  
  # Find the corresponding KDE and peaks
  kde_info <- kde_results$kde_list %>%
    filter(pitcher == pitcher_name, year == year_filter, count_type == context_type)
  
  if (nrow(kde_info) == 0) {
    stop("No KDE data found for this pitcher/year/context")
  }
  
  kde_result <- kde_info$kde[[1]]
  peaks <- kde_info$peaks[[1]]
  
  # --- Pull the right Command+ value for pitcher-year ---
  cmd_val <- command_plus %>%
    filter(pitcher == pitcher_name, year == year_filter) %>%
    pull(command_plus)
  if (length(cmd_val) == 0) cmd_val <- NA
  
  # Create visualization data
  kde_df <- expand.grid(x = kde_result$x, y = kde_result$y) %>%
    mutate(density = as.vector(kde_result$z))
  
  # Plot
  ggplot() +
    geom_tile(data = kde_df, aes(x = x, y = y, fill = density)) +
    scale_fill_viridis_c(option = "magma", alpha = 0.85, trans = "sqrt",
                         guide = guide_colorbar(title = "Density\n(sqrt scale)")) +
    geom_polygon(data = home_plate, aes(x = x, y = y),
                 fill = "gray70", color = "white") +
    geom_point(data = pitcher_data, aes(x = px, y = pz, color = outcome_type),
               alpha = 0.7, size = 2) +
    scale_color_manual(values = c("good" = "green", "bad" = "red", "neutral" = "white"),
                       name = "Pitch Outcome") +
    geom_point(data = peaks, aes(x = x, y = z),
               color = "cyan", size = 6, shape = 8, stroke = 2) +
    geom_text(data = peaks %>% mutate(peak_num = row_number()), 
              aes(x = x, y = z, label = paste0("Peak\n", round(density, 3))),
              color = "white", size = 3.5, fontface = "bold", vjust = -1) +
    {if(any(pitcher_data$px >= -0.83 & pitcher_data$px <= 0.83 & 
            pitcher_data$pz >= 1.5 & pitcher_data$pz <= 3.5, na.rm = TRUE)) {
      list(
        geom_rect(aes(xmin = -0.83, xmax = 0.83, ymin = 1.5, ymax = 3.5),
                  color = "white", fill = NA, linewidth = 1.5)
      )
    }} +
    coord_equal() +
    theme_minimal() +
    theme(
      panel.background = element_rect(fill = "black"),
      plot.background = element_rect(fill = "black"),
      text = element_text(color = "white"),
      axis.text = element_text(color = "white"),
      panel.grid = element_line(color = "gray30", size = 0.3),
      legend.background = element_rect(fill = "black"),
      legend.text = element_text(color = "white")
    ) +
    labs(
      title = paste0(pitcher_name, " - Command Heat Map with Intent Zones"),
      subtitle = paste(
        "Command+:", round(cmd_val),
        "| Year:", year_filter, 
        "| Context:", context_type, 
        "| Peaks:", nrow(peaks), 
        "| Total pitches:", nrow(pitcher_data)),
      x = "Horizontal Location (feet from center of plate)",
      y = "Height (feet above ground)",
      caption = "Cyan stars = Intent zones (peak density areas)"
    )
}


# Apply Command+ standardization to multi-peak KDE results 
apply_command_plus_multiPeak <- function(pitcher_metrics, 
                                         min_pitches = 50, 
                                         min_contexts = 2) { # Filter based on minimum pitch requirements 
  filtered <- pitcher_metrics %>% 
    filter(total_pitches >= min_pitches) # Calculate Command+ (standardized to league average) 
  
  command_plus <- filtered %>% 
    group_by(year) %>% 
    mutate(league_avg = mean(raw_command, na.rm = TRUE), 
           league_sd = sd(raw_command, na.rm = TRUE), 
           command_plus = 100 + (raw_command - league_avg) / league_sd * 15 ) %>% 
    ungroup() %>% 
    dplyr::select(pitcher, year, total_pitches, avg_distance_command, 
                  avg_consistency, raw_command, command_plus) %>% 
    arrange(desc(command_plus)) 
  
  return(command_plus) 
}

kde_results <- calculate_command_metrics_adaptive_kde_fast(
  ST, 
  grid_size = 75, 
  n_cores = detectCores() - 1,
  alpha = 0.5,
  min_pitches = 10
)

saveRDS(kde_results, "ST_kde.rds")
#kde_results <- readRDS("ST_kde.rds")

ST_classified <- classify_outcomes(ST)
pitcher_metrics_multiPeak <- kde_results$pitcher_metrics

saveRDS(pitcher_metrics_multiPeak, "ST_raw.rds")
#pitcher_metrics_multiPeak <- readRDS("ST_raw.rds")

kde_list_multiPeak <- kde_results$kde_list
command_plus <- apply_command_plus_multiPeak(pitcher_metrics_multiPeak, min_pitches = 100)
visualize_pitcher_multiPeak(kde_results, ST_classified, command_plus, "Garrett Crochet", 2025, "even")

rv <- read_csv("run_values.csv") |>
  filter(pitch_type == "ST")

test <- ST |> 
  mutate(in_zone = ifelse(px >= -(10/12) & px <= (10/12) & 
                            pz <= pitchData.strikeZoneTop & 
                            pz >= pitchData.strikeZoneBottom, 1, 0 )) 

test2 <- test |> 
  group_by(year, pitcher, matchup.pitcher.id) |> 
  summarise(total_pitches = n(), 
            in_zone_count = sum(in_zone, na.rm = TRUE), 
            izp = in_zone_count / total_pitches, .groups = "drop" )

test2 <- left_join(test2, rv)

# Merge the datasets 
correlation_data <- test2 |> 
  inner_join(command_plus_100, by = c("pitcher", "year")) |> 
  filter(!is.na(command_plus), !is.na(izp)) 

correlation_plot <- ggplot(correlation_data, 
                           aes(x = izp, 
                               y = command_plus)) +
  geom_point(alpha = 0.6, size = 2, color = "steelblue") + 
  geom_smooth(method = "lm", se = TRUE, color = "red", linewidth = 1) +
  
  # Add correlation coefficient 
  annotate("text", x = min(correlation_data$izp, na.rm = TRUE) + 
             0.05, y = max(correlation_data$command_plus, na.rm = TRUE) - 10,
           label = paste("Correlation =", round(cor(correlation_data$izp, 
                                                    correlation_data$command_plus, 
                                                    use = "complete.obs"), 3)), 
           size = 5, fontface = "bold") + 
  scale_x_continuous(labels = scales::percent_format(accuracy = 1)) + 
  labs(title = "Strike Zone Percentage vs Command+",
       subtitle = paste("n =", nrow(correlation_data), "pitcher-seasons"), 
       x = "In-Zone Percentage", 
       y = "Command+ Score", 
       caption = "Strike zone defined as 20 inches wide, batter-specific height" ) + 
  theme_minimal(base_size = 12) + 
  theme(plot.title = element_text(size = 14, face = "bold"), 
        panel.grid.minor = element_blank()) 


print(correlation_plot) 

# Summary statistics 
cat("Correlation Summary:\n") 
cat("Correlation coefficient:", 
    round(cor(correlation_data$izp, 
              correlation_data$command_plus, 
              use = "complete.obs"), 4), "\n") 
cat("Sample size:", nrow(correlation_data), "pitcher-seasons\n")



correlation_plot <- ggplot(correlation_data, 
                           aes(x = run_value, 
                               y = command_plus)) +
  geom_point(alpha = 0.6, size = 2, color = "steelblue") + 
  geom_smooth(method = "lm", se = TRUE, color = "red", linewidth = 1) +
  
  # Add correlation coefficient 
  annotate("text", 
           x = min(correlation_data$run_value, na.rm = TRUE) + 0.05, 
           y = max(correlation_data$command_plus, na.rm = TRUE) - 10,
           label = paste("Correlation =", round(cor(correlation_data$run_value, 
                                                    correlation_data$command_plus, 
                                                    use = "complete.obs"), 3)), 
           size = 5, fontface = "bold") + 
  labs(title = "Run Value vs Command+",
       subtitle = paste("n =", nrow(correlation_data), "pitcher-seasons"), 
       x = "Run Value", 
       y = "Command+ Score", 
       caption = "Strike zone defined as 20 inches wide, batter-specific height") + 
  theme_minimal(base_size = 12) + 
  theme(plot.title = element_text(size = 14, face = "bold"), 
        panel.grid.minor = element_blank()) 


print(correlation_plot) 

# Summary statistics 
cat("Correlation Summary:\n") 
cat("Correlation coefficient:", 
    round(cor(correlation_data$run_value, 
              correlation_data$command_plus, 
              use = "complete.obs"), 4), "\n") 
cat("Sample size:", nrow(correlation_data), "pitcher-seasons\n")


