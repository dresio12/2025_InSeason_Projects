library(tidyverse)
library(baseballr)
library(progress)
library(purrr)
library(readr)


game_ids <- baseballr::mlb_schedule(season = 2025, level_ids = "1") |>
  filter(game_type == "R" & !is.na(is_tie))

# Function to standardize column types 
standardize_column_types <- function(df) {
  df %>%
    mutate(
      game_date = as.Date(game_date),
      startTime = as.character(startTime), # or as.POSIXct if it's a datetime
      game_pk = as.integer(game_pk),
      # Add more columns here, depending on what you know might be misbehaving
    )
}

#function to identify game_pks in game_ids df, combine into single df
get_pbp_for_games <- function(game_ids, save_path = "pbp_progress.rds") {
  # Load existing data if the save file exists
  if (file.exists(save_path)) {
    message("Loading existing saved progress...")
    pbp_combined <- readRDS(save_path)  # Change to readRDS
    
    # Identify already pulled game_pks
    pulled_game_pks <- unique(pbp_combined$game_pk)
  } else {
    pbp_combined <- tibble()
    pulled_game_pks <- c()
  }
  
  game_pks <- game_ids$game_pk
  
  # Only pull game_pks that have NOT been pulled yet
  game_pks_to_pull <- setdiff(game_pks, pulled_game_pks)
  
  message(length(game_pks_to_pull), " game(s) to pull...")
  
  for (pk in game_pks_to_pull) {
    message("Pulling game_pk: ", pk)
    
    pbp_data <- possibly(mlb_pbp, otherwise = NULL)(pk)
    
    if (!is.null(pbp_data)) {
      
      # Bind the rows
      pbp_combined <- bind_rows(pbp_combined, pbp_data)
      
      # Save the current progress after each successful pull
      saveRDS(pbp_combined, save_path)  # Save as RDS
      
      message("Saved progress after game_pk: ", pk)
    } else {
      message("No data for game_pk: ", pk)
    }
  }
  
  return(pbp_combined)
}


# Use function to generate pbp df
get_pbp_for_games(game_ids, save_path = "pbp_progress.rds")


  