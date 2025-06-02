library(tidyverse)
library(baseballr)
library(ggplot2)
library(mlbplotR)
library(stringr)
library(tidyr)

#load in csv
pbp <- readRDS("pbp_progress.rds")

#change to numeric
pbp$atBatIndex <- as.numeric(pbp$atBatIndex)

#add pitch_result column
pbp <- pbp |>
  mutate(pitch_result = case_when(
    details.description %in% c("In play, out(s)") ~ "In Play",
    details.description %in% c("In play, no out") ~ "In Play",
    details.description %in% c("In play, run(s)") ~ "In Play",
    details.description %in% c("Called Strike", "Swinging Strike", "Foul", "Foul Tip",
                               "Swinging Strike (Blocked)", "Foul Bunt", "Missed Bunt",
                               "Automatic Strike - Batter Timeout Violation",
                               "Automatic Strike - Batter Pitch Timer Violation") ~ "Strike",
    TRUE ~ "Ball"
  )) 

pbp <- pbp %>%
  mutate(
    result.event = ifelse(details.description == "Automatic Ball - Intentional" & 
                            result.event != "Intent Walk", 
                          "Intent Walk", 
                          result.event
    )
  )

#change to numeric
pbp$atBatIndex <- as.numeric(pbp$atBatIndex)

pbp <- pbp |>
  mutate(
    atBatIndex = ifelse(result.event == "Intent Walk" & result.eventType != "intent_walk", 
                        atBatIndex - 1, 
                        atBatIndex
    )
  )


# Go backward through the rows
for (i in nrow(pbp):1) {
  if (pbp$details.description[i] == "Automatic Ball - Intentional") {
    
    correct_idx <- pbp$atBatIndex[i]
    
    # Walk backwards to fix earlier rows with bad atBatIndex
    j <- i - 1
    while (j > 0 && pbp$atBatIndex[j] > correct_idx) {
      pbp$atBatIndex[j] <- correct_idx
      pbp$result.event[j] <- "Intent Walk"
      j <- j - 1
    }
  }
}

pbp <- pbp %>%
  mutate(
    result.eventType = ifelse(result.event == "Intent Walk", 
                              "intent_walk", 
                              result.eventType
    )
  )

pbp$atBatIndex[c(120414:120416)] <- 31
pbp <- pbp[-6750, ]

# Create True Count
pbp <- pbp |>
  group_by(game_pk, atBatIndex) |>
  mutate(
    is_first_pitch = row_number() == 1,
    prev_ball = if_else(is_first_pitch, 0L, lag(count.balls.start)),
    prev_strike = if_else(is_first_pitch, 0L, lag(count.strikes.start)),
    pitchCount = paste0(prev_ball, "-", prev_strike)
  ) |>
  ungroup()


pbp <- pbp %>%
  mutate(
    batter_out = case_when(
      result.event %in% c("Sac Bunt", "Fielders Choice Out") & pitch_result == "In Play" & (
        (!is.na(matchup.postOnFirst.fullName) & matchup.batter.fullName == matchup.postOnFirst.fullName) |
          (!is.na(matchup.postOnSecond.fullName) & matchup.batter.fullName == matchup.postOnSecond.fullName) |
          (!is.na(matchup.postOnThird.fullName) & matchup.batter.fullName == matchup.postOnThird.fullName)
      ) ~ "Other",
      
      result.event %in% c("Sac Bunt", "Fielders Choice Out") & pitch_result == "In Play" ~ "Out",
      
      TRUE ~ NA_character_
    )
  )


pbp <- pbp |>
  mutate(
    specific_pitch_result = case_when(
      pitch_result != "In Play" ~ NA_character_,
      result.event == "Single" ~ "Single",
      result.event == "Double" ~ "Double",
      result.event == "Triple" ~ "Triple",
      result.event == "Home Run" ~ "Home Run",
      result.event %in% c(
        "Groundout", "Flyout", "Pop Out", "Lineout", "Double Play", "Sac Fly", 
        "Bunt Groundout", "Sac Fly Double Play", "Triple Play", "Bunt Pop Out", 
        "Bunt Lineout") ~ "Out",
      result.event %in% c("Field Error", "Fielders Choice", "Catcher Interference", "Forceout") ~ "Other Batter Reach",
      result.event == "Sac Bunt" & batter_out == "Out"  ~ "Out",
      result.event == "Sac Bunt" & batter_out == "Other" ~ "Other Batter Reach",
      result.event == "Fielders Choice Out" & batter_out == "Out"  ~ "Out",
      result.event == "Fielders Choice Out" & batter_out == "Other" ~ "Other Batter Reach",
      TRUE ~ "Other Batter Reach"  # fallback 
    )
  )

pbp <- pbp |>
  mutate(
    bip_type = case_when(
      pitch_result != "In Play" ~ NA_character_,
      hitData.trajectory %in% c("ground_ball", "bunt_grounder") ~ "Groundball",
      hitData.trajectory %in% c("line_drive", "bunt_line_drive") ~ "Linedrive",
      hitData.trajectory %in% c("fly_ball") ~ "Flyball",
      hitData.trajectory %in% c("popup", "bunt_popup") ~ "Pop Up",
      TRUE ~ "Catcher Interference"
    )
  )

pbp <- pbp |>
  mutate(
    specific_pitch_result = ifelse(
      is.na(specific_pitch_result) & 
        details.call.description %in% c(
        "Swinging Strike", "Foul Tip", "Swinging Strike (Blocked)",
          "Missed Bunt" ), "Swinging Strike", specific_pitch_result
    ),
    specific_pitch_result = ifelse(
      is.na(specific_pitch_result) & 
        details.call.description %in% c(
          "Called Strike", "Automatic Strike - Batter Pitch Timer Violation", 
          "Automatic Strike - Batter Timeout Violation"), 
      "Called Strike", specific_pitch_result
    ),
    specific_pitch_result = ifelse(
      is.na(specific_pitch_result) & 
        details.call.description %in% c("Foul", "Foul Bunt"), 
      "Foul", specific_pitch_result
    ),
    specific_pitch_result = ifelse(
      is.na(specific_pitch_result) & 
        details.call.description %in% c(
          "Ball", "Ball In Dirt", "Pitchout", "Automatic Ball - Pitcher Pitch Timer Violation",
          "Automatic Ball - Intentional",  "Automatic Ball",
          "Automatic Ball - Catcher Pitch Timer Violation"), 
      "Ball", specific_pitch_result
    ),
    specific_pitch_result = ifelse(
      is.na(specific_pitch_result) & 
        details.call.description %in% c(
          "Hit By Pitch"), 
      "Hit By Pitch", specific_pitch_result
    )
  )

pbp <- pbp |>
  mutate(
    bip_type = ifelse(pitch_result == "Strike", 
                      pitch_result, 
                      bip_type),
    bip_type = ifelse(pitch_result == "Ball", 
                      pitch_result, 
                      bip_type)
  )


saveRDS(pbp, "zone_pbp.rds")
