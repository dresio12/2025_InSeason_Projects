library(dplyr)
library(readr)
library(lubridate)
library(tidyverse)

# Vector of pitch types
types <- c("CH", "CS", "CU", "FA", "FF", "FC", "FO", "FS",
           "EP", "KC", "KN", "SC", "SI", "SL", "ST", "SV")

# Initialize list to store processed pitch data
pitch_list <- list()

for(p in types) {
  # Build file names
  data_file <- paste0("base_", p, ".rds")
  comm_file <- paste0(p, "_raw.rds")
  
  # Load data
  df <- readRDS(data_file)
  df_comm <- readRDS(comm_file)
  
  # Process
  df <- df %>%
    rename(pitcher = matchup.pitcher.fullName) %>%
    mutate(game_date = as.Date(game_date),
           year = year(game_date)) %>%
    group_by(year, pitcher) %>%
    summarise(pitches = n(), .groups = "drop") %>%
    left_join(df_comm, by = c("pitcher", "year")) %>%   # assuming df_comm has 'pitcher' column
    select(-total_pitches) %>%
    mutate(pitch_type = p)
  
  # Store in list
  pitch_list[[p]] <- df
}

# Combine all pitch types into one big dataframe
all_pitch_data <- bind_rows(pitch_list) 

all_pitch_data <- all_pitch_data |>
  rename(
    Pitcher = pitcher,
    Year = year
  )

saveRDS(all_pitch_data, "command_pitch_data.rds")

all_pitch_data <- readRDS("command_pitch_data.rds")


compute_command_plus_precomputed <- function(all_pitch_data,
                                             min_pitchers_per_type = 5,
                                             min_pitches = 100) {
  
  # --- 1) Count pitchers per pitch type per year ---
  pitch_type_counts <- all_pitch_data %>%
    group_by(Year, pitch_type) %>%
    summarise(n_pitchers_type = n_distinct(Pitcher), .groups = "drop")
  
  df <- all_pitch_data %>%
    left_join(pitch_type_counts, by = c("Year", "pitch_type"))
  
  # --- 2) Per-pitch-type Command+ (with fallback for rare types) ---
  df <- df %>%
    group_by(Year, pitch_type) %>%
    mutate(
      league_avg_type = mean(raw_command, na.rm = TRUE),
      league_sd_type  = sd(raw_command, na.rm = TRUE),
      command_plus_type = ifelse(
        n_pitchers_type >= min_pitchers_per_type & league_sd_type > 0,
        100 + (raw_command - league_avg_type) / league_sd_type * 15,
        NA_real_
      ),
      rare_pitch_type = n_pitchers_type < min_pitchers_per_type
    ) %>%
    ungroup() %>%
    group_by(Year) %>%
    mutate(
      league_avg_all = mean(raw_command, na.rm = TRUE),
      league_sd_all  = sd(raw_command, na.rm = TRUE),
      command_plus_fallback = ifelse(
        !is.na(command_plus_type),
        command_plus_type,
        ifelse(!is.na(league_sd_all) & league_sd_all > 0,
               100 + (raw_command - league_avg_all) / league_sd_all * 15,
               NA_real_)
      )
    ) %>%
    ungroup()
  
  # --- 3) Overall leaderboard (fallback) ---
  leaderboard_fallback <- df %>%
    group_by(Pitcher, Year) %>%
    summarise(
      total_pitches = sum(pitches, na.rm = TRUE),
      avg_command_plus_fallback = ifelse(total_pitches > 0,
                                         sum(command_plus_fallback * pitches, na.rm = TRUE) / total_pitches,
                                         NA_real_),
      pct_rare_type = ifelse(total_pitches > 0,
                             sum(pitches[rare_pitch_type], na.rm = TRUE) / total_pitches,
                             0),
      pct_low_sample = ifelse(total_pitches > 0,
                              sum(pitches[pitches < min_pitches], na.rm = TRUE) / total_pitches,
                              0),
      .groups = "drop"
    )
  
  # --- 4) Leaderboard: omission ---
  df_omission_rows <- df %>%
    filter(!is.na(command_plus_type) & pitches >= min_pitches)
  
  leaderboard_omission <- df_omission_rows %>%
    group_by(Pitcher, Year) %>%
    summarise(
      total_pitches_omitted = sum(pitches, na.rm = TRUE),
      avg_command_plus_omission = ifelse(total_pitches_omitted > 0,
                                         sum(command_plus_type * pitches, na.rm = TRUE) / total_pitches_omitted,
                                         NA_real_),
      .groups = "drop"
    )
  
  # --- 5) FB / BRK / OFF groups ---
  pitch_groups <- list(
    FB  = c("FF", "FA", "FC", "SI", "FT"),
    BRK = c("SL", "CU", "KC", "KN", "SC", "CS", "EP", "ST", "SV"),
    OFF = c("CH", "FS", "FO", "SC")
  )
  
  df_groups <- df %>%
    mutate(
      pitch_group = case_when(
        pitch_type %in% pitch_groups$FB  ~ "FB",
        pitch_type %in% pitch_groups$BRK ~ "BRK",
        pitch_type %in% pitch_groups$OFF ~ "OFF",
        TRUE ~ "OTHER"
      )
    ) %>%
    group_by(Pitcher, Year, pitch_group) %>%
    summarise(
      group_pitches = sum(pitches, na.rm = TRUE),
      avg_command_plus_group = ifelse(group_pitches > 0,
                                      sum(command_plus_fallback * pitches, na.rm = TRUE) / group_pitches,
                                      NA_real_),
      .groups = "drop"
    ) %>%
    tidyr::pivot_wider(names_from = pitch_group,
                       values_from = avg_command_plus_group,
                       names_prefix = "grp_")
  
  # --- 6) Pivot per-pitch-type Command+ into leaderboard columns ---
  df_pitchtype_cols <- df %>%
    select(Pitcher, Year, pitch_type, command_plus_fallback) %>%
    tidyr::pivot_wider(names_from = pitch_type,
                       values_from = command_plus_fallback,
                       names_prefix = "cmd_")
  
  # --- 7) Assemble final leaderboard ---
  leaderboard <- leaderboard_fallback %>%
    left_join(leaderboard_omission, by = c("Pitcher", "Year")) %>%
    left_join(df_groups, by = c("Pitcher", "Year")) %>%
    left_join(df_pitchtype_cols, by = c("Pitcher", "Year")) %>%
    arrange(desc(avg_command_plus_fallback))
  
  # --- 8) Return precomputed tables ---
  list(
    per_pitch = df,          # detailed per-pitch rows for subrows in app
    leaderboard = leaderboard # main leaderboard (fallback + omission + FB/BRK/OFF + per-pitch)
  )
}

all_pitches <- compute_command_plus_precomputed(all_pitch_data)

view(all_pitches$leaderboard)
view(all_pitches$per_pitch)


build_leaderboards <- function(all_pitch_data_precomputed) {
  
  df <- all_pitch_data_precomputed$per_pitch
  
  df <- df %>%
    mutate(cmd_use = ifelse(!is.na(command_plus_type), command_plus_type, command_plus_fallback))
  
  pitch_types_main <- c("FF","SI","FC","FS","CU","KC","SL","ST","SV","CH","FO")
  pitch_types_extra <- c("SC","KN","EP")
  
  df_pitch_agg <- df %>%
    group_by(Pitcher, Year, pitch_type) %>%
    summarise(
      cmd_use = weighted.mean(cmd_use, pitches, na.rm = TRUE),
      total_pitches_type = sum(pitches, na.rm = TRUE),
      .groups = "drop"
    )
  
  df_pitch_wide <- df_pitch_agg %>%
    select(Pitcher, Year, pitch_type, cmd_use) %>%
    tidyr::pivot_wider(
      names_from = pitch_type,
      values_from = cmd_use,
      names_prefix = "Cmd+ "
    )
  
  pitch_groups <- list(
    FB  = c("FF","FA","FC","SI","FT"),
    BRK = c("SL","CU","KC","KN","SC","CS","EP","ST","SV"),
    OFF = c("CH","FS","FO","SC")
  )
  
  df_groups <- df %>%
    mutate(
      pitch_group = case_when(
        pitch_type %in% pitch_groups$FB  ~ "FB",
        pitch_type %in% pitch_groups$BRK ~ "BRK",
        pitch_type %in% pitch_groups$OFF ~ "OFF",
        TRUE ~ NA_character_
      )
    ) %>%
    filter(!is.na(pitch_group)) %>%
    group_by(Pitcher, Year, pitch_group) %>%
    summarise(cmd_group = weighted.mean(cmd_use, pitches, na.rm = TRUE), .groups = "drop") %>%
    tidyr::pivot_wider(
      names_from = pitch_group,
      values_from = cmd_group,
      names_prefix = "Cmd+ "
    )
  
  df_overall <- df %>%
    group_by(Pitcher, Year) %>%
    summarise(
      total_pitches = sum(pitches, na.rm = TRUE),
      `Cmd+ Overall` = ifelse(total_pitches > 0,
                              sum(cmd_use * pitches, na.rm = TRUE)/total_pitches,
                              NA_real_),
      .groups = "drop"
    )
  
  pitch_type_counts <- df %>%
    group_by(Pitcher, Year, pitch_type) %>%
    summarise(pitches = sum(pitches, na.rm = TRUE), .groups = "drop") %>%
    tidyr::pivot_wider(names_from = pitch_type, values_from = pitches, values_fill = 0)
  
  df_group_totals <- df %>%
    mutate(
      pitch_group = case_when(
        pitch_type %in% pitch_groups$FB  ~ "Fastball",
        pitch_type %in% pitch_groups$BRK ~ "Breaking",
        pitch_type %in% pitch_groups$OFF ~ "Offspeed",
        TRUE ~ NA_character_
      )
    ) %>%
    filter(!is.na(pitch_group)) %>%
    group_by(Pitcher, Year, pitch_group) %>%
    summarise(pitches = sum(pitches, na.rm = TRUE), .groups = "drop") %>%
    tidyr::pivot_wider(names_from = pitch_group, values_from = pitches, values_fill = 0)
  
  pitch_type_counts <- pitch_type_counts %>%
    left_join(df_group_totals, by = c("Pitcher","Year"))
  
  pitch_types_main_prefixed <- paste0("Cmd+ ", pitch_types_main)
  pitch_types_extra_prefixed <- paste0("Cmd+ ", pitch_types_extra)
  
  leaderboard <- df_overall %>%
    left_join(df_pitch_wide %>% select(Year, Pitcher, all_of(pitch_types_main_prefixed)), by = c("Pitcher","Year")) %>%
    left_join(df_groups, by = c("Year","Pitcher")) %>%
    rename(`Cmd+ Fastball` = `Cmd+ FB`, `Cmd+ Breaking` = `Cmd+ BRK`, `Cmd+ Offspeed` = `Cmd+ OFF`)
  
  leaderboard_ext <- leaderboard %>%
    left_join(df_pitch_wide %>% select(Pitcher, Year, all_of(pitch_types_extra_prefixed)), by = c("Pitcher","Year")) %>%
    select(
      Year, Pitcher, total_pitches,
      all_of(paste0("Cmd+ ", pitch_types_main)),
      all_of(pitch_types_extra_prefixed),
      `Cmd+ Fastball`, `Cmd+ Breaking`, `Cmd+ Offspeed`,
      `Cmd+ Overall`
    )
  
  final_cols <- c("Cmd+ Overall", "Cmd+ Fastball", "Cmd+ Breaking", "Cmd+ Offspeed")
  
  leaderboard <- leaderboard %>% select(all_of(setdiff(names(leaderboard), final_cols)), all_of(final_cols))
  leaderboard_ext <- leaderboard_ext %>% select(all_of(setdiff(names(leaderboard_ext), final_cols)), all_of(final_cols))
  
  leaderboard <- leaderboard |> rename(Pitches = total_pitches)
  leaderboard_ext <- leaderboard_ext |> rename(Pitches = total_pitches)
  
  # Reorder columns so Year then Pitcher are first
  leaderboard <- leaderboard %>%
    select(Year, Pitcher, everything())
  
  leaderboard_ext <- leaderboard_ext %>%
    select(Year, Pitcher, everything())
  
  
  list(
    leaderboard = leaderboard,
    leaderboard_ext = leaderboard_ext,
    pitch_type_counts = pitch_type_counts
  )
}


# --- Example usage ---
leaderboards <- build_leaderboards(all_pitches)

View(leaderboards$leaderboard)
View(leaderboards$leaderboard_ext)
View(leaderboards$pitch_type_counts)

saveRDS(leaderboards, "cmd_leaderboard.rds")
