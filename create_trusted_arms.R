#load dfs

ta2025 <- read_csv("ta2025.csv") |>
  mutate(game_date = as.Date(game_date, format = "%m/%d/%Y"))
ta20252 <- read_csv("ta20252.csv") |>
  mutate(game_date = as.Date(game_date, format = "%m/%d/%Y"))
ta2024 <- read_csv("ta2024.csv") |>
  mutate(game_date = as.Date(game_date, format = "%m/%d/%Y"))
ta20242 <- read_csv("ta20242.csv") |>
  mutate(game_date = as.Date(game_date, format = "%m/%d/%Y"))
ta2023 <- read_csv("ta2023.csv") |>
  mutate(game_date = as.Date(game_date, format = "%m/%d/%Y"))
ta20232 <- read_csv("ta20232.csv") |>
  mutate(game_date = as.Date(game_date, format = "%m/%d/%Y"))
ta2022 <- read_csv("ta2022.csv") |>
  mutate(game_date = as.Date(game_date, format = "%m/%d/%Y"))
ta20222 <- read_csv("ta20222.csv") |>
  mutate(game_date = as.Date(game_date, format = "%m/%d/%Y"))
ta2021 <- read_csv("ta2021.csv") |>
  mutate(game_date = as.Date(game_date, format = "%m/%d/%Y"))
ta20212 <- read_csv("ta20212.csv") |>
  mutate(game_date = as.Date(game_date, format = "%m/%d/%Y"))
ta2020 <- read_csv("ta2020.csv") |>
  mutate(game_date = as.Date(game_date, format = "%m/%d/%Y"))
ta20202 <- read_csv("ta20202.csv") |>
  mutate(game_date = as.Date(game_date, format = "%m/%d/%Y"))


#join dfs
ta2025 <- left_join(ta20252,ta2025, by = c("playerid", "player_name", "game_date", "game_pk", "season"))

ta2025 <- ta2025 |>
  mutate(pitches.y = ifelse(is.na(pitches.y), 0 , pitches.y),
         total_pitches.y = ifelse(is.na(total_pitches.y), 0 , total_pitches.y),
         pitch_percent.y = ifelse(is.na(pitch_percent.y), 0 , pitch_percent.y)) 

ta2025 <- ta2025 |>
  mutate(pitches = pitches.y,
         total_pitches = pitches.x,
         pitch_percent = pitches / total_pitches) |>
  select(8, 2:5, 12:14)

ta2024 <- left_join(ta20242, ta2024, by = c("playerid", "player_name", "game_date", "game_pk", "season"))

ta2024 <- ta2024 |>
  mutate(pitches.y = ifelse(is.na(pitches.y), 0 , pitches.y),
         total_pitches.y = ifelse(is.na(total_pitches.y), 0 , total_pitches.y),
         pitch_percent.y = ifelse(is.na(pitch_percent.y), 0 , pitch_percent.y)) 

ta2024 <- ta2024 |>
  mutate(pitches = pitches.y,
         total_pitches = pitches.x,
         pitch_percent = pitches / total_pitches) |>
  select(8, 2:5, 12:14)

ta2023 <- left_join(ta20232, ta2023, by = c("playerid", "player_name", "game_date", "game_pk", "season"))

ta2023 <- ta2023 |>
  mutate(pitches.y = ifelse(is.na(pitches.y), 0 , pitches.y),
         total_pitches.y = ifelse(is.na(total_pitches.y), 0 , total_pitches.y),
         pitch_percent.y = ifelse(is.na(pitch_percent.y), 0 , pitch_percent.y)) 

ta2023 <- ta2023 |>
  mutate(pitches = pitches.y,
         total_pitches = pitches.x,
         pitch_percent = pitches / total_pitches) |>
  select(8, 2:5, 12:14)

ta2022 <- left_join(ta20222, ta2022, by = c("playerid", "player_name", "game_date", "game_pk", "season"))

ta2022 <- ta2022 |>
  mutate(pitches.y = ifelse(is.na(pitches.y), 0 , pitches.y),
         total_pitches.y = ifelse(is.na(total_pitches.y), 0 , total_pitches.y),
         pitch_percent.y = ifelse(is.na(pitch_percent.y), 0 , pitch_percent.y)) 

ta2022 <- ta2022 |>
  mutate(pitches = pitches.y,
         total_pitches = pitches.x,
         pitch_percent = pitches / total_pitches) |>
  select(8, 2:5, 12:14)

ta2021 <- left_join(ta20212, ta2021, by = c("playerid", "player_name", "game_date", "game_pk", "season"))

ta2021 <- ta2021 |>
  mutate(pitches.y = ifelse(is.na(pitches.y), 0 , pitches.y),
         total_pitches.y = ifelse(is.na(total_pitches.y), 0 , total_pitches.y),
         pitch_percent.y = ifelse(is.na(pitch_percent.y), 0 , pitch_percent.y)) 

ta2021 <- ta2021 |>
  mutate(pitches = pitches.y,
         total_pitches = pitches.x,
         pitch_percent = pitches / total_pitches) |>
  select(8, 2:5, 12:14)

ta2020 <- left_join(ta20202, ta2020, by = c("playerid", "player_name", "game_date", "game_pk", "season"))

ta2020 <- ta2020 |>
  mutate(pitches.y = ifelse(is.na(pitches.y), 0 , pitches.y),
         total_pitches.y = ifelse(is.na(total_pitches.y), 0 , total_pitches.y),
         pitch_percent.y = ifelse(is.na(pitch_percent.y), 0 , pitch_percent.y)) 

ta2020 <- ta2020 |>
  mutate(pitches = pitches.y,
         total_pitches = pitches.x,
         pitch_percent = pitches / total_pitches) |>
  select(8, 2:5, 12:14)

trusted_arms <- bind_rows(ta2020, ta2021, ta2022, ta2023, ta2024, ta2025) |>
  arrange(game_date)

saveRDS(trusted_arms, "trusted_arms.rds")
