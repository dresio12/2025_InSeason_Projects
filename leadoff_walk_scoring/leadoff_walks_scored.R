library(tidyverse)
library(baseballr)
library(ggplot2)
library(mlbplotR)

#load in csv
pbp <- readRDS("pbp_progress.rds")

#organize data to be first pitch to last
pbp <- pbp |>
  filter(!is.na(pitchNumber)) 

pbp$atBatIndex <- as.numeric(pbp$atBatIndex)

pbp <- pbp |>
  arrange(game_date, game_pk, atBatIndex, pitchNumber)


# Identify the first event of each half inning
first_event_in_inning <- pbp |>
  group_by(game_pk, about.inning, about.halfInning) |>
  mutate(row_num = row_number()) |>
  filter(row_num == 1) |>  # Get the first event (smallest row_num) in each half inning
  ungroup()

# Filter leadoff walks by checking the first event
# Excludes Extra Innings
leadoff_walks <- first_event_in_inning |>
  filter(result.event == "Walk" | result.event == "Intent Walk") 

#pull rows within half-inning after the leadoff walk
lbb_with_events <- pbp |>
  semi_join(leadoff_walks, by = c("game_pk", "about.inning", "about.halfInning")) |>
  arrange(game_pk, about.inning, about.halfInning, atBatIndex) |>
  group_by(game_pk, about.inning, about.halfInning) |>
  filter(atBatIndex >= first(atBatIndex)) |>
  ungroup() |>
  arrange(game_date, game_pk, atBatIndex, pitchNumber)


team_runs_allowed <- lbb_with_events |>
  group_by(fielding_team, game_pk, about.inning, about.halfInning) |>
  summarize(
    runs_allowed = if_else(
      unique(fielding_team) == unique(home_team),
      max(result.awayScore) - min(result.awayScore),
      max(result.homeScore) - min(result.homeScore)
    ),
    .groups = "drop"
  ) |>
  group_by(fielding_team) |>
  summarize(total_runs_allowed = sum(runs_allowed), .groups = "drop")

team_leadoff_walks <- lbb_with_events |>
  select(1, 2, 46:48, 50, 52:56, 72, 73, 81, 88, 96) |>
  unique() |>
  group_by(fielding_team, game_pk, about.inning, about.halfInning) |>
  slice_min(order_by = atBatIndex, n = 1) |>  # get first event per half-inning
  filter(result.event == "Walk") |>
  ungroup() |>
  count(fielding_team, name = "total_leadoff_walks")

league_summary <- team_leadoff_walks |>
  inner_join(team_runs_allowed, by = "fielding_team")

league_averages <- league_summary |>
  summarise(
    average_leadoff_walks = mean(total_leadoff_walks, na.rm = TRUE),
    average_runs_allowed = mean(total_runs_allowed, na.rm = TRUE)
  )

# Step 1: For each leadoff walk inning, calculate if a run scored
lbb_runs_summary <- lbb_with_events |>
  group_by(game_pk, about.inning, about.halfInning, fielding_team) |>
  summarise(
    runs_scored = if (first(about.halfInning) == "top") {
      max(result.awayScore) - min(result.awayScore)
    } else {
      max(result.homeScore) - min(result.homeScore)
    },
    .groups = "drop"
  ) |>
  mutate(
    run_scored_flag = ifelse(runs_scored > 0, 1, 0)
  )

# Step 2: Calculate % of leadoff walk innings where at least one run scored
team_run_score_rate <- lbb_runs_summary |>
  group_by(fielding_team) |>
  summarise(
    total_leadoff_walk_innings = n(),
    innings_with_run_scored = sum(run_scored_flag),
    run_score_percentage = innings_with_run_scored / total_leadoff_walk_innings * 100
  )

team_mapping <- tibble::tibble(
  full_name = c(
    "Atlanta Braves", "Arizona Diamondbacks", "Baltimore Orioles", "Boston Red Sox", "Chicago Cubs", "Cincinnati Reds", 
    "Cleveland Guardians", "Colorado Rockies", "Chicago White Sox", "Detroit Tigers", "Houston Astros", "Kansas City Royals", 
    "Los Angeles Angels", "Los Angeles Dodgers", "Miami Marlins", "Milwaukee Brewers", "Minnesota Twins", "MLB", 
    "New York Mets", "New York Yankees", "Athletics", "Philadelphia Phillies", "Pittsburgh Pirates", 
    "San Diego Padres", "Seattle Mariners", "San Francisco Giants", "St. Louis Cardinals", "Tampa Bay Rays", 
    "Texas Rangers", "Toronto Blue Jays", "Washington Nationals"
  ),
  abbr = c(
    "ATL", "AZ", "BAL", "BOS", "CHC", "CIN", "CLE", "COL", "CWS", "DET", "HOU",
    "KC", "LAA", "LAD", "MIA", "MIL", "MIN", "MLB", "NYM", "NYY", "OAK", "PHI",
    "PIT", "SD", "SEA", "SF", "STL", "TB", "TEX", "TOR", "WSH"
  )
)

# Put dataframes into a named list
dfs <- list(
  team_run_score_rate = team_run_score_rate,
  team_leadoff_walks = team_leadoff_walks,
  team_runs_allowed = team_runs_allowed,
  league_summary = league_summary
)

# Run abbreviation replacement across all of them
dfs_updated <- lapply(dfs, function(df) {
  df %>%
    left_join(team_mapping, by = c("fielding_team" = "full_name")) %>%
    mutate(fielding_team = abbr) %>%
    select(-abbr)
})

# Reassign updated dataframes back to their original names
team_run_score_rate <- dfs_updated$team_run_score_rate
team_leadoff_walks <- dfs_updated$team_leadoff_walks
team_runs_allowed <- dfs_updated$team_runs_allowed
league_summary <- dfs_updated$league_summary

league_summary <- league_summary |>
  bind_rows(
    tibble(
      fielding_team = "MLB",
      total_leadoff_walks = mean(team_leadoff_walks$total_leadoff_walks),
      total_runs_allowed = mean(team_runs_allowed$total_runs_allowed)
    )
  )


mean(team_run_score_rate$total_leadoff_walk_innings)
mean(team_run_score_rate$innings_with_run_scored)
mean(team_run_score_rate$run_score_percentage)

team_run_score_rate <- team_run_score_rate |>
  bind_rows(
    tibble(
      fielding_team = "MLB",
      total_leadoff_walk_innings = mean(team_leadoff_walks$total_leadoff_walks),
      innings_with_run_scored = mean(team_run_score_rate$innings_with_run_scored),
      run_score_percentage = round(innings_with_run_scored/total_leadoff_walk_innings*100, 2)
    )
  ) |>
  mutate(run_score_percentage = format(round(run_score_percentage, 2), nsmall = 2))
  
team_run_score_rate$run_score_percentage <- as.numeric(team_run_score_rate$run_score_percentage)

#creating total leadoff walk percentage
low_inning_pct <- league_summary |>
  select(1,2)

games <- pbp |>
  select(1,81,88, about.inning, about.halfInning) |>
  unique()

team_counts <- games |>
  distinct(game_pk, about.inning, about.halfInning, home_team, away_team) |>
  mutate(full_name = if_else(about.halfInning == "top", home_team, away_team)) |>
  count(full_name, name = "innings_pitched") |>
  arrange(desc(innings_pitched))

#total games df
tg <- left_join(team_mapping, team_counts) %>%
  select(-full_name)

low_inning_pct <- left_join(low_inning_pct, tg, by = c("fielding_team" = "abbr"))

low_inning_pct <- low_inning_pct |>
  mutate(innings_pitched = if_else(
    fielding_team == "MLB",
    mean(innings_pitched[fielding_team != "MLB"]),
    innings_pitched
  ),
  rate = total_leadoff_walks/innings_pitched*100)
  
#creating visuals
ggplot(league_summary, aes(total_leadoff_walks, total_runs_allowed)) +
  geom_mlb_logos(aes(team_abbr = fielding_team), width = 0.05) +
  scale_y_continuous(breaks = seq(floor(min(league_summary$total_runs_allowed)), 
                                  ceiling(max(league_summary$total_runs_allowed)), 
                                  by = 10)
                     ) +
  theme(
    plot.title = element_text(hjust = 0.5)
  ) +
  labs(
    title = "Leadoff Walks vs Runs Allowed",
    x = "Total Leadoff Walks Issued",
    y = "Total Runs Allowed Post-Leadoff Walk" 
  )

ggplot(team_run_score_rate, aes(total_leadoff_walk_innings, innings_with_run_scored)) +
  geom_mlb_logos(aes(team_abbr = fielding_team), width = 0.05) +
  scale_y_continuous(breaks = seq(floor(min(team_run_score_rate$innings_with_run_scored)), 
                                  ceiling(max(team_run_score_rate$innings_with_run_scored)), 
                                  by = 6)
  ) +
  theme(
    plot.title = element_text(hjust = 0.5)
  ) +
  labs(
    title = "Total Innings In Which At Least One Run Scored",
    x = "Total Leadoff Walks Issued",
    y = "Total Innings with >= 1 Run Scored" 
  )

ggplot(team_run_score_rate, aes(x = reorder(fielding_team, -run_score_percentage), y = run_score_percentage)) +
  geom_col(aes(color = fielding_team, fill = fielding_team), width = 0.5) +
  expand_limits(x = 0, y = 0) +
  scale_color_mlb(type = "secondary") +
  scale_fill_mlb(alpha = 0.4) +
  geom_text(aes(label = format(round(run_score_percentage, 1), nsmall = 1)),
            vjust = -0.5,
            size = 3
            ) + 
  labs(
    title = "Run Score Percentage after Leadoff Walk by Team",
    x = "Team",
    y = "Run Score Percentage (%)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5),  # Center the title
    axis.text.x = element_text(angle = 45, hjust = 1),  # Tilt x-axis labels
    legend.position = "none"
  )


#Percentage of Innings that start with LOW
ggplot(low_inning_pct, aes(x = reorder(fielding_team, -rate), y = rate)) +
  geom_col(aes(color = fielding_team, fill = fielding_team), width = 0.5) +
  expand_limits(x = 0, y = 0) +
  scale_color_mlb(type = "secondary") +
  scale_fill_mlb(alpha = 0.4) +
  geom_text(aes(label = format(round(rate, 1), nsmall = 1)),
            vjust = -0.5,
            size = 3
  ) + 
  labs(
    title = "Leadoff Walk Inning Percentage",
    x = "Team",
    y = "Inning Percentage (%)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5),  # Center the title
    axis.text.x = element_text(angle = 45, hjust = 1),  # Tilt x-axis labels
    legend.position = "none"
  )
