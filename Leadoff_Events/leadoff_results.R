library(tidyverse)
library(baseballr)
library(ggplot2)
library(mlbplotR)
library(stringr)
library(tidyr)

#load in csv
pbp <- readRDS("pbp_progress.rds")

#fix letters
pbp <- pbp %>%
  mutate(across(where(is.character), ~ str_replace_all(., "Ã±", "n")),
         across(where(is.character), ~ str_replace_all(., "Ã©", "e")),
         across(where(is.character), ~ str_replace_all(., "Ã³", "o")),
         across(where(is.character), ~ str_replace_all(., "Ã¡", "a")),
         across(where(is.character), ~ str_replace_all(., "Ãº", "u")),
         across(where(is.character), ~ str_replace_all(., "Ã", "i")))

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

#by team, obtain inning leadoff event counts (specific events)
lec <- first_event_in_inning |>
  count(game_pk, game_date, fielding_team, result.event, matchup.pitcher.fullName, sort = TRUE) |>
  pivot_wider(
    names_from = result.event,
    values_from = n,
    values_fill = 0  # fill missing combinations with 0
  ) |>
  arrange(fielding_team, game_date, matchup.pitcher.fullName)

#create Leadoff_Innings column
lec$Leadoff_Innings <- rowSums(lec[ , !(names(lec) %in% c("game_pk", "game_date", "fielding_team", "matchup.pitcher.fullName"))])

#batting average against addition
lec <- lec |>
  group_by(matchup.pitcher.fullName) |>
  mutate(
    cAB   = cumsum(Leadoff_Innings -  (Walk + `Intent Walk` + `Hit By Pitch` + `Sac Bunt` + `Catcher Interference`)),
    cH    = cumsum(Single + Double + Triple + `Home Run`),
    c1B   = cumsum(Single),
    c2B   = cumsum(Double),
    c3B   = cumsum(Triple),
    cHR   = cumsum(`Home Run`),
    cBB   = cumsum(Walk),
    cIBB  = cumsum(`Intent Walk`),
    cHBP  = cumsum(`Hit By Pitch`),
    cSO   = cumsum(Strikeout),
    cGO   = cumsum(Groundout),
    cFO   = cumsum(Flyout),
    cPO   = cumsum(`Pop Out`),
    cLD   = cumsum(Lineout),
    cLO_BF = cumsum(Leadoff_Innings),
    
    BAA  = round(cH / cAB, 3),
    OBP  = round((cH + cBB + cIBB + cHBP) / (cAB + cBB + cIBB + cHBP), 3),
    SLG  = round((c1B + 2 * c2B + 3 * c3B + 4 * cHR) / cAB, 3),
    OPS  = round(OBP + SLG, 3),
    wOBA = round((.697 * cBB + .728 * cHBP + .891 * c1B + 
                     1.268 * c2B + 1.607 * c3B + 2.072 * cHR) /
                    (cAB + cBB + cHBP), 3),
    
    K_pct  = round(cSO / cLO_BF * 100, 2),
    BB_pct = round(cBB / cLO_BF * 100, 2),
    HR_pct = round(cHR / cLO_BF * 100, 2),
    GO_pct = round(cGO / cLO_BF * 100, 2),
    FB_pct = round((cFO + cPO) / cLO_BF * 100, 2),
    LD_pct = round(cLD / cLO_BF * 100, 2)
  ) |>
  ungroup()


#Team LEC

# Summarize per team per game
team_game <- lec |>
  group_by(fielding_team, game_pk, game_date) |> 
  summarise(
    LO_BF = sum(Leadoff_Innings),
    AB    = LO_BF - (sum(Walk) + sum(`Intent Walk`) + sum(`Hit By Pitch`) + sum(`Sac Bunt`) + sum(`Catcher Interference`)),
    H     = sum(Single) + sum(Double) + sum(Triple) + sum(`Home Run`),
    X1B   = sum(Single),
    X2B   = sum(Double),
    X3B   = sum(Triple),
    HR    = sum(`Home Run`),
    BB    = sum(Walk),
    IBB   = sum(`Intent Walk`),
    HBP   = sum(`Hit By Pitch`),
    SO    = sum(Strikeout),
    GO    = sum(Groundout),
    FO    = sum(Flyout),
    PO    = sum(`Pop Out`),
    LD    = sum(Lineout),
    .groups = "drop"
  )

# Add cumulative columns
tlec <- team_game |>
  arrange(fielding_team, game_date) |>  
  group_by(fielding_team) |>
  mutate(
    G = row_number(),
    
    cAB   = cumsum(AB),
    cH    = cumsum(H),
    c1B   = cumsum(X1B),
    c2B   = cumsum(X2B),
    c3B   = cumsum(X3B),
    cHR   = cumsum(HR),
    cBB   = cumsum(BB),
    cIBB  = cumsum(IBB),
    cHBP  = cumsum(HBP),
    cSO   = cumsum(SO),
    cGO   = cumsum(GO),
    cFO   = cumsum(FO),
    cPO   = cumsum(PO),
    cLD   = cumsum(LD),
    cLO_BF = cumsum(LO_BF),
    
    BAA  = round(cH / cAB, 3),
    OBP  = round((cH + cBB + cIBB + cHBP) / (cAB + cBB + cIBB + cHBP), 3),
    SLG  = round((c1B + 2 * c2B + 3 * c3B + 4 * cHR) / cAB, 3),
    OPS  = round(OBP + SLG, 3),
    wOBA = round((.697 * cBB + .728 * cHBP + .891 * c1B +
                         1.268 * c2B + 1.607 * c3B + 2.072 * cHR) /
                        (cAB + cBB + cHBP), 3),
    
    K_pct  = round(cSO / cLO_BF * 100, 2),
    BB_pct = round(cBB / cLO_BF * 100, 2),
    HR_pct = round(cHR / cLO_BF * 100, 2),
    GO_pct = round(cGO / cLO_BF * 100, 2),
    FB_pct = round((cFO + cPO) / cLO_BF * 100, 2),
    LD_pct = round(cLD / cLO_BF * 100, 2)
  ) |>
  ungroup()

lec <- lec |>
  group_by(matchup.pitcher.fullName) |>
  mutate(G = row_number()) |>
  ungroup()

#Save
write.csv(tlec, "tlec.csv", row.names = FALSE)

write.csv(lec, "lec.csv", row.names = FALSE)
