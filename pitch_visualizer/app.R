library(shiny)
library(ggplot2)
library(ggforce)
library(tidyverse)
library(baseballr)
library(mlbplotR)
library(stringr)
library(tidyr)
library(shinyWidgets)


#load df
pbp <- readRDS("pbp_progress.rds")

#change to numeric
pbp$atBatIndex <- as.numeric(pbp$atBatIndex)

#add pitch_result column
pbp <- pbp |>
  mutate(pitch_result = case_when(
    details.description %in% c("In play, out(s)") ~ "In Play, Out(s)",
    details.description %in% c("In play, no out") ~ "In Play, No Out",
    details.description %in% c("In play, run(s)") ~ "In Play, Run(s)",
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

pbp <- pbp  # your dataframe

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

#create home plate (scaled to pitch location coords)
home_plate <- data.frame(
  x = c(0, -.8, -.7083, .7083, .8, 0),
  y = c(0, 0.3, 0.6, 0.6, 0.3, 0)
)


#App development
ui <- fluidPage(
  tags$head(
    tags$style(HTML("
      .pitch-plot-wrapper {
        display: flex;
        flex-wrap: wrap;
        gap: 20px;
        justify-content: center;
      }

      .pitch-plot {
        flex: 1 1 calc(25% - 20px); /* Try to fit 4 per row, but will adjust */
        min-width: 300px;
        max-width: 400px;
      }
    "))
  ),
  
  titlePanel("First Pitch Location Explorer"),
  
  # Horizontal filter row
  fluidRow(
    column(3,
           selectInput("team", "Select Team:",
                       choices = NULL, selected = NULL)
    ),
    column(3,
           selectInput("pitcher", "Select Pitcher:",
                       choices = NULL, selected = NULL)
    ),
    column(3,
           selectInput("handedness", "Select Handedness:",
                       choices = c("Handedness", "Left", "Right"), selected = "Handedness")
    ),
    
    column(3,
           pickerInput("pitch_result", "Select Pitch Result(s):",
                choices = NULL, selected = NULL,
                multiple = TRUE, options = list(`actions-box` = TRUE, `live-search` = TRUE))
    )
    
  ),
  
  fluidRow(
    column(3,
           selectInput("versus_team", "Versus Team", 
                       choices = c("All Teams" = ""), selected = "")),
    column(3,
           selectInput("versus_batter", "Versus Batter", 
                       choices = c("All Batters" = ""), selected = "")),
    column(3,
           pickerInput("count_filter", "Count",
                       choices = sort(unique(pbp$pitchCount)),
                       selected = NULL, multiple = TRUE, 
                       options = list(`actions-box` = TRUE, `live-search` = TRUE))
           )
  ),
  
  hr(),
  
  uiOutput("pitchPlots")
)


server <- function(input, output, session) {
  
  # Update input choices dynamically based on available data
  observe({
    updateSelectInput(session, "team",
                      choices = sort(unique(pbp$fielding_team)))
    updateSelectInput(session, "pitcher",
                      choices = sort(unique(pbp$matchup.pitcher.fullName)))
    updateSelectInput(session, "pitch_type",
                      choices = sort(unique(pbp$details.type.description)))
    updatePickerInput(session, "pitch_result",
                      choices = sort(unique(pbp$pitch_result)),
                      selected = unique(pbp$pitch_result))
    
  })
  
  #Update list of pitchers when team is selected
  observeEvent(input$team, {
    req(input$team)
    
    team_pitchers <- pbp %>%
      filter(fielding_team == input$team) %>%
      pull(matchup.pitcher.fullName) %>%
      unique() %>%
      sort()
    
    updateSelectInput(session, "pitcher",
                      choices = team_pitchers,
                      selected = team_pitchers[1])
  })
  
  #update pitch type list when pitcher is selected
  observeEvent(input$pitcher, {
    req(input$pitcher)
    
    available_pitch_types <- pbp %>%
      filter(matchup.pitcher.fullName == input$pitcher) %>%
      pull(details.type.description) %>%
      unique() %>%
      sort()
    
    updateSelectInput(session, "pitch_type",
                      choices = available_pitch_types,
                      selected = available_pitch_types[1])
    
    # Get all teams this pitcher has faced
    teams_faced <- pbp %>%
      filter(matchup.pitcher.fullName == input$pitcher) %>%
      pull(batting_team) %>%
      unique() %>%
      sort()
    
    updateSelectInput(session, "versus_team",
                      choices = c("All Teams" = "", teams_faced),
                      selected = "")
    
    # Get all batters this pitcher has faced
    batters_faced <- pbp %>%
      filter(matchup.pitcher.fullName == input$pitcher) %>%
      pull(matchup.batter.fullName) %>%
      unique() %>%
      sort()
    
    updateSelectInput(session, "versus_batter",
                      choices = c("All Batters" = "", batters_faced),
                      selected = "")
  })
  
  #Update list of batters when team is selected
  observeEvent(input$versus_team, {
    if (input$versus_team == "") {
      # If no team selected, limit batters to only those faced by selected pitcher
      batters_faced <- pbp %>%
        filter(matchup.pitcher.fullName == input$pitcher) %>%
        pull(matchup.batter.fullName) %>%
        unique() %>%
        sort()
      
      updateSelectInput(session, "versus_batter",
                        choices = c("All Batters" = "", batters_faced),
                        selected = "")
    } else {
      # Filter by both pitcher and team
      team_batters <- pbp %>%
        filter(matchup.pitcher.fullName == input$pitcher,
               batting_team == input$versus_team) %>%
        pull(matchup.batter.fullName) %>%
        unique() %>%
        sort()
      
      updateSelectInput(session, "versus_batter",
                        choices = c("All Batters" = "", team_batters),
                        selected = "")
    }
  })
  
  
  # Reactive filtered data
  filtered_data <- reactive({
    req(input$team, input$pitcher, input$pitch_result)
    
    df <- pbp %>%
      filter(fielding_team == input$team,
             matchup.pitcher.fullName == input$pitcher,
             pitch_result %in% input$pitch_result)
    
    if (!is.null(input$count_filter) && length(input$count_filter) > 0) {
      df <- df %>%
        filter(pitchCount %in% input$count_filter)
    }
    
    if (input$handedness != "Handedness") {
      df <- df %>%
        filter(matchup.batSide.description == input$handedness)
    }
    
    if (input$versus_team != "") {
      df <- df %>% 
        filter(batting_team == input$versus_team)
    }
    
    if (input$versus_batter != "") {
      df <- df %>% 
        filter(matchup.batter.fullName == input$versus_batter)
    }
    
    df
  })
  
  # Pitch location plot
  output$pitchPlots <- renderUI({
    df <- filtered_data()
    pitch_types <- sort(unique(df$details.type.description))
    
    plot_output_list <- lapply(pitch_types, function(ptype) {
      plotname <- paste0("plot_", make.names(ptype))
      div(class = "pitch-plot", plotOutput(plotname, height = "400px"))
    })
    
    div(class = "pitch-plot-wrapper", plot_output_list)
  })
  
  
  observe({
    df <- filtered_data()
    pitch_types <- sort(unique(df$details.type.description))
    
    for (ptype in pitch_types) {
      local({
        pt <- ptype
        plotname <- paste0("plot_", make.names(pt))
        
        output[[plotname]] <- renderPlot({
          df_pt <- df %>% filter(details.type.description == pt)
          
          ggplot(df_pt, aes(x = pitchData.coordinates.pX,
                            y = pitchData.coordinates.pZ)) +
            ggforce::geom_circle(aes(
              x0 = pitchData.coordinates.pX,
              y0 = pitchData.coordinates.pZ,
              r = 0.125,
              fill = pitch_result
            ), color = "black", alpha = 0.7) +
            coord_fixed() +
            xlim(-6.75, 6.75) +
            ylim(-3.1, 7.5) +
            labs(
              x = "Horizontal Location (ft)",
              y = "Vertical Location (ft)",
              title = paste(pt),
              fill = "Pitch Result"
            ) +
            scale_fill_manual(values = c(
              "Strike" = "red",
              "Ball" = "green",
              "In Play, Out(s)" = "#08306B",
              "In Play, No Out" = "#2171B5",
              "In Play, Run(s)" = "#6BAED6"
            )) +
            theme_minimal() +
            theme(plot.title = element_text(hjust = 0.5)) +
            geom_polygon(data = home_plate, aes(x = x, y = y),
                         fill = NA, color = "black", linewidth = 1, inherit.aes = FALSE) +
            annotate("rect", xmin = -0.7083, xmax = 0.7083,
                     ymin = 1.5, ymax = 3.5, alpha = 0.2, fill = "black")
        })
      })
    }
  })
  
  
}

shinyApp(ui = ui, server = server)
