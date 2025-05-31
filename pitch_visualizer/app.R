library(shiny)
library(ggplot2)
library(ggforce)
library(tidyverse)
library(baseballr)
library(mlbplotR)
library(stringr)
library(tidyr)
library(shinyWidgets)
library(rsconnect)

rsconnect::setAccountInfo(name='derkrez',
                          token='AD324C025BF0B0AFA396E018CD30B2D4',
                          secret='8M9ujfv1U102rDfFjxNoLuOsszaIz8iCVK4UhhkJ')

#load df
pbp <- readRDS("zone_pbp.rds")

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
        gap: 10px;
        justify-content: center;
      }

      .pitch-plot {
        flex: 1 1 calc(33% - 10px);
        min-width: 400px;
      }
      

    "))
  ),
  
  titlePanel("Pitch Location Explorer"),
  
  # Horizontal filter row
  fluidRow(
    column(3,
           selectInput("team", "Select Team:",
                       choices = NULL, selected = NULL)
    ),
    column(2,
           selectInput("pitcher", "Select Pitcher:",
                       choices = NULL, selected = NULL)
    ),
    column(2,
           selectInput("handedness", "Select Handedness:",
                       choices = c("Handedness", "Left", "Right"), selected = "Handedness")
    ),
    column(2,
           pickerInput("pitch_result", "Select Pitch Result(s):",
                       choices = NULL, selected = NULL,
                       multiple = TRUE, options = list(`actions-box` = TRUE, `live-search` = TRUE))
    ),
    column(3,
             pickerInput("bip_type", "Ball in Play Type:",
                         choices = c("Groundball", "Flyball", "Pop Up", "Linedrive"),
                         selected = NULL,
                         multiple = TRUE,
                         options = list(`actions-box` = TRUE))
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
    ),
    column(3,
           dateRangeInput(
             inputId = "date_filter",
             label = "Select Date Range:",
             start = min(pbp$game_date),
             end = max(pbp$game_date),
             min = min(pbp$game_date),
             max = max(pbp$game_date)
           )
    )
  ),
  
  hr(),
  
  uiOutput("pitchPlots")
)

server <- function(input, output, session) {
  
  # Create hierarchical choices for pitch results and hits/outs/other results
  create_hierarchical_choices <- reactive({
    # Get unique combinations of pitch_result and hits/outs/other
    result_combos <- pbp %>%
      select(pitch_result, specific_pitch_result) %>%
      distinct() %>%
      arrange(pitch_result, specific_pitch_result)
    
    # Create named list for hierarchical structure
    hierarchical_list <- list()
    
    for (pr in unique(result_combos$pitch_result)) {
      specific_options <- result_combos %>%
        filter(pitch_result == pr) %>%
        mutate(specific_pitch_result = factor(
          specific_pitch_result,
          levels = c("Called Strike", "Swinging Strike", "Foul", "Single", 
                     "Double", "Triple", "Home Run", "Out", "Other Batter Reach",
                     "Ball", "Hit By Pitch")  
        )) %>%
        arrange(specific_pitch_result) %>%
        pull(specific_pitch_result) %>%
        as.character()
      
      
      # Create sub-list for this pitch result
      sub_list <- setNames(
        paste(pr, specific_options, sep = " | "),  # Values will be "pitch_result | specific_pitch_result"
        specific_options  # Display names will be just the specific result
      )
      
      hierarchical_list[[pr]] <- sub_list
    }
    
    return(hierarchical_list)
  })
  
  # Update input choices dynamically based on available data
  observe({
    updateSelectInput(session, "team",
                      choices = sort(unique(pbp$fielding_team)))
    updateSelectInput(session, "pitcher",
                      choices = sort(unique(pbp$matchup.pitcher.fullName)))
    updateSelectInput(session, "pitch_type",
                      choices = sort(unique(pbp$details.type.description)))
    
    # Update with hierarchical choices
    hierarchical_choices <- create_hierarchical_choices()
    updatePickerInput(session, "pitch_result",
                      choices = hierarchical_choices,
                      selected = unlist(hierarchical_choices))  # Select all by default
    
    updateDateRangeInput(session, "date_filter",
                         start = min(pbp$game_date, na.rm = TRUE),
                         end = max(pbp$game_date, na.rm = TRUE),
                         min = min(pbp$game_date, na.rm = TRUE),
                         max = max(pbp$game_date, na.rm = TRUE))
  })
  
  # Update list of pitchers when team is selected
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
  
  # Update versus team and batter when pitcher OR date range changes
  observeEvent(list(input$pitcher, input$date_filter), {
    req(input$pitcher, input$date_filter)
    
    filtered_pbp <- pbp %>%
      filter(matchup.pitcher.fullName == input$pitcher,
             game_date >= input$date_filter[1],
             game_date <= input$date_filter[2])
    
    available_pitch_types <- filtered_pbp %>%
      pull(details.type.description) %>%
      unique() %>%
      sort()
    
    updateSelectInput(session, "pitch_type",
                      choices = available_pitch_types,
                      selected = available_pitch_types[1])
    
    teams_faced <- filtered_pbp %>%
      pull(batting_team) %>%
      unique() %>%
      sort()
    
    updateSelectInput(session, "versus_team",
                      choices = c("All Teams" = "", teams_faced),
                      selected = "")
    
    batters_faced <- filtered_pbp %>%
      pull(matchup.batter.fullName) %>%
      unique() %>%
      sort()
    
    updateSelectInput(session, "versus_batter",
                      choices = c("All Batters" = "", batters_faced),
                      selected = "")
  })
  
  # Update list of batters when versus_team is selected
  observeEvent(input$versus_team, {
    req(input$pitcher, input$date_filter)
    
    base_filter <- pbp %>%
      filter(matchup.pitcher.fullName == input$pitcher,
             game_date >= input$date_filter[1],
             game_date <= input$date_filter[2])
    
    if (input$versus_team == "") {
      batters_faced <- base_filter %>%
        pull(matchup.batter.fullName) %>%
        unique() %>%
        sort()
      
      updateSelectInput(session, "versus_batter",
                        choices = c("All Batters" = "", batters_faced),
                        selected = "")
    } else {
      team_batters <- base_filter %>%
        filter(batting_team == input$versus_team) %>%
        pull(matchup.batter.fullName) %>%
        unique() %>%
        sort()
      
      updateSelectInput(session, "versus_batter",
                        choices = c("All Batters" = "", team_batters),
                        selected = "")
    }
  })
  
  # Helper function to parse selected hierarchical results
  parse_selected_results <- reactive({
    req(input$pitch_result)
    
    if (is.null(input$pitch_result) || length(input$pitch_result) == 0) {
      return(list(pitch_results = character(0), specific_pitch_results = character(0)))
    }
    
    # Parse the selected values which are in format "pitch_result | specific_pitch_result"
    selected_combinations <- input$pitch_result
    
    if (length(selected_combinations) == 0) {
      return(list(pitch_results = character(0), specific_pitch_results = character(0)))
    }
    
    # Split each combination
    parsed <- strsplit(selected_combinations, " \\| ")
    
    pitch_results <- sapply(parsed, function(x) x[1])
    specific_pitch_results <- sapply(parsed, function(x) x[2])
    
    return(list(
      pitch_results = unique(pitch_results),
      specific_pitch_results = specific_pitch_results
    ))
  })
  
  # Modified reactive filtered data to include specific results
  filtered_data <- reactive({
    req(input$team, input$pitcher, input$pitch_result, input$date_filter)
    
    # Parse the hierarchical selections
    parsed_results <- parse_selected_results()
    
    df <- pbp %>%
      filter(fielding_team == input$team,
             matchup.pitcher.fullName == input$pitcher,
             game_date >= input$date_filter[1],
             game_date <= input$date_filter[2])
    
    # Filter by the specific combinations selected
    if (length(parsed_results$specific_pitch_results) > 0) {
      # Create a filter for exact pitch_result + specific_pitch_result combinations
      selected_combinations <- input$pitch_result
      
      # Split and create filter conditions
      filter_conditions <- map_dfr(selected_combinations, function(combo) {
        parts <- strsplit(combo, " \\| ")[[1]]
        data.frame(
          pitch_result = parts[1],
          specific_pitch_result = parts[2],
          stringsAsFactors = FALSE
        )
      })
      
      # Apply the combination filter
      df <- df %>%
        semi_join(filter_conditions, by = c("pitch_result", "specific_pitch_result"))
    }
    
    if (!is.null(input$bip_type) && length(input$bip_type) > 0) {
      df <- df %>%
        filter(bip_type %in% input$bip_type)
    }
    
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
          
          x_abs <- max(abs(df_pt$pitchData.coordinates.pX), na.rm = TRUE)
          y_range <- range(df_pt$pitchData.coordinates.pZ, na.rm = TRUE)
          y_padding <- 0.25
          y_lower <- min(y_range[1] - y_padding, 0)
          y_upper <- max(y_range[2] + y_padding, 4.5)
          
          x_limit <- ceiling(x_abs * 1.1)
          
          ggplot(df_pt, aes(x = pitchData.coordinates.pX,
                            y = pitchData.coordinates.pZ)) +
            ggforce::geom_circle(aes(
              x0 = pitchData.coordinates.pX,
              y0 = pitchData.coordinates.pZ,
              r = 0.125,
              fill = specific_pitch_result
            ), color = "black", alpha = 1) +
            coord_fixed(
              xlim = c(-x_limit, x_limit),
              ylim = c(y_lower, y_upper)
            ) +
            labs(
              x = "Horizontal Location (ft)",
              y = "Vertical Location (ft)",
              title = paste(pt),
              fill = "Pitch Result"
            ) +
            scale_fill_manual(values = c(
              "Called Strike" = "#800000",
              "Swinging Strike" = "red",
              "Foul" = "#fa8072",
              "Single" = "#dff2ff",
              "Double" = "#82eefd",
              "Triple" = "#0492c2",
              "Home Run" = "#00008b",
              "Out" = "#a020f0",
              "Other Batter Reach" = "blue",
              "Ball" = "#90ee90",
              "Hit By Pitch" = "#006400"
            )) +
            theme_minimal() +
            theme(plot.title = element_text(hjust = 0.5)) +
            geom_polygon(data = home_plate, aes(x = x, y = y),
                         fill = NA, color = "black", linewidth = 1, inherit.aes = FALSE) +
            annotate("rect", xmin = -0.8333, xmax = 0.8333,
                     ymin = 1.5, ymax = 3.5, alpha = 0.2, fill = "black")
        })
      })
    }
  })
}

shinyApp(ui = ui, server = server)