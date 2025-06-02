library(shiny)
library(ggplot2)
library(ggforce)
library(tidyverse)
library(baseballr)
library(mlbplotR)
library(stringr)
library(tidyr)
library(plotly)
library(ggnewscale)
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

#For contour map
calculate_contour_params <- function(data, x_var, y_var) {
  
  # Extract x and y coordinates
  x_coords <- data[[x_var]]
  y_coords <- data[[y_var]]
  
  # Remove NA values
  valid_data <- !is.na(x_coords) & !is.na(y_coords)
  x_coords <- x_coords[valid_data]
  y_coords <- y_coords[valid_data]
  
  n <- length(x_coords)
  
  # Check if we have enough data for density plots
  if (n < 1) {
    return(list(
      h = NULL,
      bins = NULL,
      can_plot = FALSE,
      n = n
    ))
  }
  
  # Calculate data ranges
  x_range <- diff(range(x_coords, na.rm = TRUE))
  y_range <- diff(range(y_coords, na.rm = TRUE))
  
  # Use the larger range to determine bandwidth (prevents stretching)
  max_range <- max(x_range, y_range)
  
  # Baseball-optimized bandwidth calculation using the same value for both x and y
  # Strike zone is ~1.67 ft wide, ~2 ft tall
  # We want good coverage but smooth contours
  
  if (n < 10) {
    # Very small samples: moderate smoothing, fewer bins
    bandwidth <- max(0.4, max_range / 3)
    optimal_bins <- 4
  } else if (n < 25) {
    # Small samples: balanced smoothing
    bandwidth <- max(0.3, max_range / 4)
    optimal_bins <- 5
  } else if (n < 50) {
    # Medium samples: less smoothing, more detail
    bandwidth <- max(0.25, max_range / 5)
    optimal_bins <- 11
  } else if (n < 100) {
    # Larger samples: fine detail but still smooth
    bandwidth <- max(0.2, max_range / 6)
    optimal_bins <- 15
  } else {
    # Large samples: Baseball Savant style - smooth contours
    bandwidth <- max(0.18, max_range / 7)
    optimal_bins <- pmax(6, pmin(10, round(6 + log10(n/100))))
  }
  
  return(list(
    h = c(bandwidth, bandwidth),  # Same bandwidth for both x and y
    bins = optimal_bins,
    can_plot = TRUE,
    n = n
  ))
}

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
  
  titlePanel("Pitch Location Visualizer"),
  
  hr(),
  
  # Horizontal filter row
  fluidRow(
    column(4,
           dateRangeInput(
             inputId = "date_filter",
             label = "Select Date Range:",
             start = min(pbp$game_date),
             end = max(pbp$game_date),
             min = min(pbp$game_date),
             max = max(pbp$game_date)
           )
    ),
    column(4,
           selectInput("plot_type", "Visualization Type:",
                       choices = c("Individual Pitches" = "individual",
                                   "Heatmap" = "heatmap", 
                                   "Pitches + Heatmap" = "both"),
                       selected = "individual")
    ),
    column(4,
           pickerInput("pitch_zone", "Pitch Zone:",
                       choices = list(
                         "Zone Groups" = c("In Zone", "Out of Zone"),
                         "Individual Zones" = as.character(1:14)
                       ),
                       selected = NULL,
                       multiple = TRUE,
                       options = list(`actions-box` = TRUE, 
                                      `live-search` = TRUE,
                                      `title` = "All Zones"))
    )
  ),
  
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
           pickerInput("pitch_result", "Select Pitch Result(s):",
                       choices = NULL, selected = NULL,
                       multiple = TRUE, options = list(`actions-box` = TRUE, 
                                                       `live-search` = TRUE))
    ),
    column(3,
           pickerInput("bip_type", "Ball in Play Type:",
                       choices = c("Groundball", "Flyball", "Pop Up", "Linedrive",
                                   "Strike", "Ball"),
                       selected = NULL,
                       multiple = TRUE,
                       options = list(`actions-box` = TRUE,
                                      `title` = "All Types"))
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
           selectInput("handedness", "Select Batter Handedness:",
                       choices = c("Handedness", "Left", "Right"), selected = "Handedness")
    ),
    column(3,
           pickerInput("count_filter", "Count",
                       choices = sort(unique(pbp$pitchCount)),
                       selected = NULL, multiple = TRUE, 
                       options = list(`actions-box` = TRUE, 
                                      `live-search` = TRUE,
                                      `title` = "All Counts"))
    )
  ),
  
  fluidRow(
    column(2,
           selectInput("duration_type", "Situation Selection:", 
                       choices = c("Innings", "Pitches Thrown", "Batters Faced"),
                       selected = "Innings"),
           
           uiOutput("duration_slider")  # dynamic slider
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
  
  # Handle pitch zone selection logic
  observeEvent(input$pitch_zone, {
    if ("In Zone" %in% input$pitch_zone) {
      # Select zones 1-9 and remove 10-14 if they're selected
      current_selection <- input$pitch_zone
      new_selection <- c(current_selection[current_selection != "In Zone"], as.character(1:9))
      new_selection <- new_selection[!new_selection %in% as.character(10:14)]
      new_selection <- unique(new_selection)
      
      updatePickerInput(session, "pitch_zone", selected = new_selection)
    }
    
    if ("Out of Zone" %in% input$pitch_zone) {
      # Select zones 10-14 and remove 1-9 if they're selected
      current_selection <- input$pitch_zone
      new_selection <- c(current_selection[current_selection != "Out of Zone"], as.character(10:14))
      new_selection <- new_selection[!new_selection %in% as.character(1:9)]
      new_selection <- unique(new_selection)
      
      updatePickerInput(session, "pitch_zone", selected = new_selection)
    }
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
  
  # Create reactive data filtered by all filters EXCEPT duration
  base_filtered_data <- reactive({
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
    
    # Apply pitch zone filter
    if (!is.null(input$pitch_zone) && length(input$pitch_zone) > 0) {
      # Remove "In Zone" and "Out of Zone" from the selection as they're just UI helpers
      zone_selection <- input$pitch_zone[!input$pitch_zone %in% c("In Zone", "Out of Zone")]
      
      if (length(zone_selection) > 0) {
        df <- df %>%
          filter(as.character(pitchData.zone) %in% zone_selection)
      }
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
  
  # Dynamic input fields for duration filtering
  output$duration_slider <- renderUI({
    req(input$duration_type)
    
    df <- base_filtered_data()
    
    if (nrow(df) == 0) {
      return(div(
        p("No data available for current filters"),
        style = "color: gray; font-style: italic;"
      ))
    }
    
    if (input$duration_type == "Innings") {
      # Get available innings from the data
      if ("about.inning" %in% names(df)) {
        available_innings <- sort(unique(df$about.inning))
        min_inning <- min(available_innings, na.rm = TRUE)
        max_inning <- max(available_innings, na.rm = TRUE)
        
        tagList(
          div(
            style = "display: flex; gap: 10px; align-items: end;",
            div(
              style = "flex: 1;",
              numericInput("duration_min", "Min Inning:", 
                           value = min_inning, min = min_inning, max = max_inning, step = 1)
            ),
            div(
              style = "flex: 1;",
              numericInput("duration_max", "Max Inning:", 
                           value = max_inning, min = min_inning, max = max_inning, step = 1)
            )
          ),
          p(paste("Available innings:", paste(available_innings, collapse = ", ")), 
            style = "font-size: 12px; color: gray; margin-top: 5px;")
        )
      } else {
        p("Inning data not available", style = "color: red;")
      }
      
    } else if (input$duration_type == "Pitches Thrown") {
      # Get pitch sequence numbers within each game
      pitch_sequences <- df %>%
        group_by(game_pk) %>%
        arrange(pitchNumber) %>%
        mutate(pitch_sequence = row_number()) %>%
        ungroup() %>%
        pull(pitch_sequence)
      
      min_pitch <- min(pitch_sequences, na.rm = TRUE)
      max_pitch <- max(pitch_sequences, na.rm = TRUE)
      
      tagList(
        div(
          style = "display: flex; gap: 10px; align-items: end;",
          div(
            style = "flex: 1;",
            numericInput("duration_min", "Min Pitch Number:", 
                         value = min_pitch, min = 1, max = max_pitch, step = 1)
          ),
          div(
            style = "flex: 1;",
            numericInput("duration_max", "Max Pitch Number:", 
                         value = max_pitch, min = 1, max = max_pitch, step = 1)
          )
        ),
        p(paste("Range: Pitch", min_pitch, "-", max_pitch, "within each game"), 
          style = "font-size: 12px; color: gray; margin-top: 5px;")
      )
      
    } else if (input$duration_type == "Batters Faced") {
      # Get batter sequence numbers within each game
      batter_sequences <- df %>%
        group_by(game_pk) %>%
        arrange(atBatIndex) %>%
        mutate(batter_sequence = dense_rank(atBatIndex)) %>%
        ungroup() %>%
        pull(batter_sequence)
      
      min_batter <- min(batter_sequences, na.rm = TRUE)
      max_batter <- max(batter_sequences, na.rm = TRUE)
      
      tagList(
        div(
          style = "display: flex; gap: 10px; align-items: end;",
          div(
            style = "flex: 1;",
            numericInput("duration_min", "Min Batter Number:", 
                         value = min_batter, min = 1, max = max_batter, step = 1)
          ),
          div(
            style = "flex: 1;",
            numericInput("duration_max", "Max Batter Number:", 
                         value = max_batter, min = 1, max = max_batter, step = 1)
          )
        ),
        p(paste("Range: Batter", min_batter, "-", max_batter, "within each game"), 
          style = "font-size: 12px; color: gray; margin-top: 5px;")
      )
    }
  })
  
  # Modified reactive filtered data to include duration filter
  filtered_data <- reactive({
    req(input$duration_type)
    
    df <- base_filtered_data()
    
    if (is.null(input$duration_min) || is.null(input$duration_max) || nrow(df) == 0) {
      return(df)
    }
    
    if (input$duration_type == "Innings") {
      # Filter by specific innings (situational analysis)
      if ("about.inning" %in% names(df)) {
        df <- df %>%
          filter(about.inning >= input$duration_min & about.inning <= input$duration_max)
      }
      
    } else if (input$duration_type == "Pitches Thrown") {
      # Filter by pitch sequence within each game
      df <- df %>%
        group_by(game_pk) %>%
        arrange(pitchNumber) %>%
        mutate(pitch_sequence = row_number()) %>%
        ungroup() %>%
        filter(pitch_sequence >= input$duration_min & pitch_sequence <= input$duration_max)
      
    } else if (input$duration_type == "Batters Faced") {
      # Filter by batter sequence within each game
      df <- df %>%
        group_by(game_pk) %>%
        arrange(atBatIndex) %>%
        mutate(batter_sequence = dense_rank(atBatIndex)) %>%
        ungroup() %>%
        filter(batter_sequence >= input$duration_min & batter_sequence <= input$duration_max)
    }
    
    df
  })
  
  # Pitch location plot with percentages and ordering
  output$pitchPlots <- renderUI({
    df <- filtered_data()
    
    # Calculate pitch type percentages and order by percentage (descending)
    # Filter out NA or missing pitch types
    pitch_type_stats <- df %>%
      filter(!is.na(details.type.description) & details.type.description != "" & details.type.description != "NA") %>%
      group_by(details.type.description) %>%
      summarise(count = n(), .groups = 'drop') %>%
      mutate(percentage = round((count / sum(count)) * 100, 1)) %>%
      arrange(desc(percentage))  # Order by percentage descending
    
    # Get ordered pitch types
    ordered_pitch_types <- pitch_type_stats$details.type.description
    
    # Create plot list in the ordered sequence
    plot_output_list <- lapply(ordered_pitch_types, function(ptype) {
      plotname <- paste0("plot_", make.names(ptype))
      div(class = "pitch-plot", plotOutput(plotname, height = "400px"))
    })
    
    div(class = "pitch-plot-wrapper", plot_output_list)
  })
  
  observe({
    df <- filtered_data()
    
    # Calculate pitch type percentages for titles
    # Filter out NA or missing pitch types
    pitch_type_stats <- df %>%
      filter(!is.na(details.type.description) & details.type.description != "" & details.type.description != "NA") %>%
      group_by(details.type.description) %>%
      summarise(count = n(), .groups = 'drop') %>%
      mutate(percentage = round((count / sum(count)) * 100, 1)) %>%
      arrange(desc(percentage))
    
    # Get ordered pitch types
    ordered_pitch_types <- pitch_type_stats$details.type.description
    
    for (ptype in ordered_pitch_types) {
      local({
        pt <- ptype
        plotname <- paste0("plot_", make.names(pt))
        
        # Get the percentage for this pitch type
        pt_percentage <- pitch_type_stats %>%
          filter(details.type.description == pt) %>%
          pull(percentage)
        
        pt_count <- pitch_type_stats %>%
          filter(details.type.description == pt) %>%
          pull(count)
        
        output[[plotname]] <- renderPlot({
          df_pt <- df %>% 
            filter(details.type.description == pt,
                   !is.na(details.type.description),
                   details.type.description != "",
                   details.type.description != "NA")
          
          x_abs <- max(abs(df_pt$pitchData.coordinates.pX), na.rm = TRUE)
          y_range <- range(df_pt$pitchData.coordinates.pZ, na.rm = TRUE)
          y_padding <- 0.25
          y_lower <- min(y_range[1] - y_padding, 0)
          y_upper <- max(y_range[2] + y_padding, 4.5)
          
          x_limit <- ceiling(x_abs * 1.1)
          
          # Create title with pitch type, percentage, and count
          plot_title <- paste0(pt, " (", pt_percentage, "% - ", pt_count, " pitches)")
          
          base_plot <- ggplot(df_pt, aes(x = pitchData.coordinates.pX,
                                         y = pitchData.coordinates.pZ)) +
            coord_fixed(
              xlim = c(-x_limit, x_limit),
              ylim = c(y_lower, y_upper)
            ) +
            labs(
              x = "Horizontal Location (ft)",
              y = "Vertical Location (ft)",
              title = plot_title,  # Updated title with percentage
              fill = "Pitch Result"
            ) +
            theme_minimal() +
            theme(plot.title = element_text(hjust = 0.5, size = 12)) +  # Adjusted size for longer titles
            geom_polygon(data = home_plate, aes(x = x, y = y),
                         fill = NA, color = "black", linewidth = 1, inherit.aes = FALSE)
          
          # Add visualization layers based on user selection
          if (input$plot_type == "heatmap") {
            # Calculate optimal contour parameters
            contour_params <- calculate_contour_params(df_pt, 
                                                       "pitchData.coordinates.pX", 
                                                       "pitchData.coordinates.pZ")
            
              # Heatmap only - using filled contours
              base_plot +
                stat_density_2d_filled(
                  alpha = 0.7,
                  h = contour_params$h,
                  bins = contour_params$bins,
                  breaks = function(x, ...) {
                    # Generate the normal breaks, ignoring extra arguments with ...
                    breaks_all <- scales::extended_breaks(n = contour_params$bins + 1)(x)
                    # Remove the first break (which creates the lowest/background contour)
                    breaks_all[-1]
                  },
                  show.legend = FALSE
                ) +
                scale_fill_viridis_d(option = "H", alpha = 0.7) +
                # Strike zone on top
                annotate("rect", xmin = -0.8333, xmax = 0.8333,
                         ymin = 1.5, ymax = 3.5, fill = NA, color = "black", linewidth = 1.5)
            
            
          } else if (input$plot_type == "individual") {
            # Individual pitches only (unchanged)
            base_plot +
              ggforce::geom_circle(aes(
                x0 = pitchData.coordinates.pX,
                y0 = pitchData.coordinates.pZ,
                r = 0.125,
                fill = specific_pitch_result
              ), color = "black", alpha = 1) +
              scale_fill_manual(values = c(
                "Called Strike" = "#800000", "Swinging Strike" = "red",
                "Foul" = "#fa8072", "Single" = "#dff2ff",
                "Double" = "#82eefd", "Triple" = "#0492c2",
                "Home Run" = "#00008b", "Out" = "#a020f0",
                "Other Batter Reach" = "blue", "Ball" = "#90ee90",
                "Hit By Pitch" = "#006400"
              )) +
              # Strike zone on top
              annotate("rect", xmin = -0.8333, xmax = 0.8333,
                       ymin = 1.5, ymax = 3.5, fill = NA, color = "black", linewidth = 1.5)
            
          } else if (input$plot_type == "both") {
            # Calculate optimal contour parameters for the "both" option too
            contour_params <- calculate_contour_params(df_pt, 
                                                       "pitchData.coordinates.pX", 
                                                       "pitchData.coordinates.pZ")
            
            # Both heatmap and individual pitches - using SAME filled contours as heatmap mode
            base_plot +
              # Heatmap layer first
              stat_density_2d_filled(
                alpha = 0.8,
                h = contour_params$h,
                bins = contour_params$bins,
                breaks = function(x, ...) {
                  breaks_all <- scales::extended_breaks(n = contour_params$bins + 1)(x)
                  breaks_all[-1]
                },
                show.legend = FALSE
              ) +
              scale_fill_viridis_d(option = "H", alpha = 0.9) +
              
              # NEW SCALE before second use of fill aesthetic
              ggnewscale::new_scale_fill() +
              
              # Circles with their own fill mapping
              ggforce::geom_circle(
                aes(
                  x0 = pitchData.coordinates.pX,
                  y0 = pitchData.coordinates.pZ,
                  r = 0.125,
                  fill = specific_pitch_result
                ),
                color = "black",
                alpha = 0.8
              ) +
              scale_fill_manual(
                name = "Pitch Result", 
                values = c(
                "Called Strike" = "#800000", "Swinging Strike" = "red",
                "Foul" = "#fa8072", "Single" = "#dff2ff",
                "Double" = "#82eefd", "Triple" = "#0092c2",
                "Home Run" = "#00008b", "Out" = "#a020f0",
                "Other Batter Reach" = "blue", "Ball" = "#90ee90",
                "Hit By Pitch" = "#006400"
              )) +
              
              # Strike zone
              annotate("rect", xmin = -0.8333, xmax = 0.8333,
                       ymin = 1.5, ymax = 3.5, fill = NA, color = "black", linewidth = 1.5)
          }
        })
      })
    }
  })
}

shinyApp(ui = ui, server = server)