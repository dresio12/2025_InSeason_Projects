library(shiny)
library(DT)
library(dplyr)

# --- Load Leaderboard Data ---
leaderboards <- readRDS("cmd_leaderboard.rds")
leaderboard <- leaderboards$leaderboard          # base leaderboard
leaderboard_ext <- leaderboards$leaderboard_ext  # extended leaderboard with more columns
pitch_type_counts <- leaderboards$pitch_type_counts  # pitch-level counts by pitcher

# --- UI Layout ---
ui <- fluidPage(
  titlePanel("Pitch Command Leaderboard"),
  
  # Instruction text for user
  tags$p(
    style = "font-size: 14px; color: #555;",
    "Use the controls below to filter by year, minimum pitches, and minimum Overall Command+ score. 
   Click the checkbox to see the extended leaderboard. Click on a pitcher’s row to expand 
   detailed pitch type counts. Sort the table by any column by clicking the column header."
  ),
  
  # --- Custom CSS for styling ---
  tags$head(
    tags$style(HTML("
    /* Styling for input panels and table appearance */
    .row:first-of-type {
      background-color: #e6f3ff;
      padding: 15px;
      border-radius: 8px;
      margin-bottom: 15px;
    }
    .shiny-input-panel {
      background-color: #e6f3ff;
      padding: 10px;
      border-radius: 5px;
      margin-bottom: 10px;
    }
    .details-row {
      background-color: #f8f9fa;
      padding: 10px;
      border-left: 3px solid #007bff;
      margin: 5px 0;
      font-family: monospace;
    }
    .details-title {
      font-weight: bold;
      color: #495057;
      margin-bottom: 5px;
    }
    .dataTables_wrapper {
      font-size: 12px !important;
    }
    table.dataTable {
      background-color: transparent !important;
      border-collapse: separate !important;
      border-spacing: 0px !important;
    }
    table.dataTable thead th {
      background-color: #e6f3ff !important;
      color: #000;
      white-space: nowrap;
      border-right: 1px solid #d3d3d3 !important;
      border-bottom: 1px solid #d3d3d3 !important;
    }
    table.dataTable tbody td.thick-right-border {
      border-right: 3px solid black !important;
      border-bottom: 1px solid #d3d3d3 !important;
    }
    table.dataTable tbody td {
      border-right: 1px solid #d3d3d3 !important;
      border-bottom: 1px solid #d3d3d3 !important;
    }
    table.dataTable thead th.thick-right-border {
      border-right: 3px solid black !important;
    }
    .dt-nowrap {
      white-space: nowrap !important;
    }
  "))
  ),
  
  # --- JavaScript to handle expandable child rows ---
  tags$script(HTML("
    $(document).on('shiny:connected', function() {
      Shiny.addCustomMessageHandler('show_child_row', function(message) {
        var pitcher = message.pitcher;
        var html = message.html;
        $('#details_' + pitcher).html('<div class=\"details-row\"><div class=\"details-title\">Pitch Details for ' + message.original_pitcher + ':</div>' + html + '</div>');
      });
    });
  ")),
  
  # --- Input Controls ---
  fluidRow(
    column(4, selectInput("year", "Select Year:", 
                          choices = sort(unique(leaderboard$Year)), 
                          selected = max(leaderboard$Year))),
    column(4, numericInput("min_pitches", "Minimum Pitches:", value = 1000, min = 0, step = 10)),
    column(4, numericInput("min_cmd", "Minimum Cmd+ Overall:", value = 0, step = 1)),
    column(4, selectInput("pitch_filter", "Pitch Type Filter:",
                          choices = c("None", sort(names(pitch_type_counts)[-(1:2)])), # all pitch types except Year/Pitcher
                          selected = "None")),
    column(4, numericInput("min_pitch_type", "Min Pitches of Selected Type:", value = 0, min = 0, step = 10)),
    column(4, checkboxInput("extended", "Show Extended Leaderboard", value = FALSE))
  ),
  
  # --- Output: Main Leaderboard Table ---
  fluidRow(
    column(12, DTOutput("table"))
  )
)

# --- Server Logic ---
server <- function(input, output, session) {
  
  # Reactive: filter leaderboard based on user input
  filtered_leaderboard <- reactive({
    # Switch between standard and extended leaderboard
    lb <- if (input$extended) leaderboard_ext else leaderboard
    
    # Always filter by year, total pitches, and Cmd+ threshold
    lb <- lb |>
      filter(
        Year == input$year,
        Pitches >= input$min_pitches,
        `Cmd+ Overall` >= input$min_cmd
      )
    
    # Optional filter: require a minimum number of a selected pitch type
    if (input$pitch_filter != "None" && input$min_pitch_type > 0) {
      pitch_counts_year <- pitch_type_counts |>
        filter(Year == input$year) |>
        select(Pitcher, !!sym(input$pitch_filter))
      
      lb <- lb |>
        inner_join(pitch_counts_year, by = "Pitcher") |>           # bring in pitch counts
        filter(.data[[input$pitch_filter]] >= input$min_pitch_type) |>  # apply threshold
        select(-all_of(input$pitch_filter))   # drop helper column so it doesn’t appear in the table
    }
    
    # Round numeric columns for cleaner display
    lb |> mutate(across(where(is.numeric), ~round(., 0)))
  })
  
  # Render the leaderboard table
  output$table <- renderDT({
    df <- filtered_leaderboard()
    overall_col_index <- which(names(df) == "Cmd+ Overall") - 1   # for default sort
    
    # Used for styling thick border placement
    thick_col_index <- ncol(df) - 5
    
    datatable(
      df,
      rownames = FALSE,
      escape = FALSE,
      options = list(
        pageLength = 25,
        lengthMenu = c(10, 25, 50, 100, 250, 500, 1000),
        scrollX = TRUE,
        order = list(list(overall_col_index, 'desc')),  # sort by Cmd+ Overall by default
        columnDefs = list(
          list(className = "dt-nowrap", targets = "_all"),
          list(targets = 2, className = "thick-right-border"),  # after Pitches column
          list(targets = thick_col_index, className = "thick-right-border")  # before Cmd+ Overall
        ),
        # Add click-to-expand functionality
        rowCallback = JS(
          "function(row, data) {",
          "  $(row).css('cursor', 'pointer');",
          "  $(row).on('click', function() {",
          "    var table = $(this).closest('table').DataTable();",
          "    var tr = $(this);",
          "    var row_dt = table.row(tr);",
          "    if (row_dt.child.isShown()) {",
          "      row_dt.child.hide();",
          "      tr.removeClass('shown');",
          "    } else {",
          "      var pitcher = data[1];",
          "      Shiny.setInputValue('clicked_pitcher', {pitcher: pitcher, timestamp: new Date().getTime()});",
          "      row_dt.child('<div id=\"details_' + pitcher.replace(/[^a-zA-Z0-9]/g, '_') + '\"></div>').show();",
          "      tr.addClass('shown');",
          "    }",
          "  });",
          "}"
        )
      )
    )
  })
  
  # When a row is clicked: show expanded pitch-type breakdown
  observeEvent(input$clicked_pitcher, {
    pitcher <- input$clicked_pitcher$pitcher
    row_details <- pitch_type_counts |>
      filter(Year == input$year, Pitcher == pitcher) |>
      select(-Year, -Pitcher)
    
    if (nrow(row_details) > 0) {
      pitch_counts <- row_details[1, ]
      
      # Define pitch groupings
      pitch_types_main <- c("FF", "SI", "FC", "FS", "CU", "KC", "SL", "ST", "SV", "CH", "FO")
      pitch_types_extra <- c("SC", "KN", "EP")
      group_types <- c("Fastball", "Breaking", "Offspeed")
      
      # Helper: build section text only if pitcher threw that type
      create_section <- function(types, section_name) {
        available_types <- intersect(types, names(pitch_counts))
        if (length(available_types) > 0) {
          non_zero_types <- available_types[pitch_counts[available_types] > 0]
          if (length(non_zero_types) > 0) {
            counts_text <- paste(paste(non_zero_types, pitch_counts[non_zero_types], sep = ": "), collapse = ", ")
            return(paste0("<strong>", section_name, ":</strong> ", counts_text))
          }
        }
        return(NULL)
      }
      
      # Build expandable breakdown text
      html_parts <- c()
      if (!is.null(create_section(pitch_types_main, "Main Pitches"))) 
        html_parts <- c(html_parts, create_section(pitch_types_main, "Main Pitches"))
      if (!is.null(create_section(pitch_types_extra, "Other Pitches"))) 
        html_parts <- c(html_parts, create_section(pitch_types_extra, "Other Pitches"))
      if (!is.null(create_section(group_types, "Pitch Groups"))) 
        html_parts <- c(html_parts, create_section(group_types, "Pitch Groups"))
      
      html_content <- if(length(html_parts) > 0) paste(html_parts, collapse = "<br/>") else "No pitch data available"
      
      # Send expandable content to client
      session$sendCustomMessage("show_child_row", list(
        pitcher = gsub("[^a-zA-Z0-9]", "_", pitcher),
        original_pitcher = pitcher,
        html = html_content
      ))
    }
  })
}

# --- Launch the Shiny App ---
shinyApp(ui, server)
