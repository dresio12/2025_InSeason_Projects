library(shiny)
library(DT)
library(dplyr)

leaderboards <- readRDS("cmd_leaderboard.rds")
leaderboard <- leaderboards$leaderboard
leaderboard_ext <- leaderboards$leaderboard_ext
pitch_type_counts <- leaderboards$pitch_type_counts

ui <- fluidPage(
  titlePanel("Pitch Command Leaderboard"),
  
  tags$p(
    style = "font-size: 14px; color: #555;",
    "Use the controls below to filter by year, minimum pitches, and minimum Overall Command+ score. 
   Click the checkbox to see the extended leaderboard. Click on a pitcher’s row to expand 
   detailed pitch type counts. Sort the table by any column by clicking the column header."
  ),
  
  tags$head(
    tags$style(HTML("
    /* Light blue background for the selection panel row */
    .row:first-of-type {
      background-color: #e6f3ff;
      padding: 15px;
      border-radius: 8px;
      margin-bottom: 15px;
    }

    /* Selection panel background */
    .shiny-input-panel {
      background-color: #e6f3ff;
      padding: 10px;
      border-radius: 5px;
      margin-bottom: 10px;
    }

    /* Details row style */
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

    /* Apply smaller font size globally to DT widget */
    .dataTables_wrapper {
      font-size: 12px !important;
    }

    /* Table formatting */
    table.dataTable {
      background-color: transparent !important;
      border-collapse: separate !important;
      border-spacing: 0px !important;
    }

    /* Header bar */
    table.dataTable thead th {
      background-color: #e6f3ff !important;
      color: #000;
      white-space: nowrap;
      border-right: 1px solid #d3d3d3 !important;
      border-bottom: 1px solid #d3d3d3 !important;
    }

    /* Thick black vertical borders for specific columns */
    table.dataTable tbody td.thick-right-border {
      border-right: 3px solid black !important;
      border-bottom: 1px solid #d3d3d3 !important;
    }

    /* Gray borders for all other cells */
    table.dataTable tbody td {
      border-right: 1px solid #d3d3d3 !important;
      border-bottom: 1px solid #d3d3d3 !important;
    }
    
    /* Ensure header has same vertical separation */
    table.dataTable thead th.thick-right-border {
      border-right: 3px solid black !important;
    }

    /* Prevent text wrapping */
    .dt-nowrap {
      white-space: nowrap !important;
    }
  "))
  )
  ,
  
  tags$script(HTML("
    $(document).on('shiny:connected', function() {
      Shiny.addCustomMessageHandler('show_child_row', function(message) {
        var pitcher = message.pitcher;
        var html = message.html;
        $('#details_' + pitcher).html('<div class=\"details-row\"><div class=\"details-title\">Pitch Details for ' + message.original_pitcher + ':</div>' + html + '</div>');
      });
    });
  ")),
  
  fluidRow(
    column(3, selectInput("year", "Select Year:", 
                          choices = sort(unique(leaderboard$Year)), 
                          selected = max(leaderboard$Year))),
    column(3, numericInput("min_pitches", "Minimum Pitches:", value = 1000, min = 0, step = 10)),
    column(3, numericInput("min_cmd", "Minimum Cmd+ Overall:", value = 0, step = 1)),
    column(3, checkboxInput("extended", "Show Extended Leaderboard", value = FALSE))
    
  ),
  
  fluidRow(
    column(12, DTOutput("table"))
  )
)

server <- function(input, output, session) {
  
  filtered_leaderboard <- reactive({
    lb <- if (input$extended) leaderboard_ext else leaderboard
    lb <- lb %>%
      filter(
        Year == input$year,
        Pitches >= input$min_pitches,
        `Cmd+ Overall` >= input$min_cmd
      ) %>%
      mutate(across(where(is.numeric), ~round(., 0)))
    lb
  })
  
  output$table <- renderDT({
    df <- filtered_leaderboard()
    overall_col_index <- which(names(df) == "Cmd+ Overall") - 1
    
    # Determine the column before overall for the thick black border
    thick_col_index <- ncol(df) - 5
    
    datatable(
      df,
      rownames = FALSE,
      escape = FALSE,
      options = list(
        pageLength = 25,
        lengthMenu = c(10, 25, 50, 100, 250, 500, 1000),
        scrollX = TRUE,
        order = list(list(overall_col_index, 'desc')),
        columnDefs = list(
          list(className = "dt-nowrap", targets = "_all"),
          list(targets = 2, className = "thick-right-border"),  # after Pitches
          list(targets = thick_col_index, className = "thick-right-border")  # before Cmd+ Overall
        ),
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
  
  observeEvent(input$clicked_pitcher, {
    pitcher <- input$clicked_pitcher$pitcher
    row_details <- pitch_type_counts %>%
      filter(Year == input$year, Pitcher == pitcher) %>%
      select(-Year, -Pitcher)
    
    if (nrow(row_details) > 0) {
      pitch_counts <- row_details[1, ]
      
      pitch_types_main <- c("FF", "SI", "FC", "FS", "CU", "KC", "SL", "ST", "SV", "CH", "FO")
      pitch_types_extra <- c("SC", "KN", "EP")
      group_types <- c("Fastball", "Breaking", "Offspeed")
      
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
      
      html_parts <- c()
      main_section <- create_section(pitch_types_main, "Main Pitches")
      if (!is.null(main_section)) html_parts <- c(html_parts, main_section)
      extra_section <- create_section(pitch_types_extra, "Other Pitches")
      if (!is.null(extra_section)) html_parts <- c(html_parts, extra_section)
      group_section <- create_section(group_types, "Pitch Groups")
      if (!is.null(group_section)) html_parts <- c(html_parts, group_section)
      
      html_content <- if(length(html_parts) > 0) paste(html_parts, collapse = "<br/>") else "No pitch data available"
      
      session$sendCustomMessage("show_child_row", list(
        pitcher = gsub("[^a-zA-Z0-9]", "_", pitcher),
        original_pitcher = pitcher,
        html = html_content
      ))
    }
  })
}

shinyApp(ui, server)
