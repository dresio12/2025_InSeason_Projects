library(shiny)
library(DT)
library(dplyr)
library(shinythemes)

# Load your data
r5_all <- readRDS("r5_all.rds") |> select(1:10)
r5_bsplit <- readRDS("r5_bsplit.rds")
r5_psplit <- readRDS("r5_psplit.rds")
r5_btot <- readRDS("r5_btot.rds")
r5_ptot <- readRDS("r5_ptot.rds")

r5_advh <- readRDS("r5_advh.rds") |> select(-20, -21)
r5_advp <- readRDS("r5_advp.rds") |> select(-19, -20)

# Higher-is-better columns
advh_cols <- c("PS Score", "xwOBA", "wOBA", "xBA", "BA", "OBP", "xSLG", "SLG", 
               "BB%", "Max EV", "Spd", "Z-Contact%", "Hard Hit%", "PullAir%")
advp_cols <- c("PS Score", "Max Velo", "K%", "Chase%", "Whiff%", "SwStr%")

# Lower-is-better columns
advh_cols_bad <- c("K%", "Chase%", "Whiff%", "SwStr%")
advp_cols_bad <- c("xwOBA", "wOBA", "xBA", "BA", "OBP", "xSLG", "SLG", "BB%",
                   "Hard Hit%", "Z-Contact%")

# Dataset classification
dataset_types <- list(
  "R5 Roster" = "other",
  "Batting Split" = "batting",
  "Pitching Split" = "pitching",
  "Batting Totals" = "batting",
  "Pitching Totals" = "pitching",
  "Advanced Hitting" = "adv_batting",
  "Advanced Pitching" = "adv_pitching"
)

# Helper for creating tabs
dataset_tab <- function(name) {
  tabPanel(
    name,
    fluidPage(
      fluidRow(
        column(3, uiOutput(paste0("org_filter_", name))),
        column(3, uiOutput(paste0("position_filter_", name))),
        column(3, uiOutput(paste0("level_filter_", name))),
        column(3, uiOutput(paste0("pitches_filter_ui_", name)))
      ),
      br(),
      downloadButton(paste0("download_", name), "Download CSV"),
      br(), br(),
      DTOutput(paste0("table_", name))
    )
  )
}

# UI
ui <- fluidPage(
  theme = shinytheme("superhero"),
  tags$head(
    tags$style(HTML("
      th, td {
        white-space: nowrap;
        overflow: hidden;
        text-overflow: ellipsis;
        color: #ffffff !important;
      }
      .dataTables_scrollBody {
        background-color: #2b3e50;
      }
      .dataTables_wrapper .dataTables_filter label,
      .dataTables_wrapper .dataTables_length label {
        color: white !important;
      }
    "))
  ),
  titlePanel("R5 Minor League Player Data"),
  
  # --- Main Data Tabs ---
  tabsetPanel(
    dataset_tab("R5 Roster"),
    dataset_tab("Batting Split"),
    dataset_tab("Pitching Split"),
    dataset_tab("Batting Totals"),
    dataset_tab("Pitching Totals")
  ),
  
  br(), hr(), br(),
  
  # --- Advanced Data Tabs ---
  h3("Advanced Metrics", style = "color: #fff; text-align:center;"),
  tabsetPanel(
    dataset_tab("Advanced Hitting"),
    dataset_tab("Advanced Pitching")
  ),
  
  br(),
  div(
    style = "text-align:center; font-size: 12px; color: gray; margin-top: 20px;",
    "Statistical and eligibility data acquired from FanGraphs and RosterResource.
    Level in the R5 Roster tab is the FanGraphs projected level – for the player's
    actual level, refer to their MiLB profile. Advanced Stats courtesy of Prospect
    Savant."
  )
)

# Server
server <- function(input, output, session) {
  
  datasets <- list(
    "R5 Roster" = r5_all,
    "Batting Split" = r5_bsplit,
    "Pitching Split" = r5_psplit,
    "Batting Totals" = r5_btot,
    "Pitching Totals" = r5_ptot,
    "Advanced Hitting" = r5_advh,
    "Advanced Pitching" = r5_advp
  )
  
  # Helper function to create gradient colors (Excel-like Red-Yellow-Green)
  make_gradient <- function(vals, good_color = TRUE) {
    # Remove NAs for range calculation
    clean_vals <- vals[!is.na(vals)]
    if (length(clean_vals) == 0) return(list(vals = vals, colors = rep("transparent", length(vals))))
    
    min_val <- min(clean_vals)
    max_val <- max(clean_vals)
    
    # Normalize values to 0-1 scale
    if (max_val == min_val) {
      normalized <- rep(0.5, length(vals))
    } else {
      normalized <- (vals - min_val) / (max_val - min_val)
    }
    
    # Create Red-Yellow-Green color gradient
    colors <- sapply(seq_along(vals), function(i) {
      x <- normalized[i]
      if (is.na(vals[i])) return("transparent")
      
      if (!good_color) {
        # Reverse for "lower is better" metrics
        x <- 1 - x
      }
      
      # Red-Yellow-Green gradient
      if (x < 0.5) {
        # Red to Yellow (0 to 0.5)
        ratio <- x * 2
        r <- 248
        g <- round(105 + (255 - 105) * ratio)
        b <- round(107 + (117 - 107) * ratio)
      } else {
        # Yellow to Green (0.5 to 1)
        ratio <- (x - 0.5) * 2
        r <- round(255 - (255 - 99) * ratio)
        g <- round(255 - (255 - 190) * ratio)
        b <- round(117 - (117 - 123) * ratio)
      }
      
      sprintf("rgb(%d,%d,%d)", r, g, b)
    })
    
    return(list(vals = vals, colors = colors))
  }
  
  for (name in names(datasets)) {
    local({
      nm <- name
      df <- datasets[[nm]]
      type <- dataset_types[[nm]]
      
      # Filters
      output[[paste0("org_filter_", nm)]] <- renderUI({
        if ("Organization" %in% names(df))
          selectInput(paste0("organization_", nm), "Organization:",
                      choices = c("All", sort(unique(df$Organization))),
                      selected = "All")
      })
      
      output[[paste0("position_filter_", nm)]] <- renderUI({
        if ("Position" %in% names(df))
          selectInput(paste0("position_", nm), "Position:",
                      choices = c("All", sort(unique(df$Position))),
                      selected = "All")
      })
      
      output[[paste0("level_filter_", nm)]] <- renderUI({
        if ("Level" %in% names(df))
          selectInput(paste0("level_", nm), "Level:",
                      choices = c("All", sort(unique(df$Level))),
                      selected = "All")
      })
      
      output[[paste0("pitches_filter_ui_", nm)]] <- renderUI({
        if (type %in% c("batting", "adv_batting"))
          numericInput(paste0("pitches_filter_", nm), "Minimum PA:", value = 50, min = 0)
        else if (type %in% c("pitching", "adv_pitching"))
          numericInput(paste0("pitches_filter_", nm), "Minimum Pitches:", value = 100, min = 0)
      })
      
      # Filtered data reactive
      filtered_data <- reactive({
        tmp <- df
        
        org_input <- input[[paste0("organization_", nm)]]
        pos_input <- input[[paste0("position_", nm)]]
        lvl_input <- input[[paste0("level_", nm)]]
        pitch_input <- input[[paste0("pitches_filter_", nm)]]
        
        if (!is.null(org_input) && org_input != "All") tmp <- tmp |> filter(Organization == org_input)
        if (!is.null(pos_input) && pos_input != "All") tmp <- tmp |> filter(Position == pos_input)
        if (!is.null(lvl_input) && lvl_input != "All") tmp <- tmp |> filter(Level == lvl_input)
        
        if (!is.null(pitch_input)) {
          if (type %in% c("batting", "adv_batting") && "PA" %in% names(tmp)) tmp <- tmp |> filter(PA >= pitch_input)
          if (type %in% c("pitching", "adv_pitching") && "Pitches" %in% names(tmp)) tmp <- tmp |> filter(Pitches >= pitch_input)
        }
        
        tmp
      })
      
      # Render Table
      output[[paste0("table_", nm)]] <- renderDT({
        tmp <- filtered_data()
        
        # Default sort
        order_opt <- NULL
        if (nm == "R5 Roster" && "Org Rank" %in% names(tmp))
          order_opt <- list(list(which(names(tmp) == "Org Rank") - 1, 'asc'))
        else if (type %in% c("batting", "adv_batting") && "wRC+" %in% names(tmp))
          order_opt <- list(list(which(names(tmp) == "wRC+") - 1, 'desc'))
        else if (type %in% c("pitching", "adv_pitching") && "ERA" %in% names(tmp))
          order_opt <- list(list(which(names(tmp) == "ERA") - 1, 'asc'))
        else if (grepl("Advanced", nm) && "PS Score" %in% names(tmp))
          order_opt <- list(list(which(names(tmp) == "PS Score") - 1, 'desc'))
        
        dt <- datatable(
          tmp,
          options = list(
            scrollY = "400px",
            scrollX = TRUE,
            pageLength = 10,
            scrollCollapse = TRUE,
            paging = FALSE,
            order = order_opt
          ),
          rownames = FALSE
        )
        
        # Gradient formatting for advanced datasets (Red-Yellow-Green)
        if (nm == "Advanced Hitting") {
          for (col in advh_cols) {
            if (col %in% names(tmp)) {
              gradient <- make_gradient(tmp[[col]], good_color = TRUE)
              dt <- dt |> formatStyle(col, backgroundColor = styleEqual(gradient$vals, gradient$colors))
            }
          }
          for (col in advh_cols_bad) {
            if (col %in% names(tmp)) {
              gradient <- make_gradient(tmp[[col]], good_color = FALSE)
              dt <- dt |> formatStyle(col, backgroundColor = styleEqual(gradient$vals, gradient$colors))
            }
          }
        }
        if (nm == "Advanced Pitching") {
          for (col in advp_cols) {
            if (col %in% names(tmp)) {
              gradient <- make_gradient(tmp[[col]], good_color = TRUE)
              dt <- dt |> formatStyle(col, backgroundColor = styleEqual(gradient$vals, gradient$colors))
            }
          }
          for (col in advp_cols_bad) {
            if (col %in% names(tmp)) {
              gradient <- make_gradient(tmp[[col]], good_color = FALSE)
              dt <- dt |> formatStyle(col, backgroundColor = styleEqual(gradient$vals, gradient$colors))
            }
          }
        }
        
        dt
      })
      
      # Download CSV
      output[[paste0("download_", nm)]] <- downloadHandler(
        filename = function() paste0(gsub(" ", "_", nm), "_filtered.csv"),
        content = function(file) write.csv(filtered_data(), file, row.names = FALSE)
      )
    })
  }
}

# Run App
shinyApp(ui, server)