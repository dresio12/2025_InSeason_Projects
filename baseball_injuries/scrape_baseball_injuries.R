library(dplyr)
library(rvest)
library(httr)
library(stringr)

###########################################################################
# Session-Based Scraper - Manual Browser Cookie Extraction
###########################################################################

# INSTRUCTIONS:
# 1. Go to https://www.prosportstransactions.com/baseball/Search/
# 2. Complete the human verification
# 3. Open Developer Tools
# 4. Select Network tab
# 5. Check boxes for DL/IL and missed due to injury (boxes 3 and 4)
# 6. Choose your Begin and End Date, Execute Search, 
     #Paste Search URL into test_url below
# 7. Right-click on the request and "Copy as cURL (cmd)"
# 8. Extract the cookies from the cURL command
# 9. Paste them in the cookies variable below


scrape_with_session <- function(rds_filename = "baseball_injuries_session.rds") {
  
  # Check if existing data file exists and load it
  if (file.exists(rds_filename)) {
    cat("Found existing data file:", rds_filename, "\n")
    all_injuries <- readRDS(rds_filename)
    cat("Loaded existing data with", nrow(all_injuries), "rows\n")
  } else {
    cat("No existing data file found. Starting fresh.\n")
    all_injuries <- data.frame()
  } 
 
  cookies <- c(
    "cf_clearance" = "YOUR_CLOUDFLARE_TOKEN_HERE_(dont include '^' at end of string"
  )
  
  cat("Using Cloudflare clearance token...\n")
  
  #checks to make sure your token is present
  if (cookies["cf_clearance"] == "YOUR_CLOUDFLARE_TOKEN_HERE_(dont include '^' at end of string") {
    stop("Please replace YOUR_CLOUDFLARE_TOKEN_HERE... with your actual Cloudflare token!")
  }
  
  # Create session handle
  session <- handle("https://www.prosportstransactions.com")
  
  # Set cookies using set_cookies
  cookie_header <- paste(names(cookies), cookies, sep="=", collapse="; ")
  
  # REPLACE WITH YOUR BROWSER: Set headers to match your browser
  # Update User-Agent and other headers from your cURL command
  headers <- add_headers(
    "User-Agent" = "YOUR_USER_AGENT_HERE",
    "Accept" = "text/html,application/xhtml+xml,application/xml;q=0.9,image/avif,image/webp,image/apng,*/*;q=0.8,application/signed-exchange;v=b3;q=0.7",
    "Accept-Language" = "en-US,en;q=0.9",
    "Referer" = "https://www.prosportstransactions.com/baseball/Search/Search.php",
    "Cookie" = cookie_header,
    "sec-ch-ua" = '"YOUR_BROWSER_INFO"',
    "sec-ch-ua-mobile" = "?0",
    "sec-ch-ua-platform" = '"YOUR_PLATFORM"',
    "sec-fetch-dest" = "document",
    "sec-fetch-mode" = "navigate",
    "sec-fetch-site" = "same-origin",
    "sec-fetch-user" = "?1",
    "upgrade-insecure-requests" = "1"
  )
  
  # Test the session
  test_url <- "YOUR_SEARCH_PAGE_URL_HERE"
  
  response <- GET(test_url, headers)
  
  if (status_code(response) != 200) {
    stop("Session cookies don't work. Status code: ", status_code(response))
  }
  
  cat("Session working! Starting scrape...\n")
  
  # Get pagination links
  page_content <- content(response)
  link_nodes <- page_content %>% html_nodes(".bodyCopy a")
  each_link <- link_nodes %>% html_attr("href")
  main_link <- "http://www.prosportstransactions.com/baseball/Search/"
  each_link <- paste0(main_link, each_link)
  all_links <- c(test_url, each_link) %>% unique()
  
  cat("Found", length(all_links), "pages to scrape\n")
  
  # Scrape all pages
  # Only reset if it's empty (i.e., no file was loaded)
  if (!exists("all_injuries") || nrow(all_injuries) == 0) {
    all_injuries <- data.frame()
  }
  
  
  for (i in 1:length(all_links)) {
    cat("Scraping page", i, "of", length(all_links), "\n")
    
    Sys.sleep(runif(1, 2, 5))
    
    response <- GET(all_links[i], headers)
    
    if (status_code(response) == 200) {
      page_content <- content(response)
      
      # Extract table
      tables <- page_content %>% html_nodes(".datatable") %>% html_table()
      
      if (length(tables) > 0) {
        each_page_df <- tables[[1]]
        
        if (nrow(each_page_df) > 1) {
          columnNames <- each_page_df[1,] %>% as.character()
          colnames(each_page_df) <- columnNames
          each_page_df <- each_page_df[-1,]
          
          each_page_df <- each_page_df %>% mutate_all(as.character)
          each_page_df$page_url <- all_links[i]
          each_page_df$page_num <- i
          
          all_injuries <- bind_rows(all_injuries, each_page_df)
          cat("Successfully scraped", nrow(each_page_df), "rows\n")
          
          # Save after each page
          saveRDS(all_injuries, "baseball_injuries_session.rds")
          cat("Data saved to baseball_injuries_session.rds (", nrow(all_injuries), "total rows)\n")
        }
      }
    } else {
      cat("Failed to access page", i, "- Status:", status_code(response), "\n")
    }
  }
  
  # Remove duplicates
  all_injuries <- all_injuries %>%
    distinct(Date, Team, Acquired, Relinquished, Notes, .keep_all = T)
  
  cat("Final dataset:", nrow(all_injuries), "rows\n")
  
  # Save results
  saveRDS(all_injuries, "baseball_injuries_session.rds")
  cat("Data saved to baseball_injuries_session.rds\n")
  
  return(all_injuries)
}

result <- scrape_with_session()

#remove leading bullet point from Acquired and Relinquished
result$Acquired <- gsub("^\\s*•\\s*", "", result$Acquired)
result$Relinquished <- gsub("^\\s*•\\s*", "", result$Relinquished)

result <- result |>
  arrange(desc(Date))

saveRDS(result, "baseball_injuries_session.rds")
