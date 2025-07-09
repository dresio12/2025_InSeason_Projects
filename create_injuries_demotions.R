library(dplyr)
library(rvest)
library(httr)
library(stringr)

###########################################################################
# Session-Based Scraper - Manual Browser Cookie Extraction
###########################################################################

# INSTRUCTIONS:
# 1. Open your browser and go to the website
# 2. Complete the human verification
# 3. Open Developer Tools (F12)
# 4. Go to Network tab
# 5. Make a request to the search page
# 6. Right-click on the request and "Copy as cURL"
# 7. Extract the cookies from the cURL command
# 8. Paste them in the cookies variable below

scrape_with_session <- function(rds_filename = "baseball_injdem_session.rds") {
  
  # Check if existing data file exists and load it
  if (file.exists(rds_filename)) {
    cat("Found existing data file:", rds_filename, "\n")
    all_injuries <- readRDS(rds_filename)
    cat("Loaded existing data with", nrow(all_injuries), "rows\n")
  } else {
    cat("No existing data file found. Starting fresh.\n")
    all_injuries <- data.frame()
  }
  
  # Extracted from your cURL command
  cookies <- c(
    "cf_clearance" = "cdM0NtrvtmYqnGDO_vWwH29e51cGNOsCD7XPsdjiBTc-1751782559-1.2.1.1-jEdbSdqnTOANzJ7DaOVQphKp1TOXKu5fruHzkqx8L00PrDiHgfpRNDJNCWXTm8nhuouQ4ipZvAnlc_NCoXjgU1xFSG2rAINBOwRlD7EC8Iwu8mWgWOUkUbklm5prWwGgYLwcH_v0lk.hPpfHnr1zWYZdRSpVNFWn1YD2hZBRTNMC6cmq9m.ey1xUNi7CFdXHQwXh7fIoQW0bK1qEL1AgTsi0R51tyrLq0qIKFwCeQxA"
  )
  
  cat("Using Cloudflare clearance token from your browser session...\n")
  
  # Create session handle
  session <- handle("https://www.prosportstransactions.com")
  
  # Set cookies using set_cookies
  cookie_header <- paste(names(cookies), cookies, sep="=", collapse="; ")
  
  # Set headers to match your browser exactly (including cookies)
  headers <- add_headers(
    "User-Agent" = "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/134.0.0.0 Safari/537.36 OPR/119.0.0.0",
    "Accept" = "text/html,application/xhtml+xml,application/xml;q=0.9,image/avif,image/webp,image/apng,*/*;q=0.8,application/signed-exchange;v=b3;q=0.7",
    "Accept-Language" = "en-US,en;q=0.9",
    "Referer" = "https://www.prosportstransactions.com/baseball/Search/Search.php",
    "Cookie" = cookie_header,
    "sec-ch-ua" = '"Chromium";v="134", "Not:A-Brand";v="24", "Opera GX";v="119"',
    "sec-ch-ua-mobile" = "?0",
    "sec-ch-ua-platform" = '"Windows"',
    "sec-fetch-dest" = "document",
    "sec-fetch-mode" = "navigate",
    "sec-fetch-site" = "same-origin",
    "sec-fetch-user" = "?1",
    "upgrade-insecure-requests" = "1"
  )
  
  # Test the session
  test_url <- "https://www.prosportstransactions.com/baseball/Search/SearchResults.php?Player=&Team=&BeginDate=2025-03-15&EndDate=2025-07-02&MinorsChkBx=yes&DLChkBx=yes&submit=Search"
  
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
          saveRDS(all_injuries, "baseball_injdem_session.rds")
          cat("Data saved to baseball_injdem_session.rds (", nrow(all_injuries), "total rows)\n")
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
  saveRDS(all_injuries, "baseball_injdem_session.rds")
  cat("Data saved to baseball_injdem_session.rds\n")
  
  return(all_injuries)
}

# To extract cookies from browser:
# 1. Complete CAPTCHA in browser
# 2. F12 -> Network tab
# 3. Visit the search page
# 4. Right-click request -> Copy as cURL
# 5. Extract cookie values and add to cookies variable above

# Ready to run with your cookies:
result <- scrape_with_session()

result <- readRDS("baseball_injdem_session.rds")

#remove leading bullet point from Acquired and Relinquished
result$Acquired <- gsub("^\\s*•\\s*", "", result$Acquired)
result$Relinquished <- gsub("^\\s*•\\s*", "", result$Relinquished)

result <- result |> select(-page_num, -page_url)

result <- result |>
  mutate(
    Acquired = str_remove_all(Acquired, "\\s*\\([^)]*\\)"),
    Relinquished = str_remove_all(Relinquished, "\\s*\\([^)]*\\)")
  )

team_lookup <- tibble::tibble(
  Team_Full = c("Diamondbacks", "Braves", "Orioles", "Red Sox", "White Sox", 
                "Cubs", "Reds", "Guardians", "Rockies", "Tigers", "Astros", 
                "Royals", "Angels", "Dodgers", "Marlins", "Brewers", "Twins", 
                "Yankees", "Mets", "Athletics", "Phillies", "Pirates", 
                "Padres", "Giants", "Mariners", "Cardinals", "Rays", 
                "Rangers", "Blue Jays", "Nationals", "Indians"),
  Team_Abbr = c("ARI", "ATL", "BAL", "BOS", "CHW", "CHC", "CIN", "CLE", "COL", 
                "DET", "HOU", "KCR", "LAA", "LAD", "MIA", "MIL", "MIN", "NYY", 
                "NYM", "ATH", "PHI", "PIT", "SDP", "SFG", "SEA", "STL", "TBR", 
                "TEX", "TOR", "WSN", "CLE")
)

result <- result |>
  left_join(team_lookup, by = c("Team" = "Team_Full")) |>
  mutate(Team = Team_Abbr) |>
  select(-Team_Abbr)


result <- result %>%
  mutate(Relinquished = str_split(Relinquished, "•")) %>%  # split into list of names
  unnest(Relinquished) %>%                             # explode list into separate rows
  mutate(Relinquished = str_trim(Relinquished))            # remove leading/trailing whitespace

result <- result %>%
  mutate(Acquired = str_split(Acquired, "•")) %>%  # split into list of names
  unnest(Acquired) %>%                             # explode list into separate rows
  mutate(Acquired = str_trim(Acquired))            # remove leading/trailing whitespace

result <- result %>%
  filter(
    !grepl("spring training ", Notes)
  ) |>
  unique()

result$Date <- as.Date(result$Date)

saveRDS(result, "baseball_injdem.rds")

