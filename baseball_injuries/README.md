# Scrapes Baseball Injury Data from Pro Sports Transactions

The .R file gives a template for scraping from Pro Sports Transactions. The .rds file it creates saves after every page is downloaded so that if the function needs to be stopped due to an expired Cloudfare clearance, or any other reason, no progress is lost. The script does not have the ability to pick up where it left off, so you'll need to change your begin date based on the last page scraped, execute a new search, and update the test_url and Cloudfare token. Lines at the end of the .R file will remove duplicate rows.
