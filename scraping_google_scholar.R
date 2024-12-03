library(rvest)
library(dplyr)
library(purrr)
library(stringr)
rm(list = ls())
# Base URL for Google Scholar search
base_url <- "https://scholar.google.com/scholar"

# Define the search term
search_term1 <- "Ministerial Replacement"
search_term <- "Cabinet Instability"

# Number of pages to scrape
num_pages <- 10  # Adjust this to scrape more or fewer pages

# Initialize lists to store articles and authors
all_articles <- list()
all_authors <- list()

# Function to retrieve author profile URLs and proper names
# Function to retrieve author profile URLs and proper names
get_author_profiles <- function(authors) {
  # Split the authors string into individual names
  author_list <- str_split(authors, ",\\s*")[[1]]
  # Initialize a named list for author URLs and proper names
  author_data <- tibble(Author = character(), Profile_URL = character(), Proper_Name = character())
  
  # Iterate over each author and construct their Google Scholar profile URL
  for (author in author_list) {
    # Construct the search URL for the author
    author_search_url <- paste0("https://scholar.google.com/citations?view_op=search_authors&mauthors=", URLencode(author))
    cat("Searching author profile:", author_search_url, "\n")
    
    # Try to retrieve the author's profile search page
    author_page <- tryCatch(
      { read_html(author_search_url) },
      error = function(e) {
        message("Error reading author search page for ", author)
        NULL
      }
    )
    
    # Extract the first author profile link if available
    profile_url <- if (!is.null(author_page)) {
      author_page %>%
        html_node(".gs_ai_name a") %>%
        html_attr("href") %>%
        { if (!is.na(.)) paste0("https://scholar.google.com", .) else "No profile available" }
    } else {
      "Error loading profile page"
    }
    
    # If the profile URL is valid, visit it to extract the proper name
    proper_name <- if (profile_url != "No profile available" && profile_url != "Error loading profile page") {
      profile_page <- tryCatch(
        { read_html(profile_url) },
        error = function(e) {
          message("Error reading profile page for URL: ", profile_url)
          NULL
        }
      )
      if (!is.null(profile_page)) {
        profile_page %>%
          html_node("#gsc_prf_in") %>%
          html_text(trim = TRUE)
      } else {
        "Error loading profile page"
      }
    } else {
      "No proper name available"
    }
    
    # Add author, profile URL, and proper name to the dataset
    author_data <- bind_rows(author_data, tibble(Author = author, Profile_URL = profile_url, Proper_Name = proper_name))
  }
  
  return(author_data)
}


# Loop through the pages
for (i in seq(0, by = 10, length.out = num_pages)) {
  # Construct the URL for the current page
  page_url <- paste0(base_url, "?start=", i, "&q=", URLencode(search_term))
  
  # Print the current URL being scraped
  cat("Scraping:", page_url, "\n")
  
  # Download the page
  page <- tryCatch(
    { read_html(page_url) },
    error = function(e) {
      message("Error reading page: ", page_url)
      NULL
    }
  )
  
  # Skip to the next iteration if the page could not be loaded
  if (is.null(page)) next
  
  # Extract the articles from the page
  articles <- page %>%
    html_nodes(".gs_r") %>%
    map_df(~{
      title <- .x %>% html_node(".gs_rt a") %>% html_text(trim = TRUE)
      url <- .x %>% html_node(".gs_rt a") %>% html_attr("href")
      journal <- .x %>% html_node(".gs_a") %>% html_text(trim = TRUE) %>% str_extract(" - (.+)$") %>% str_trim()
      authors <- .x %>% html_node(".gs_a") %>% html_text(trim = TRUE) %>% str_extract("^[^-]+") %>% str_trim()
      citations <- .x %>% html_node(".gs_fl a:nth-child(3)") %>% html_text(trim = TRUE) %>%
        str_extract("[0-9]+") %>% as.integer()

      citations_url <- .x %>% html_node(".gs_fl a:nth-child(3)") %>% html_attr("href") %>%
        { if (!is.na(.)) paste0("https://scholar.google.com", .) else "No citation URL available" }
      
      
      # Retrieve author profile URLs and proper names
      author_profiles <- get_author_profiles(authors)
      
      # Append author profiles to the global authors dataset
      all_authors <<- bind_rows(all_authors, author_profiles)
      
      # Combine data into a single row with dynamic columns for authors
      author_names <- author_profiles$Proper_Name
      author_columns <- set_names(as.list(author_names), paste0("Author_", seq_along(author_names)))
      tibble(
        Title = title,
        URL = url,
        Journal = journal,
        Amount_Citation = citations,
        Citation_URL = citations_url
      ) %>%
        bind_cols(author_columns)
    })
  
  # Append the articles to the global dataset
  all_articles[[length(all_articles) + 1]] <- articles
}

# Combine all pages into a single dataset
articles_df <- bind_rows(all_articles)

# Remove duplicate authors from the authors dataset
authors_df <- distinct(all_authors)

#saveRDS(articles_df, "C:/Users/User/Documents/GitHub/scraping-google-scholar/articles_df_ministerial_replacement.rds")
#saveRDS(authors_df, "C:/Users/User/Documents/GitHub/scraping-google-scholar/authors_df_ministerial_replacement.rds")

# Display the datasets
cat("Articles Dataset:\n")
print(articles_df)

cat("\nAuthors Dataset:\n")
print(authors_df)

articles_df_ministerial_replacement <- readRDS( "C:/Users/User/Documents/GitHub/scraping-google-scholar/articles_df_ministerial_replacement.rds")
authors_df_ministerial_replacement <- readRDS( "C:/Users/User/Documents/GitHub/scraping-google-scholar/authors_df_ministerial_replacement.rds")
citations_df<- readRDS("C:/Users/User/Documents/GitHub/scraping-google-scholar/citations_df_partial_36_ministerial_replacement.rds")
# Scrape citations: -------------------------------------------------------

library(rvest)
library(dplyr)
library(purrr)
library(stringr)

# Base URL of the citations page
#base_url <- "https://scholar.google.com/scholar?cites=6401457674541723717&as_sdt=20000005&sciodt=0,21&hl=en&oe=ASCII"

get_num_pages <- function(url) {
  page <- read_html(url)
  total_results_text <- page %>%
    html_node("#gs_ab_md") %>%
    html_text(trim = TRUE)
  total_results <- as.numeric(stringr::str_extract(total_results_text, "\\d+"))
  results_per_page <- 10
  num_pages <- ceiling(total_results / results_per_page)
  return(num_pages)
}

#articles_df <- articles_df_ministerial_replacement
#authors_df <- authors_df_ministerial_replacement

articles_df <- articles_df %>%
  filter(Citation_URL != unique(citations_df$Citation_URL))

library(httr)
user_agents <- c(
  "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/85.0.4183.121 Safari/537.36",
  "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/85.0.4183.121 Safari/537.36"
)
random_agent <- sample(user_agents, 1)


page <- tryCatch(
  {
    httr::GET(page_url, user_agent(random_agent)) %>%
      content(as = "text") %>%
      read_html()
  },
  error = function(e) {
    message("Error reading page: ", page_url)
    NULL
  }
)

for(base_url in unique(articles_df$Citation_URL[articles_df$Citation_URL != "No citation URL available" & !is.na(articles_df$Citation_URL)])[2]){
  # Function to calculate the number of pages
  # Calculate the total number of pages
  num_pages <- get_num_pages(base_url)
  
  # Initialize an empty list to store all citation data
  all_citations <- list()
  
  # Loop through each page of results
  for (i in seq(0, by = 10, length.out = num_pages)) {
    
    # Sample a random sleep duration
    sleep_duration <- sample(1:5, 1)
    cat("Sleeping for", sleep_duration, "seconds...\n")
    Sys.sleep(sleep_duration)
    
    # Construct the URL for the current page
    page_url <- paste0(base_url, "&start=", i)
    cat("Scraping page:", page_url, "\n")
    
    # Download the page
    page <- tryCatch(
      { read_html(page_url) },
      error = function(e) {
        message("Error reading page: ", page_url)
        NULL
      }
    )
    
    if (is.null(page)) next
    
    # Extract citation details
    citations <- page %>%
      html_nodes(".gs_r") %>%
      map_df(~{
        title <- .x %>% html_node(".gs_rt a") %>% html_text(trim = TRUE)
        citation_url <- .x %>% html_node(".gs_rt a") %>% html_attr("href")
        authors <- .x %>% html_node(".gs_a") %>% html_text(trim = TRUE) %>% str_extract("^[^-]+") %>% str_trim()
        year <- .x %>% html_node(".gs_a") %>% html_text(trim = TRUE) %>%
          str_extract("\\b\\d{4}\\b") %>% as.numeric()
        journal <- .x %>% html_node(".gs_a") %>% html_text(trim = TRUE) %>% str_extract(" - (.+)$") %>% str_trim()
        num_citations <- .x %>% html_node(".gs_fl a:nth-child(3)") %>% html_text(trim = TRUE) %>%
          str_extract("\\d+") %>% as.numeric()
        
        # Retrieve authors' Google Scholar profile URLs
        author_data <- tibble(Author = character(), Author_Profile_URL = character())
        if (!is.na(authors)) {
          author_list <- str_split(authors, ",\\s*")[[1]]
          for (author in author_list) {
            author_search_url <- paste0("https://scholar.google.com/citations?view_op=search_authors&mauthors=", URLencode(author))
            author_page <- tryCatch(
              { read_html(author_search_url) },
              error = function(e) {
                message("Error reading author search page for ", author)
                NULL
              }
            )
            profile_url <- if (!is.null(author_page)) {
              author_page %>%
                html_node(".gs_ai_name a") %>%
                html_attr("href") %>%
                { if (!is.na(.)) paste0("https://scholar.google.com", .) else "No profile available" }
            } else {
              "Error loading profile page"
            }
            author_data <- bind_rows(author_data, tibble(Author = author, Author_Profile_URL = profile_url))
          }
        }
        
        # Combine data into a single row
        tibble(
          Title = title,
          Citation_URL = citation_url,
          Authors = paste(author_data$Author, collapse = "; "),
          Author_Profile_URLs = paste(author_data$Author_Profile_URL, collapse = "; "),
          Year = year,
          Journal = journal,
          Number_of_Citations = num_citations
        )
      })
    
    # Append the citations to the global list
    all_citations[[length(all_citations) + 1]] <- citations
  }
  
  # Combine all pages into a single data frame
  citations_df_1 <- bind_rows(all_citations)

}

saveRDS(citations_df, "C:/Users/User/Documents/GitHub/scraping-google-scholar/citations_df_partial_ministerial_replacement.rds")

# Save the dataset to a CSV file if needed
write.csv(citations_df, "citations_data.csv", row.names = FALSE)
