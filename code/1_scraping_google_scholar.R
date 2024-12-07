library(rvest)
library(dplyr)
library(purrr)
library(stringr)
rm(list = ls())
# Base URL for Google Scholar search
base_url <- "https://scholar.google.com/scholar"

# Define the search term
search_term <- "Ministerial Replacement"

# Number of pages to scrape
num_pages <- 10  # Adjust this to scrape more or fewer pages

# Initialize lists to store articles and authors
all_articles <- list()
all_authors <- list()

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

saveRDS(articles_df, "./data/articles_df_ministerial_replacement.rds")
saveRDS(authors_df, "./data/authors_df_ministerial_replacement.rds")

# Display the datasets
cat("Articles Dataset:\n")
print(articles_df)

cat("\nAuthors Dataset:\n")
print(authors_df)
