# Load necessary library
library(dplyr)  # For bind_rows
library(stringr)

rm(list = ls())
articles_df_ministerial_replacement <- readRDS("./data/articles_df_ministerial_replacement.rds")

# Specify the folder containing the CSV files
folder_path <- "./data"  # Replace with your folder path

# List all CSV files in the folder
csv_files <- list.files(path = folder_path, pattern = "\\.csv$", full.names = TRUE)

# Read and combine all CSV files
combined_data <- csv_files %>%
  lapply(read.csv) %>%  # Read each CSV file into a list
  bind_rows()           # Combine all data frames into one

# View the combined data
head(combined_data,2)

combined_data_ <- combined_data %>%
  distinct(URL_citacion, Citation_URL, .keep_all = T) %>%
  filter(Citation_URL != "") %>%
  filter(!grepl("start=50", URL_citacion)) %>%
  filter(!grepl("start=60", URL_citacion)) %>%
  filter(!grepl("start=70", URL_citacion)) %>%
  filter(!grepl("start=80", URL_citacion)) %>%
  filter(!grepl("start=90", URL_citacion)) %>%
  filter(!grepl("start=100", URL_citacion))  %>%
  mutate(
    # Extract the 'start' value using regex
    start = str_extract(URL_citacion, "start=\\d+") %>% str_remove("start=") %>% as.integer(),
    # Remove the "&hl=en&oe=ASCII&start=..." part from the URL
    URL_citacion = str_remove(URL_citacion, "&hl=en&oe=ASCII&start=\\d+")
  ) %>%
  select(url_articulo_principal=URL_citacion, 
         url_articulo_secundario=Citation_URL, 
         title_articulo_secundario=Title, 
         number_citation_articulo_secundario= Number_of_Citations)


#rm(combined_data)
head(articles_df_ministerial_replacement)

articles_df_ministerial_replacement <- articles_df_ministerial_replacement %>%
  select(url_articulo_principal=Citation_URL,title_articulo_principal=Title, number_citation_articulo_principal=Amount_Citation ) %>%
  filter(url_articulo_principal != "No citation URL available") %>%
  mutate(url_articulo_principal = str_remove(url_articulo_principal, "&hl=en&oe=ASCII"))

combined_data_<- combined_data_ %>%
  filter(title_articulo_secundario != unique(articles_df_ministerial_replacement$title_articulo_principal))

join_data <- articles_df_ministerial_replacement %>%
  left_join(combined_data_, by = "url_articulo_principal")

saveRDS(join_data, "./data/join_data.rds")
