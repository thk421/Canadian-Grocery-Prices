#### Preamble ####
# Purpose: Downloads and saves Canadian Grocery Price Data
# Author: Tina Kim
# Date: 19 November 2024
# Contact: tinak.kim@mail.utoronto.ca
# License: MIT
# Pre-requisites: 
  # - The `tidyverse` package must be installed and loaded
# Any other information needed? None


#### Workspace setup ####
library(tidyverse)


#### Download data ####

# Define URL and destination
url <- "https://jacobfilipp.com/hammerdata/hammer-5-csv.zip"
destfile <- "hammer-5-csv.zip"

# Download the ZIP file
download.file(url, destfile, mode = "wb")

# Extract the ZIP file
unzip(destfile, exdir = "unzipped_files")

# Read the CSV file
csv_file <- list.files("unzipped_files", pattern = "\\.csv$", full.names = TRUE)
data_price <- read.csv("unzipped_files/hammer-4-raw.csv")
data_product <- read.csv("unzipped_files/hammer-4-product.csv")


#### Save data ####
write_csv(data_price, "data/01-raw_data/data_price.csv")
write_csv(data_product, "data/01-raw_data/data_product.csv")