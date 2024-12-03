#### Preamble ####
# Purpose: Tests the cleaned analysis data
# Author: Tina Kim
# Date: 28 November 2024
# Contact: tinak.kim@mail.utoronto.ca
# License: MIT
# Pre-requisites: 
  # - Have the analysis data loaded
  # - `tidyverse`, `arrow` and `lubridate` packages must be installed and loaded
# Any other information needed? None


#### Workspace setup ####
library(tidyverse)
library(lubridate)
library(arrow)

analysis_data <- read_parquet("data/02-analysis_data/analysis_data.parquet")


#### Test data ####
# Test 1: Check if there are any missing values in the essential columns
if (all(!is.na(analysis_data$nowtime), 
        !is.na(analysis_data$current_price), 
        !is.na(analysis_data$vendor), 
        !is.na(analysis_data$product_name), 
        !is.na(analysis_data$stock_status), 
        !is.na(analysis_data$is_sale), 
        !is.na(analysis_data$is_best), 
        !is.na(analysis_data$food_category), 
        !is.na(analysis_data$price_per_standard_unit))) {
  message("Test Passed: No missing values in essential columns.")
} else {
  stop("Test Failed: There are missing values in essential columns.")
}

# Test 2: Check if 'nowtime' is in the correct datetime format
if (inherits(analysis_data$nowtime, "POSIXct")) {
  message("Test Passed: 'nowtime' is in the correct datetime format (YYYY-MM-DD HH:MM:SS).")
} else {
  stop("Test Failed: 'nowtime' is not in the correct datetime format (YYYY-MM-DD HH:MM:SS).")
}

# Test 3: Check if 'current_price' is numeric and positive
if (all(is.numeric(analysis_data$current_price) & analysis_data$current_price > 0)) {
  message("Test Passed: 'current_price' is numeric and positive.")
} else {
  stop("Test Failed: 'current_price' is not numeric or contains non-positive values.")
}

# Test 4: Check if 'vendor' contains valid vendor names
valid_vendors <- c("Walmart", "Voila", "TandT", "SaveOnFoods", "NoFrills", "Metro", "Loblaws", "Galleria")
if (all(analysis_data$vendor %in% valid_vendors)) {
  message("Test Passed: All vendor values are valid.")
} else {
  stop("Test Failed: Some vendor values are invalid.")
}

# Test 5: Check if 'food_category' contains only valid categories
valid_food_categories <- c("Fruits & Vegetables", "Protein Foods", "Grain Products")
if (all(analysis_data$food_category %in% valid_food_categories)) {
  message("Test Passed: All food categories are valid.")
} else {
  stop("Test Failed: Some food categories are invalid.")
}

# Test 6: Check if 'price_per_standard_unit' is numeric and greater than zero
if (all(is.numeric(analysis_data$price_per_standard_unit) & analysis_data$price_per_standard_unit > 0)) {
  message("Test Passed: 'price_per_standard_unit' is numeric and greater than zero.")
} else {
  stop("Test Failed: 'price_per_standard_unit' is not numeric or contains non-positive values.")
}

# Test 7: Check if 'price_decrease' is logical (numeric value/NA)
if (all(is.numeric(analysis_data$price_decrease))) {
  message("Test Passed: 'price_decrease' are either NA or numeric.")
} else {
  stop("Test Failed: 'price_decrease' contain invalid values (not NA or numeric).")
}

# Test 8: Check if 'stock_status' contains only valid values
valid_stock_status <- c("in_stock", "out_of_stock")
if (all(analysis_data$stock_status %in% valid_stock_status)) {
  message("Test Passed: 'stock_status' contains only valid values.")
} else {
  stop("Test Failed: 'stock_status' contains invalid values.")
}

# Test 9: Check if 'is_sale', 'is_best', and 'is_organic' are logical (TRUE/FALSE)
if (all(analysis_data$is_sale %in% c(TRUE, FALSE), na.rm = TRUE) & 
    all(analysis_data$is_best %in% c(TRUE, FALSE), na.rm = TRUE) &
    all(analysis_data$is_organic %in% c(TRUE, FALSE), na.rm = TRUE)) {
  message("Test Passed: 'is_sale', 'is_best', and 'is_organic' are logical.")
} else {
  stop("Test Failed: 'is_sale', 'is_best', and 'is_organic' are not logical.")
}

# Test 10: Check for no exact duplicates
if (nrow(analysis_data) == nrow(distinct(analysis_data))) {
  message("Test Passed: There are no exact duplicate rows in the dataset.")
} else {
  stop("Test Failed: The dataset contains exact duplicate rows.")
}