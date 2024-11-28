#### Preamble ####
# Purpose: Tests the structure and validity of the simulated 
  # Canadian Grocery Store Prices dataset
# Author: Tina Kim
# Date: 19 November 2024
# Contact: tinak.kim@mail.utoronto.ca
# License: MIT
# Pre-requisites: 
  # - The `tidyverse` package must be installed and loaded
  # - 00-simulate_data.R must have been run
# Any other information needed? None


#### Workspace setup ####
library(tidyverse)

simulated_data <- read_csv("data/00-simulated_data/simulated_data.csv")

# Test if the data was successfully loaded
if (exists("simulated_data")) {
  message("Test Passed: The dataset was successfully loaded.")
} else {
  stop("Test Failed: The dataset could not be loaded.")
}


#### Test data ####

# Check if the dataset has 1000 rows
if (nrow(simulated_data) == 1000) {
  message("Test Passed: The dataset has 1000 rows.")
} else {
  stop("Test Failed: The dataset does not have 1000 rows.")
}

# Check if the dataset has 5 columns
if (ncol(simulated_data) == 5) {
  message("Test Passed: The dataset has 5 columns.")
} else {
  stop("Test Failed: The dataset does not have 5 columns.")
}

# Check if there are any missing values in the dataset
if (all(!is.na(simulated_data))) {
  message("Test Passed: The dataset contains no missing values.")
} else {
  stop("Test Failed: The dataset contains missing values.")
}

# Check if all prices are within the expected range (1 to 20)
if (all(simulated_data$current_price >= 1 & simulated_data$current_price <= 20)) {
  message("Test Passed: All current prices are within the expected range (1 to 20).")
} else {
  stop("Test Failed: Some current prices are outside the expected range.")
}

# Check if all `nowtime` values are between `2022-01-01` and `2024-01-01`
if (all(simulated_data$nowtime >= as.Date('2022-01-01') & simulated_data$nowtime 
        <= as.Date('2024-01-01'))) {
  message("Test Passed: All dates are within the expected range.")
} else {
  stop("Test Failed: Some dates are outside the expected range.")
}

# Check if vendors are distributed
vendor_counts <- table(simulated_data$vendor)
if (all(vendor_counts >= 50)) {
  message("Test Passed: Vendors are sampled correctly.")
} else {
  stop("Test Failed: Some vendors have fewer than 50 samples.")
}

# Check if food categories are sampled correctly
food_category_counts <- table(simulated_data$food_category)
if (all(food_category_counts >= 100)) {
  message("Test Passed: Food categories are sampled correctly.")
} else {
  stop("Test Failed: Some food categories have fewer than 100 samples.")
}

# Check if there are exactly identical entries for the same vendor
duplicates <- simulated_data %>%
  group_by(vendor, product_name, current_price, food_category, nowtime) %>%
  summarise(count = n()) %>%
  filter(count > 1)

if (nrow(duplicates) == 0) {
  message("Test Passed: No duplicate product entries for the same vendor.")
} else {
  stop("Test Failed: There are duplicate product entries for the same vendor.")
}

# Make sure that all products are mapped to the correct food category
category_map <- c(
  "Apples" = "Fruits & Vegetables",
  "Bananas" = "Fruits & Vegetables",
  "Broccoli" = "Fruits & Vegetables",
  "Carrots" = "Fruits & Vegetables",
  "Whole Grain Bread" = "Grain Products",
  "Oats" = "Grain Products",
  "Brown Rice" = "Grain Products",
  "Eggs" = "Protein Foods",
  "Chicken Breast" = "Protein Foods",
  "Salmon" = "Protein Foods",
  "Tofu" = "Protein Foods",
  "Greek Yogurt" = "Protein Foods"
)

test_category_match <- all(simulated_data$product_name %in% names(category_map)) & 
  all(simulated_data$food_category == category_map[simulated_data$product_name])

# Output test result
if (test_category_match) {
  message("Test Passed: All product categories match the expected categories.")
} else {
  message("Test Failed: Some product categories do not match the expected categories.")
}