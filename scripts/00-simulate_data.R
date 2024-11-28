#### Preamble ####
# Purpose: Simulates a Canadian Grocery Store Prices dataset
# Author: Tina Kim
# Date: 19 November 2024
# Contact: tinak.kim@mail.utoronto.ca
# License: MIT
# Pre-requisites: None
# Any other information needed? None


#### Workspace setup ####
library(tidyverse)
library(lubridate)
set.seed(123)


#### Simulate data ####

# Number of records to simulate
n <- 1000

# Define the category_map (product -> category)
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

# Vendor Data
vendors <- c("Metro", "Galleria", "T&T", "Walmart", "SaveOnFoods")

# Grocery Product Data
products <- c("Apples", "Bananas", "Carrots", "Broccoli", "Chicken Breast", 
              "Salmon", "Eggs", "Tofu", "Greek Yogurt", "Oats", 
              "Brown Rice", "Whole Grain Bread")

# Generate simulated data
simulated_data <- tibble(
  vendor = sample(vendors, n, replace = TRUE),
  product_name = sample(products, n, replace = TRUE),
  current_price = runif(n, 1, 20),  # Prices between 1 and 20
  nowtime = sample(seq(from = as.Date('2022-01-01'), to = as.Date('2024-01-01'), by = 'day'), n, replace = TRUE)  # Generating a sequence of dates
)

# Assign food categories based on product names using the category_map
simulated_data <- simulated_data %>%
  mutate(food_category = category_map[product_name])

# View the simulated data
head(simulated_data)

#### Save data ####
write_csv(simulated_data, "data/00-simulated_data/simulated_data.csv")