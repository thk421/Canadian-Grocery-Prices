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

# Vendor Data
vendors <- c("Vendor_A", "Vendor_B", "Vendor_C", "Vendor_D", "Vendor_E")

# Grocery Product Data
products <- c("Apples", "Bananas", "Carrots", "Broccoli", "Chicken Breast", 
              "Salmon", "Eggs", "Tofu", "Greek Yogurt", "Whole Grain Bread")

# Food Category Data
food_categories <- c("Fruits & Vegetables", "Protein Foods", "Grain Products")

# Generate simulated data
simulated_data <- tibble(
  vendor = sample(vendors, n, replace = TRUE),
  product_name = sample(products, n, replace = TRUE),
  current_price = runif(n, 1, 20),  # Prices between 1 and 20
  food_category = sample(food_categories, n, replace = TRUE),
  nowtime = sample(seq(from = as.Date('2022-01-01'), to = as.Date('2024-01-01'), by = 'day'), n, replace = TRUE)  # Generating a sequence of dates
)

# View the simulated data
head(simulated_data)

#### Save data ####
write_csv(simulated_data, "data/00-simulated_data/simulated_data.csv")