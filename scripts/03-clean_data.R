#### Preamble ####
# Purpose: Cleans the two raw Canadian Grocery Prices data
# Author: Tina Kim
# Date: 19 November 2024
# Contact: tinak.kim@mail.utoronto.ca
# License: MIT
# Pre-requisites: Need to have downloaded the data
# Any other information needed? None

#### Workspace setup ####
library(tidyverse)
library(arrow)


#### Clean data ####

# Merge price and product data

product <- read_csv("data/01-raw_data/data_product.csv")
price <- read_csv("data/01-raw_data/data_price.csv")

merged_data <- price %>%
  inner_join(product, by = c("product_id" = "id"))

# Define a vector of keywords for each food category
fruit_veg_key <- c("apples", "bananas", "lettuce", "broccoli", "carrots",
                   "potatoes", "cucumbers", "avocadoes")
protein_key <- c("eggs", "chicken breast", "salmon", "tofu", "almonds",
                 "greek yogurt", "low fat milk")
grain_key <- c("oats", "whole grain bread", "brown rice")


unit_conversion <- c(
  "kg" = 1000,  # 1 kg = 1000 grams
  "g" = 1,      # 1 gram = 1 gram
  "lb" = 453.592, # 1 lb = 453.592 grams
  "oz" = 28.3495, # 1 oz = 28.3495 grams
  "ml" = 1,     # 1 ml = 1 ml
  "l" = 1000,    # 1 liter = 1000 milliliters
  "each" = 1,    # 1 item = 1 item
  "ea" = 1,      # 1 item = 1 item
  "bunch" = 1,   # 1 bunch = 1 bunch (no conversion needed)
  "pack" = 1     # 1 pack = 1 pack (no conversion needed)
)


# Clean the data
cleaned_data <- merged_data %>%
  # Remove rows with missing units or current price
  filter(!is.na(current_price), !is.na(units)) %>%
  # Create a price decrease and increase column for when old prices are 
  # higher and lower than current prices
  mutate(
    old_price = as.numeric(old_price),
    current_price = as.numeric(current_price),
    price_decrease = ifelse(!is.na(old_price) & old_price > current_price, 
                      old_price - current_price, 
                      NA)
  ) %>%
  # Use the other column to make useful flags
  mutate(stock_status = ifelse(grepl("Out of stock", other), "out_of_stock", "in_stock"),
         is_sale = grepl("SALE", other),
         is_best = grepl("Best seller", other),
         is_organic = grepl("organic", product_name, ignore.case = TRUE)) %>%
  # Delete unnecessary columns
  select(-other, -brand, -detail_url, -sku, -upc) %>%
  # Add food category to the dataset
  mutate(food_category = case_when(
    grepl(paste(fruit_veg_key, collapse = "|"), tolower(product_name)) ~ "Fruits & Vegetables",
    grepl(paste(protein_key, collapse = "|"), tolower(product_name)) ~ "Protein Foods",
    grepl(paste(grain_key, collapse = "|"), tolower(product_name)) ~ "Grain Products",
    TRUE ~ "Other"
  )) %>%
  # Remove rows where food category is 'Other'
  filter(food_category != "Other")

  
# Clean and update the data
cleaned_unit_data <- cleaned_data %>%
  # Remove rows with missing units or current price
  filter(!is.na(current_price), !is.na(units)) %>%
  
  # Standardize units based on the unit_mapping
  mutate(
    # Remove digits and decimals from units
    unit_cleaned = gsub("[0-9.]", "", units),
    
    # Convert to lowercase to handle case insensitivity
    unit_cleaned = tolower(unit_cleaned),
    
    # Standardize units based on substrings
    unit_standardized = case_when(
      grepl("kg", unit_cleaned) ~ "kg",
      grepl("gram|g", unit_cleaned) ~ "g",
      grepl("ml|millilitre|milliliter", unit_cleaned) ~ "ml",
      grepl("l|litre", unit_cleaned) ~ "l",
      grepl("each|ea|un", unit_cleaned) ~ "each",
      grepl("pack|pk", unit_cleaned) ~ "pack",
      grepl("oz", unit_cleaned) ~ "oz",
      grepl("lb|lbs", unit_cleaned) ~ "lb",
      grepl("bunch", unit_cleaned) ~ "bunch",
      grepl("tray", unit_cleaned) ~ "tray",
      TRUE ~ "unknown"  # For unrecognized units
    )
  ) %>%
  
  # Now calculate price per unit by converting the current price to a standardized unit
  mutate(
    price_per_standard_unit = case_when(
      unit_standardized %in% c("kg", "g", "lb", "oz") ~ current_price / unit_conversion[unit_standardized],  # Solid items (kg, g, lb, oz)
      unit_standardized %in% c("ml", "l") ~ current_price / unit_conversion[unit_standardized],             # Liquid items (ml, l)
      unit_standardized %in% c("each", "ea", "bunch", "pack", "tray") ~ current_price,                          # Item-based units (each, pack, bunch)
      TRUE ~ NA_real_  # For unknown units, return NA
    )
  ) %>%
  
  # Remove rows where price_per_standard_unit is NA (could indicate unrecognized units)
  filter(!is.na(price_per_standard_unit))

# Remove unnecessary columns and arrange vendors in order
cleaned_unit_data <- cleaned_unit_data %>%
  select(-old_price, -price_per_unit, -product_id, -concatted, -units, -unit_cleaned, -unit_standardized) %>%
  arrange(desc(vendor))

# Remove exact duplicates
distinct_cleaned_data <- cleaned_unit_data %>% distinct()

#### Save data ####
write_csv(distinct_cleaned_data, "data/02-analysis_data/analysis_data.csv")
write_parquet(distinct_cleaned_data, "data/02-analysis_data/analysis_data.parquet")