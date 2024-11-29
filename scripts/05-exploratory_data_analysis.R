#### Preamble ####
# Purpose: Analyzes, visualizes, and summarizes data
# Author: Tina Kim
# Date: 28 November 2024
# Contact: tinak.kim@mail.utoronto.ca
# License: MIT
# Pre-requisites: Have the analysis data loaded
# Any other information needed? None


#### Workspace setup ####
library(tidyverse)
library(rstanarm)
library(dplyr)
library(ggplot2)
library(tidyr)


#### Read data ####
analysis_data <- read_csv("data/02-analysis_data/analysis_data.csv")


#### Analyze data ####
## View the distinct vendor names
distinct_vendors <- analysis_data %>%
  distinct(vendor) %>%
  pull(vendor)

## Show list of distinct products from each vendor that contain keywords
keywords <- c("apples", "bananas", "lettuce", "broccoli", "carrots", "potatoes", 
              "cucumbers", "avocadoes", "oats", "whole grain bread", 
              "brown rice", "eggs", "chicken breast", "salmon", 
              "tofu", "almonds", "greek yogurt", "low fat milk")

distinct_products_data <- analysis_data %>% 
  filter(grepl(paste(keywords, collapse = "|"), tolower(product_name))) %>%
  filter(!grepl("chocolate", tolower(product_name))) %>%
  distinct(nowtime, current_price, vendor, product_name, price_decrease,
           stock_status, is_sale, food_category, price_per_standard_unit) %>%
  ungroup()


## Keep the most recent nowtime for each distinct product and vendor pair
distinct_products_data <- distinct_products_data %>%
  # Convert 'nowtime' column to datetime if it's not already
  mutate(nowtime = as.POSIXct(nowtime, format = "%Y-%m-%d %H:%M:%S")) %>%
  
  # Group by product_name and vendor
  group_by(product_name, vendor) %>%
  
  # Keep only the most recent row for each group (product_name and vendor)
  slice_max(order_by = nowtime, n = 1) %>%
  
  # Ungroup to return to a regular dataframe
  ungroup() %>%
  
  # Order the data by vendor (and product_name if needed)
  arrange(vendor, product_name)


## Average cost of a weekly meal plan by vendor adhering to the 2019 CFG

# Calculate the weighted average cost for the weekly meal plan per vendor and food category
# Save the new calculations in a separate dataset
weekly_meal_plan_cost_per_vendor <- distinct_products_data %>%
  filter(!is.na(price_per_standard_unit), !is.na(food_category)) %>%
  group_by(vendor, food_category) %>%
  summarise(average_cost = mean(price_per_standard_unit, na.rm = TRUE)) %>%
  mutate(
    weight = case_when(
      food_category == "Fruits & Vegetables" ~ 0.50,
      food_category == "Protein Foods" ~ 0.25,
      food_category == "Grain Products" ~ 0.25,
      TRUE ~ 0  # For other categories, apply 0 weight
    ),
    weighted_cost = average_cost * weight  # Calculate weighted cost for each food category
  ) %>%
  # Summarise both weighted cost and total weighted cost by vendor and food category
  group_by(vendor, food_category) %>%
  summarise(
    weighted_cost = sum(weighted_cost, na.rm = TRUE), # Sum of weighted costs for each food category and vendor
    total_weighted_cost = sum(weighted_cost, na.rm = TRUE)  # Same calculation, to display both
  )

## Calculate average number of products on sale by vendor
avg_products_on_sale <- distinct_products_data %>%
  filter(is_sale == TRUE) %>% # Filter for items on sale
  group_by(vendor) %>% # Group by vendor
  summarise(avg_products_on_sale = n()) %>% # Count the number of items on sale per vendor
  arrange(desc(avg_products_on_sale)) # Sort by highest average

## Summarize the number of sales and quiet sales by vendor
vendor_sales_data <- analysis_data %>%
  group_by(vendor) %>%
  summarise(
    sale_count = sum(is_sale, na.rm = TRUE),  # Count of sales
    quiet_sale_count = sum(!is.na(price_decrease) & price_decrease > 0, na.rm = TRUE)  # Count of quiet sales
  ) %>%
  pivot_longer(cols = c(sale_count, quiet_sale_count), 
               names_to = "sale_type", 
               values_to = "count")  # Reshape for ggplot


## Calculate average number of products in stock by vendor
avg_products_in_stock <- distinct_products_data %>%
  filter(stock_status == "in_stock") %>% # Filter for items in stock
  group_by(vendor) %>% # Group by vendor
  summarise(avg_products_in_stock = n()) %>% # Count the number of items in stock per vendor
  arrange(desc(avg_products_in_stock)) # Sort by highest average

# View the vendors with the most products in stock
print(avg_products_in_stock)

## Calculate average number of products that are organic/fresh by vendor
organic_product_data <- analysis_data %>%
  filter(grepl("organic", product_name, ignore.case = TRUE))

# Summarize the count of organic products by vendor and food category
organic_summary <- organic_product_data %>%
  group_by(vendor, food_category) %>%
  summarise(product_count = n(), .groups = "drop")
  

### Model data ####

# Bar chart: Total weighted cost for each vendor
avg_price_bar <- 
  ggplot(weekly_meal_plan_cost_per_vendor, aes(x = reorder(vendor, total_weighted_cost), y = total_weighted_cost)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(
    title = "Total Weighted Cost of Weekly Meal Plan per Vendor",
    x = "Vendor",
    y = "Total Weighted Cost ($)",
    caption = "Source: Your Data"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels for readability

# Stacked bar chart: Breakdown of meal plan costs by category per vendor
avg_price_stacked_bar <-
  ggplot(weekly_meal_plan_cost_per_vendor, aes(x = reorder(vendor, total_weighted_cost), 
                                               y = weighted_cost, fill = food_category)) +
  geom_bar(stat = "identity") +
  labs(
    title = "Cost Breakdown of Weekly Meal Plan by Vendor and Food Category",
    x = "Vendor",
    y = "Weighted Cost ($)",
    fill = "Food Category",
    caption = "Source: Your Data"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels for readability

# Box plot: Distribution of weighted costs for each vendor
avg_price_box <-
  ggplot(weekly_meal_plan_cost_per_vendor, aes(x = vendor, y = total_weighted_cost)) +
  geom_boxplot(fill = "lightblue", color = "blue") +
  labs(
    title = "Distribution of Total Weighted Costs by Vendor",
    x = "Vendor",
    y = "Total Weighted Cost ($)",
    caption = "Source: Your Data"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels for readability

# Facet grid: Average number of products in stock by vendor and food category
avg_stock_facet <-
  ggplot(distinct_products_data %>% filter(stock_status == "in_stock"), aes(x = vendor, y = price_per_standard_unit, fill = food_category)) +
  geom_bar(stat = "identity") +
  facet_wrap(~food_category) +
  labs(
    title = "Average Number of Products in Stock by Vendor and Food Category",
    x = "Vendor",
    y = "Average Number of Products in Stock",
    caption = "Source: Your Data"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels for readability

# Bar Plot: Number of sale occurrences by vendor
is_sale_bar <-
  ggplot(analysis_data, aes(x = vendor, fill = is_sale)) +
  geom_bar(position = "dodge") +
  labs(title = "Sale Occurrence by Vendor", x = "Vendor", y = "Count of Products on Sale") +
  theme_minimal()

# Box Plot: Quiet price decrease across vendors and food categories
quiet_price_dec_box <-
  ggplot(analysis_data, aes(x = vendor, y = price_decrease)) +
  geom_boxplot(fill = "lightblue", color = "black") +
  labs(title = "Quiet Price Decrease by Vendor", x = "Vendor", y = "Quiet Price Decrease") +
  theme_minimal()

# Bar Plot: Number of quiet price decreases per vendor
quiet_price_dec_bar <-
  ggplot(analysis_data %>% filter(!is.na(price_decrease)), aes(x = vendor)) +
  geom_bar(fill = "lightgreen", color = "black") +
  labs(title = "Count of Quiet Price Decreases by Vendor", x = "Vendor", y = "Count") +
  theme_minimal()

# Bar Plot: Comparison of sales vs quiet sales per vendor
sales_vs_quiet_sales_bar <-
  ggplot(vendor_sales_data, aes(x = vendor, y = count, fill = sale_type)) +
  geom_bar(stat = "identity", position = position_dodge()) +  # Grouped bars
  labs(
    title = "Sales and Quiet Sales Across Vendors",
    x = "Vendor",
    y = "Count",
    fill = "Sale Type",
    caption = "Quiet sales refer to price decreases without explicit sale labels."
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels for readability

# Bar plot: Number of best-seller products
is_best_bar <-
  ggplot(analysis_data, aes(x = vendor, fill = is_best)) +
  geom_bar(position = "fill") +
  labs(title = "Proportion of Best Sellers by Vendor", x = "Vendor", y = "Proportion") +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_manual(values = c("gray", "green"), labels = c("Not Best Seller", "Best Seller")) +
  theme_minimal()

# Bar plot: Number of organic or fresh products per vendor by category
organic_product_bar <-
  ggplot(organic_summary, aes(x = vendor, y = product_count, fill = food_category)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(
    title = "Count of Organic Products by Vendor and Food Category",
    x = "Vendor",
    y = "Number of Organic Products",
    fill = "Food Category"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#### Save model ####
saveRDS(
  avg_price_bar,
  file = "models/avg_price_bar.rds"
)

saveRDS(
  avg_price_stacked_bar,
  file = "models/avg_price_stacked_bar.rds"
)

saveRDS(
  avg_price_box,
  file = "models/avg_price_box.rds"
)

saveRDS(
  avg_stock_facet,
  file = "models/avg_stock_facet.rds"
)

saveRDS(
  is_sale_bar,
  file = "models/is_sale_bar.rds"
)

saveRDS(
  quiet_price_dec_box,
  file = "models/quiet_price_dec_box.rds"
)

saveRDS(
  quiet_price_dec_bar,
  file = "models/quiet_price_dec_bar.rds"
)

saveRDS(
  sales_vs_quiet_sales_bar,
  file = "models/sales_vs_quiet_sales_bar.rds"
)

saveRDS(
  is_best_bar,
  file = "models/is_best_bar.rds"
)

saveRDS(
  organic_product_bar,
  file = "models/organic_product_bar.rds"
)