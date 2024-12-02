#### Preamble ####
# Purpose: Models results for the data analysis
# Author: Tina Kim
# Date: 28 November 2024
# Contact: tinak.kim@mail.utoronto.ca
# License: MIT
# Pre-requisites: Have the 05-exploratory_data_analysis.R script loaded
# Any other information needed? None


#### Workspace setup ####
library(tidyverse)
library(rstanarm)
library(brms)
library(bayesplot)


#### Read data ####
analysis_data <- read_csv("data/02-analysis_data/analysis_data.csv")

### Model data ####

## Bar Plot: Vendor overall score based on affordability, reliability, transparency, quality, and appeal.

# Normalize a column between 0 and 1
normalize <- function(x) (x - min(x)) / (max(x) - min(x))

overall_best_vendor_data <- weekly_meal_plan_cost_per_vendor %>%
  group_by(vendor) %>%
  summarise(total_weighted_cost = sum(total_weighted_cost, na.rm = TRUE)) %>%
  left_join(avg_products_on_sale, by = "vendor") %>%
  left_join(avg_products_in_stock, by = "vendor") %>%
  left_join(vendor_sales_data %>%
              pivot_wider(names_from = sale_type, values_from = count), by = "vendor") %>%
  left_join(organic_summary %>%
              group_by(vendor) %>%
              summarise(product_count = sum(product_count, na.rm = TRUE)), by = "vendor") %>%
  mutate(
    # Replace NAs with default values
    avg_products_on_sale = replace_na(avg_products_on_sale, 0),
    avg_products_in_stock = replace_na(avg_products_in_stock, 0),
    sale_count = replace_na(sale_count, 0),
    quiet_sale_count = replace_na(quiet_sale_count, 0),
    organic_count = replace_na(product_count, 0),
    
    # Normalize metrics, handling NAs with defaults
    affordability = 1 - normalize(total_weighted_cost),  # Lower cost is better
    sale_transparency = normalize(sale_count / (sale_count + quiet_sale_count)),  # Proportion of explicit sales
    reliability = normalize(avg_products_in_stock),  # Higher is better
    product_appeal = normalize(organic_count),  # Higher count of organic products is better
    promotional_effort = normalize(avg_products_on_sale),  # Higher sale frequency is better
    
    # overall score calculation
    overall_score = 0.5 * affordability +
      0.2 * reliability +
      0.1 * sale_transparency +
      0.1 * product_appeal +
      0.1 * promotional_effort
  ) %>%
  arrange(desc(overall_score))

# Bar plot of overall vendor scores

overall_vendor_scores_bar <-
  ggplot(overall_best_vendor_data, aes(x = reorder(vendor, overall_score), y = overall_score)) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  labs(
    title = "Overall Vendor Scores",
    x = "Vendor",
    y = "Overall Score"
  ) +
  theme_minimal()


## Bayesian Inference: Predicting future grocery price trends

# Prepare the data
bayesian_data <- analysis_data %>%
  arrange(vendor, product_name, nowtime) %>%
  group_by(vendor, product_name) %>%
  mutate(future_price = lead(current_price)) %>%
  ungroup() %>%
  filter(!is.na(current_price) & !is.na(nowtime)) %>%
  mutate(
    week = lubridate::week(nowtime),
    month = lubridate::month(nowtime),
    year = lubridate::year(nowtime)
  )

# Training data (subset of bayesian_data)
set.seed(123)  # Set a seed for reproducibility
smaller_training_data <- bayesian_data %>%
  sample_n(5000) 

# Fit the Bayesian model
bayesian_model <- brm(
  future_price ~ current_price + food_category + vendor + month + 
    price_decrease + (1 | product_name),
  data = smaller_training_data,
  family = gaussian(),  # Assuming prices are normally distributed
  prior = c(
    prior(normal(0, 10), class = "b"),     # Prior for coefficients
    prior(normal(0, 10), class = "Intercept"),  # Prior for intercept
    prior(cauchy(0, 1), class = "sigma")  # Prior for residual standard deviation
  ),
  chains = 2,  # Number of Markov chains
  iter = 2000,  # Number of iterations
  warmup = 500,  # Warmup iterations
  cores = 2  # Use available cores
)

# Summarize the model
summary(bayesian_model)

# Plot posterior distributions
plot(bayesian_model)

# Predict future prices
future_prices <- predict(bayesian_model, newdata = smaller_training_data, re_formula = NA)

# Summarize predictions
future_prices_summary <- as.data.frame(future_prices) %>%
  mutate(
    mean = Estimate,
    lower = Q2.5,  # 2.5% credible interval
    upper = Q97.5  # 97.5% credible interval
  ) %>%
  mutate(time = smaller_training_data$month,
         vendor = smaller_training_data$vendor) 

predicted_price_trend <- 
  ggplot(future_prices_summary, aes(x = time, y = mean)) +
  geom_line() +  # Line plot for predicted prices
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2) +  # Confidence ribbon
  facet_wrap(~ vendor) +  # Separate plots by vendor
  labs(
    title = "Predicted Future Price Trends",
    x = "Time (in Months)",
    y = "Predicted Price (in CAD)"
  ) +
  theme_minimal()

print(predicted_price_trend)

pp_check(bayesian_model) 


#### Save model ####

saveRDS(
  overall_vendor_scores_bar,
  file = "models/overall_vendor_scores_bar.rds"
)

saveRDS(
  bayesian_model,
  file = "models/bayesian_model.rds"
)

saveRDS(
  predicted_price_trend,
  file = "models/predicted_price_trend.rds"
)