# Predicting Grocery Price Trends Using Bayesian Models

## Overview
This study uses Bayesian statistical models to analyze grocery price trends across eight Canadian vendors by examining current and historical grocery price data. Our findings show that vendor-specific factors and product type significantly influence price changes. This research provides actionable insights for consumers and vendors looking to optimize their grocery purchasing and pricing strategies while adhering to the 2019 Canada's Food Guide, especially in light of the growing demand for affordable, sustainable food choices.

## File Structure

The repo is structured as:

-   `data/00-simulated_data` contains the simulated grocery prices data.
-   `data/01-raw_data` is missing because the original data file is too large. The original dataset can be downloaded through 02-download_data.R
-   `data/02-analysis_data` contains the cleaned dataset that was constructed.
-   `models` contains fitted models used in the analysis paper. 
-   `other` contains details about the datasheet, LLM chat interactions, and sketches.
-   `paper` contains the files used to generate the paper, including the Quarto document and reference bibliography file, as well as the PDF of the paper. 
-   `scripts` contains the R scripts used to simulate, test, download, clean, explore, and model the data.


## Statement on LLM usage

Aspects of the code were written with the help of ChatGPT and the entire chat history is available in other/llm_usage/usage.txt.
