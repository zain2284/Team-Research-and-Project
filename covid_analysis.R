# ============================================================================
# 7COM1079 COVID-19 Case Fatality Rate Analysis
# Dataset: Corona Virus Report (country_wise_latest.csv)
# Research Question: Is there a difference in mean case fatality rate 
# between developed and developing countries?
# ============================================================================

# Load required libraries
library(tidyverse)
library(dplyr)
library(ggplot2)
library(readr)
library(countrycode)
library(car)        # For Levene's test
library(effsize)    # For effect size
library(knitr)      # For nice tables

# ============================================================================
# SECTION 1: DATA LOADING AND PREPROCESSING
# ============================================================================

# Setup the Path
setwd("C:/Users/MCS/Documents/R file/")
# Load the dataset
covid_data <- read.csv("country_wise_latest.csv", stringsAsFactors = FALSE)

# Display dataset structure
cat("=== DATASET OVERVIEW ===\n")
cat("Dataset dimensions:", dim(covid_data)[1], "rows x", dim(covid_data)[2], "columns\n\n")
cat("Column names:\n")
print(names(covid_data))
cat("\nFirst few rows:\n")
print(head(covid_data, 3))

# Rename column for consistency
covid_data <- covid_data %>%
  rename(
    Country = Country.Region,
    CFR = Deaths...100.Cases
  )

# ============================================================================
# SECTION 2: CLASSIFY COUNTRIES BY DEVELOPMENT STATUS
# ============================================================================

# Create a classification of developed vs developing countries
# Using World Bank classification and common economic indicators

developed_countries <- c(
  "United States", "Canada", "United Kingdom", "Germany", "France", 
  "Italy", "Spain", "Japan", "Australia", "South Korea", "Netherlands",
  "Switzerland", "Sweden", "Norway", "Denmark", "Finland", "Belgium",
  "Austria", "Ireland", "Singapore", "Israel", "New Zealand", "Portugal",
  "Greece", "Czech Republic", "Hungary", "Poland", "Slovakia", "Estonia",
  "Latvia", "Lithuania", "Slovenia", "Croatia", "Cyprus", "Malta",
  "Luxembourg", "Iceland", "Qatar", "United Arab Emirates", "Saudi Arabia",
  "Kuwait", "Bahrain", "Oman"
)

# Classify countries
covid_data <- covid_data %>%
  mutate(
    Development_Status = case_when(
      Country %in% developed_countries ~ "Developed",
      TRUE ~ "Developing"
    )
  ) %>%
  mutate(Development_Status = factor(Development_Status, 
                                     levels = c("Developed", "Developing")))

# Check classification
cat("\n=== DEVELOPMENT STATUS CLASSIFICATION ===\n")
status_summary <- covid_data %>%
  group_by(Development_Status) %>%
  summarise(
    Count = n(),
    Percentage = round(n() / nrow(covid_data) * 100, 1)
  )
print(status_summary)

