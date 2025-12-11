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

#  EXPLORATORY DATA ANALYSIS 

summary_stats <- covid_data %>%
  group_by(Development_Status) %>%
  summarise(
    n_countries = n(),
    mean_CFR = round(mean(CFR, na.rm = TRUE), 3),
    sd_CFR = round(sd(CFR, na.rm = TRUE), 3),
    median_CFR = round(median(CFR, na.rm = TRUE), 3),
    min_CFR = round(min(CFR, na.rm = TRUE), 3),
    max_CFR = round(max(CFR, na.rm = TRUE), 3),
    IQR_CFR = round(IQR(CFR, na.rm = TRUE), 3)
  )

print(summary_stats)


##DATA VISUALIZATION 

main_plot <- ggplot(covid_data, aes(x = Development_Status, y = CFR, 
                                    fill = Development_Status)) +
  geom_boxplot(alpha = 0.8, outlier.color = "red", outlier.size = 2.5) +
  geom_jitter(width = 0.2, alpha = 0.5, size = 1.8, color = "gray30") +
  stat_summary(fun = mean, geom = "point", shape = 23, size = 4, 
               fill = "white", color = "black") +
  scale_fill_manual(values = c("Developed" = "#2E86AB", 
                               "Developing" = "#A23B72")) +
  labs(
    title = "Comparison of COVID-19 Case Fatality Rate (CFR) by Development Status",
    subtitle = paste("Dataset includes", nrow(covid_data), 
                     "countries | CFR = Deaths per 100 Confirmed Cases"),
    x = "Development Status",
    y = "Case Fatality Rate (%)",
    fill = "Status",
    caption = paste("Data source: Kaggle Corona Virus Report |", 
                    "Analysis date:", Sys.Date())
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
    plot.subtitle = element_text(hjust = 0.5, size = 11, color = "gray40"),
    axis.title = element_text(face = "bold", size = 12),
    axis.text = element_text(size = 11),
    legend.position = "bottom",
    legend.title = element_text(face = "bold"),
    panel.grid.minor = element_blank()
  )

# Display and save main plot
print(main_plot)
ggsave("CFR_comparison_boxplot.png", plot = main_plot, 
       width = 10, height = 6, dpi = 300, bg = "white")

# 4.2 Supplementary Plot: Histograms with density curves
histogram_plot <- ggplot(covid_data, aes(x = CFR, fill = Development_Status)) +
  geom_histogram(aes(y = ..density..), alpha = 0.7, bins = 25, 
                 position = "identity", color = "white") +
  geom_density(alpha = 0.4, adjust = 1.5) +
  facet_wrap(~Development_Status, ncol = 2) +
  scale_fill_manual(values = c("Developed" = "#2E86AB", 
                               "Developing" = "#A23B72")) +
  labs(
    title = "Distribution of Case Fatality Rates by Development Status",
    x = "Case Fatality Rate (%)",
    y = "Density",
    fill = "Development Status"
  ) +
  theme_minimal() +
  theme(
    strip.text = element_text(face = "bold", size = 11),
    panel.spacing = unit(1.5, "lines")
  )

print(histogram_plot)
ggsave("CFR_distribution_histogram.png", plot = histogram_plot, 
       width = 10, height = 6, dpi = 300, bg = "white")

# 4.3 Additional Visualization: Scatter plot of CFR vs Confirmed Cases
scatter_plot <- ggplot(covid_data, 
                       aes(x = Confirmed, y = CFR, 
                           color = Development_Status,
                           size = Deaths)) +
  geom_point(alpha = 0.7) +
  scale_x_log10(labels = scales::comma) +
  scale_color_manual(values = c("Developed" = "#2E86AB", 
                                "Developing" = "#A23B72")) +
  labs(
    title = "CFR vs Total Confirmed Cases by Development Status",
    x = "Total Confirmed Cases (log scale)",
    y = "Case Fatality Rate (%)",
    color = "Development Status",
    size = "Total Deaths",
    caption = "Bubble size represents total number of deaths"
  ) +
  theme_minimal() +
  theme(
    legend.position = "right",
    panel.grid.minor = element_blank()
  )

print(scatter_plot)
ggsave("CFR_vs_cases_scatter.png", plot = scatter_plot, 
       width = 10, height = 6, dpi = 300, bg = "white")

