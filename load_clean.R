# --- Load and Clean Data: Customer Segmentation Project ---
# Load required packages
library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)

# Load dataset
data <- read_excel("data/Online Retail.xlsx")

# Preview data
head(data)
str(data)

# --- Basic cleaning ---
# Remove rows with missing CustomerID
data <- data %>% drop_na(CustomerID)

# Remove negative or zero quantities (these are usually returns)
data <- data %>% filter(Quantity > 0, UnitPrice > 0)

# Create a new column: TotalPrice = Quantity * UnitPrice
data <- data %>%
  mutate(TotalPrice = Quantity * UnitPrice)

# Check duplicates
data <- distinct(data)

# Preview cleaned dataset
glimpse(data)

# Save cleaned dataset
write.csv(data, "data/cleaned_retail.csv", row.names = FALSE)

cat("✅ Data cleaning complete. File saved as data/cleaned_retail.csv\n")
