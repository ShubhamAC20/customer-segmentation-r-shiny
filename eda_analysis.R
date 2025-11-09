# --- Exploratory Data Analysis: Customer Segmentation Project ---

# Load required libraries
library(dplyr)
library(ggplot2)
library(lubridate)
library(readr)

# Load cleaned dataset
data <- read_csv("data/cleaned_retail.csv")

# Convert InvoiceDate to Date type
data$InvoiceDate <- as_datetime(data$InvoiceDate)

# --- 1. Basic overview ---
summary(data)
glimpse(data)

# --- 2. Sales trends over time ---
daily_sales <- data %>%
  group_by(Date = as.Date(InvoiceDate)) %>%
  summarise(TotalSales = sum(TotalPrice))

ggplot(daily_sales, aes(x = Date, y = TotalSales)) +
  geom_line(color = "steelblue", linewidth = 1) +
  labs(title = "Daily Sales Trend", x = "Date", y = "Total Sales") +
  theme_minimal()

# --- 3. Top 10 selling products ---
top_products <- data %>%
  group_by(Description) %>%
  summarise(TotalSales = sum(TotalPrice)) %>%
  arrange(desc(TotalSales)) %>%
  head(10)

ggplot(top_products, aes(x = reorder(Description, TotalSales), y = TotalSales)) +
  geom_col(fill = "darkorange") +
  coord_flip() +
  labs(title = "Top 10 Selling Products", x = "Product", y = "Total Sales") +
  theme_minimal()

# --- 4. Country-wise sales ---
country_sales <- data %>%
  group_by(Country) %>%
  summarise(TotalSales = sum(TotalPrice)) %>%
  arrange(desc(TotalSales))

ggplot(head(country_sales, 10), aes(x = reorder(Country, TotalSales), y = TotalSales)) +
  geom_col(fill = "seagreen") +
  coord_flip() +
  labs(title = "Top 10 Countries by Sales", x = "Country", y = "Total Sales") +
  theme_minimal()

# assume daily_sales, top_products, country_sales exist
library(ggplot2)

p1 <- ggplot(daily_sales, aes(x = Date, y = TotalSales)) +
  geom_line(color = "steelblue", linewidth = 1) +
  labs(title = "Daily Sales Trend", x = "Date", y = "Total Sales") +
  theme_minimal()

p2 <- ggplot(top_products, aes(x = reorder(Description, TotalSales), y = TotalSales)) +
  geom_col(fill = "darkorange") +
  coord_flip() +
  labs(title = "Top 10 Selling Products", x = "Product", y = "Total Sales") +
  theme_minimal()

p3 <- ggplot(head(country_sales, 10), aes(x = reorder(Country, TotalSales), y = TotalSales)) +
  geom_col(fill = "seagreen") +
  coord_flip() +
  labs(title = "Top 10 Countries by Sales", x = "Country", y = "Total Sales") +
  theme_minimal()

# show them
print(p1)
print(p2)
print(p3)

# save copies
dir.create("plots", showWarnings = FALSE)
ggsave("plots/daily_sales.png", p1, width = 11, height = 5, dpi = 150)
ggsave("plots/top_products.png", p2, width = 10, height = 6, dpi = 150)
ggsave("plots/country_sales.png", p3, width = 10, height = 6, dpi = 150)
