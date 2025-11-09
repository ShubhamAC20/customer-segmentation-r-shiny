# rfm_segmentation.R
# Full, robust RFM segmentation script (paste into RStudio)
# Author: Shubham
# ---------------------------------------------------------

# --- Libraries ---
library(readr)
library(dplyr)
library(lubridate)
library(ggplot2)
library(factoextra)
library(cluster)
library(reshape2)
library(tidyr)
library(scales)

# --- Load cleaned dataset ---
data <- read_csv("data/cleaned_retail.csv", show_col_types = FALSE)
data$InvoiceDate <- as_datetime(data$InvoiceDate)

# Make output folders
dir.create("plots", showWarnings = FALSE)
dir.create("output", showWarnings = FALSE)

# --- Step 1: Compute correct RFM metrics ---
snapshot_date <- max(data$InvoiceDate, na.rm = TRUE) + days(1)

rfm <- data %>%
  group_by(CustomerID) %>%
  summarise(
    Recency = as.numeric(difftime(snapshot_date, max(InvoiceDate), units = "days")), # days since last purchase
    Frequency = n_distinct(InvoiceNo),
    Monetary = sum(TotalPrice),
    .groups = "drop"
  )

cat("RFM computed for", nrow(rfm), "customers\n")

# --- Step 2: Transform Monetary (reduce skew) ---
rfm <- rfm %>% mutate(Monetary_log = log1p(Monetary))

# --- Step 3: Scale features used for clustering ---
rfm_scaled <- scale(rfm %>% select(Recency, Frequency, Monetary_log))

# --- Step 4: Diagnostics (Elbow, Silhouette, Gap on sample) ---
set.seed(123)

# Elbow (WSS)
p_elbow <- fviz_nbclust(rfm_scaled, kmeans, method = "wss") +
  ggtitle("Elbow method (WSS) - full data")
print(p_elbow)
ggsave("plots/elbow_wss.png", p_elbow, width = 9, height = 5, dpi = 150)

# Silhouette
p_sil <- fviz_nbclust(rfm_scaled, kmeans, method = "silhouette") +
  ggtitle("Silhouette method - full data")
print(p_sil)
ggsave("plots/silhouette.png", p_sil, width = 9, height = 5, dpi = 150)

# Gap statistic (sample-based)
n_sample <- min(5000, nrow(rfm_scaled))
sample_idx <- sample(seq_len(nrow(rfm_scaled)), n_sample)
cat("Running gap statistic on a sample of", n_sample, "rows (may take ~1-2 min)...\n")
gap_stat <- tryCatch({
  clusGap(rfm_scaled[sample_idx, , drop = FALSE], FUN = kmeans, nstart = 20, K.max = 10, B = 50)
}, error = function(e) {
  message("Gap statistic failed: ", e$message)
  return(NULL)
})

if (!is.null(gap_stat)) {
  p_gap <- fviz_gap_stat(gap_stat) + ggtitle("Gap statistic (sample)")
  print(p_gap)
  ggsave("plots/gap_stat.png", p_gap, width = 9, height = 5, dpi = 150)
} else {
  message("Gap statistic unavailable — proceed with elbow/silhouette.")
}

# --- Step 5: Choose k and robustly run KMeans ---
# Default: business-friendly clusters; change if diagnostics suggest otherwise
k_opt <- 5
cat("Using k_opt =", k_opt, "\n")

set.seed(42)
kmeans_model <- kmeans(rfm_scaled, centers = k_opt, nstart = 100, iter.max = 200)
rfm$Cluster <- factor(kmeans_model$cluster)

# Check convergence warnings in console; increasing nstart/iter.max generally helps.

# --- Step 6: Cluster profile summary (numeric) ---
rfm_summary <- rfm %>%
  group_by(Cluster) %>%
  summarise(
    Avg_Recency = round(mean(Recency), 1),
    Avg_Frequency = round(mean(Frequency), 2),
    Avg_Monetary = round(mean(Monetary), 2),
    Count = n(),
    .groups = "drop"
  ) %>%
  arrange(as.integer(Cluster))

print(rfm_summary)
write_csv(rfm_summary, "output/rfm_cluster_summary_raw.csv")

# --- Step 7: Auto-label clusters (heuristic based on Avg_Monetary) ---
# Create ranking by Avg_Monetary descending; map top -> VIP, next -> Loyal, etc.
ranked <- rfm_summary %>%
  arrange(desc(Avg_Monetary)) %>%
  mutate(Rank = row_number())

num_clusters <- nrow(ranked)
default_labels <- switch(
  as.character(min(5, num_clusters)),
  "1" = c("VIP"),
  "2" = c("VIP", "Low-Value"),
  "3" = c("VIP", "Loyal", "Low-Value"),
  "4" = c("VIP", "Loyal", "Occasional", "At-Risk"),
  "5" = c("VIP", "Loyal", "Occasional", "At-Risk", "Low-Value")
)
if (num_clusters > 5) {
  extras <- paste0("Segment_", 6:num_clusters)
  default_labels <- c(default_labels, extras)
}
label_map <- setNames(default_labels[1:num_clusters], ranked$Cluster)

rfm <- rfm %>% mutate(Segment = unname(label_map[as.character(Cluster)]))
rfm$Segment <- factor(rfm$Segment, levels = unique(rfm$Segment))

# Save labeled segment summary
rfm_summary_labeled <- rfm %>%
  group_by(Segment) %>%
  summarise(
    Avg_Recency = round(mean(Recency), 1),
    Avg_Frequency = round(mean(Frequency), 2),
    Avg_Monetary = round(mean(Monetary), 2),
    Count = n(),
    .groups = "drop"
  ) %>%
  arrange(desc(Avg_Monetary))

print(rfm_summary_labeled)
write_csv(rfm_summary_labeled, "output/rfm_cluster_summary_labeled.csv")

# Save labeled customers
write_csv(rfm, "output/customer_segments_labeled.csv")

# --- Step 8: Visualizations (PCA, Centroid heatmap, Frequency vs Monetary) ---

# PCA projection of scaled RFM
pca <- prcomp(rfm_scaled, center = TRUE, scale. = TRUE)
pca_df <- data.frame(PC1 = pca$x[, 1], PC2 = pca$x[, 2],
                     Cluster = rfm$Cluster, Segment = rfm$Segment)

p_rfm_pca <- ggplot(pca_df, aes(x = PC1, y = PC2, color = Cluster)) +
  geom_point(alpha = 0.6, size = 1.8) +
  labs(title = "RFM Clusters: PCA projection", x = "PC1", y = "PC2") +
  theme_minimal() +
  scale_color_brewer(palette = "Set2")

print(p_rfm_pca)
ggsave("plots/rfm_pca_clusters.png", p_rfm_pca, width = 9, height = 6, dpi = 150)

# Correct centroid table and heatmap
centroids <- as.data.frame(kmeans_model$centers)
centroids$Cluster <- paste0("Cluster_", seq_len(nrow(centroids)))
cent_m <- tidyr::pivot_longer(centroids, cols = -Cluster, names_to = "Feature", values_to = "Value")

p_centroid <- ggplot(cent_m, aes(x = Feature, y = Cluster, fill = Value)) +
  geom_tile() +
  scale_fill_viridis_c(option = "plasma") +
  labs(title = "Cluster centroid heatmap (scaled features)", x = "Feature", y = "Cluster") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 30, hjust = 1))

print(p_centroid)
ggsave("plots/centroid_heatmap.png", p_centroid, width = 9, height = 5, dpi = 150)

# Frequency vs Monetary scatter by cluster
p_fm <- ggplot(rfm, aes(x = Frequency, y = Monetary, color = Cluster)) +
  geom_jitter(alpha = 0.6, size = 1.5, width = 0.15, height = 0) +
  labs(title = "Customer segments: Frequency vs Monetary",
       x = "Frequency (distinct invoices)",
       y = "Monetary (total spend)") +
  theme_minimal() +
  scale_y_continuous(labels = scales::comma) +
  scale_color_brewer(palette = "Set2")

print(p_fm)
ggsave("plots/frequency_monetary_clusters.png", p_fm, width = 9, height = 6, dpi = 150)

# --- Step 9: Final messages & tips ---
cat("\n✅ RFM segmentation complete.\n")
cat("Outputs saved to 'output/' and plots saved to 'plots/'.\n")
cat("- Labeled customers: output/customer_segments_labeled.csv\n")
cat("- Cluster summary (raw): output/rfm_cluster_summary_raw.csv\n")
cat("- Cluster summary (labeled): output/rfm_cluster_summary_labeled.csv\n")
cat("- Plots: plots/*.png\n\n")

cat("Next steps: inspect 'rfm_cluster_summary_labeled.csv' and if any label seems off, tell me the summary and I'll map cluster numbers to specific names (VIP, Loyal, At-Risk, etc.) for you.\n")
