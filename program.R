# Install if missing
packages <- c("tidyverse", "corrplot", "car", "FactoMineR", "factoextra", 
              "cluster", "mclust", "fpc", "ggplot2", "gridExtra")

install_if_missing <- function(p) {
  if (!require(p, character.only = TRUE)) install.packages(p)
}
invisible(lapply(packages, install_if_missing))

# Load all
library(tidyverse)
library(corrplot)
library(car)
library(FactoMineR)
library(factoextra)
library(cluster)
library(mclust)
library(fpc)
library(readr)

clean_data<- read_csv("business_risk_clean_imputed_dataset.csv")


# Verify
dim(clean_data)        # Should be 18120 x 35
sum(is.na(clean_data)) # Should be 0


#Variable Selection 

domains <- list(
  financial_health = c("debt_to_assets", "quick_ratio", "current_ratio", 
                       "overdue_30d_rate", "cash_conversion_days", "ebit_margin", 
                       "utilisation_rate"),
  fraud_risk = c("txn_volatility", "chargeback_rate", "complaint_rate", 
                 "device_change_rate", "late_filing_count"),
  growth_resilience = c("employee_growth", "web_traffic_growth", 
                        "avg_txn_value", "monthly_txn_count"),
  digital_behavior = c("digital_share", "cash_share"),
  operational_concentration = c("supplier_concentration", "customer_concentration", 
                                "operational_complexity_index"),
  lifecycle = c("years_on_book", "firm_age_years"),
  categorical_vars = c("sector", "region", "primary_product", 
                       "acquisition_channel", "employee_band", 
                       "marketing_segment_code")
)

# NOTE: Excluding legacy_risk_score, exposure_band_index, 
# client_engagement_score, onboarding_year as per  plan


# Function to flag high correlations
check_correlations <- function(data, var_list, threshold = 0.5) {
  if(length(var_list) < 2) return(NULL)
  
  cor_mat <- cor(data[, var_list], use = "pairwise.complete.obs")
  high_cor <- which(abs(cor_mat) > threshold & upper.tri(cor_mat), arr.ind = TRUE)
  
  if(nrow(high_cor) > 0) {
    high_cor_pairs <- data.frame(
      var1 = rownames(cor_mat)[high_cor[,1]],
      var2 = colnames(cor_mat)[high_cor[,2]],
      cor = cor_mat[high_cor]
    )
    return(high_cor_pairs)
  }
  return(NULL)
}

# Check each numeric domain
for(domain_name in names(domains)[1:6]) {
  vars <- domains[[domain_name]]
  vars <- vars[vars %in% colnames(clean_data)]
  if(length(vars) > 1) {
    high <- check_correlations(clean_data, vars)
    if(!is.null(high)) {
      cat("\n=== High correlations in", domain_name, "===\n")
      print(high)
    }
  }
}

#VIF screening 

# Select all numeric variables (excluding categoricals)
numeric_vars <- unlist(domains[1:6])
numeric_vars <- numeric_vars[numeric_vars %in% colnames(clean_data)]

# Calculate VIF (need to remove perfect collinearity first)
vif_data <- clean_data[, numeric_vars]
vif_data <- vif_data[, apply(vif_data, 2, var, na.rm = TRUE) > 1e-8]

# VIF calculation (use car::vif with lm)
vif_model <- lm(scale(debt_to_assets) ~ ., data = vif_data)
vif_values <- vif(vif_model)
vif_df <- data.frame(variable = names(vif_values), VIF = vif_values)

# Flag VIF > 5
vif_df %>% filter(VIF > 5) %>% arrange(desc(VIF))


# View all VIF values (not just >5)
vif_df %>% arrange(desc(VIF)) %>% head(10)

# Summary of VIFs
summary(vif_df$VIF)

# Histogram of VIF values
hist(vif_df$VIF, breaks = 20, 
     main = "VIF Distribution", 
     xlab = "VIF", 
     col = "steelblue")
abline(v = 5, col = "red", lwd = 2, lty = 2)
abline(v = 10, col = "darkred", lwd = 2, lty = 2)
legend("topright", c("VIF=5 threshold", "VIF=10 threshold"), 
       col = c("red", "darkred"), lty = 2)



library(FactoMineR)
library(factoextra)
library(dplyr)

famd_data <- clean_data #no prunning 

# Ensure categoricals are factors
famd_data <- famd_data %>%
  mutate(across(where(is.character), as.factor))

# Run FAMD
set.seed(42)
famd_result <- FAMD(famd_data, ncp = 10, graph = FALSE)

# Scree plot
fviz_screeplot(famd_result, addlabels = TRUE, ncp = 10) +
  ggtitle("FAMD Scree Plot")

# Determine dimensions to retain
variance <- famd_result$eig[,2]  # % of variance
n_dims <- sum(variance > 5)  # keep dims with >5% variance
cat("Dimensions to retain:", n_dims, "\n")

# If n_dims < 2, use at least 2
n_dims <- max(n_dims, 2)

# Extract scores
famd_scores <- as.data.frame(famd_result$ind$coord[, 1:n_dims])
colnames(famd_scores) <- paste0("FAMD_Dim", 1:n_dims)



# This works without any packages
plot(famd_result$eig[,2], type = "b", 
     xlab = "Dimension", ylab = "Variance Explained (%)",
     main = "FAMD Scree Plot", 
     pch = 16, col = "blue", lwd = 2)

# Add cumulative variance line
lines(famd_result$eig[,3], type = "b", col = "red", pch = 17, lwd = 2)
legend("topright", c("Individual %", "Cumulative %"), 
       col = c("blue", "red"), pch = c(16, 17), lty = 1)



# Scatterplot of all clients in 2D risk space
plot(famd_scores$FAMD_Dim1, famd_scores$FAMD_Dim2,
     xlab = "Dimension 1 (Interpret after contributions)",
     ylab = "Dimension 2 (Interpret after contributions)",
     main = "Clients in Latent Risk Space",
     pch = 16, cex = 0.2, col = rgb(0,0,0,0.1))

# Top contributors to Dimension 1
top10_dim1 <- head(sort(famd_result$var$contrib[,1], decreasing = TRUE), 10)
barplot(top10_dim1, main = "Top Contributors - Dimension 1",
        xlab = "Variable", ylab = "Contribution (%)",
        las = 2, cex.names = 0.7, col = "steelblue")

# Top contributors to Dimension 2
top10_dim2 <- head(sort(famd_result$var$contrib[,2], decreasing = TRUE), 10)
barplot(top10_dim2, main = "Top Contributors - Dimension 2",
        xlab = "Variable", ylab = "Contribution (%)",
        las = 2, cex.names = 0.7, col = "darkorange")


eig_df <- as.data.frame(famd_result$eig)
colnames(eig_df) <- c("Eigenvalue", "VariancePercent", "CumulativePercent")
eig_df










# ============================================
# COMPLETE ANALYSIS - BASE R + FACTOMINER ONLY
# ============================================

# 1. Use 10 FAMD dimensions
n_dims <- 10
famd_scores <- as.data.frame(famd_result$ind$coord[, 1:n_dims])
colnames(famd_scores) <- paste0("Dim", 1:n_dims)

# 2. Find optimal K using silhouette (base R implementation)
set.seed(42)
max_k <- 8
sil_width <- numeric(max_k - 1)

for(k in 2:max_k) {
  # K-means
  km <- kmeans(famd_scores, centers = k, nstart = 25, iter.max = 100)
  
  # Calculate silhouette manually (base R)
  dist_mat <- as.matrix(dist(famd_scores))
  sil_scores <- numeric(nrow(famd_scores))
  
  for(i in 1:nrow(famd_scores)) {
    # Within-cluster distance
    same_cluster <- which(km$cluster == km$cluster[i])
    if(length(same_cluster) > 1) {
      a_i <- mean(dist_mat[i, same_cluster[same_cluster != i]])
    } else {
      a_i <- 0
    }
    
    # Nearest other cluster
    other_clusters <- unique(km$cluster[km$cluster != km$cluster[i]])
    b_i <- Inf
    for(cl in other_clusters) {
      other_idx <- which(km$cluster == cl)
      b_ic <- mean(dist_mat[i, other_idx])
      if(b_ic < b_i) b_i <- b_ic
    }
    
    sil_scores[i] <- (b_i - a_i) / max(a_i, b_i)
  }
  
  sil_width[k-1] <- mean(sil_scores, na.rm = TRUE)
  cat("K =", k, "Silhouette =", round(sil_width[k-1], 4), "\n")
}

best_k <- which.max(sil_width) + 1
cat("\n>>> OPTIMAL SEGMENTS:", best_k, "\n")

# 3. Final clustering
final_clusters <- kmeans(famd_scores, centers = best_k, nstart = 50)$cluster
clean_data$segment <- final_clusters

# 4. Segment sizes
cat("\n=== SEGMENT SIZES ===\n")
print(table(clean_data$segment))

# 5. Profile segments (key variables)
key_vars <- c("debt_to_assets", "quick_ratio", "overdue_30d_rate", 
              "txn_volatility", "chargeback_rate", "utilisation_rate")

profiles <- aggregate(clean_data[, key_vars], 
                      by = list(Segment = clean_data$segment), 
                      FUN = mean)
cat("\n=== SEGMENT PROFILES ===\n")
print(round(profiles, 3))

# 6. Visualize (base R)
plot(famd_scores$Dim1, famd_scores$Dim2,
     col = final_clusters, pch = 16, cex = 0.3,
     xlab = "FAMD Dim1 (7.4%)", ylab = "FAMD Dim2 (5.1%)",
     main = paste("Risk Segments (K =", best_k, ")"))
legend("topright", legend = paste("Segment", 1:best_k), 
       col = 1:best_k, pch = 16, cex = 0.8)


