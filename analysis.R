# PCA on numeric variables only
library(factoextra)
library(FactoMineR)


clean_data<- read_csv("business_risk_clean_imputed_dataset.csv")
# Select numeric variables
numeric_vars <- clean_data %>% select(where(is.numeric)) %>% names()

# Run PCA
pca_result <- PCA(clean_data[, numeric_vars], scale.unit = TRUE, 
                  ncp = length(numeric_vars), graph = FALSE)

# Variance explained
pca_var <- as.data.frame(pca_result$eig)
colnames(pca_var) <- c("Eigenvalue", "Variance%", "Cumulative%")
print(round(pca_var[1:15, ], 2))

# Scree plot
fviz_screeplot(pca_result, addlabels = TRUE, ncp = 20) +
  ggtitle("PCA Scree Plot - Numeric Variables Only") +
  geom_hline(yintercept = 5, linetype = "dashed", color = "red")





# =============================================================
# REPORT READY SCREE PLOT
# PCA - Numeric Variables Only
# =============================================================

library(factoextra)
library(ggplot2)

# Create publication ready scree plot
scree_plot <- fviz_screeplot(pca_result, 
                             addlabels = TRUE, 
                             ncp = 20,
                             barfill = "steelblue",
                             barcolor = "steelblue",
                             linecolor = "darkred",
                             title = "PCA Scree Plot: Numeric Variables Only") +
  geom_hline(yintercept = 5, linetype = "dashed", color = "red", linewidth = 0.8) +
  geom_vline(xintercept = 4.5, linetype = "dotted", color = "darkblue", linewidth = 0.8) +
  annotate("text", x = 4.5, y = 12, label = "Elbow at 4 components", 
           angle = 90, vjust = -0.5, size = 3.5, color = "darkblue") +
  annotate("text", x = 15, y = 6, label = "50% variance threshold", 
           color = "red", size = 3.5, hjust = 0) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
    axis.title = element_text(face = "bold"),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank()
  ) +
  labs(
    x = "Principal Component",
    y = "Variance Explained (%)"
  )

# Display the plot
print(scree_plot)

# Save for report (high resolution)
ggsave("pca_scree_plot.png", scree_plot, width = 10, height = 6, dpi = 300)

# How many dimensions to capture 50% variance?
n_dims_pca <- which(pca_var$`Cumulative%` >= 50)[1]
cat("Dimensions needed for 50% variance (PCA):", n_dims_pca, "\n")
cat("Variance captured by first 10 dimensions:", pca_var$`Cumulative%`[10], "%\n")




# Define categorical variables
categorical_vars <- c("sector", "region", "primary_product", 
                      "acquisition_channel", "employee_band", 
                      "marketing_segment_code")

# Convert to factors
clean_data_pcamix <- clean_data
clean_data_pcamix[categorical_vars] <- lapply(clean_data_pcamix[categorical_vars], as.factor)

# Verify
sapply(clean_data_pcamix[, categorical_vars], class)


# Define numeric variables (exclude categoricals)
numeric_vars <- c("debt_to_assets", "quick_ratio", "current_ratio", 
                  "overdue_30d_rate", "cash_conversion_days", "ebit_margin",
                  "utilisation_rate", "txn_volatility", "chargeback_rate",
                  "complaint_rate", "late_filing_count", "device_change_rate",
                  "employee_growth", "web_traffic_growth", "avg_txn_value",
                  "monthly_txn_count", "digital_share", "cash_share",
                  "supplier_concentration", "customer_concentration",
                  "operational_complexity_index", "years_on_book", "firm_age_years")

# Ensure they exist
numeric_vars <- numeric_vars[numeric_vars %in% colnames(clean_data_pcamix)]

# Create numeric data frame (base R, not tibble)
X.quanti <- as.data.frame(clean_data_pcamix[, numeric_vars])
X.quali <- as.data.frame(clean_data_pcamix[, categorical_vars])

# Check structure
str(X.quanti[1:5, 1:5])
str(X.quali[1:5, 1:3])



library(PCAmixdata)

# Run PCAmix
set.seed(42)
pcamix_result <- PCAmix(X.quanti = X.quanti, 
                        X.quali = X.quali,
                        ndim = 20,
                        rename.level = TRUE,
                        graph = FALSE)

# Check if it worked
if(!is.null(pcamix_result)) {
  cat("PCAmixdata ran successfully!\n")
  
  # Extract eigenvalues/variance
  eig <- pcamix_result$eig
  print(round(eig[1:15, ], 2))
  
  # Extract scores (first 10 dimensions)
  pcamix_scores <- pcamix_result$ind$coord[, 1:10]
  
  # Calculate cumulative variance
  var_df <- data.frame(
    Dimension = 1:15,
    Variance = round(eig[1:15, 2], 2),
    Cumulative = round(eig[1:15, 3], 2)
  )
  print(var_df)
  
  # How many dimensions for 50% variance?
  n_50 <- which(eig[, 3] >= 50)[1]
  cat("\nDimensions needed for 50% variance:", n_50, "\n")
  
  # Variance at 10 dimensions
  var_10 <- eig[10, 3]
  cat("Variance captured by 10 dimensions:", round(var_10, 2), "%\n")
}




# =============================================================
# FAMD ON MIXED DATA (Numeric + Categorical)
# =============================================================

library(FactoMineR)
library(factoextra)

# Prepare data for FAMD (categoricals as factors)
famd_data <- clean_data
categorical_vars <- c("sector", "region", "primary_product", 
                      "acquisition_channel", "employee_band", 
                      "marketing_segment_code")

famd_data[categorical_vars] <- lapply(famd_data[categorical_vars], as.factor)

# Run FAMD
set.seed(42)
famd_result <- FAMD(famd_data, ncp = 30, graph = FALSE)

# =============================================================
# RECORD RESULTS FOR REPORT
# =============================================================

# 1. Eigenvalues and variance explained
eig_famd <- as.data.frame(famd_result$eig)
colnames(eig_famd) <- c("Eigenvalue", "VariancePercent", "CumulativePercent")

cat("\n========== FAMD EIGENVALUES ==========\n")
print(round(eig_famd[1:20, ], 2))

# 2. Key variance capture points
cat("\n========== VARIANCE CAPTURE SUMMARY ==========\n")
cat("Variance at 5 dimensions:", round(eig_famd[5, 3], 2), "%\n")
cat("Variance at 10 dimensions:", round(eig_famd[10, 3], 2), "%\n")
cat("Variance at 15 dimensions:", round(eig_famd[15, 3], 2), "%\n")
cat("Variance at 20 dimensions:", round(eig_famd[20, 3], 2), "%\n")

# 3. Dimensions needed for 50% variance
n_50 <- which(eig_famd[, 3] >= 50)[1]
cat("\nDimensions needed for 50% variance:", n_50, "\n")

# 4. Top contributing variables to first 5 dimensions
cat("\n========== TOP CONTRIBUTORS TO DIMENSIONS 1-5 ==========\n")
for(d in 1:5) {
  cat("\n--- Dimension", d, "---\n")
  contrib <- sort(famd_result$var$contrib[, d], decreasing = TRUE)
  print(round(head(contrib, 8), 2))
}

# 5. Save scores for potential use
famd_scores <- famd_result$ind$coord[, 1:20]

# =============================================================
# CREATE SUMMARY TABLE FOR REPORT
# =============================================================

variance_table <- data.frame(
  Dimensions = c(5, 10, 15, 20),
  CumulativeVariance = round(c(eig_famd[5, 3], 
                               eig_famd[10, 3], 
                               eig_famd[15, 3], 
                               eig_famd[20, 3]), 2)
)

cat("\n========== SUMMARY TABLE FOR REPORT ==========\n")
print(variance_table)



# =============================================================
# LATENT CLASS ANALYSIS FOR CATEGORICAL VARIABLES
# =============================================================

library(poLCA)

# Define categorical variables
categorical_vars <- c("sector", "region", "primary_product", 
                      "acquisition_channel", "employee_band", 
                      "marketing_segment_code")

# Ensure they are factors
clean_data[categorical_vars] <- lapply(clean_data[categorical_vars], as.factor)

# Convert primary_product to factor
clean_data$primary_product <- as.factor(clean_data$primary_product)

# Convert primary_product to factor
clean_data$primary_product <- as.factor(clean_data$primary_product)

# Now check the levels
levels(clean_data$primary_product)

# See frequency of each product
table(clean_data$primary_product)


# Now check the levels
levels(clean_data$primary_product)
# Prepare data for LCA
lca_data <- clean_data[, categorical_vars]

# Create formula for LCA (all variables as indicators)
lca_formula <- as.formula(paste0("cbind(", 
                                 paste(categorical_vars, collapse = ", "), 
                                 ") ~ 1"))

# =============================================================
# FIND OPTIMAL NUMBER OF CLASSES (2 to 6)
# =============================================================

lca_bic <- c()
lca_models <- list()

for(K in 2:6) {
  set.seed(42)
  lca_model <- poLCA(lca_formula, 
                     data = lca_data, 
                     nclass = K, 
                     maxiter = 1000, 
                     nrep = 10,      # 10 random starts to avoid local maxima
                     verbose = FALSE)
  
  lca_bic[K] <- lca_model$bic
  lca_models[[K]] <- lca_model
  
  cat("K =", K, "| BIC =", round(lca_model$bic, 2), 
      "| Log-Likelihood =", round(lca_model$llik, 2), "\n")
}

# Find optimal number of classes (lowest BIC)
best_K <- which.min(lca_bic)
cat("\n=== OPTIMAL NUMBER OF CLASSES ===\n")
cat("Best K =", best_K, "with BIC =", round(lca_bic[best_K], 2), "\n")

# =============================================================
# RESULTS FOR THE OPTIMAL MODEL
# =============================================================

best_lca <- lca_models[[best_K]]

# Class sizes
cat("\n=== CLASS SIZES ===\n")
class_probs <- best_lca$P
for(c in 1:best_K) {
  cat("Class", c, ":", round(class_probs[c] * 100, 2), "% (", 
      round(class_probs[c] * nrow(lca_data), 0), "clients)\n")
}

# Conditional probabilities (item response probabilities per class)
cat("\n=== CONDITIONAL PROBABILITIES ===\n")
for(i in 1:length(categorical_vars)) {
  cat("\n---", categorical_vars[i], "---\n")
  print(round(best_lca$probs[[i]], 3))
}

# Posterior probabilities (assign each client to most likely class)
clean_data$seg_categorical <- best_lca$predclass

# =============================================================
# VALIDATION: ASSOCIATION WITH NUMERIC RISK
# =============================================================

# Check if LCA classes align with numeric risk indicators
cat("\n=== LCA CLASSES vs NUMERIC RISK INDICATORS ===\n")
risk_by_class <- clean_data %>%
  group_by(seg_categorical) %>%
  summarise(
    n = n(),
    mean_overdue = mean(overdue_30d_rate, na.rm = TRUE),
    mean_debt = mean(debt_to_assets, na.rm = TRUE),
    mean_chargeback = mean(chargeback_rate, na.rm = TRUE)
  )
print(round(risk_by_class, 3))

# =============================================================
# SAVE ASSIGNMENTS
# =============================================================

# Add to your main dataset
clean_data$lca_class <- best_lca$predclass

# Summary table for report
cat("\n=== SUMMARY TABLE FOR REPORT ===\n")
summary_table <- data.frame(
  Class = 1:best_K,
  Size = as.vector(table(best_lca$predclass)),
  Percentage = round(as.vector(prop.table(table(best_lca$predclass)) * 100), 1)
)
print(summary_table)




# =============================================================
# GMM ON PCA SCORES (15 COMPONENTS)
# =============================================================

library(mclust)
library(tidyverse)

# =============================================================
# STEP 1: EXTRACT PCA SCORES (15 COMPONENTS)
# =============================================================

# Assuming pca_result already exists from earlier
pca_scores_15 <- pca_result$ind$coord[, 1:15]

cat("PCA scores extracted:", dim(pca_scores_15), "\n")
set.seed(42)
gmm_result <- Mclust(pca_scores_15, G = 2:13, verbose = FALSE)

# Then report:
cat("Optimal clusters:", gmm_result$G, "\n")
cat("Optimal model:", gmm_result$modelName, "\n")
cat("BIC:", round(gmm_result$bic, 2), "\n")




# Extract BIC for different G values
bic_values <- sapply(2:13, function(g) {
  gmm_temp <- Mclust(pca_scores_15, G = g, modelNames = "VVE", verbose = FALSE)
  return(gmm_temp$bic)
})

# Plot BIC
plot(2:13, bic_values, type = "b", 
     xlab = "Number of Clusters (G)", 
     ylab = "BIC",
     main = "BIC for GMM on PCA Scores",
     pch = 16, col = "blue")

# Highlight the dip at G = 6
points(6, bic_values[5], col = "red", pch = 16, cex = 2.5)
text(6, bic_values[5], labels = "  G = 6 (optimal)", col = "red", pos = 4, cex = 1.2)

# Add reference line at G = 6
abline(v = 6, lty = 2, col = "gray", lwd = 1.5)

# Add grid for readability
grid(col = "lightgray", lty = 3)



# Create plot with reversed y-axis
plot(2:13, bic_values, type = "b", 
     xlab = "Number of Clusters (G)", 
     ylab = "BIC ",
     main = "BIC for VVE Model on PCA Scores",
     pch = 16, col = "blue",
     ylim = rev(range(bic_values)))  # This reverses the y-axis

# Highlight the dip at G = 6
points(6, bic_values[5], col = "red", pch = 16, cex = 2.5)
text(6, bic_values[5], labels = "  G = 6 (optimal)", col = "red", pos = 4, cex = 1.2)

# Add reference line at G = 6
abline(v = 6, lty = 2, col = "gray", lwd = 1.5)

# Add grid
grid(col = "lightgray", lty = 3)



# =============================================================
# 1. BIC FOR EACH G (Already computed)
# =============================================================

bic_results <- data.frame(G = 2:13, BIC = NA, ICL = NA, Silhouette = NA)

for(g in 2:13) {
  set.seed(42)
  gmm_temp <- Mclust(pca_scores_15, G = g, modelNames = "VVE", verbose = FALSE)
  bic_results$BIC[g-1] <- gmm_temp$bic
  bic_results$ICL[g-1] <- gmm_temp$icl
  cat("G =", g, "| BIC =", round(gmm_temp$bic, 2), "| ICL =", round(gmm_temp$icl, 2), "\n")
}

# =============================================================
# 2. SILHOUETTE WIDTH FOR EACH G
# =============================================================

for(g in 2:13) {
  set.seed(42)
  gmm_temp <- Mclust(pca_scores_15, G = g, modelNames = "VVE", verbose = FALSE)
  
  # Calculate silhouette width
  dist_mat <- dist(pca_scores_15)
  sil <- silhouette(gmm_temp$classification, dist_mat)
  bic_results$Silhouette[g-1] <- mean(sil[, 3])
  
  cat("G =", g, "| Silhouette =", round(mean(sil[, 3]), 4), "\n")
}

# =============================================================
# 3. DISPLAY COMPLETE RESULTS TABLE
# =============================================================

cat("\n========== MODEL SELECTION SUMMARY ==========\n")
print(bic_results)

# Identify optimal by each criterion
best_BIC <- bic_results$G[which.min(bic_results$BIC)]
best_ICL <- bic_results$G[which.min(bic_results$ICL)]
best_Silhouette <- bic_results$G[which.max(bic_results$Silhouette)]

cat("\n========== OPTIMAL BY EACH CRITERION ==========\n")
cat("Best by BIC (lower = better): G =", best_BIC, "\n")
cat("Best by ICL (lower = better): G =", best_ICL, "\n")
cat("Best by Silhouette (higher = better): G =", best_Silhouette, "\n")

# =============================================================
# 4. PLOT ALL THREE CRITERIA
# =============================================================

par(mfrow = c(1, 3), mar = c(4, 4, 3, 1))

# BIC plot
plot(bic_results$G, bic_results$BIC, type = "b", 
     xlab = "Number of Clusters (G)", ylab = "BIC",
     main = "BIC (lower = better)", pch = 16, col = "blue")
points(6, bic_results$BIC[5], col = "red", pch = 16, cex = 1.5)

# ICL plot
plot(bic_results$G, bic_results$ICL, type = "b", 
     xlab = "Number of Clusters (G)", ylab = "ICL",
     main = "ICL (lower = better)", pch = 16, col = "darkgreen")
points(6, bic_results$ICL[5], col = "red", pch = 16, cex = 1.5)

# Silhouette plot
plot(bic_results$G, bic_results$Silhouette, type = "b", 
     xlab = "Number of Clusters (G)", ylab = "Silhouette Width",
     main = "Silhouette (higher = better)", pch = 16, col = "purple")
points(6, bic_results$Silhouette[5], col = "red", pch = 16, cex = 1.5)
abline(h = 0.5, lty = 2, col = "gray", label = "Good separation threshold")

par(mfrow = c(1, 1))

# =============================================================
# 5. DETAILED SILHOUETTE FOR G = 6
# =============================================================

set.seed(42)
gmm_6 <- Mclust(pca_scores_15, G = 6, modelNames = "VVE", verbose = FALSE)
dist_mat <- dist(pca_scores_15)
sil_6 <- silhouette(gmm_6$classification, dist_mat)

# Summary statistics
cat("\n========== SILHOUETTE SUMMARY FOR G = 6 ==========\n")
cat("Mean silhouette:", round(mean(sil_6[, 3]), 4), "\n")
cat("Median silhouette:", round(median(sil_6[, 3]), 4), "\n")
cat("Min silhouette:", round(min(sil_6[, 3]), 4), "\n")
cat("Max silhouette:", round(max(sil_6[, 3]), 4), "\n")

# Silhouette by cluster
sil_by_cluster <- aggregate(sil_6[, 3], by = list(Cluster = sil_6[, 1]), FUN = mean)
colnames(sil_by_cluster) <- c("Cluster", "Mean_Silhouette")
cat("\nSilhouette by cluster:\n")
print(round(sil_by_cluster, 4))


# Run GMM with optimal G = 6
set.seed(42)
gmm_final <- Mclust(pca_scores_15, G = 6, verbose = FALSE)



# Generate summary text
cat("The GMM model selected a", gmm_final$modelName, "covariance structure with", 
    gmm_final$G, "clusters. Mean classification uncertainty was", 
    round(mean(gmm_final$uncertainty), 4), 
    "with", round(mean(gmm_final$uncertainty < 0.1) * 100, 1), 
    "% of clients assigned with high confidence (uncertainty < 0.1).")



# ============================================================
# DIAGNOSTICS FOR YOUR GMM MODEL
# ============================================================

# 1. Basic model info
cat("=== MODEL SPECIFICATION ===\n")
cat("Model name:", gmm_final$modelName, "\n")
cat("Number of clusters:", gmm_final$G, "\n")
cat("Number of observations:", gmm_final$n, "\n")
cat("Input dimensions (PCA):", gmm_final$d, "\n")

# 2. Fit statistics
cat("\n=== FIT STATISTICS ===\n")
cat("BIC:", round(gmm_final$bic, 2), "\n")
cat("Log-likelihood:", round(gmm_final$loglik, 2), "\n")
cat("ICL:", round(gmm_final$icl, 2), "\n")
cat("Degrees of freedom:", gmm_final$df, "\n")

# 3. Cluster sizes
cat("\n=== CLUSTER SIZES ===\n")
cluster_sizes <- table(gmm_final$classification)
names(cluster_sizes) <- paste0("Cluster_", names(cluster_sizes))
print(cluster_sizes)
cat("\nPercentages:\n")
print(round(prop.table(cluster_sizes) * 100, 1))

# 4. Classification uncertainty
cat("\n=== CLASSIFICATION UNCERTAINTY ===\n")
cat("Mean uncertainty:", round(mean(gmm_final$uncertainty), 4), "\n")
cat("Median uncertainty:", round(median(gmm_final$uncertainty), 4), "\n")
cat("Min uncertainty:", round(min(gmm_final$uncertainty), 4), "\n")
cat("Max uncertainty:", round(max(gmm_final$uncertainty), 4), "\n")
cat("SD uncertainty:", round(sd(gmm_final$uncertainty), 4), "\n")

# 5. Uncertainty distribution
cat("\nUncertainty quantiles:\n")
print(round(quantile(gmm_final$uncertainty, probs = c(0.25, 0.5, 0.75, 0.9, 0.95, 0.99)), 4))

# 6. Percentage of high-uncertainty assignments
cat("\n--- Classification Confidence ---\n")
cat("% with uncertainty < 0.05 (very confident):", 
    round(mean(gmm_final$uncertainty < 0.05) * 100, 1), "%\n")
cat("% with uncertainty 0.05-0.10 (moderately confident):", 
    round(mean(gmm_final$uncertainty >= 0.05 & gmm_final$uncertainty < 0.10) * 100, 1), "%\n")
cat("% with uncertainty 0.10-0.20 (uncertain):", 
    round(mean(gmm_final$uncertainty >= 0.10 & gmm_final$uncertainty < 0.20) * 100, 1), "%\n")
cat("% with uncertainty > 0.20 (very uncertain):", 
    round(mean(gmm_final$uncertainty >= 0.20) * 100, 1), "%\n")

# 7. Mixing proportions
cat("\n=== MIXING PROPORTIONS ===\n")
cat("These represent the estimated prior probability of each cluster:\n")
print(round(gmm_final$parameters$pro, 4))






# =============================================================
# CLUSTER CHARACTERISTICS AND VARIABLE DRIVERS
# =============================================================

library(tidyverse)
library(factoextra)

# Ensure cluster assignments are in your data
clean_data$numeric_segment <- gmm_final$classification

# =============================================================
# 1. CLUSTER SIZES
# =============================================================

cat("\n========== CLUSTER SIZES ==========\n")
cluster_sizes <- table(clean_data$numeric_segment)
print(cluster_sizes)
cat("\nPercentages:\n")
print(round(prop.table(cluster_sizes) * 100, 1))

# =============================================================
# 2. CLUSTER PROFILES ON ORIGINAL VARIABLES
# =============================================================

# Select key numeric variables that define risk
key_vars <- c("debt_to_assets", "quick_ratio", "current_ratio", 
              "overdue_30d_rate", "cash_conversion_days", "ebit_margin",
              "utilisation_rate", "txn_volatility", "chargeback_rate",
              "complaint_rate", "late_filing_count", "device_change_rate",
              "employee_growth", "web_traffic_growth", "avg_txn_value",
              "monthly_txn_count", "digital_share", "cash_share")

# Calculate means per cluster
cluster_profiles <- clean_data %>%
  group_by(numeric_segment) %>%
  summarise(
    n = n(),
    across(all_of(key_vars), mean, na.rm = TRUE)
  )

cat("\n========== CLUSTER PROFILES (MEANS) ==========\n")
print(round(cluster_profiles, 3))

# =============================================================
# 3. STANDARDIZED PROFILES FOR COMPARISON (Z-SCORES)
# =============================================================
# Remove the 'n' column and convert to matrix for scaling
profile_matrix <- cluster_profiles %>%
  dplyr::select(-n, -numeric_segment) %>%
  as.matrix()

# Scale the matrix
profile_scaled <- scale(profile_matrix)

# Convert back to data frame with cluster labels
profile_z <- as.data.frame(profile_scaled)
profile_z$numeric_segment <- cluster_profiles$numeric_segment

# Reorder columns to put cluster first
profile_z <- profile_z %>% select(numeric_segment, everything())

# Print standardized profiles
cat("\n========== STANDARDIZED PROFILES (Z-SCORES) ==========\n")
print(round(profile_z, 2))

# =============================================================
# 4. IDENTIFY MOST DISCRIMINATING VARIABLES (ANOVA)
# =============================================================

# For each variable, test if means differ significantly across clusters
anova_results <- data.frame()

for(var in key_vars) {
  aov_result <- aov(clean_data[[var]] ~ as.factor(clean_data$numeric_segment))
  f_val <- summary(aov_result)[[1]][["F value"]][1]
  p_val <- summary(aov_result)[[1]][["Pr(>F)"]][1]
  
  anova_results <- rbind(anova_results,
                         data.frame(Variable = var,
                                    F_statistic = f_val,
                                    P_value = p_val))
}

# Sort by F-statistic (higher = more discriminating)
anova_results <- anova_results %>%
  arrange(desc(F_statistic)) %>%
  filter(P_value < 0.001)

cat("\n========== MOST DISCRIMINATING VARIABLES (ANOVA) ==========\n")
cat("Variables that differ most across clusters (all p < 0.001):\n")
print(head(anova_results, 15))

# =============================================================
# 5. VISUALIZE CLUSTER PROFILES (HEATMAP)
# =============================================================
library(pheatmap)
library(tidyverse)

# Get top 10 discriminating variables
top_vars <- head(anova_results$Variable, 10)

# Prepare data for heatmap
heatmap_data <- cluster_profiles %>%
  dplyr::select(numeric_segment, all_of(top_vars)) %>%
  column_to_rownames("numeric_segment")

# Scale for heatmap
heatmap_scaled <- scale(heatmap_data)

# Display heatmap (this should open a graphics window)
pheatmap(heatmap_scaled,
         main = "Cluster Profiles: Top 10 Discriminating Variables",
         cluster_rows = TRUE,
         cluster_cols = TRUE,
         color = colorRampPalette(c("red", "white", "steelblue"))(100))


# Open a new graphics device
dev.new()

# Now plot
pheatmap(heatmap_scaled,
         main = "Cluster Profiles: Top 10 Discriminating Variables",
         cluster_rows = TRUE,
         cluster_cols = TRUE,
         color = colorRampPalette(c("red", "white", "steelblue"))(100))


# =============================================================
# ALTERNATIVE: SIMPLE BAR PLOT FOR CLUSTER COMPARISON
# =============================================================

# If fmsb fails, use this simpler visualization
library(ggplot2)

# Reshape data for ggplot
radar_long <- radar_normalized %>%
  rownames_to_column("cluster") %>%
  pivot_longer(cols = -cluster, names_to = "variable", values_to = "value")

# Create bar plot
ggplot(radar_long, aes(x = variable, y = value, fill = cluster)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Cluster Profiles on Key Risk Variables",
       x = "Risk Variable", y = "Normalized Value (0-1)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# =============================================================
# CATEGORICAL VARIABLE DISTRIBUTION BY CLUSTER
# =============================================================

# Add cluster assignments to clean_data
clean_data$numeric_segment <- gmm_final$classification

# Check categorical distribution
categorical_vars <- c("sector", "region", "employee_band", "primary_product")

for(var in categorical_vars) {
  cat("\n========== ", var, " ==========\n")
  
  # Create contingency table
  cont_table <- table(Cluster = clean_data$numeric_segment, 
                      Category = clean_data[[var]])
  
  # Row percentages
  row_pct <- prop.table(cont_table, 1) * 100
  
  # Print top categories (first 5)
  print(round(row_pct[, 1:min(5, ncol(row_pct))], 1))
}
# =============================================================

library(fmsb)

# Select key variables for radar (normalized to 0-1)
radar_vars <- c("debt_to_assets", "overdue_30d_rate", "utilisation_rate",
                "txn_volatility", "chargeback_rate", "digital_share")

radar_data <- cluster_profiles %>%
  select(numeric_segment, all_of(radar_vars)) %>%
  column_to_rownames("numeric_segment")

# Add max and min rows for radar
radar_data <- rbind(rep(1, ncol(radar_data)), 
                    rep(0, ncol(radar_data)), 
                    radar_data)

# Radar chart
radarchart(radar_data,
           axistype = 1,
           pcol = rainbow(6),
           pfcol = adjustcolor(rainbow(6), alpha.f = 0.3),
           plwd = 2,
           title = "Cluster Profiles on Key Risk Variables")
legend(x = 1.2, y = 1, 
       legend = rownames(radar_data)[-c(1,2)], 
       col = rainbow(6), lty = 1, lwd = 2, cex = 0.8)



# =============================================================
# MULTIPLE IMPUTATION WITH MICE (m = 25)
# =============================================================

library(mice)
library(mclust)
library(cluster)
library(clue)

# Run multiple imputation
imp_all <- mice(data_final_all_imp, m = 25, method = "pmm", seed = 42)

# =============================================================
# EXTRACT PCA SCORES FOR EACH IMPUTATION
# =============================================================

# Store PCA scores for all 25 imputations
all_pca_scores <- list()
all_gmm_models <- list()
all_classifications <- list()
all_posteriors <- list()

for(i in 1:25) {
  # Extract completed dataset
  ds <- complete(imp_all, i)
  
  # Ensure categoricals are factors
  categorical_vars <- c("sector", "region", "primary_product", 
                        "acquisition_channel", "employee_band", 
                        "marketing_segment_code")
  ds[categorical_vars] <- lapply(ds[categorical_vars], as.factor)
  
  # Run PCA on numeric variables
  numeric_vars <- names(ds)[sapply(ds, is.numeric)]
  pca_result <- PCA(ds[, numeric_vars], scale.unit = TRUE, graph = FALSE)
  
  # Extract 15 PCA components
  all_pca_scores[[i]] <- pca_result$ind$coord[, 1:15]
  
  cat("Imputation", i, "- PCA completed\n")
}

# =============================================================
# 1. BIC FOR EACH G ACROSS IMPUTATIONS
# =============================================================

# Store results for each imputation
bic_by_imputation <- matrix(NA, nrow = 25, ncol = 12)  # 25 imputations x 12 G values
icl_by_imputation <- matrix(NA, nrow = 25, ncol = 12)
silhouette_by_imputation <- matrix(NA, nrow = 25, ncol = 12)

for(i in 1:25) {
  cat("\nProcessing imputation", i, "...\n")
  
  for(g in 2:13) {
    set.seed(42)
    gmm_temp <- Mclust(all_pca_scores[[i]], G = g, modelNames = "VVE", verbose = FALSE)
    
    bic_by_imputation[i, g-1] <- gmm_temp$bic
    icl_by_imputation[i, g-1] <- gmm_temp$icl
    
    # Calculate silhouette width
    dist_mat <- dist(all_pca_scores[[i]])
    sil <- silhouette(gmm_temp$classification, dist_mat)
    silhouette_by_imputation[i, g-1] <- mean(sil[, 3])
    
    cat("  G =", g, "| BIC =", round(gmm_temp$bic, 2), 
        "| ICL =", round(gmm_temp$icl, 2),
        "| Silhouette =", round(mean(sil[, 3]), 4), "\n")
  }
}

# =============================================================
# 2. POOL RESULTS ACROSS IMPUTATIONS (Rubin's Rule)
# =============================================================

# Average BIC, ICL, and Silhouette across imputations
pooled_bic <- colMeans(bic_by_imputation)
pooled_icl <- colMeans(icl_by_imputation)
pooled_silhouette <- colMeans(silhouette_by_imputation)

# Create results table
bic_results <- data.frame(
  G = 2:13,
  BIC = pooled_bic,
  ICL = pooled_icl,
  Silhouette = pooled_silhouette
)

cat("\n========== POOLED MODEL SELECTION SUMMARY ==========\n")
print(round(bic_results, 2))

# Identify optimal by each criterion
best_BIC <- bic_results$G[which.min(bic_results$BIC)]
best_ICL <- bic_results$G[which.min(bic_results$ICL)]
best_Silhouette <- bic_results$G[which.max(bic_results$Silhouette)]

cat("\n========== OPTIMAL BY EACH CRITERION (Pooled) ==========\n")
cat("Best by BIC (lower = better): G =", best_BIC, "\n")
cat("Best by ICL (lower = better): G =", best_ICL, "\n")
cat("Best by Silhouette (higher = better): G =", best_Silhouette, "\n")

# =============================================================
# 3. RUN FINAL GMM ON EACH IMPUTATION AND POOL POSTERIORS
# =============================================================

for(i in 1:25) {
  set.seed(42)
  gmm_final <- Mclust(all_pca_scores[[i]], G = 6, modelNames = "VVE", verbose = FALSE)
  all_gmm_models[[i]] <- gmm_final
  all_classifications[[i]] <- gmm_final$classification
  all_posteriors[[i]] <- gmm_final$z
}

# =============================================================
# 4. ALIGN CLUSTER LABELS USING HUNGARIAN ALGORITHM
# =============================================================

# Use imputation 1 as reference
ref_probs <- all_posteriors[[1]]

align_labels <- function(ref_probs, target_probs) {
  G <- ncol(ref_probs)
  
  # Build cost matrix
  cost_matrix <- matrix(0, G, G)
  for (g in 1:G) {
    for (k in 1:G) {
      cost_matrix[g, k] <- sum((ref_probs[, g] - target_probs[, k])^2)
    }
  }
  
  # Hungarian algorithm finds best permutation
  perm <- as.integer(solve_LSAP(cost_matrix))
  inv_perm <- order(perm)
  
  return(target_probs[, inv_perm])
}

# Align all imputations to reference
aligned_probs <- vector("list", 25)
aligned_probs[[1]] <- ref_probs

for(i in 2:25) {
  aligned_probs[[i]] <- align_labels(ref_probs, all_posteriors[[i]])
  cat("Aligned imputation", i, "\n")
}

# =============================================================
# 5. POOL POSTERIOR PROBABILITIES (Rubin's Rule)
# =============================================================

pooled_probs <- Reduce("+", aligned_probs) / length(aligned_probs)

# Assign final clusters
final_clusters <- apply(pooled_probs, 1, which.max)

cat("\n========== FINAL CLUSTER SIZES (Pooled across 25 imputations) ==========\n")
print(table(final_clusters))

# =============================================================
# 6. PLOT POOLED RESULTS
# =============================================================

par(mfrow = c(1, 3), mar = c(4, 4, 3, 1))

# Pooled BIC plot
plot(bic_results$G, bic_results$BIC, type = "b", 
     xlab = "Number of Clusters (G)", ylab = "BIC",
     main = "Pooled BIC (lower = better)", pch = 16, col = "blue")
points(6, bic_results$BIC[5], col = "red", pch = 16, cex = 1.5)

# Pooled ICL plot
plot(bic_results$G, bic_results$ICL, type = "b", 
     xlab = "Number of Clusters (G)", ylab = "ICL",
     main = "Pooled ICL (lower = better)", pch = 16, col = "darkgreen")
points(6, bic_results$ICL[5], col = "red", pch = 16, cex = 1.5)

# Pooled Silhouette plot
plot(bic_results$G, bic_results$Silhouette, type = "b", 
     xlab = "Number of Clusters (G)", ylab = "Silhouette Width",
     main = "Pooled Silhouette (higher = better)", pch = 16, col = "purple")
points(6, bic_results$Silhouette[5], col = "red", pch = 16, cex = 1.5)
abline(h = 0.5, lty = 2, col = "gray")

par(mfrow = c(1, 1))

# =============================================================
# 7. SUMMARY TABLE FOR REPORT
# =============================================================

cat("\n========== SUMMARY TABLE FOR REPORT ==========\n")
summary_table <- data.frame(
  G = 2:13,
  BIC = round(pooled_bic, 0),
  ICL = round(pooled_icl, 0),
  Silhouette = round(pooled_silhouette, 4)
)
print(summary_table)

cat("\n========== FINAL CLUSTER STABILITY ==========\n")
cat("Number of imputations used:", 25, "\n")
cat("Cluster alignment method: Hungarian algorithm\n")
cat("Pooling method: Rubin's Rule (averaged posterior probabilities)\n")
cat("Final number of clusters: 6\n")

