# Load essential packages
library(readxl)
library(dplyr)
library(ggplot2)
library(gridExtra)  # For organizing plots

# Data import using readxl
whitewine_data <- read_excel("path/Whitewine_v6.xlsx")

# Data cleaning steps
# Removal of rows with NA values to prepare for clustering analysis
whitewine_data <- na.omit(whitewine_data)

# Dropping the 'quality' column for clustering analysis
whitewine_data <- select(whitewine_data, -quality)

# Data scaling
# Normalization is essential for accurate distance calculations in clustering
normalized_data <- as.data.frame(scale(whitewine_data))

# Initial visualization of data spread
pre_outlier_removal_plot <- ggplot(stack(normalized_data), aes(ind, values)) + 
  geom_boxplot() + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  ggtitle("Boxplot Before Outlier Removal")

# Defining outlier boundaries
compute_iqr_bounds <- function(column, scale_factor = 1.5) {
  q1 <- quantile(column, 0.25)
  q3 <- quantile(column, 0.75)
  iqr <- q3 - q1
  return(c(lower = q1 - scale_factor * iqr, upper = q3 + scale_factor * iqr))
}

# Filtering outliers
clean_data <- normalized_data

for (variable in names(normalized_data)) {
  bounds <- compute_iqr_bounds(normalized_data[[variable]])
  clean_data <- clean_data %>%
    filter(.data[[variable]] >= bounds[1] & .data[[variable]] <= bounds[2])
}

# Post-outlier removal visualization
post_outlier_removal_plot <- ggplot(stack(clean_data), aes(ind, values)) + 
  geom_boxplot() + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  ggtitle("Boxplot After Outlier Removal")

# Compare initial and final boxplots
grid.arrange(pre_outlier_removal_plot, post_outlier_removal_plot, ncol = 2)

# Clustering analysis setup
library(NbClust)
set.seed(190)

# Determine optimal number of clusters using NbClust
nb_clusters <- NbClust(data = clean_data, diss = NULL, distance = "euclidean",
                       min.nc = 2, max.nc = 10, method = "kmeans")

# Visualize clustering indices
barplot(nb_clusters$Best.nc[1,], names.arg=nb_clusters$Best.nc[2,], 
        xlab="Index Value", ylab="Number of Clusters", main="NbClust Barplot")

# Further cluster visualization techniques
library(cluster)
library(factoextra)

# Elbow method visualization
fviz_nbclust(clean_data, kmeans, method = "wss") +
  labs(subtitle = "Elbow Method Results")

# Silhouette method for cluster quality assessment
fviz_nbclust(clean_data, kmeans, method = "silhouette")+
  labs(subtitle = "Silhouette Method Results")

# Gap statistic to determine optimal cluster number
fviz_nbclust(clean_data, kmeans, nstart = 25, method = "gap_stat", 
             nboot = 50, iter.max=50) + labs(subtitle = "Gap Statistic Results")

# Perform k-means clustering on the filtered dataset with k=2
set.seed(190)
clusters <- kmeans(clean_data, 2, nstart = 25)

# Cluster analysis calculations
total_bss <- sum(clusters$size * apply(clusters$centers, 1, function(center) sum((center - colMeans(clean_data))^2)))
cat("Between-cluster sum of squares (BSS):", total_bss, "\n")
print(clusters)

# Within-cluster sum of squares analysis
for (i in 1:2) {
  cat("Cluster", i, "WSS:", clusters$withinss[i], "\n")
}
cat("Total WSS:", clusters$tot.withinss, "\n")

# Ratio of BSS to total sum of squares (TSS)
total_tss <- sum(apply(clean_data, 2, function(col) sum((col - mean(col))^2)))
bss_tss_ratio <- (total_bss / total_tss) * 100
cat("BSS/TSS ratio:", bss_tss_ratio, "%\n")

# Silhouette analysis to validate cluster consistency
silhouette_info <- silhouette(clusters$cluster, dist(clean_data))
average_silhouette_width <- mean(silhouette_info[, "sil_width"])
cat("Average silhouette width:", average_silhouette_width, "\n")

# Plot silhouette analysis
plot(silhouette_info, col = 1:2, border = NA, main = "Silhouette Analysis Plot")

sil.plot <- fviz_silhouette(silhouette(clusters$cluster, dist(clean_data))) 
print(sil.plot) 

#e
# Conduct PCA to reduce dimensionality
pca_analysis <- prcomp(clean_data)

# Derive eigenvalues for variance explanation
principal_values <- pca_analysis$sdev^2

# Obtain eigenvectors for PCA components
principal_vectors <- pca_analysis$rotation

# Calculate the variance proportion explained by each principal component
explained_variance <- principal_values / sum(principal_values)

# Visualize the proportion of variance explained
plot(explained_variance, ylab = "Proportion of Variance", type="b", 
     main="Scree Plot of PCA")

# Output eigenvalues and eigenvectors
print(principal_values)
print(principal_vectors)

# Calculate cumulative variance explained
cumulative_explained_variance <- cumsum(explained_variance)

# Plot cumulative variance against principal components
plot(cumulative_explained_variance, xlab = "Principal Components", 
     ylab = "Cumulative Variance Explained")
abline(h = 0.85, lty = 2, lwd = 2) 

# Determine the number of principal components covering at least 85% variance
num_PCs <- min(which(cumulative_explained_variance > 0.85))
print(num_PCs)

# Retain only the significant principal components
reduced_data <- as.data.frame(pca_analysis$x[, 1:num_PCs])

#f
# Optimize cluster number using multiple indices
library(NbClust)
set.seed(190)
nb_results <- NbClust(reduced_data, distance = "euclidean", min.nc = 2, max.nc = 10, method = "kmeans", index = "all")
barplot(nb_results$Best.nc[1,], names.arg=nb_results$Best.nc[2,], xlab="Index Value", 
        ylab="Number of Clusters", main="NbClust Results")

library(factoextra)
# Visualization of cluster validity using different statistical methods
fviz_nbclust(reduced_data, kmeans, method = "wss") + labs(subtitle = "Elbow Method Analysis")
fviz_nbclust(reduced_data, kmeans, method = "silhouette") + labs(subtitle = "Silhouette Method Analysis")
fviz_nbclust(reduced_data, kmeans, nstart = 25, method = "gap_stat", nboot = 50, iter.max=50) + labs(subtitle = "Gap Statistic Analysis")

#g
# Perform clustering with optimal number of clusters
optimal_k <- 2
set.seed(190)
kmeans_result <- kmeans(clean_data, optimal_k, nstart = 25)
# Calculate between-cluster variance
between_cluster_variance <- sum(kmeans_result$size * apply(kmeans_result$centers, 1, function(center) sum((center - colMeans(clean_data))^2)))
cat("Between-cluster variance:", between_cluster_variance, "\n")

print(kmeans_result)

# Display within-cluster variance for each cluster
for (i in 1:optimal_k) {
  cat("Cluster", i, "Variance:", kmeans_result$withinss[i], "\n")
}
cat("Total within-cluster variance:", kmeans_result$tot.withinss, "\n")

# Compute the variance ratio (BSS/TSS)
total_variance <- sum(apply(reduced_data, 2, function(col) sum((col - mean(col))^2)))
variance_ratio <- (between_cluster_variance / total_variance) * 100
cat("Variance ratio (BSS/TSS):", variance_ratio, "%\n")

#h
# Silhouette plot for visualizing cluster quality
#silhouette_plot <- fviz_silhouette(silhouette(kmeans_result$cluster, dist(clean_data)))
#print(silhouette_plot)

# Silhouette analysis on the reduced data after PCA
library(cluster) # Ensure the cluster library is loaded
silhouette_reduced <- silhouette(kmeans_result$cluster, dist(reduced_data))
average_silhouette_width_reduced <- mean(silhouette_reduced[, "sil_width"])
cat("Average silhouette width after PCA:", average_silhouette_width_reduced, "\n")

# Visualize the silhouette plot for the reduced data
library(factoextra)
silhouette_plot_reduced <- fviz_silhouette(silhouette_reduced)
print(silhouette_plot_reduced)



#i
# Calculate Calinski-Harabasz index to evaluate cluster stability
calinski_harabasz_index <- function(cluster_results, data) {
  cluster_count <- length(unique(cluster_results$cluster))
  data_points_count <- nrow(data)
  betweenSS <- cluster_results$betweenss
  withinSS <- cluster_results$tot.withinss
  ch_index <- ((data_points_count - cluster_count) / (cluster_count - 1)) * (betweenSS / withinSS)
  return(ch_index)
}

ch_index_value <- calinski_harabasz_index(kmeans_result, reduced_data)
cat("Calinski-Harabasz Index:", ch_index_value, "\n")

# Compute and plot the Calinski-Harabasz index for various cluster counts
ch_indices <- numeric(9)
set.seed(190)
for (k in 2:10) {
  kmeans_temp <- kmeans(reduced_data, k, nstart = 25)
  ch_indices[k - 1] <- calinski_harabasz_index(kmeans_temp, reduced_data)
}
plot(2:10, ch_indices, type = "b", xlab = "Number of Clusters", 
     ylab = "Calinski-Harabasz Index", main = "Calinski-Harabasz Index vs. Cluster Count")
