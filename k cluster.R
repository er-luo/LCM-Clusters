library(e1071)
library(cluster)
library(factoextra)
library(dplyr)
library(tidyverse)
library(gridExtra)

set.seed(10)

samplesize <- 1000
n_items <- 5
n_classes <- 4
n_datasets <- 100

##class membership probability vector
#pop_distribution_classes <- c(.2, .1, .4, .3)
pop_distribution_classes <- c(.4, .3, .2, .1)
print(cumsum(pop_distribution_classes))

population_response_probability_model <- rbind(rep(0.9, 5), 
                                               c(rep(0.1, 3), 
                                                 rep(0.9, 2)),
                                               c(rep(0.9, 3), 
                                                 rep(0.1, 2)), 
                                               rep(0.1,5))

population_response_pattern <- as.data.frame(population_response_probability_model)
population_response_pattern[population_response_pattern == 0.9] <- 1
population_response_pattern[population_response_pattern == 0.1] <- 2

#make a list to store sim matrices 
sim_data_list <- vector(mode="list", length=n_datasets)

sim_matrix <- matrix(NA,
                     ncol = 1+n_items,
                     nrow = samplesize)
col_names <- c("Class")

col_names <- append(col_names,paste0("Item_", 1:5))

colnames(sim_matrix) <- col_names
colnames(population_response_pattern) <- col_names[-1]

## Randomly Generates n_datasets according to the population distribution 
for (j in 1:n_datasets) {
  for (i in 1:samplesize) {
    tmp1 <- runif(n=1)
    ###   print(tmp1)
    class <- match(TRUE, tmp1 < cumsum(pop_distribution_classes))
    
    observation_data <- runif(n = 5)
    observation_data_coded <- c(0,5)
    
    for ( x in 1:length(observation_data)){
      if (observation_data[x] < population_response_probability_model[class, x]){
        observation_data_coded[x] = 1
      } else {
        observation_data_coded[x] = 2
      }
    }
    
    sim_matrix[i, ] <- c(class, observation_data_coded)
    ### print(simdata.mat)
  }
  ## Save the simulated data into a list of datasets
  sim_data_list[[j]] <- data.frame(sim_matrix)
}

#Aggregated matrix code
# Initialize an empty matrix
agg_conf_mat <- matrix(0, nrow = n_classes, ncol = n_classes)

#Accuracy matrix
#Initialize accuracy matrix 
accuracy_mat <- matrix(
  NA_real_,
  nrow = length(sim_data_list),
  ncol = n_classes
)

for (j in 1:n_datasets) {
  data <- sim_data_list[[j]]
  true_class <- data$Class         # save true labels
  cluster_data <- data[, -1]       # remove Class for kmeans
  
  kclust <- kmeans(cluster_data, centers = n_classes)
  cm <- table(true_class, kclust$cluster)
  
  # Get cluster sizes
  cluster_sizes <- table(kclust$cluster)
  
  # Order clusters from largest to smallest
  new_order <- order(cluster_sizes, decreasing = TRUE)
  
  # Create mapping: old cluster -> new cluster label
  cluster_map <- setNames(1:n_classes, new_order)
  
  # Relabel clusters
  kclust_reordered <- cluster_map[as.character(kclust$cluster)]
  
  # New confusion matrix
  conf_mat <- table(sim_data_list[[j]]$Class, kclust_reordered)
  conf_mat

  
  cm <- as.matrix(conf_mat)
  
  agg_conf_mat <- agg_conf_mat + cm
  
  #accuracy computations 
  correct <- diag(as.matrix(cm))
  true_totals <- table(sim_data_list[[j]][, 1])
  
  clustering_accuracy_score <- ifelse(
    true_totals == 0,
    0.00,
    correct / true_totals
  )
  
  accuracy_mat[j, ] <- clustering_accuracy_score
}


# Convert aggregated cf to data.frame for plotting
agg_conf_df <- as.data.frame(as.table(agg_conf_mat))
colnames(agg_conf_df) <- c("TrueClass", "KMeansCluster", "Count")

ggplot(agg_conf_df, aes(x=KMeansCluster, y=TrueClass, fill=Count)) +
  geom_tile(color="white") +
  geom_text(aes(label=Count), color="black") +
  scale_fill_gradient(low="white", high="green") +
  labs(title="Aggregated Confusion Matrix Across 100 Simulations") +
  theme_minimal()

# Convert accuracy mat to a dataframe for plotting 
accuracy_df <- as.data.frame(accuracy_mat)
colnames(accuracy_df) <- paste0("True_Class", 1:4)

test_pivot <- accuracy_df %>%
  pivot_longer(
    cols = starts_with("True_Class"),
    names_to = "Class",
    values_to = "Accuracy"
  )

kmeans_summary <- test_pivot %>%
  group_by(Class) %>%
  summarise(
    Mean = mean(Accuracy, na.rm = TRUE),
    Median = median(Accuracy, na.rm = TRUE),
    SD = sd(Accuracy, na.rm = TRUE),
    IQR = IQR(Accuracy, na.rm = TRUE),
    Min = min(Accuracy, na.rm = TRUE),
    Max = max(Accuracy, na.rm = TRUE)
  )

kmeans_summary

ggplot(test_pivot, aes(x = factor(Class), y = Accuracy, fill = Class)) +
  geom_boxplot() +
  labs(title = "K-means Accuracy Across Simulations",
       x = "True Class",
       y = "Accuracy") +
  theme_minimal()
