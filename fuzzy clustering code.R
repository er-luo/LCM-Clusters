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

dupes <- 0
mega_fuzzy_results <- data.frame()
for (j in 1:n_datasets) {
  m = 1.5 #fuzziness coefficent
  result <- cmeans(sim_data_list[[j]][, -1], centers = 4, m=m)
  
  fuzzy_membership_matrix <- result$membership
  fuzzy_centers <- result$centers
  
  cluster_membership <- as.data.frame(result$membership)
  true_memberships <- sim_data_list[[j]][, 1]
  
  colnames(cluster_membership) <- paste0("Fuzzy Cluster ", 1:4)
  
  #reorder by centers, compare centers from fuzzy with population response probability model
  
  cluster_label_reordered <- data.frame(matrix(ncol = 4, nrow = 1))
  colnames(cluster_label_reordered) <- paste0("Fuzzy Cluster ", 1:4)
  
  for (i in 1:n_classes) {
    cluster_label_reordered[i] <- which.min(abs(apply(population_response_pattern-fuzzy_centers[i,], 1, sum)))
  }
  
  cluster_membership_reordered <- cluster_membership
  
  for (i in 1:n_classes) {
    colnames(cluster_membership_reordered)[i] <- paste0("Fuzzy Cluster ", cluster_label_reordered[i])
  }
  
  
  #cbind true memberships
  cluster_true_comparison_reordered <- as_tibble(cluster_membership_reordered)
  cluster_true_comparison_reordered$True_Class <- factor(true_memberships)
  

  if (any(duplicated(cluster_label_reordered)) == TRUE) {
    dupes <- dupes + 1
  }
  
  mega_fuzzy_results <- bind_rows(mega_fuzzy_results, cluster_true_comparison_reordered)
}

mega_fuzzy_long <- mega_fuzzy_results %>%
  pivot_longer(
    cols = starts_with("Fuzzy Cluster "),
    names_to = "Assigned Cluster",
    values_to = "Membership Probability"
  )

mega_fuzzy_long$`Assigned Cluster` <- factor(mega_fuzzy_long$`Assigned Cluster`,
                                      levels = paste0("Fuzzy Cluster ", 1:n_classes))

summary_stats <- mega_fuzzy_long %>%
  group_by(True_Class, `Assigned Cluster`) %>%
  summarise(
    Mean = mean(`Membership Probability`, na.rm = TRUE),
    Median = median(`Membership Probability`, na.rm = TRUE),
    SD = sd(`Membership Probability`, na.rm = TRUE),
    IQR = IQR(`Membership Probability`, na.rm = TRUE),
    Min = min(`Membership Probability`, na.rm = TRUE),
    Max = max(`Membership Probability`, na.rm = TRUE),
    .groups = "drop"
  )

summary_stats


fuzzy_plots <- ggplot(mega_fuzzy_long,
       aes(x = `Assigned Cluster`,
           y = `Membership Probability`,
           fill = True_Class)) +
  geom_boxplot(outlier.alpha = 0.2) +
  facet_wrap(~ True_Class, nrow=2) +
  labs(
    title = "Fuzzy Cluster Membership Probabilities by True Latent Class",
    x = "Fuzzy Cluster",
    y = "Membership Probability",
    fill = "True Latent Class"
  ) +
  theme_minimal()

fuzzy_plots

# non reordered result 
cluster_true_comparison <- as_tibble(cluster_membership)
cluster_true_comparison$True_Class <- factor(true_memberships)

cluster_long <- cluster_true_comparison %>%
  pivot_longer(
    cols = starts_with("Fuzzy_Class_"),
    names_to = "Assigned_Class",
    values_to = "Membership_Probability"
  )

cluster_long$Assigned_Class <- factor(cluster_long$Assigned_Class,
                                                levels = paste0("Fuzzy_Class_", 1:n_classes))

euc_dist <- ggplot(cluster_long,
                             aes(x = Assigned_Class,
                                 y = Membership_Probability,
                                 fill = True_Class)) +
  geom_boxplot(outlier.alpha = 0.2) +
  facet_wrap(~ True_Class, nrow = 2) +
  labs(
    title = "Fuzzy Cluster Membership Probabilities by True Latent Class",
    x = "Fuzzy Cluster",
    y = "Membership Probability"
  ) +
  theme_minimal()

euc_dist

