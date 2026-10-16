library(dplyr)
library(tidyverse)
library(gridExtra)

set.seed(10)

samplesize <- 1000
nitems <- 5
n_datasets <- 100

##class membership probability vector
pop_distribution_classes <- c(.4, .3, .2, .1)
print(cumsum(pop_distribution_classes))

population_response_probability_model <- rbind(rep(0.9, 5), 
                                               c(rep(0.1, 3), 
                                                 rep(0.9, 2)),
                                               c(rep(0.9, 3), 
                                                 rep(0.1, 2)), 
                                               rep(0.1,5))

#make a list to store sim matrixes 
sim_data_list <- vector(mode="list", length=n_datasets)

sim_matrix <- matrix(NA,
                     ncol = 1+nitems,
                     nrow = samplesize)
col_names <- c("Class")

col_names <- append(col_names,paste0("Item_", 1:5))

colnames(sim_matrix) <- col_names

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


hierarchical_clustering <- function(sim_datasets, 
                                    distance_measures, 
                                    linkage_methods, 
                                    n_classes = 4, 
                                    n_datasets) {
  results <- list()
  
  for (distance in distance_measures){
    for (linkage in linkage_methods) {
      combo_name <- paste(distance, linkage, sep = "_")
      
      accuracy_mat <- matrix(
        NA_real_,
        nrow = length(sim_datasets),
        ncol = n_classes
      )
      
      colnames(accuracy_mat) <- paste0("Class_", seq_len(n_classes))
      
      for (i in 1:n_datasets) {
        
        dist_matrix <- dist(sim_datasets[[i]][, -1], method = distance)
        hc <- hclust(dist_matrix, method = linkage)
        memberships <- cutree(hc, n_classes)
        
        accuracy_df <- table(sim_datasets[[i]][, 1], memberships)
        
        max_df <- apply(accuracy_df, 2, max)
        rankings <- names(sort(max_df, decreasing = TRUE))
        
        reordered_accuracy_df <- accuracy_df[, rankings[seq_len(n_classes)], drop = FALSE]
        
        correct <- diag(as.matrix(reordered_accuracy_df))
        true_totals <- table(sim_datasets[[i]][, 1])
        
        clustering_accuracy_score <- ifelse(
          true_totals == 0,
          0.00,
          correct / true_totals
        )
        
        accuracy_mat[i, ] <- clustering_accuracy_score
      }
      
      results[[combo_name]] <- as.data.frame(accuracy_mat)
      }
    }
  return(results)
}


distance_measures <- c("euclidean", "manhattan")
linkage_methods <- c("average", "complete", "single", "ward.D")

hierchical_results <- hierarchical_clustering (
  sim_datasets = sim_data_list,
  distance_measures = distance_measures,
  linkage_methods = linkage_methods,
  n_datasets = n_datasets
)

all_results <- bind_rows(
  lapply(names(hierchical_results), function(name) {
    df <- hierchical_results[[name]]
    df$Method <- name
    df$Dataset <- seq_len(nrow(df))
    return(df)
  })
)

all_long <- all_results %>%
  pivot_longer(
    cols = starts_with("Class_"),
    names_to = "Class",
    values_to = "Accuracy"
  )

all_plots <- lapply(unique(all_long$Method), function(m) {
  df <- subset(all_long, Method == m)
  
  ggplot(df, aes(x = Class, y = Accuracy, fill = Class)) +
    geom_boxplot(na.rm = TRUE) +
    labs(title = m) +
    theme_minimal() +
    scale_y_continuous(limits = c(0, 1))
})

# Save all plots into one multi-page PDF
ggsave("hierarchical_accuracy_all.pdf",
       marrangeGrob(all_plots, nrow = 1, ncol = 1),
       width = 8, height = 6)

summary_stats <- all_long |> 
  group_by(Method, Class) |>
  summarise(
    mean  = mean(Accuracy, na.rm = TRUE),
    median  = median(Accuracy, na.rm = TRUE),
    sd    = sd(Accuracy, na.rm = TRUE),
    min   = min(Accuracy, na.rm = TRUE),
    max   = max(Accuracy, na.rm = TRUE),
    .groups = "drop"
  )
