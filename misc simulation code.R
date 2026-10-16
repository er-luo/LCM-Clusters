hierarchical_{{distance_measures}}_{{linkage_methods}}_sim_accuracy_df <- data.frame(
  "Class_1" = numeric(),
  "Class_2" = numeric(),
  "Class_3" = numeric(),
  "Class_4" = numeric()
)

#makes the model and gets the accuracy
for (i in 1:n_datasets) {
  dist_matrix <- dist(sim_data_list[[i]][,-1], method='manhattan')
  hclust_avg <- hclust(dist_matrix, method="average")
  h_memberships_manhattan <- cutree(hclust_avg, 4)
  
  accuracy_df <- data.frame(rbind(table(sim_data_list[[i]][,1], h_memberships_manhattan)))
  max_df <- sapply(accuracy_df, max)
  rankings <- names(sort(max_df, decreasing=TRUE))
  reordered <- c(
    rankings[1],  # new 1st column = 3rd largest max
    rankings[2],  # new 2nd column = smallest max
    rankings[3],  # new 3rd column = largest max
    rankings[4]   # new 4th column = second largest max
  )
  reordered_accuracy_df <- accuracy_df[, reordered]
  correct_clustering_class_memberships <- diag(as.matrix(reordered_accuracy_df))
  
  true_sim_class_totals <- table(sim_data_list[[i]][,1])
  
  clustering_accuracy_score <- correct_clustering_class_memberships/true_sim_class_totals
  
  hierarchical_manhattan_average_sim_accuracy_df[i,] <- clustering_accuracy_score
}
}