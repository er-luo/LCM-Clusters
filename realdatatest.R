library(dplyr)
library(tidyverse)
library(gridExtra)
library(poLCA)
library(e1071)
library(cluster)

set.seed(10)

#ciggt10 & f1t1 & ever smoked $>10$ cigarettes in a day (1 = yes; 5 = no) \\
#sedbarb & f2t1 & ever sedatives or barbituates \\
#ampstim & f3t1 & ever tried amphetamines or stimulants \\
#analges & f4t1 & ever tried analgesics \\
#tranq & f5t1 & ever tried tranquilizers \\
#inhal & f6t1 & ever tried inhalants \\
#marhash & f7t1 & ever tried marijuana or hash \\
#powdcoc & f8t1 & ever tried powder cocaine \\
#crack & f9t1 & ever tried crack cocaine \\
#hallucin & f10t1 & ever tried hallucinogens \\
#heroin & f11t1 & ever tried heroin \\
#alc & f17t1 & ever more than a sip of alcohol \\ \hline

#outfile ciggt10 alc marhash sedbarb ampstim analges tranq inhal ///
#  powdcoc crack hallucin heroin using "subuse.txt", nolabel replace wide

subuse <- read.table("subuse.txt", na.strings=".")

#this recodes the dataframe so PoLCA can work with it. 0s are converted to 1s and 1s are converted to 2s.
#subuse <- subuse + 1
#this adds column names to text files so PoLCA can work with it. 
colnames(subuse) <- c("ciggt10", "alc", "marhash", "sedbarb", "ampstim", "analges",
                      "tranq", "inhal", "powdcoc", "crack", "hallucin", "heroin")
n_classes = 5

polca.form <- cbind(ciggt10, alc, marhash, sedbarb, ampstim, analges,
                    tranq, inhal, powdcoc, crack, hallucin, heroin) ~ 1

#lc <- poLCA(formula = polca.form, data=subuse, nclass=n_classes, nrep=10)

source("M5.dump")

probs <- lapply(M5$probs, as.data.frame)
list2env(probs, envir = .GlobalEnv)

#best performing hierarchical cluster: manhattan, wards 
hier_manhattan_wards <- hclust(dist(subuse, method = "manhattan"), method="ward.D")
hier_manhattan_wards_results <- cutree(hier_manhattan_wards, n_classes)

subuse["hier_class"] <- hier_manhattan_wards_results

cent <- NULL
for(k in 1:n_classes){
  cent <- rbind(cent, colMeans(subuse[hier_manhattan_wards_results == k, , drop = FALSE]))
}

#hierarchical class count
subuse |>
  ggplot(aes(x=hier_class)) + 
  geom_bar() +
  geom_text(stat = 'count', aes(label = after_stat(count)), vjust = -0.5)


test <- as.data.frame(table(M5$predclass))
#latent class visualization 
plot <- ggplot(as.data.frame(table(M5$predclass)), aes(x=Var1, y=Freq)) + 
  geom_col() +
  labs(x = "Latent Class", y = "Frequency") +
  geom_text(aes(label = Freq), vjust = -0.5)

plot 
M5$probs

table(M5$predclass,hier_manhattan_wards_results)

#fuzzy clustering:
m = 1.5 #fuzziness coefficent
fuzzy_result <- cmeans(subuse, centers = n_classes, m=m)
fuzzy_membership_matrix <- fuzzy_result$membership

M5$posterior

fuzzy_result$centers

tableprobs.2 <- function(mat1, mat2) {
  
  ## mat1 and mat2 expected to be matrices of class/cluster
  ##  membership probabilities.
  
  ## I think matrices have to be the same length.
  ##  may not need to be the same width. Shouldn't need to be.
  
  n1 <- nrow(mat1)
  n2 <- nrow(mat2)
  
  if(n1 != n2) stop("Matrices should be the same number of rows")
  
  restable <- matrix(0, nrow=ncol(mat1), ncol=ncol(mat2))
  for(a in 1:n1) {
    for(i in 1:ncol(mat1)) {
      for(j in 1:ncol(mat2)) {
        restable[i,j] <- restable[i,j] + (mat1[a,i] * mat2[a,j])
      }
    }
  }
  restable
}

#combined table probabilities
prob_result <- tableprobs.2(M5$posterior, fuzzy_result$membership)

#row conditional probability table
#row is class, column is fuzzy cluster 
t(apply(prob_result, 1, function(x){x/sum(x)}))

