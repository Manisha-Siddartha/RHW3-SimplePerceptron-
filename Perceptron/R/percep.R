#' Finds the classification line or plane that seperates the given data set into two classes
#'
#'Take in any numeric data frame first column being the class label
#'@export

percep2D <- function(input){
  
  #calculates the mean of entire dataset
  sum=0
  v <- lapply(input[,2:ncol(input)],mean,na.rm = TRUE)
  for(i in 1:length(v)){
    sum <- sum + v[[i]]
  }
  threshold <- sum/length(v)
  
  label <- input[,1]
  
  # points <- as.matrix(input[,-1])
  pts <- cbind(label, x0 = rep(1, nrow(input)), input[,-1])
  pts <- as.matrix(pts)
  
  #starts with random weights and iterates untill it find the exact weight for the classifier.
  w <- c(-threshold, runif(ncol(pts) - 2))
  n <- nrow(pts)
  label <- pts[ , 1]
  obs <- pts[ , 2:ncol(pts)]
  
  misclassfied <- TRUE
  while (misclassfied) {
    misclassfied <- FALSE
    for (i in 1:n) {
      if ((label[i] * sign(obs[i , ] %*% w)) <= 0) {
        w <- w + label[i] * obs[i , ]
        misclassfied <- TRUE
      }
    }
  }
  
  #plots the data points.
  plot(pts[, 3:4], xlab = "X", ylab = "Y",
       pch = 15,
       col = ifelse(pts[, 1] == 1, "blue", "green"))
  
  #draws the partition.
  abline(-w[1]/w[3], -w[2]/ w[3])
  
}

percep3D <- function(input){
  #library for 3D plot.
  library(rgl)
  
  #calculates the mean of entire dataset
  sum=0
  v <- lapply(input[,2:ncol(input)],mean,na.rm = TRUE)
  for(i in 1:length(v)){
    sum <- sum + v[[i]]
  }
  threshold <- sum/length(v)
  
  label <- input[,1]
  
  # points <- as.matrix(input[,-1])
  pts <- cbind(label, x0 = rep(1, nrow(input)), input[,-1])
  pts <- as.matrix(pts)
  
  #starts with random weights and iterates untill it find the exact weight for the classifier.
  w <- c(-threshold, runif(ncol(pts) - 2))
  n <- nrow(pts)
  label <- pts[ , 1]
  obs <- pts[ , 2:ncol(pts)]
  
  misclassfied <- TRUE
  while (misclassfied) {
    misclassfied <- FALSE
    for (i in 1:n) {
      if ((label[i] * sign(obs[i , ] %*% w)) <= 0) {
        w <- w + label[i] * obs[i , ]
        misclassfied <- TRUE
      }
    }
  }
  
  #plots the data points.
  plot3d(pts[, 3:5], xlab = "X", ylab = "Y", zlab = "Z",
         pch = 15,
         col = ifelse(pts[, 1] == 1, "blue", "green"))
  
  #draws the partition.
  planes3d(w[4], w[3], w[2], w[1])
}
