#' K-Means Clustering
#' @description This package allows users to perform k-means clustering on a
#' data matrix using Lloyd's (1982) algorithm.
#' @param x x is a numeric data matrix, or an object that can be turned into a
#'  matrix, i.e. vector or dataframe. These may only contain numeric values.
#' @param k k is either a number or matrix. If k is a number, a random set of
#' rows in x is chosen as the initial centers. If k is a matrix a set of
#' initial cluster centres is specified by the user.
#' @param maxIteration The maximum number of allowed iterations before the
#' algorithm stops.
#' @param stopCrit Threshold for mean difference of centers that stops the
#' algorithm
#' @return The algorithm returns k clusters for the specified data x.
#' @examples
#' x <- cbind((sample(1:100, 20)), sample(1:100, 20))
#' # create four clusters for data x with random initial centers
#' kClustering <- myMeans(x, 4)
#'
#' # choose initial centers manually
#' kInitial <- matrix(c(20, 35, 40, 65), nrow = 2)
#' # create two clusters for x with pre-defined centers
#' kClustering <- myMeans(x, kInitial)
#' @export

myMeans <- function(x, k, maxIteration = 10, stopCrit = 0){

  # fundamental sanity check with control structures
  sanityCheck(x, k, maxIteration, stopCrit)

  # defining x as a matrix
  x <- as.matrix(x)

  # creating initial centers using control structures
  currentCenters <- initialCenters(x, k)

  # core of algorithm: iterative clustering and centers adjustment
  startingCriterion <- 2500 #  setting this high initially
  clusterMatrix <- as.matrix(rep(0, nrow(x))) #  create an empty cluster matrix
  isConverged <- FALSE #  set base value
  currentIteration <- 0 #  set starting iteration
  while (startingCriterion >= stopCrit &      #  checks for convergence and
         isConverged == FALSE &               #  iteration count
         currentIteration <= maxIteration) {  #  if reached, algorithm stops
    currentIteration <- currentIteration + 1
    if (startingCriterion <= stopCrit) {
      isConverged <- TRUE                     #  converges, when startC < stopC
    }
    if (currentIteration >= maxIteration) {   #  stops, if max iteration reached
      stop("did not converge in specified number of iterations")
    }
    centersBefore <- currentCenters
    clusters <- clusterAssignment(x,              #  clusters are assigned using
                                  clusterMatrix,  #  imported function
                                  currentCenters, #  based on distance measure
                                  centersBefore)
    for (i in 1:nrow(currentCenters)) {           #  centers are updated
      currentCenters[i, ] <- apply(x[clusters == i, , drop = F], 2, mean)
    }
    startingCriterion <- mean((centersBefore - currentCenters)^2)
  }


  # sum of squares measures
  totalSS <- totalSumOfSquares(x)
  withinSS <- withinSumOfSquares(x, currentCenters, clusters)
  totalWithinSS <- sum(withinSS)
  betweenSS <- totalSS - totalWithinSS

  # return object
  out <- list()
  out$centers <- currentCenters
  out$clusters <- as.vector(clusters)
  out$iter <- currentIteration
  out$withinss <- withinSS
  out$tot.withinss <- totalWithinSS
  out$totss <- totalSS
  out$betweenss <- betweenSS
  out$size <- as.data.frame(table(clusters))
  out$data <- x

  # defining s3 class
  class(out) <- "myMeans"
  out
}

#' Assignment of clusters based on distance
#' @rdname clusterAssignment
#' @description Points are assigned to the nearest cluster based on the squared
#' Euclidean distance.
#' @param x Numeric data matrix
#' @param currentCenters Current mean values of each cluster
#' @param clusterMatrix Matrix for clustering purposes
#' @param centersBefore Centers for current iteration-1
#' @return Returns a matrix with the assignment of n observations to k clusters.
#' @export
clusterAssignment <- function(x, clusterMatrix, currentCenters, centersBefore){
  for (i in 1:nrow(x)) {
    minDist <- 10e10
    for (center in 1:nrow(currentCenters)) {
      distanceToCenters <- sum((currentCenters[center, ] - x[i,])^2)
      if (distanceToCenters <= minDist) {
        clusterMatrix[i] <- center
        minDist <- distanceToCenters
      }
    }
  }
  clusterMatrix
}

#' Calculation of total sum of squares
#' @description This function calculates the total sum of squares.
#' @rdname totalSumOfSquares
#' @param x data matrix
#' @return returns the total sum of squares metric.
#' @export
totalSumOfSquares <- function(x){
  totssJ <- c()
  for (j in 1:ncol(x)) {
    totssVec <- c()
    for (i in 1:nrow(x)) {
      b <- sum(((x[i,j]) - mean(x[,j]))^2)
      totssVec <- c(totssVec, b)
    }
    totssJ <- c(totssJ, sum(totssVec))
  }
  totss <- sum(totssJ)
}

#' Calculation of within-cluster sum of squares
#' @rdname withinSumOfSquares
#' @description This function calculates the within-cluster sum of squares.
#' @param x Numeric data matrix
#' @param currentCenters Mean values of each center
#' @param clusters Cluster assignment of each data point
#' @return Returns the within-cluster sum of square metric.
#' @export
withinSumOfSquares <- function(x, currentCenters, clusters){
  outerVector <- c()
  for (c in 1:nrow(currentCenters)) {
    clusterAssignment <- which(clusters == c)
    innerVector <- c()
    for (i in 1:nrow(x)) {
      if (i %in% clusterAssignment) {
        distanceToCenter <- sum((x[i,] - currentCenters[c,])^2)
        innerVector <- c(innerVector, distanceToCenter)
      }
    }
    outerVector <- c(outerVector, sum(innerVector))
  }
  withinSumSquares <- outerVector
}

#' Creation of initial centers
#' @rdname initialCenters
#' @description Initial centers are created based on input or random selection.
#' @param x Numeric data matrix
#' @param k Either a number or matrix to define initial centers
#' @return Returns the initial k centers.
#' @export
initialCenters <- function(x, k){
  if (length(k) > 1) {
    currentCenters <- as.matrix(k)
  }
  else {
    currentCenters <- x[sample.int(nrow(x), k), , drop = FALSE]
  }
}

#' Checking for sanity
#' @rdname sanityCheck
#' @description The inputs are checked for validity and correctness.
#' @param x Numeric data matrix
#' @param k Either a number or matrix to define initial centers
#' @param maxIteration The maximum allowed number of iterations
#' @param stopCrit Stopping criterion of algorithm
#' @return Stops the execution of the myMeans function if inputs do not satisfy
#' the conditions.
#' @export
sanityCheck <- function(x, k, maxIteration, stopCrit){
  if (missing(x)) {
    stop("x must be specified")
  }
  if (any(is.na(x))) {
    stop("x contains NAs")
  }
  if (missing(k)) {
    stop("k must be a number or matrix")
  }
  if (!is.numeric(k)) {
    stop("k is not numeric")
  }
  if (!is.numeric(maxIteration)) {
    stop("maxIteration not numeric")
  }
  if (!is.numeric(stopCrit)) {
    stop("stopCrit is not numeric")
  }
}
