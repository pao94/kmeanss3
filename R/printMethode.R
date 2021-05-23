#' @title Print Method for S3 Class \code{myMeans}
#' @description Function to print k-means output less verbose
#' @param x An object of class \code{myMeans}
#' @param ... Additional arguments
#' @return A text consisting of different components is returned.
#' @import scales
#' @export

print.myMeans <- function(x, ...) {
  cat("\nK-means clustering with", nrow(x$centers), "clusters of sizes",
  table(x$clusters),"\n")
  cat("\nCluster means:\n")
  print(x$centers)
  cat("\nClustering vector:\n")
  print(x$clusters)
  cat("\n")
  cat("\nWithin cluster sum of squares by cluster:\n")
  print(x$withinss)
  cat("\n(between_SS / total_SS = ")
  cat(percent(x$betweenss/x$totss))
  cat(")\n")
  cat("\nAvailable components:\n")
  print(c("clusters", "centers", "totss", "withinss", "tot.withinss",
          "betweenss", "size", "iter", "data"))
}
