#' @title Plot Method for S3 Class \code{myMeans}
#' @description plot() can be used to visualize the clustered data. Depending
#' on the dimensions of the input data, the user may be required to select
#' which columns are to be graphed.
#' @param x An object of class myMeans \code{myMeans}
#' @param ... Additional arguments
#' @return A two-dimensional plot is returned.
#' @import graphics
#' @export

plot.myMeans <- function(x, ...){

  # conversion for easier plotting
  data <- as.data.frame(x$data)
  #centers <- as.data.frame(x$centers)
  cluster <- factor(x$clusters)
  dataPlot <- data.frame(x$data, cluster = x$clusters)

  # graphing process for data with more than two columns
  if (ncol(x$data) > 2) {
    xInput <- readline(prompt =
                        "Enter column number of data to be modeled in x-Axis: ")
    yInput <- readline(prompt =
                        "Enter column number of data to be modeled in y-Axis: ")

    xAxis <- x$data[,as.integer(xInput)]    # select data for graphing of x
    yAxis <- x$data[,as.integer(yInput)]    # select data for graphing of y

    xCenters <- x$centers[,as.integer(xInput)]  # select centers of x
    yCenters <- x$centers[,as.integer(yInput)]  # select centers of y
    xyCenters <- cbind(xCenters, yCenters)

    par(mar = c(5,4,4,5))
    plot(xAxis, yAxis, col = cluster)
    points(xyCenters, col = levels(cluster), pch = 16, cex = 2)
    legend("right", legend = levels(cluster), pch = 16, col = levels(cluster),
           bty = "n",
           inset = c(-0.2,0),
           xpd = TRUE,
           title = "centers")
  }

  # graphing kmeans with 2-column data
  if (ncol(x$data) <= 2) {
  par(mar = c(5,4,4,5))
  plot(x$data, col = cluster, main = "k-means clustering")
  points(x$centers, col = levels(cluster), pch = 16, cex = 2)
  legend("right", legend = levels(cluster), pch = 16, col = levels(cluster),
         bty = "n",
         inset = c(-0.2,0),
         xpd = TRUE,
         title = "centers")
  }
}




