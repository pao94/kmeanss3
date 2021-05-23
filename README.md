# kmeanss3
k-means clustering package from scratch developed in R.

## Installation
Install package from Github with:
```
devtools::install_github("pao94/kmeanss3")
```
## Example
Begin by loading some data
```
data <- read.csv("iris.csv")
```
Initiate k-means with 4 clusters
```
kMeans <- myMeans(data[1:2], 4)
```
Plot results
```
plot(kMeans)
```
![Image](/man/figs/plot.PNG "k-means plot")
