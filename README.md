# sobel
---
title: "Untitled"
author: "Jiyi Jiang"
date: "June 25, 2015"
output: html_document
---


# Load MNIST data

```{r}
setwd("C:/Users/sumloaner/Downloads")

load_mnist <- function() {
  load_image_file <- function(filename) {
    ret = list()
    f = file(filename,'rb')
    readBin(f,'integer',n=1,size=4,endian='big')
    ret$n = readBin(f,'integer',n=1,size=4,endian='big')
    nrow = readBin(f,'integer',n=1,size=4,endian='big')
    ncol = readBin(f,'integer',n=1,size=4,endian='big')
    x = readBin(f,'integer',n=ret$n*nrow*ncol,size=1,signed=F)
    ret$x = matrix(x, ncol=nrow*ncol, byrow=T)
    close(f)
    ret
  }
  load_label_file <- function(filename) {
    f = file(filename,'rb')
    readBin(f,'integer',n=1,size=4,endian='big')
    n = readBin(f,'integer',n=1,size=4,endian='big')
    y = readBin(f,'integer',n=n,size=1,signed=F)
    close(f)
    y
  }
  train <<- load_image_file('train-images-idx3-ubyte')
  test <<- load_image_file('t10k-images-idx3-ubyte')
  
  train$y <<- load_label_file('train-labels-idx1-ubyte')
  test$y <<- load_label_file('t10k-labels-idx1-ubyte')  
}


show_digit <- function(arr784, col=gray(12:1/12), ...) {
  image(matrix(arr784, nrow=28)[,28:1], col=col, ...)
}

load_mnist()
train$n


# create an index vector
#ind <- sample(nrow(X),size=2000,replace=FALSE)
#labels <- train$y[ind]
```


Run Sobel Operator on MNIST
```{r}
R <- train$x[2,]
  M <- matrix(R, nrow=28)[,28:1]
   MFeature <- matrix(R, nrow=28)[,28:1]
  F <- matrix(c(0),3,3)
    G_x <- c(-1,0,-1,-2,0,2,-1,0,1)
      G_y <- c(1,2,1,0,0,0,-1,-2,-1)
        k <- 2
for (i in 2:27){
  for (j in 2:27){
    for (r in (i-1):(i+1)){
      F[r-(i-2),] <- c(M[r, (j-1):(j+1)])
      v <-c(F[k-1,],F[k,],F[k+1,])
      g_x <- G_x %*% v
      #g_y <- G_y %*% v
      MFeature[i,j] <- sqrt((g_x)^2+(g_y)^2)
    }  
  }
}
```
# Run Rtsne

```{r}
set.seed(620251)
ind <- sample(nrow(train$x),size=6000,replace=FALSE)
labels <- train$y[ind]
Rtsne_input=train$x[ind,]# number of random rows of 60000.
library(Rtsne)
Rtsne_result=Rtsne(Rtsne_input, dims = 3, initial_dims = 20, perplexity = 25,
        theta = 0.1, check_duplicates = TRUE, pca = TRUE, max_iter = 1000,
        verbose = FALSE, is_distance = FALSE)


pc1 <- matrix(Rtsne_result$Y[,1])
pc2 <- matrix(Rtsne_result$Y[,2])
pc3 <- matrix(Rtsne_result$Y[,3])
cc1=gsub("1","gold",labels)
cc2=gsub("0","darkgreen",cc1)
colorlabels <- cc2

#plot3d(pc1, pc2, pc3)
library(plot3D)
library(rgl)
plot3d(pc1,pc2,pc3, col=cc2)
```


# Load mapper2D

```{r}
cluster_cutoff_at_first_empty_bin <- function(heights, diam, num_bins_when_clustering) {
  
  # if there are only two points (one height value), then we have a single cluster
  if (length(heights) == 1) {
    if (heights == diam) {
      cutoff <- Inf
      return(cutoff)
    }
  }
  
  bin_breaks <- seq(from=min(heights), to=diam, 
                    by=(diam - min(heights))/num_bins_when_clustering)
  myhist <- hist(c(heights,diam), breaks=bin_breaks, plot=FALSE)
  z <- (myhist$counts == 0)
  if (sum(z) == 0) {
    cutoff <- Inf
    return(cutoff)
  } else {
    #  which returns the indices of the logical vector (z == TRUE), min gives the smallest index
    cutoff <- myhist$mids[ min(which(z == TRUE)) ]
    return(cutoff)
  }
  
}

mapper2D <- function(
  distance_matrix = dist(data.frame( x=2*cos(1:100), y=sin(1:100) )),
  filter_values = list( 2*cos(1:100), sin(1:100) ),
  num_intervals = c(5,5),
  percent_overlap = 50,
  num_bins_when_clustering = 10
  ) {

  # initialize variables
  vertex_index <- 0

  # indexed from 1 to the number of vertices
  level_of_vertex <- c()
  points_in_vertex <- list()
  # filter_values_in_vertex <- list() # just use filter_values[points_in_vertex[[i]]]

  # indexed from 1 to the number of levels
  points_in_level <- list()
  vertices_in_level <- list()
  # filter_values_in_level <- list() # just use filter_values[points_in_level[[i]]]

  filter_min_1 <- min(filter_values[[1]])
  filter_max_1 <- max(filter_values[[1]])
  filter_min_2 <- min(filter_values[[2]])
  filter_max_2 <- max(filter_values[[2]])

  interval_length_1 <- (filter_max_1 - filter_min_1) / (num_intervals[1] - (num_intervals[1] - 1) * percent_overlap/100 )
  interval_length_2 <- (filter_max_2 - filter_min_2) / (num_intervals[2] - (num_intervals[2] - 1) * percent_overlap/100 )

  step_size_1 <- interval_length_1 * (1 - percent_overlap/100)
  step_size_2 <- interval_length_2 * (1 - percent_overlap/100)

  num_levels <- num_intervals[1] * num_intervals[2]

  # level_index_matrix <- matrix(1:num_levels, num_intervals[1], num_intervals[2])
  #
  # Given any sequential index i in 1:num_levels,
  # you can get the ordered pair index (row_index,col_index) by
  # row_index <- which(level_index_matrix == i, arr.ind=TRUE)[1]
  # col_index <- which(level_index_matrix == i, arr.ind=TRUE)[2]
  #
  # Given any ordered pair index (row_index,col_index),
  # you can get the sequential index i in 1:num_levels by
  # i <- level_index_matrix[row_index,col_index]

  # Given any sequential index i in 1:num_levels,
  # "row_index" = level_indices_1[i]
  # "col_index" = level_indices_2[i]
  level_indices_1 <- rep(1:num_intervals[1], num_intervals[2])
  level_indices_2 <- rep(1:num_intervals[2], each=num_intervals[1])

  # begin mapper main loop
  for (level in 1:num_levels) {

    level_1 <- level_indices_1[level]
    level_2 <- level_indices_2[level]

    min_value_in_level_1 <- filter_min_1 + (level_1 - 1) * step_size_1
    min_value_in_level_2 <- filter_min_2 + (level_2 - 1) * step_size_2
    max_value_in_level_1 <- min_value_in_level_1 + interval_length_1
    max_value_in_level_2 <- min_value_in_level_2 + interval_length_2

    points_in_level_logical <-
      (min_value_in_level_1 <= filter_values[[1]]) &
      (min_value_in_level_2 <= filter_values[[2]]) &
      (filter_values[[1]] <= max_value_in_level_1) &
      (filter_values[[2]] <= max_value_in_level_2)

    num_points_in_level <- sum(points_in_level_logical)
    points_in_level[[level]] <- which(points_in_level_logical==TRUE)

    if (num_points_in_level == 0) {
      print('Level set is empty')
      next
    }

    if (num_points_in_level == 1) {
      print('Level set has only one point')
      num_vertices_in_level <- 1
      cluster_indices_within_level <- c(1)
    }

    if (num_points_in_level > 1) {
      # use as.matrix() to put the distance matrix in square form,
      # and as.dist() to put it in vector form
      # This could probably use some optimization...
      level_distance_matrix <- as.dist(as.matrix(distance_matrix)[points_in_level_logical,points_in_level_logical])
      level_max_distance <- max(level_distance_matrix)
      # use R's hclust (provided by stats or fastcluster)
      # in place of Matlab's linkage function.
      level_hcluster_ouput <- hclust(level_distance_matrix,method="single")
      heights <- level_hcluster_ouput$height
      cutoff <- cluster_cutoff_at_first_empty_bin(heights, level_max_distance, num_bins_when_clustering)

      # use as.vector() to get rid fo the names for the vector entries
      cluster_indices_within_level <- as.vector( cutree(level_hcluster_ouput, h=cutoff) )
      num_vertices_in_level <- max( cluster_indices_within_level )

      # points_in_level[[level]] and cluster_indices_within_level have the same length.
      # heights has length 1 less than points_in_level[[level]] and cluster_indices_within_level
      # print(heights)
      # print(points_in_level[[level]])
      # print(cluster_indices_within_level)
    }

    vertices_in_level[[level]] <- vertex_index + (1:num_vertices_in_level)

    for (j in 1:num_vertices_in_level) {

      vertex_index <- vertex_index + 1

      # points_in_level_logical is a l ogical vector, so use which(points_in_level_logical==TRUE) to convert it to a numerical vector of indices
      #nodeset <- which(points_in_level_logical==TRUE)[cluster_indices_within_level == j]

      level_of_vertex[vertex_index] <- level
      points_in_vertex[[vertex_index]] <- which(points_in_level_logical==TRUE)[cluster_indices_within_level == j]
      #points_in_vertex[[vertex_index]] <- nodeset
      #filter_values_in_vertex[[vertex_index]] <- filter_values[nodeset]

    }

  } # end mapper main loop


  # Note: num_vertices = vertex index.
  # Create the adjacency matrix for the graph, starting with a matrix of zeros
  adja <- mat.or.vec( vertex_index, vertex_index )

  for (i in 1:num_intervals[1]) {
    for (j in 2:num_intervals[2]) {

      # For adjacent level sets L_{i,j} and L_{i,j-1}, get the sequential index values k1 and k2
      k1 <- which( (level_indices_1 == i) & (level_indices_2 == j) )
      k2 <- which( (level_indices_1 == i) & (level_indices_2 == j-1))

      # check that both level sets are nonemtpy
      if ( (length(vertices_in_level[[k1]]) > 0) & (length(vertices_in_level[[k2]]) > 0) ) {
        print(k1)
        for (v1 in vertices_in_level[[k1]]) {
          for (v2 in vertices_in_level[[k2]]) {
            # return 1 if the intersection is nonempty
            adja[v1,v2] <- ( length(intersect(points_in_vertex[[v1]],
                                              points_in_vertex[[v2]])) > 0 )
            adja[v2,v1] <- adja[v1,v2]
          }
        }

      }
    } # end part 1 of constructing adjacency matrix
  }
  for (j in 1:num_intervals[2]) {
    for (i in 2:num_intervals[1]) {

      # For adjacent level sets L_{i,j} and L_{i-1,j}, get the sequential index values k1 and k2
      k1 <- which( (level_indices_1 == i) & (level_indices_2 == j) )
      print(k1)
      k2 <- which( (level_indices_1 == i-1) & (level_indices_2 == j))

      # check that both level sets are nonemtpy
      if ( (length(vertices_in_level[[k1]]) > 0) & (length(vertices_in_level[[k2]]) > 0) ) {

        for (v1 in vertices_in_level[[k1]]) {
          for (v2 in vertices_in_level[[k2]]) {
            # return 1 if the intersection is nonempty
            adja[v1,v2] <- ( length(intersect(points_in_vertex[[v1]],
                                              points_in_vertex[[v2]])) > 0 )
            adja[v2,v1] <- adja[v1,v2]
          }
        }

      }
    } # end part 2 of constructing adjacency matrix
  }

  mapperoutput <- list(adjacency = adja,
                       num_vertices = vertex_index,
                       level_of_vertex = level_of_vertex,
                       points_in_vertex = points_in_vertex,
                       #filter_values_in_vertex = filter_values_in_vertex,
                       points_in_level = points_in_level,
                       vertices_in_level = vertices_in_level
  )

  class(mapperoutput) <- "TDAmapper"

  return(mapperoutput)

} # end mapper2D function

```


# Run mapper2D

```{r}
#library(TDAmapper)
m2 <- mapper2D(
    distance_matrix = dist(train$x[ind,]),
    filter_values = list(Rtsne_result$Y[,1],Rtsne_result$Y[,3]),
    num_intervals = c(5,5),
    percent_overlap = 10,
    num_bins_when_clustering = 5)
library(igraph)
g2 <- graph.adjacency(m2$adjacency, mode="undirected")
plot(g2, layout = layout.auto(g2) )
```
