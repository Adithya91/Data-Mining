#Influence of the starting points selection on the quality of several clustering algorithm
#(e.g..k-means,k-median).###############

##R-Script

##Before running the experiments in the R-script, we execute the functions 

####Functions

##Function to perform selection of initial points in random from the datapoints

init_pt_sel <- function(dataset, k, iter.max = 10,nstart = 10,...) 
{
  # number of data points
  n <- nrow(dataset) 
  # the ids of centers
  centers <- numeric(k) 
  # distance[i, j]: The distance between dataset[i,] and dataset[centers[j],]
  distance <- matrix(numeric(n * (k - 1)), ncol = k - 1) 
  
  Algo_2 <- list(tot.withinss = Inf) 
  # choose the better result among nstart iterations
  for (rep in 1:nstart) 
  {
    # probability for sampling centers
    pr <- rep(1, n) 
    for (i in 1:(k - 1)) 
    {
      centers[i] <- sample.int(n, 1, prob = pr) # Choose the ith center
      # Compute (the square of) distance to the center
      distance[, i] <- colSums((t(dataset) - dataset[centers[i], ])^2) 
      # Compute probaiblity for the next sampling
      pr <- distance[cbind(1:n, max.col(-distance[, 1:i, drop = FALSE]))] 
    }
    centers[k] <- sample.int(n, 1, prob = pr)
    ## Perform k-means with the obtained centers
    res <- kmeans(dataset, dataset[centers, ], iter.max = iter.max, nstart = 1, ...)
    res$inicial.centers <- dataset[centers, ]
    ## Store the best result
    if (res$tot.withinss < Algo_2$tot.withinss) {
      Algo_2 <- res
    }
  }
  Algo_2
}


#Function to remove outliers
top.n.custs <- function (data,cols,n=5){
  ids.to.remove <-integer(0) 
  for (c in cols){
    col.order <-order(data[,c],decreasing=T)
    ids <-head(col.order, n)
    ids.to.remove <-union(ids.to.remove,idxs)
  }
  return(ids.to.remove) 
}



#data set 
download.file('http://archive.ics.uci.edu/ml/machine-learning-databases/00292/Wholesale%20customers%20data.csv','Wholesale_customers_data.csv')
wcd <- read.csv("Wholesale_customers_data.csv",header = TRUE)
View(wcd)

#libraries for data visualization
library(cluster)
library(ggplot2)


summary(wcd)
#    Channel          Region          Fresh             Milk          Grocery     
# Min.   :1.000   Min.   :1.000   Min.   :     3   Min.   :   55   Min.   :    3  
# 1st Qu.:1.000   1st Qu.:2.000   1st Qu.:  3128   1st Qu.: 1533   1st Qu.: 2153  
# Median :1.000   Median :3.000   Median :  8504   Median : 3627   Median : 4756  
# Mean   :1.323   Mean   :2.543   Mean   : 12000   Mean   : 5796   Mean   : 7951  
# 3rd Qu.:2.000   3rd Qu.:3.000   3rd Qu.: 16934   3rd Qu.: 7190   3rd Qu.:10656  
# Max.   :2.000   Max.   :3.000   Max.   :112151   Max.   :73498   Max.   :92780  
#     Frozen        Detergents_Paper    Delicassen     
# Min.   :   25.0   Min.   :    3.0   Min.   :    3.0  
# 1st Qu.:  742.2   1st Qu.:  256.8   1st Qu.:  408.2  
# Median : 1526.0   Median :  816.5   Median :  965.5  
# Mean   : 3071.9   Mean   : 2881.5   Mean   : 1524.9  
# 3rd Qu.: 3554.2   3rd Qu.: 3922.0   3rd Qu.: 1820.2  
# Max.   :60869.0   Max.   :40827.0   Max.   :47943.0  



#Since the data contains outliers, the top customers from each category would be removed.
#e.g. Fresh increases from a minimum value of 3 to a maximum value of 112151.  
#From a business perspective, a clustering algorithm is not required to identify what the top customers are buying.  
#But need clustering and segmentation for the middle 50%.


#We shall remove top 5 customers
top.cust <-top.n.custs(wcd,cols=3:8,n=5)
length(top.cust) 
#Examining the customers
top.cust 
#Remove the Customers
Actual_data <- wcd[!wcd[,3:8] %in% top.cust,] 
#Set the seed for reproducibility
set.seed(1234)



## Comparison between the clustering algorithms


#(1)
##Performing k-means algorithm without selection of initial points
##Number of clusters = 2

Algo_1 = kmeans(Actual_data,2, iter.max = 10)
str(Algo_1)


##Calculating accuracy
Acc1 = Algo_1$betweenss/Algo_1$totss*100
Acc1


#2D representation of cluster
clusplot(Actual_data, Algo_1$cluster, main='2D representation of the Cluster solution',
         color=TRUE, shade=TRUE,
         labels=2, lines=0)

#Visualizing the cluster
clusters = as.factor(Algo_1$cluster)

ggplot(Actual_data,aes(x= Fresh,y = Detergents_Paper,color = clusters)) + geom_point() + ggtitle("Detergents_Paper vs Fresh")



##Performing k-means algorithm by selecting random initial points
##Number of clusters = 2

Algo_2 = init_pt_sel(Actual_data,2)

##Calculating accuracy
Acc2 = Algo_2$betweenss/Algo_2$totss*100
Acc2


#2D representation of cluster
clusplot(Actual_data, Algo_2$cluster, main='2D representation of the Cluster solution',
         color=TRUE, shade=TRUE,
         labels=2, lines=0)

#Visualizing the cluster
clusters = as.factor(Algo_2$cluster)

ggplot(Actual_data,aes(x= Fresh,y = Detergents_Paper,color = clusters)) + geom_point() + ggtitle("Detergents_Paper vs Fresh")


#(2)
##Performing k-means algorithm without selection of initial points
##Number of clusters = 5
Algo_1 = kmeans(Actual_data,5, iter.max = 10)
str(Algo_1)


##Calculating accuracy
Acc1 = Algo_1$betweenss/Algo_1$totss*100
Acc1


#2D representation of cluster
clusplot(Actual_data, Algo_1$cluster, main='2D representation of the Cluster solution',
         color=TRUE, shade=TRUE,
         labels=2, lines=0)

#Visualizing the cluster
clusters = as.factor(Algo_1$cluster)

ggplot(Actual_data,aes(x= Fresh,y = Detergents_Paper,color = clusters)) + geom_point() + ggtitle("Detergents_Paper vs Fresh")



##Performing k-means algorithm by selecting random initial points
##Number of clusters = 5

Algo_2 = init_pt_sel(Actual_data,5)

##Calculating accuracy
Acc2 = Algo_2$betweenss/Algo_2$totss*100
Acc2


#2D representation of cluster
clusplot(Actual_data, Algo_2$cluster, main='2D representation of the Cluster solution',
         color=TRUE, shade=TRUE,
         labels=2, lines=0)

#Visualizing the cluster
clusters = as.factor(Algo_2$cluster)

ggplot(Actual_data,aes(x= Fresh,y = Detergents_Paper,color = clusters)) + geom_point() + ggtitle("Detergents_Paper vs Fresh")

#(3)
##Performing k-means algorithm without selection of initial points
##Number of clusters = 20

Algo_1 = kmeans(Actual_data,20, iter.max = 10)
str(Algo_1)


##Calculating accuracy
Acc1 = Algo_1$betweenss/Algo_1$totss*100
Acc1


#2D representation of cluster
clusplot(Actual_data, Algo_1$cluster, main='2D representation of the Cluster solution',
         color=TRUE, shade=TRUE,
         labels=2, lines=0)

#Visualizing the cluster
clusters = as.factor(Algo_1$cluster)

ggplot(Actual_data,aes(x= Fresh,y = Detergents_Paper,color = clusters)) + geom_point() + ggtitle("Detergents_Paper vs Fresh")



##Performing k-means algorithm by selecting random initial points
##Number of clusters = 20

Algo_2 = init_pt_sel(Actual_data,20)

##Calculating accuracy
Acc2 = Algo_2$betweenss/Algo_2$totss*100
Acc2


#2D representation of cluster
clusplot(Actual_data, Algo_2$cluster, main='2D representation of the Cluster solution',
         color=TRUE, shade=TRUE,
         labels=2, lines=0)

#Visualizing the cluster
clusters = as.factor(Algo_2$cluster)

ggplot(Actual_data,aes(x= Fresh,y = Detergents_Paper,color = clusters)) + geom_point() + ggtitle("Detergents_Paper vs Fresh")







