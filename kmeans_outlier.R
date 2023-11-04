############## remove outliers by kmeans ##################
# remove species from the data to cluster
 iris2 <- iris[,1:4]
 kmeans.result <- kmeans(iris2, centers=3)
 # cluster centers
 kmeans.result$centers
kmeans.result$cluster
# calculate distances between objects and cluster centers
 centers <- kmeans.result$centers[kmeans.result$cluster, ]
 distances <- sqrt(rowSums((iris2 - centers)^2))
 # pick top 5 largest distances
 outliers <- order(distances, decreasing=T)[1:5]
 # who are outliers
 print(outliers)
iris2<-iris[-outliers,]
dim(iris2)
row.names(iris2)<-1:nrow(iris2)
iris2

############## NB  Á¤È®µµ ################################
library(e1071)
#iris2=iris
N <- nrow(iris2)
set.seed(1)
train.index <- sample(N, size = N*0.75)
features <- c(1: 4)
train.x <- iris2[train.index, features,drop=FALSE]; train.y <- iris2[train.index, 5]
test.x <- iris2[-train.index, features, drop=F]; test.y <- iris2[-train.index, 5]

model <- naiveBayes(train.x, train.y)
pred <- predict(model, test.x)

table(pred, test.y)
sum(pred == test.y) / length(pred)

