library(caTools)
library(class)

data(iris)


head(iris)

set.seed(2018)
iris.split <- caTools::sample.split(SplitRatio = 0.3,
                                    Y = iris$Species)

iris.train <- iris[!iris.split,1:4]
iris.test <- iris[iris.split,1:4]
iris.train.species <- as.character(iris[!iris.split,5])


iris.test$knn_Specie <- knn(train = iris.train[,1:4],
             test = iris.test[,1:4],
             cl = iris.train.species,
             k = 12)

table(iris[iris.split,5],iris.test$knn_Specie)
