##unsupervised learning: k-means clustering

#load required libraries
library(dplyr)
library(ggplot2)


iris_numerics <- select(iris, -Species) %>%
  scale()
#we only want numerics to find means

iris_clusters <- kmeans(iris_numerics, centers = 3)
iris_clusters #our results

iris_clusters$cluster #vector designating a cluster for each row
iris$cluster <- iris_clusters$cluster #add cluster column to original dataset
head(iris)


ggplot(iris, aes(x = Sepal.Length, y = Sepal.Width)) +
  geom_point(aes(color = as.factor(cluster)))

ggplot(iris, aes(x = Sepal.Length, y = Sepal.Width)) +
  geom_point(aes(color = Species))


cor(iris$Petal.Length, iris$Petal.Length)
cor(iris$Petal.Length, iris$Sepal.Width)
cor(iris$Petal.Length, iris$Petal.Width)

#choose features
#Petal.Width, Sepal.Width, Sepal.Length

#split into training, test, and validation sets
greetings <- c(rep("hello", 5), rep("goodbye", 3)) %>%
  sample(8, replace = F)
#replace makes sure none are used again, if T then they can be 
# used more than 5 or 3 times
greetings

iris_len <- nrow(iris)

iris$label <- c(rep("training", ceiling(.6*iris_len)), 
                rep("test", ceiling(.2*iris_len)),
                rep("validation", ceiling(.2*iris_len))) %>%
  sample(iris_len, replace = F)

head(iris)


#choosing a model!
#when we use a model, we "train" it using the training set and 
# "test" it using the testing set
iris_train <- filter(iris, label == "training")
iris_test <- filter(iris, label == "test")
iris_valid <- filter(iris, label == "validation")

#linear model

iris_lm <- lm(Petal.Length ~ Petal.Width + Sepal.Length + Sepal.Width, data = iris_train)
iris_lm

#select out only the x values we use (petal width and sepal length)
iris_lm_predictions <- select(iris_test, Petal.Width, Sepal.Length, Sepal.Width) %>%
  predict(object = iris_lm) #object is the model that we just created

iris_train$lm_pred <- iris_lm_predictions
head(iris_train)
