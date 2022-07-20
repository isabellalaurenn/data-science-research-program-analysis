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


cor(iris$Petal.Length, iris$Petal.Length) #1
cor(iris$Petal.Length, iris$Sepal.Width)  #-.4284401
cor(iris$Petal.Length, iris$Petal.Width)  #.9628654

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

iris_lm <- lm(Petal.Length ~ Petal.Length + Sepal.Length + Sepal.Width, data = iris_train)
#smallest p value first
iris_lm
summary(iris_lm) #if we need to rearrange stuff in the pluses
#petal width is like x1, sepal length is like x2, sepal width is like x3

#select out only the x values we use (petal width and sepal length)
iris_lm_predictions <- select(iris_test, Petal.Width, Sepal.Length, Sepal.Width) %>%
  predict(object = iris_lm, type = "response") #object is the model that we just created

iris_test$lm_pred <- iris_lm_predictions
head(iris_test)


#logistic model

#create our two categories
#mean(iris$Petal.Length) = 3.758
iris_train_glm <- iris_train %>%
  mutate(petal_length_cat = as.factor(ifelse(Petal.Length < 3.758, "short", "long")))
head(iris_train_glm)

iris_glm <- glm(petal_length_cat ~ Sepal.Length + Sepal.Width + Species, 
                data = iris_train_glm,
                family = binomial(link = "logit"))
#warning is okay, errors are not lol

iris_glm_preds <- iris_test %>%
  select(Sepal.Length, Sepal.Width,  Species) %>%
  predict(object = iris_glm) #object means which model are we using, and only
# give the x models that we need


iris_test$glm_pred <- iris_glm_preds

iris_test <- iris_test %>%
  mutate(petal_length_cat = as.factor(ifelse(Petal.Length < 3.758, "short", "long")))
head(iris_test)

#filter down to 2 categories
iris_train_2species <- filter(iris_train, Species %in% c("setosa", "virginica"))

#create the model
iris_glm <- glm(Species ~ Sepal.Width + Sepal.Length + Petal.Width + Petal.Length, 
                data = iris_train_2species, 
                family = binomial(link = "logit"))

#create test set with only 2 species
iris_test_2species <- iris_test %>% 
  filter(Species %in% c("setosa", "virginica"))
#make predictions based on model
iris_2species_preds <- iris_test_2species %>%
  select(-Species) %>%
  predict(object = iris_glm)
#add predictions to test set
iris_test_2species$glm_2spec_pred <- iris_2species_preds


#generalized boosted (making own corrections) regression modeling
install.packages("gbm")
library(gbm)

#create model
iris_gbm <- gbm(Petal.Length ~ Petal.Width + Sepal.Length + Sepal.Width + Species, 
                data = iris,
                n.trees = 500)#iris_test was too small, make sure to use test for your data
#order doesnt matter as much for this, can be rlly anything
summary(iris_gbm) #gives a plot! woah, see the relative influence on the petal length

#select out only x vals from test and predict
iris_gbm_preds <- iris_test %>%
  select(Petal.Width, Sepal.Length, Sepal.Width, Species) %>%
  predict(object = iris_gbm)

#save predictions back into test set
iris_test$gbm_pred <- iris_gbm_preds

#evaluate performance of models
View(iris_test)


#error
install.packages("Metrics") #was DMwR but its old lol
library(Metrics)

#calculate rmse between predictions and true evalues
rmse(iris_test$Petal.Length, iris_test$lm_pred)
rmse(iris_test$Petal.Length, iris_test$gbm_pred) #wins! smaller error

#calculate mae between predictions and true values
mae(iris_test$Petal.Length, iris_test$lm_pred) 
mae(iris_test$Petal.Length, iris_test$gbm_pred) #wins! smaller error


#accuracy
iris_test <- iris_test %>%
  mutate(glm_petal_cat = ifelse(glm_pred < 0, "long", "short"))
View(iris_test)

true_vals <- sum(iris_test$glm_petal_cat == iris_test$petal_length_cat)
total_vals <- nrow(iris_test)

accuracy <- true_vals/total_vals

#f1 score tells us about false positive and false negative rates
f1(iris_test$glm_petal_cat, iris_test$petal_length_cat)

