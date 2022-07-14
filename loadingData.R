#### load in csv file ####
#now this data is an R object, storing the contents of the csv file
cerealData <- read.csv("data/cerealData.csv")
View(cerealData)
#saveRDS(cerealData, "data/cerealData.RDS")
#saving R object as a file (if the file is too large) ^^

#### loading in ggplot ####
install.packages("ggplot2")
library(ggplot2)

#### practice ####
#plotting with your dataset
ggplot(data = cerealData, aes(x = calories_content, y = fat_content))  +
  geom_point()

ggplot(data = cerealData, aes(x = Producer)) +
  geom_bar()
