## z scores and confidence levels
# lets say we eat 100 boces of cookies an find the av
#  number of cookies ina  box is 38.2

sample_mean = 38.2 #alt hyp
pop_mean = 40 #null hyp
sd = 10 # stand dev
n = 100 #samp size

z = (sample_mean - pop_mean) / (sd/sqrt(n))


## t tests
#one sample t test
mean_sepal_length <- mean(iris$Sepal.Length)
mean_sepal_length

random_sample <- sample_n(iris, 30)
random_sample

#null hypothesis
#random sample mean == population mean

#alt hypothesis
#randoms sample mean != pop mean

pop_sepal_length <- iris$Sepal.Length

random_sample_sep_len <- random_sample$Sepal.Length

t.test(mu = mean_sepeal_length, x = random_sample_sep_len)

setosa <- filter(iris, Species == "setosa")
setosa_sep_len <- setosa$Sepal.Length

t.test(mu = mean_sepeal_length, x = setosa_sep_len)

#is the p-value less than .05? reject null hyp
#is the p-value greater than .05? accept null hyp


#mon jul 18 notes

sleep_hours <- c(5, 5, 5, 6, 6, 7, 7, 7, 8, 9)
mean(sleep_hours)
sd(sleep_hours)

t.test(sleep_hours, mu = 7, alternative = "less")
#default alt is not equal, specify if you want more or less


#using iris data set
pop_mean <- mean(iris$Sepal.Length)

setosa <- filter(iris, Species == "setosa")

t.test(setosa$Sepal.Length, mu = pop_mean)
#if the sample mean (of setosa sepal lengths) is  5.843 (the same as the pop_mean)
# then .00000000000000002% of the time, this difference (or more)
# will happen by chance

pop_mean2 <- mean(iris$Petal.Length)

versicolor <- filter(iris, Species == "versicolor")

t.test(versicolor$Petal.Length, mu = pop_mean2)

#paired t test
install.packages("datarium")
library(datarium)
mice2

t.test(mice2$before, mice2$after, paired = T)



#anova
sepal_len_anova <- aov(data = iris, Sepal.Length ~ Species)

# are any categoriese different
summary(sepal_len_anova)

#which groups are significanetly differeny
TukeyHSD(sepal_len_anova)

sepal_width_anova <- aov(data = iris, Sepal.Width ~ Species)
summary(sepal_width_anova)
TukeyHSD(sepal_width_anova)

View(diamonds)
diamond_price_color <- aov(data = diamonds, price ~ color)
summary(diamond_price_color)
#save results, use #cat_var
signif_results <- TukeyHSD(diamond_price_color)$color
arrange(as.data.frame(signif_results), `p adj`)
