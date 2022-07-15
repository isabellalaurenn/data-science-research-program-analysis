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