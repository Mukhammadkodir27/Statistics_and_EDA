###################################################################
#           Statistics and Explaratoroty Data Analysis            #
#                        Laboratory 2                             #
#                 Solutions to exercises for Students             #
###################################################################

###################################################################
#Exercise 1
###################################################################
# Create 1000 random sampling from normal distributions with parameters ?=2 and??? ?????^2=0.5. 
# Can you guess what will be the mean, the median and mode for this sample?
# Check your guess with R functions. 

set.seed(123)
x<- rnorm(1000, 2, sqrt(0.5))
mean(x)
median(x)

#mode for continous data
d <- density(x)
plot(d)
d$x[d$y==max(d$y)]
    

###################################################################
# Exercise 2
###################################################################
# Use R dataset called ?anorexia? from MASS library. Calculate the average rate of change in patients?
#weight for different kinds of treatments. Which type of mean will you use and why?
library(MASS)
data <- anorexia
data$change <- (data$Postwt-data$Prewt)/data$Prewt

data$change2 <- 1+data$change
View(data)

mean(data$change)
mean(data$change2)
#geometric mean
prod(data$change2)^(1/length(data$change2))
library(psych)
geometric.mean(data$change2)

###################################################################
# Exercise 3
###################################################################
# Use data on wages from the NLSY dataset for the US. 
# How would you summarize wage data? Use various measures such as mean, median,
# trimmed mean and Winsorized mean. What can you say about the wage distribution?
# Calculate the mean, the median, quartiles, 1st and 9th deciles, as well as 5th and 95th percentiles 
# of hours by sex. Interpret the data. 
# Do the same for experience and tenure. What can you say about these two variables?
NLSY<-read.csv("NLSY_EDA_class.csv ", 
               header=T, 
               sep=",",
               dec="." ) 
View(NLSY)
str(NLSY)

#summarize wage data
summary(NLSY$wage)
mean(NLSY$wage, trim=0.2)
winsor.mean(NLSY$wage, trim=0.1)

wage_dens <- density(NLSY$wage)
plot(wage_dens)
#wage distribution is not symmetric --> there are many observations close to zero and few very large values 
#this makes the mean>>median
#trimming or censoring the mean with WInsor mean makes the mean much closer to median

#the mean, the median, quartiles, 1st and 9th deciles, as well as 5th and 95th percentiles 
# of hours by sex
aggregate(NLSY[,c(6)], list(NLSY$sex), mean) #mean
aggregate(NLSY[,c(6)], list(NLSY$sex), median) #median
aggregate(NLSY[,c(6)], list(NLSY$sex), quantile, probs=(c(0.25, 0.5, 0.75))) #quairtiles
aggregate(NLSY[,c(6)], list(NLSY$sex), quantile, probs=(c(0.1, 0.9))) #deciles
aggregate(NLSY[,c(6)], list(NLSY$sex), quantile, probs=(c(0.05, 0.95))) #percentiles

#the distribution of hours is much more symmetric for men than for women
#men tend to work longer hours than women (compare 9deciles and 99percintiles)

#the mean, the median, quartiles, 1st and 9th deciles, as well as 5th and 95th percentiles 
# of experience by sex
aggregate(NLSY[,c(5)], list(NLSY$sex), mean) #mean
aggregate(NLSY[,c(5)], list(NLSY$sex), median) #median
aggregate(NLSY[,c(5)], list(NLSY$sex), quantile, probs=(c(0.25, 0.5, 0.75))) #quairtiles
aggregate(NLSY[,c(5)], list(NLSY$sex), quantile, probs=(c(0.1, 0.9))) #deciles
aggregate(NLSY[,c(5)], list(NLSY$sex), quantile, probs=(c(0.05, 0.95))) #percentiles


#the mean, the median, quartiles, 1st and 9th deciles, as well as 5th and 95th percentiles 
# of experience by sex
aggregate(NLSY[,c(8)], list(NLSY$sex), mean) #mean
aggregate(NLSY[,c(8)], list(NLSY$sex), median) #median
aggregate(NLSY[,c(8)], list(NLSY$sex), quantile, probs=(c(0.25, 0.5, 0.75))) #quairtiles
aggregate(NLSY[,c(8)], list(NLSY$sex), quantile, probs=(c(0.1, 0.9))) #deciles
aggregate(NLSY[,c(8)], list(NLSY$sex), quantile, probs=(c(0.05, 0.95))) #percentiles

exper_dens <- density(NLSY$exper)
plot(exper_dens)

ten_dens <- density(NLSY$tenure)
plot(ten_dens)

#expeience: mean<median --> few observations with no experience
#tenure: mean>median --> many observations with little tenure

###################################################################
# Exercise 4
###################################################################
#Use R dataset called “Animals” from MASS library. 
#weight for different kinds of treatments. Which type of mean will you use and why?
library(MASS)
animals <- Animals

#•	What is the mean, median and mode body and brain weight of the 28 species in the dataset?
#body weight
mean(animals$body)
median(animals$body)
table(animals$body)

hist(animals$body)

#brain weight
mean(animals$brain)
median(animals$brain)
table(animals$brain)

hist(animals$brain)
#•	Are the distributions of body and brain weight symmetric? Answers by using mean, median and mode values. 
#no --> median<mean

#•	Dropping two extreme values from the top and bottom of body weight
#distribution what is the mean body weight?
mean(animals$body, trim=2/28)
#•	What is the mean share of brain weight in the total body weight?
mean((animals$brain/1000)/animals$body)
#•	What is the body weight below which there is 27% of the sample?
quantile(animals$body, probs=c(0.27))
