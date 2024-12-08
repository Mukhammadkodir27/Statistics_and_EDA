###################################################################
#           Statistics and Explaratoroty Data Analysis            #
#                        Laboratory 2                             #
#                 Measures of location                            #
###################################################################

install.packages("cluster")
install.packages("factoextra")
install.packages("flexclust")
install.packages("fpc")
install.packages("clustertend")
install.packages("ClusterR")

library(cluster)
library(factoextra)
library(flexclust)
library(fpc)
library(clustertend)
library(ClusterR)


#Set English in R
Sys.setenv(LANG = "en")

#Set working directory
setwd("C:\\Users\\mchle\\Dysk Google\\WNE Przedmioty\\SEDA\\Statistics and Explanatory Data Analysis\\2") #Desktop

###################################################################
###Basis measures of location###
###################################################################

#########################################
#Exercise 1: Use data on Apple stocks and 
#calculate rate of change for each year.
#Then calculate:
#mean
#geometric mean
#weighted mean
#trimmed mean 
#Winsorized mean
#######################################
#get the data
apple<-read.csv("Apple_data.csv", 
                header=T, 
                sep=",",
                dec="." ) 
str(apple)
View(ap)

#calculate yearly rate of change
apple$change <- (apple$Close-apple$Open)/apple$Open

apple$rate.change <- 1+apple$change

#mean 
mean(apple$rate.change)

#geometric mean
prod(apple$rate.change)^(1/length(apple$rate.change))
#or use:
library(psych)
geometric.mean(apple$rate.change)

#weighted mean
apple$weights <- apple$Volume/sum(apple$Volume)
weighted.mean(apple$rate.change, apple$weights)

#trimmed & Winsorized means
#at 10%
mean(apple$rate.change, trim=0.1) # we delete 10% of lowest and highest values from our mean 
winsor.mean(apple$rate.change, trim=0.1)
#let's change first 100 values for extremely high values
apple$rate.change2 <- apple$rate.change
apple$rate.change2[0:100] <- max(apple$rate.change)+0.5 # 100 values will be really high 

mean(apple$rate.change2)
mean(apple$rate.change2, trim=0.1)
mean(apple$rate.change2, trim=0.25)
winsor.mean(apple$rate.change2, trim=0.1)
winsor.mean(apple$rate.change2, trim=0.25)

########################################################
#Exercise 2: Create 1000 random sampling from 
#binomial distribution with n=10 and p=0.6 
#Calculate mean, median and mode of your sample
########################################################

# create 1000 random sampling from binomial distribution. 
set.seed(123)
binom <- rbinom(10000, 10, 0.6)

#mean  
mean(binom)
#median 
median(binom)
#mode
table(binom)
names(sort(-table(binom)))[1] # sortin in a descending order and indexing first item


########################################################
#Exercise 3: Create 1000 random sampling 
#from log-normal distribution. 
#Calculate mean and median of your sample.
#Interpret the relations between the measures 
#what do they imply?
########################################################

# create random sampling from log-normal distribution
set.seed(123)
logx <- rlnorm(1000)
#Calculate mean, median

mean(logx)
median(logx)

#graph the data
pdf <- density(logx)
plot(pdf)

#########################################################
#Exercise 4: Use data on wages from the NYSE 
#dataset for the US (data for 2010)
#Calculate mean and median wages by sex, 
#by race and by education and intepret the values
#Calculate the value of 1st,2nd and 3rd quartile, 
#1st and 9th decile and 1st, 90th, 99th percentile for full sample and by sex
#Interpret the values
############################################################

#get the data
NLSY<-read.csv("NLSY_EDA_class.csv", 
                header=T, 
                sep=",",
                dec="." ) 
View(NLSY)
str(NLSY)

#mean and median wages by sex
aggregate(NLSY[,c(7)], list(NLSY$sex), mean)
aggregate(NLSY[,c(7)], list(NLSY$sex), median) # mean higher than median - rightskeewed distribution - we should proceed furhter with median

#mean and median wages by race
aggregate(NLSY[,c(7)], list(NLSY$race), mean)
aggregate(NLSY[,c(7)], list(NLSY$race), median)

#mean and median wages by education
aggregate(NLSY[,c(7)], list(NLSY$education), mean)
aggregate(NLSY[,c(7)], list(NLSY$education), median)

#1st quartile
quantile(NLSY$wage, (0.25))

#1st, 2nd and 3rd quartile, for full sample
quantile(NLSY$wage, probs=c(0.25, 0.5, 0.75))
#1st, 2nd and 3rd quartile by sex
aggregate(NLSY[,c(7)], list(NLSY$sex), quantile, probs=c(0.25, 0.5, 0.75))

#1st and 9th decile for full sample
quantile(NLSY$wage, probs=c(0.1, 0.9))
#1st and 9th decile by sex
aggregate(NLSY[,c(7)], list(NLSY$sex), quantile, probs=c(0.1, 0.9))

#1st, 90th, 99th percentile for full sample
quantile(NLSY$wage, probs=c(0.01, 0.90, 0.99))
#1st, 90th, 99th percentile by sex
aggregate(NLSY[,c(7)], list(NLSY$sex), quantile, probs=c(0.01, 0.90, 0.99))

library(DescTools)
Desc(NLSY$wage)

######################################################
#Exercise 5: Calculate the interquartile mean
#for the following temperature data:
#28, 43, 32, 18, 7, 15, 22, 23, 29, 9, 11, 16. 
#####################################################
v <- c(28, 43, 32, 18, 7, 15, 22, 23, 29, 9, 11, 16) #define vector
sort(v) #sort values

quarter_n <- length(v)/4 #count how many observations in each quartile (25% of distribution)
n <- 2*quarter_n #count how many observations in interquartile
n

#get the vector with values in intequartile range
vector.sum <- v[v>quantile (v, 0.25, type=2)& v<quantile (v, 0.75, type=2)]
vector.sum

#calculate iqm
iqm <- (1/n)*(sum(vector.sum))
iqm



