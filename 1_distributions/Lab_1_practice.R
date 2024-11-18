###################################################################
#           Statistics and Explaratoroty Data Analysis            #
#                        Laboratory 1                             #
#                 Random variables, pdf, cdf functions            #
###################################################################


#Set English in R
Sys.setenv(LANG = "en")

#Set working directory
setwd("C:\\Users\\mchle\\Mój dysk\\SEDA\\2022\\#1") #Desktop

###################################################################
#PDF and CDF#######################################################
###################################################################
#Exercise 1:
#Use data on consumers’ satisfaction and age from the file „pizza.csv”.
#Create the pdf and cdf functions for variables satisfaction and age. 
#What is the probability that the consumers are younger than 40?
#What is the share of the consumers that are younger than 40?

#load the data
pizza <- read.csv('Pizza.csv', 
                  header=TRUE, 
                  sep=",", 
                  dec=".") 


#Create the pdf and cdf fuctions for variables satisfaction and age. 
#pdf for discrete variable: satisfaction
pdf1 <- table(pizza$Satisfaction)
barplot(pdf1)

#cdf for discrete variable: satisfaction
c1 <-  ecdf(pizza$Satisfaction)
plot(c1)
?ecdf


#pdf for continous variable: age
pdf2 <- density(pizza$Age) 
plot(pdf2) 

#cdf for continous variable: age
c2 <-  ecdf(pizza$Age)
plot(c2)

#What is the probabiliy that the consumers are younger than 40?
#What is the share of the consumers that are younger than 40?
#combine the pdf and cdf plots and answer
resetPar <- function() {
    dev.new()
    op <- par(no.readonly = TRUE)
    dev.off()
    op
}
par(resetPar())


par(new=TRUE,mfrow = c(2, 1))
plot(pdf2)
plot(pdf2)
plot(c2)


###################################################################
###DISCRETE DISTRIBUTIONS###
###################################################################
#Exercise 2: Binomial distribution
#Calculate the probablity of obtaining 0 heads in 4 coin tossing.
#We have k=0 successes in 4 trials in which p=1/2
dbinom(0, 4, 0.5)

#What are the chances of receiving more than 3 heads in 4 coin tossing?
#we can derive the whole distribution
#there are 4 trials, so the number of successes can be: 0,1,2,3,4 and 
#the respective probabilities are:
probs <-  dbinom(0:4, 4, 0.5)
data.frame(0:4, probs) 

#we can also calculate cdf at 3
1-pbinom(3, 4, 0.5)
#or cdf for each value
cdf <- pbinom(0:4, 4, 0.5)
data.frame(0:4, cdf) 


#Exercise 3: Poisson distribution
#Consider a population of raisin buns for which there are an average of
#3 raisins per bun, i.e. lambda = 3. The number of raisins in
#a particular bun is uncertain; the possible numbers of raisins are 0, 1, 2, . . .
#Calculate the probability of finding exactly 2 raisins in a bun. 
#What are the chances of finding more than 3 raisins in a bun?

#Probability of finding exactly 2 raisins in a bun
#We need to calculate the probability that exactly k=2 events occur in a bun, 
#when we know that on average there are 3 raisins in a bun (lambda=3)
dpois(0, 3)

#Probability of finding more than 3 raisins in the bun
#We need to calculate the probability that we find more than 3 raisins, i.e. 4,5,6, .... 
#It is easier to calculate the probability of finding at most 3 raisins and substract it from 1 

#Let's calculate the probability that we find at most 3 raisins, i.e. 0,1,2,or 3 raisins
#We can do that by calculating cdf at 3
ppois(3, 3) -> a
#then the chances of finding more than 3 raisins in a bun are
1-a 

###################################################################
###CONTINOUS DISTRIBUTIONS###
###################################################################
#Exercise 4: Normal distribution
#Consider a farmer who sells apples in wooden boxes.
#The weights of the boxes vary and are assumed to be normally distributed 
#with miu=15 kg and sigma^2=9/4 kg2. 
#The farmer wants to avoid customers being unsatisfied because the boxes are
#too low in weight. He therefore asks the following question: 
#What is the probability that a box with a weight of 10 to 15 kg is sold?
#Answer this question by making relevent calculations and by using pdf and cdf graphs.

#We need to calculate the probability that the box weights in between 10-15kg
#We can do that by calculating the probability that a box is less than 15 kg
#and than it is less than 10 kg and subsrtracting these values
#The probability that a box is less than 15 kg is a cdf at 15
b <- pnorm(15, 15, sqrt(9/4) )
b
#The probability that a box is less than 10 kg is cdf at 10
c <- pnorm(10, 15, sqrt(9/4) )
c
#and the probability that a box with a weight of 10 to 15 kg is
b-c

#Answer this question graphically generate a graph of pdf 
X1 <- seq(0, 30, length=100) #define values the wieght of the box may take
pdf_norm <- dnorm(X1, 15, sqrt(9/4) ) #calculate pdf for these values assuming N(15,9/4)
cdf_norm <- pnorm(X1, 15, sqrt(9/4) ) #calculate cdf for these value assuming N(15,9/4)

#plot the pdf
plot(X1, pdf_norm, type="l", lty=1, xlab="x value", 
     ylab="Density", main="PDF for N(15, 9/4)")

#plot the cdf
plot(X1, cdf_norm, type="l", lty=1, xlab="x value",
     ylab="Density", main="CDF for N(15, 9/4)")


#Exercise 5: Student's t distribution
# Display the Student's t distributions with 1,2,4 and 30
# degrees of freedom and compare it to the normal distribution

#Let's start with standard normal distribution
X2 <- seq(-4, 4, length=100) #define x values
pdfx <- dnorm(X2) #calculate pdf

par(mfrow = c(1, 1)) #set the plotting window to display one graph
plot(X2, pdfx, type="l",lty=2, xlab="x value",
     ylab="Density", main="Comparison of t Distributions")

#Now let's do the same for Student's t distribution
t1 <- dt(X2, df = 1) #calculate pdf
t2 <- dt(X2, df = 2) #calculate pdf
t3 <- dt(X2, df = 4) #calculate pdf
t4 <- dt(X2, df = 30) #calculate pdf

#colors and labels
colors <- c("red", "blue", "green", "yellow", "black")
labels <- c("df=1", "df=2", "df=4", "df=30", "normal")


#add these to the graph to st. normal
plot(X2, pdfx, type="l",lty=2, xlab="x value",
     ylab="Density", main="Comparison of t Distributions")
    lines(X2, t1, col="red", lwd=2)
    lines(X2, t2, col="blue", lwd=2)
    lines(X2, t3, col="green", lwd=2)
    lines(X2, t4, col="yellow", lwd=2)
    
#can also add legend
#define colors and labels
colors <- c("red", "blue", "green", "yellow", "black")
labels <- c("df=1", "df=2", "df=4", "df=30", "normal")
    
#add legend
    legend("topright", inset=.05, title="Distributions", 
    labels, lwd=2, lty=c(1, 1, 1, 1, 2), col=colors)

###################################################################
###FITTING DISTRIBUTIONS###
###################################################################
#Exercise 6: Log-normal distribution
#Create 1000 random sampling from log-normal distribution. 
#Verify the values of the parameters of the distribution.

# create random sampling from log-normal distribution
#let' assume that our sample should have a mean of 1 and standard deviation of 1 (parameters)
set.seed(123)
logx <- rlnorm(1000,1,1)
# estimate parameters
library(MASS)
fitdistr(logx, "lognormal")


#Exercise 7: Fitting and comparing distributions of random variables
#Use data on air quality avialable in R. 
#Use variable describing temperature in New York 
#and fit its distribution assuming:
#(1) normal distribution; 
#(2)log-normal distribution; 
#(3)gamma distribution
library(datasets)
database <- airquality
str(database) #check the variables in the dataset
#plot distribution of temperature data
#install.packages("fitdistrplus")
library(fitdistrplus)
plotdist(database$Temp, histo = TRUE, demp = TRUE)
#fit the distributions
fit_n <- fitdist(database$Temp, "norm")
fit_ln <- fitdist(database$Temp, "lnorm")
fit_g  <- fitdist(database$Temp, "gamma")
summary(fit_n)
summary(fit_ln)
summary(fit_g)
#plot the results
plot.legend <- c("normal", "lognormal", "gamma")
denscomp(list(fit_n, fit_ln, fit_g), legendtext = plot.legend)
cdfcomp (list(fit_n, fit_ln, fit_g), legendtext = plot.legend)
