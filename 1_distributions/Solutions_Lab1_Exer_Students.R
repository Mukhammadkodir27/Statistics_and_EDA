###################################################################
#           Statistics and Explaratoroty Data Analysis            #
#                        Laboratory 1                             #
#                 Solutions to exercises for Students             #
###################################################################

###################################################################
#Exercise 1
###################################################################
# Suppose you roll an unfair die 50 times with the probability of obtaining “6” equal to 0.6 
# (i.e. we deal with binomial distribution with n=50 and p=0.6). 
# a)	Plot the probability density function for this distribution
# b)	What is the probability of obtaining more than 40 times “6”?
n <- 50 #50 times rolling of a die
pmf <- dbinom(0:n, n, 0.6) #pdf for 50 times of Binomial rv with p=0.6 
plot (0:n, pmf, type = „h”)

a <- cbinom(40, 50, 0.6)  #probabilty of having <=40
1-a #probability of having more than 40

###################################################################
# Exercise 2
###################################################################
# A study on birds collects information such as the length of their eggs (in mm). 
# Assume that the length is normally distributed with µ = 42.1mm and ??2 = 20.82. 
# a)	What is the probability of finding an egg with a length greater than 50 mm?
# b)	 What is the probability of finding an egg between 30 and 40 mm in length?
# c)	 generate probability distribution and cumulative distribution functions for this distribution 
# and show graphically probabilities you calculated in points (a) and (b)

# probability of finding an egg with a length <50 mm
b <- pnorm(50, 42, sqrt(20.8) )
# probability of finding an egg with a length greater than 50 mm
1 – b
#  probability of finding an egg with a length <40 mm
c <- pnorm(40, 42, sqrt(20.8) )
#  probability of finding an egg with a length <30 mm
d <- pnorm(30, 42, sqrt(20.8) )
# probability of finding an egg between 30 and 40 mm in length?
c-d
#generate a graph of pdf 
X1 <- seq(-50, 50, length=100)
pdf_norm=dnorm(X1, 42, sqrt(20.8))
plot(X1, pdf_norm, type="l", lty=1, xlab="x value",
     ylab="Density", main="PDF for N(42, 20.8)")

#generate a graph of cdf 
cdf_norm <- pnorm(X1, 42, sqrt(20.8))
plot(X1, cdf_norm, type="l", lty=1, xlab="x value",
     ylab="Density", main="CDF for N(42, 20.8)")

###################################################################
# Exercise 3
###################################################################
# Generate 1000 random numbers from a standard normal distribution. 
# Plot the empirical cumulative distribution function of your sample (use ecdf fuction). 
# What is the value of the ecdf for the random number of 0? What does it mean?
normal_numbers <- rnorm(1000, 0,1)
normal_ecdf <- ecdf(normal_numners)
plot(normal_ecdf, main = 'Empirical Cumulative Distribution')

###################################################################
# Exercise 4
###################################################################
# Use the data on crime rates in the US that are available in R (USArrests). 
# a)	Plot the distribution of murders and rapes in the US
# b)	Fit the distribution for these two variables. Try different distributions we covered during the class. 
Library(datasets)
Data <- USArrests  #get the data
str(USArrests) #check the data

library(fitdistrplus)
murder  <- USArrests$Murder
rape <- USArrests$Rape

plotdist(murder, histo=TRUE, demp=FALSE)
plotdist(rape, histo=TRUE, demp=FALSE)


#fit the distributions
fit_n_murder <- fitdist(murder, "norm")
fit_ln_murder <- fitdist(murder, "lnorm")
fit_g_murder  <- fitdist(murder, "gamma")
fit_x_murder  <- fitdist(murder, "exp")

fit_n_rape<- fitdist(rape, "norm")
fit_ln_rape <- fitdist(rape, "lnorm")
fit_g_rape  <- fitdist(rape, "gamma")
fit_x_rape  <- fitdist(rape, "exp")

plot.legend <- c("normal", "lognormal", "gamma", "exp")
denscomp(list(fit_n_murder, fit_ln_murder, fit_g_murder, fit_x_murder), legendtext = plot.legend)
cdfcomp (list(fit_n_murder, fit_ln_murder, fit_g_murder, fit_x_murder), legendtext = plot.legend)

plot.legend <- c("normal", "lognormal", "gamma", "exp")
denscomp(list(fit_n_rape, fit_ln_rape, fit_g_rape, fit_x_rape), legendtext = plot.legend)
cdfcomp (list(fit_n_rape, fit_ln_rape, fit_g_rape, fit_x_rape), legendtext = plot.legend)
