# 1. Consider the data set bigmac.cvs dataset obtainable from the module's Blackboard site.

# Read bigmac.csv into R
setwd("D:/UEA/Applied Statistics/Lab/Data")
path <- file.path("bigmac.csv")
data = read.csv(path,header=TRUE)
str(data)

# a. Using the relevant R-commands, find the names of the variables. [marks 4]
# attach to make variables accessible in R, names to get variables' names

attach(data)
names(data)

# b. Using the relevant R-commands:

# i. Compute the mean, variance and standard deviation for the variable bus fare. In each case 
# exclude the values for Mexico City, Milan, Montreal, Nairobi, New York, Nicosia and Sidney using 
# the relevant R-commands. [marks 7]

# Generate a vector of excluded cities
excluded_cities = c('Mexico City', 'Milan', 'Montreal', 'Nairobi', 'New York', 'Nicosia', 'Sydney')

# Assign the values of busfare that is not in the excluded cities in 'busfare_excluded' variable
busfare_excluded = data$busfare[!(data$city %in% excluded_cities)]
busfare_excluded

mean(busfare_excluded)

var(busfare_excluded)

sd(busfare_excluded)

# ii. Name the cities that have the lowest bus fare, the highest bus fare. Which
# city's bus fare is below the mean but above the median? [marks 3]

# Find city where busfare is min
data$city[data$busfare==min(data$busfare)]

# Find city where busfare is max
data$city[data$busfare==max(data$busfare)]

# Find city where busfare is below the mean but above the median
data$city[(median(data$busfare)<data$busfare) & (data$busfare<mean(data$busfare))]

# c. Construct notched box-and-whisker plots in blue for the list of original bus
# fares and also the list of bus fares with Mexico City, Milan, Montreal, Nairobi,
# New York, Nicosia and Sidney excluded.

# Store original busfare and busfare that excluded some cities in 2 variables
original_busfare = data$busfare
busfare_excluded = data$busfare[!(data$city %in% excluded_cities)]

# Notched boxplot for original busfare
boxplot(
  original_busfare,
  main='Notched box-and-whisker plot for original busfare',
  ylab='Original busfare',
  col='blue',
  notch=TRUE
)

# Notched boxplot for busfare (excluded some cities)
boxplot(
  busfare_excluded,
  main='Notched box-and-whisker plot for busfare (excluded some cities)',
  ylab='Busfare (excluded some cities)',
  col='blue',
  notch=TRUE
)

# i. Use the relevant R commands to combine your two notched box-and-whisker plots into a single 
# figure such that the y-axis is entitled bus fare and the x-axis is entitled study. The x-axis 
# should have two values called with and without where with refers to the original list of bus fares 
# and without to the list of bus fares with Mexico City, Milan, Montreal, Nairobi, New York, Nicosia 
# and Sydney excluded. [marks 6]

# Store original busfare and busfare that excluded some cities in 2 variables
original_busfare = data$busfare
busfare_excluded = data$busfare[!(data$city %in% excluded_cities)]

# Values of y axis
studies = c(original_busfare,busfare_excluded)

# Values of x axis (labels of 2 studies, the first with 45 cities, the second (excluded) with 38)
label = factor(c(rep('With', 45), rep('Without', 38)))

# Combined notched boxplot for busfare (excluded some cities)
boxplot(
  studies~label,
  main='Combined notched box-and-whisker plots for busfare',
  ylab='Busfare',
  xlab='Study',
  col='blue',
  notch=TRUE
)

# Summary statistics for original busfare:
summary(original_busfare)

# Summary statistics for busfare (excluded some cities):
summary(busfare_excluded)

# notched: 95% confidence interval of the median: median +/- 1.57*IQR/sqrt(n)

# d. Create a scatterplot for the variables engsal and teachsal such that the x-axis
# is labelled by teachsal, the plot characters are red crosses, the y-axis is labelled
# by engsal and the plot is entitled teacher salary vs engineer salary. [marks 8]

# Assign the values of teachsal and engsal to 2 variables: x,y
x = data$teachsal
y = data$engsal

# Scatterplot for 'Teacher salary vs engineer salary' (pch=4 is red crosses)
plot(
  x,
  y,
  main='Teacher salary vs engineer salary',
  xlab='teachsal',
  ylab='engsal',
  col='red',
  pch=4
)

# e. Assuming that the relationship between teacher salary and engineer salary is captured by a simple 
# linear regression model, fit a Least Squares Regression line through the dataset where engineer 
# salary is the response variable. Redo the plot you have generated in Question 1d with the Least 
# Squares Regression line included.

# Assign the values of teachsal and engsal to 2 variables: x,y
x = data$teachsal
y = data$engsal

# Scatterplot for 'Teacher salary vs engineer salary' (pch=4 is red crosses)
plot(
  x,
  y,
  main='Teacher salary vs engineer salary',
  xlab='teachsal',
  ylab='engsal',
  col='red',
  pch=4
)

# add OLS line
abline(lm(y~x))

# f. Using the relevant R commands, find the city for which the predicted 
# engineer salary is above 60 monetary units. [marks 3]

# Assign the values of predicted engineer salary (y) to the variable 'pred'
pred = predict(lm(y~x))
pred

# Find the cities which the predicted engineer salary is above 60 monetary units
data$city[pred>60]

# g. Redo the plot in Question 1e with the residuals added. [marks 5]

# Assign the values of teachsal and engsal to 2 variables: x,y
x = data$teachsal
y = data$engsal

# Scatterplot for 'Teacher salary vs engineer salary' (pch=4 is red crosses)
plot(
  x,
  y,
  main='Teacher salary vs engineer salary',
  xlab='teachsal',
  ylab='engsal',
  col='red',
  pch=4
)

# add OLS line
abline(lm(y~x))

# add residuals (for 45 cities)
for (i in 1 : 45) lines(c(x[i], x[i]),c(y[i],fitted[i]))

# h.  Are the variables engsal and teachsal correlated? Justify your answer using the
# relevant formulas but not a scatterplot. [marks 5]

# Compute Sxx, Syy, Sxy
Sxx = sum((x-mean(x))^2)

Syy = sum((y-mean(y))^2)

Sxy = sum((x-mean(x))*(y-mean(y)))

c(Sxx,Syy,Sxy)

# regression coefficients for y on x

a_hat_x = Sxy/Sxx
b_hat_x = mean(y) - a_hat_x*mean(x)

c(a_hat_x,b_hat_x)

# regression coefficients for x on y.

a_hat_y = Sxy/Syy
b_hat_y = mean(x) - a_hat_y*mean(y)

c(a_hat_y, b_hat_y)

# compute r

r = sqrt(a_hat_x*a_hat_y)

c(a_hat_x,a_hat_y,r)

# i. Create a histogram for workhrs using 11 bins. [marks 5]

# Enforces 12 break points => 11 bins 
hist(
  data$workhrs, 
  main='Histogram for workhrs',
  xlab='workhrs',
  breaks=seq(min(data$workhrs), max(data$workhrs), length.out=12)
)

# j. Is the assumption that workhrs is normally distributed justified? [marks 4]

# No, because the histogram plotted is roughly bell-shaped, but no statistical tests. 
# Could also use Q-Q plot

qqnorm(data$workhrs)
qqline(data$workhrs, lty=2)

# k. Assume that the values in workhrs are distributed normally. For each of the following two 
# questions assume symmetry and use as mean and standard deviation the mean and standard deviation 
# of the dataset, respectively.

# i. Find the probability that a value for workhrs selected at random lies between 
# 1700hrs and 1900hrs. [marks 7]

# Store mean and sd in 2 variables
mean_workhrs = mean(data$workhrs)
sd_workhrs = sd(data$workhrs)

# X:"Work hours"
# mean = mean(data$workhrs)
# sd = sd(data$workhrs)

# P(1700<=X<=1900) = P(X<=1900) - P(X<=1700)

pnorm(1900, mean=mean_workhrs, sd=sd_workhrs) - pnorm(1700, mean=mean_workhrs, sd=sd_workhrs)

# ii. Find the two values so that we can expect 40% of the reported workhrs to lie between them

# Suppose the 2 values stated above is: z1 < z2 => P(z1<=X<=z2) = P(X<=z2)-P(X<=z1) = 0.4

# 40% of the reported workhrs fall between z1, z2 -> 60% falls outside z1, z2 -> 30% falls on each side
# => p(X<=z1) = 0.3, p(X<=z2) = 0.7

qnorm(p=c(0.3,0.7), mean=mean_workhrs, sd=sd_workhrs)

# 2. A large consignment of chocolate bars is produced. A random sample of 15 chocolate
# bars is taken and inspected for flaws. If 10% of the chocolate bars in the consignment
# have a flaw, calculate (correct to 4 decimal places) the probability that the random
# sample contains.

# X:"Number of chocolate bars with flaws" (random variable)
# n: 15 (number of trials)
# p(success) = p(chocolate bar has a flaw) = 0.1

# a. 

# i. no chocolate bar with a flaw. [marks 6]

# P(X=0) = (n,k)*p^0*q^n-k = choose(15,0)*0.1^0*0.9^15
y = choose(15,0)*0.1^0*0.9^15
round(y,digits=4)

x = c(1:15)
y = dbinom(x=0, size=15, prob=0.1)
round(y,digits=4)

# ii. exactly one chocolate bar with a flaw. [marks 6]

# P(X=1) = choose(15,1)*0.1^1*0.9^14
y = choose(15,1)*0.1^1*0.9^14
round(y,digits=4)

x = c(1:15)
y = dbinom(x=1, size=15, prob=0.1)
round(y,digits=4)

# iii. between 2 and 3 chocolate bars with a flaw. [marks 6]

# P(2<=X<=3) = P(X=2 or X=3) = P(X=2) + P(X=3) = choose(15,2)*0.1^2*0.9^13 + choose(15,3)*0.1^3*0.9^12
y = choose(15,2)*0.1^2*0.9^13 + choose(15,3)*0.1^3*0.9^12
round(y,digits=4)

x = c(1:15)
y = dbinom(x=2, size=15, prob=0.1) + dbinom(x=3, size=15, prob=0.1)
round(y,digits=4)

# b. The consignment is accepted without further testing if the random sample contains no more 
# that 3 chocolate bars with a flaw. Find the probability that the batch is accepted without 
# further testing. [marks 6]

# P(X<=3) = sumk(0-3)((15,k)*p^k*q^15-k) = P(X=0) + P(X=1) + P(X=2) + P(X=3)

y = choose(15,0)*0.1^0*0.9^15 + choose(15,1)*0.1^1*0.9^14 + choose(15,2)*0.1^2*0.9^13 + choose(15,3)*0.1^3*0.9^12
round(y,digits=4)

y = pbinom(3, size=15, prob=0.1)
round(y,digits=4)

