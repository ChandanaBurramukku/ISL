#1.a ISLR 2.4 Applied Problem 8
#setting the directory
setwd("C:/R")
#reading the file from the directory
college <- read.csv("College.csv")
head(College)


#1.b -> NEED TO CHECK THIS AS WELL
rownames (college) <- college[, 1]
View (college)
college <- college[, -1]
head(college)
View (college)
#1.c.i Use the summary() function to produce a numerical summary
#of the variables in the data set
summary(college)
#1.c.ii. Use the pairs() function to produce a scatterplot matrix of the first ten columns or variables of the data. 
#Recall that you can reference the first ten columns of a matrix A using A[,1:10].
college$Private <- as.factor(college$Private)
pairs(college[, 1:10])


#1.c.iii. Use the plot() function to produce side-by-side boxplots of Outstate versus Private.
# NOT DONE NEED TO CHECK not able to get the output
plot(college$Private, college$Outstate, xlab ="Private University", ylab ="Out of State tuition in USD", main = "Outstate Tuition Plot")
pairs(college[,1:10])
plot(Outstate ~Private, data=college,col=c("green","red"))
plot(college$Private, college$Outstate, xlab = "Private University", ylab = "Tuition in $")
#iv. Create a new qualitative variable, called Elite, by binning the Top10perc variable. We are going to divide universities
#into two groups based on whether or not the proportion of students coming from the top 10 % of their high school classes exceeds 50 %.
Elite <- rep("No", nrow(college))
Elite[college$Top10perc > 50] <- "Yes"
Elite <- as.factor(Elite)
college <- data.frame(college, Elite)
summary(Elite)
plot(college$Elite, college$Outstate, xlab = "Elite University", ylab = "Tuition in $")
#1.c.v. Use the hist() function to produce some histograms with differing numbers of bins for a few of the quantitative variables. You may find the command par(mfrow = c(2, 2))
#useful: it will divide the print window into four regions so that four plots can be made simultaneously. Modifying the
#arguments to this function will divide the screen in other ways.
par(mfrow=c(2,2))
hist(college$Apps, xlab = "Applications Received", col="red",main = "")
hist(college$perc.alumni, col=2, xlab = "Perc of alumni who donate",main = "")
hist(college$S.F.Ratio, col=3, breaks=10, xlab = "Student/Faculty ratio",main = "")
hist(college$Expend, breaks=100, xlab = "Instructional expenditure per student", col="green",main = "")
#1.c.vi. Continue exploring the data, and provide a brief summary of what you discover.
summary(college$Apps)
summary(college$PhD)
row.names(college)[which.max(college$Top10perc)]
acceptance_rate <- college$Accept / college$Apps
row.names(college)[which.min(acceptance_rate)] 
plot(college$Outstate, college$Grad.Rate)
#2.a Which of the predictors are quantitative, and which are qualitative?
Auto <- read.csv("Auto.csv",na.strings="?")
Auto <- na.omit(Auto)
str(Auto)
#Except NAME and HORSEPOWER remaining are quantitative 


#2.b What is the range of each quantitative predictor? You can answer this using the range() function.
sapply(Auto[, -c(4, 9)], mean)
qualitative_columns <- which(names(Auto) %in% c("name", "origin", "originf"))
qualitative_columns
sapply(Auto[, -qualitative_columns], range)
#2.(c) What is the mean and standard deviation of each quantitative predictor?
sapply(Auto[, -qualitative_columns], mean)
sapply(Auto[, -qualitative_columns], sd)
#2.(d) Now remove the 10th through 85th observations. What is the range, mean, and standard deviation of each 
#predictor in the subset of the data that remains?
subset <- Auto[-c(10:85), -c(4,9)]
sapply(subset, range)
sapply(subset, mean)
sapply(subset, sd)
#2.e) Using the full data set, investigate the predictors graphically,using scatterplots or other tools of your choice.
#Create some plots highlighting the relationships among the predictors. Comment on your findings
pairs(Auto)
pairs(Auto[, -qualitative_columns])
with(Auto, plot(mpg, weight))
with(Auto, plot(mpg, cylinders))
# for 20 observations
Auto.sample <- Auto[sample(1:nrow(Auto), 20), ]
# ordering them
Auto.sample <- Auto.sample[order(Auto.sample$mpg), ]
# plot by using "dotchart"
with(Auto.sample, dotchart(mpg, name, xlab = "mpg"))
with(Auto, plot(origin, mpg), ylab = "mpg")
#****************Findings*****************************
#With mpg we are seeing an inverse effect for horsepower, weight and displacement
#mpg has increased drastically over years and it has twice in a decade.
#European and US cars have lower mpg than Japanese cars
#2.f) Suppose that we wish to predict gas mileage (mpg) on the basis of the other variables. Do your plots suggest that any of the
#other variables might be useful in predicting mpg? Justify your answer.
Auto$horsepower <- as.numeric(Auto$horsepower)
cor(Auto$weight, Auto$horsepower)
cor(Auto$weight, Auto$displacement)
cor(Auto$displacement, Auto$horsepower)
#For predictors we can use origin, cylinders, year and hoursepower
#We cannot use weight and displacement as they are correlated highly with each other and also with horsepower.


#3(a) To begin, load in the Boston data set. The Boston data set is part of the ISLR2 library.
#How many rows are in this data set? How many columns? What do the rows and columns represent?
library(MASS)
Boston$chas <- as.factor(Boston$chas)
nrow(Boston)
ncol(Boston)
#rows indicate the U.S Census Tracts in Boston area
#column indicate the measures of census 
#3.(b) Make some pairwise scatterplots of the predictors (columns) in this data set. Describe your findings.
par(mfrow = c(2, 2))
plot(Boston$nox, Boston$crim)
plot(Boston$rm, Boston$crim)
plot(Boston$age, Boston$crim)
plot(Boston$dis, Boston$crim)
pairs(Boston)
#the bottom two plots age and dis are looking inverse of each other


#3.(c) Are any of the predictors associated with per capita crime rate? If so, explain the relationship.
Boston.corr = cor(Boston)
Boston.corr.crim = Boston.corr[-1,1]
print(
  Boston.corr.crim[order(abs(Boston.corr.crim), decreasing = T)]
)

#3.d) Do any of the census tracts of Boston appear to have particularly high crime rates? Tax rates? 
#Pupil-teacher ratios? Comment on the range of each predictor.
#crime rate
hist(Boston$crim, breaks = 50)
nrow(Boston[Boston$crim > 20, ])
#tax rate
hist(Boston$tax, breaks = 50)
nrow(Boston[Boston$tax == 666, ])
#Pupil-teacher ratios
hist(Boston$ptratio, breaks = 50)
nrow(Boston[Boston$ptratio > 20, ])
#3.e) How many of the census tracts in this data set bound the Charles river?
nrow(Boston[Boston$chas == 1, ])
#3.f) What is the median pupil-teacher ratio among the towns in this data set?
median(Boston$ptratio)


#3.g) Which census tract of Boston has lowest median value of owneroccupied homes? What are the values of the other predictors
#for that census tract, and how do those values compare to the overall ranges for those predictors? Comment on your findings.
t(subset(Boston,medv==min(Boston$medv)))
row.names(Boston[min(Boston$medv), ])
range(Boston$tax)
Boston[min(Boston$medv), ]$tax
#3.h) In this data set, how many of the census tracts average more than seven rooms per dwelling? More than eight rooms per
#dwelling? Comment on the census tracts that average more than eight rooms per dwelling.
nrow(Boston[Boston$rm > 7, ])
nrow(Boston[Boston$rm > 8, ])


#4.a.i Is there a relationship between the predictor and the response?
library(ISLR)
library(MASS)
data("Auto")
head(Auto)
lm.fit<-lm(mpg~horsepower,data=Auto)
summary(lm.fit)
#Yes we do have relation between predictor and response because p value is 2e-16
#4.a.ii. How strong is the relationship between the predictor and the response?
#R^{2} value is equal to 61% of variable (horsepower) in mpg. Mean for mpg is 23.44 and RSE of lm.fit is 4.9 which shows the % error of 20.9%
#4.a.iii. Is the relationship between the predictor and the response positive or negative?
#The relationship between predictor and response is negative. The linear regression indicates horsepower a automobile has.
#4.a.iv. What is the predicted mpg associated with a horsepower of 98? What are the associated 95 % confidence and prediction intervals?
predict(lm.fit,data.frame(horsepower=c(98)),interval="prediction")
predict(lm.fit,data.frame(horsepower=c(98)),interval="confidence")
#4.b Plot the response and the predictor. Use the abline() function to display the least squares regression line.
attach(Auto)
plot(horsepower,mpg)
abline(lm.fit,lwd=5,col="blue")
#4.c) Use the plot() function to produce diagnostic plots of the least squares regression fit. 
#Comment on any problems you see with the fit.
which.max(hatvalues(lm.fit))
par(mfrow = c(2,2))
plot(lm.fit)
#We have non-linearity for the data in the plot for residuals and fitted values.
#For leverage and standardized residuals we have outliers. 


#5.a) Produce a scatterplot matrix which includes all of the variables in the data set.
pairs(Auto)
#5.b) Compute the matrix of correlations between the variables using
#the function cor(). You will need to exclude the name variable, cor() which is qualitative.
Auto$name<-NULL
cor(Auto,method = c("pearson"))
#5.c.i. Is there a relationship between the predictors and the response?
lm.fit<-lm(mpg~.,data=Auto)
summary(lm.fit)
#The F-statistic vale shows that we have a relationship between predictors and the response.
#5.c.ii. Which predictors appear to have a statistically significant relationship to the response?
#We have relationship for origin, weight, year and for displacement. We can say this by seeing the p-values for predictors.
#5.c.iii. What does the coefficient for the year variable suggest?
#All the predictors are constant except for mpg.  For every year cars are nearly 1 mpg/year fuel efficient. 
#The coefficient of year variable shows the effect of raise for one year is equivalent to 0.75 in mpg.
#5.d) Use the plot() function to produce diagnostic plots of the linear regression fit. Comment on any problems you see with the fit.
#Do the residual plots suggest any unusually large outliers? Does the leverage plot identify any observations with unusually high leverage?
which.max(hatvalues(lm.fit))
par(mfrow = c(2,2))
plot(lm.fit)
#We have a non-linear relationship between the response and predictors for the first graph.
#The Residuals are distributed normally and are skewed towards right for second one.
#For this model the error assumption is not true for constant variance for third graph.
#We do not have any leverage points for fourth graph. But this one stands out as potential leverage point.



