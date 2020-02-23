# Data Analysis_Descriptive Methods and Data Visualization
# by Ran Gao on 26/09/2018

# 0.Set the working environment
ls()
rm(list=ls())
dev.off(dev.list()["RStudioGD"])
options(digits=7, scipen=0);#options(digits=7, scipen=999);
opar <- par(no.readonly=TRUE)
setwd("D:/R")

# 1. For this problem we will be working with the data set 'calcio'.
# 1.1 Verify which kind of variables are in the dataset.
data("calcio")
str(calcio)

# 1.2 Verify if there are missing values in the dataset.
summary(calcio)

# 1.3 Visualize the first 10 values of the variable HomeTeam.
attach(calcio)
HomeTeam[1:10] # alternative: calcio$hometeam[1:10]

# 1.4 Provide a table with the number of home games for each team (i.e. absolute frequencies).
#     Show the same table in relative terms (i.e. relative frequencies). 
#     Which teams played the most in the A series during the observed period?
tab <- table(calcio$HomeTeam) # absolute frequencies
tab

tab_rel <- prop.table(tab) # relative frequencies
tab_rel

# 1.5 Provide an appropriate graphical representation of the variable HomeTeam.
barplot(tab,cex.names = 0.4)
pie(tab,cex = 0.4)


# 1.6 Create a new dataset calcio.complete by erasing the missing values and use it to evaluate the most common descriptive indices of the variable B365H 
#     Represent the variable with appropriate graphs. 
#     How do you judge the variability? Are there any outliers? 
#     Is the distribution symmetric? Which elements brought you to this conclusion?
summary(calcio$B365H)
calcio.complete <- na.omit(calcio)
summary(calcio.complete$B365H)
mean(calcio.complete$B365H)
median(calcio.complete$B365H)
sd(calcio.complete$B365H)
var(calcio.complete$B365H)
hist(calcio.complete$B365H,breaks=20)
boxplot(calcio.complete$B365H)
#     The Boxplots shows the presence of outliers. The distribution is not symmetric, but skewed to the right. 
#     we can deduce it from many different considerations, as for example:
#     ? The histogram shows a long tail on the right;
#     ? The whiskers of the boxplot are at different distances from the median;
#     ? the mean is considerably greater than the median.

# 1.7 Evaluate the mean share of B365H for every different value of the variable FTR,
#     paying attention to missing values. What should we expect? How do we interpret this result?
tapply(calcio.complete$B365H,calcio.complete$FTR,mean)

# 1.8 Is it reasonable to assume that B365 comes from a Gaussian distribution?
qqnorm(calcio.complete$B365H)
qqline(calcio.complete$B365H)
#     The quantiles of the variables are very far apart from the theoretical ones of the normal distribution.

# 1.9 Evaluate the correlation between B365H e B365A. Could one expect a similar result?
#     What would a positive correlation entail? Provide a graphical representation as well.
cor(calcio.complete$B365H,calcio.complete$B365A)
plot(calcio.complete$B365H,calcio.complete$B365A)


# 2. For this problem we wil be working with the data set 'iris'.
# 2.1 Read about the data set in the help, load and explore the data set.
data("iris")
help("iris")
class(iris)
str(iris)
summary(iris)
View(iris)

# 2.2 Use at least 3 mathematical functions on individual columns of the data
#     set to confirm at least 3 statistics you see when using summary.
min(iris$Sepal.Length)
median(iris$Sepal.Length)
mean(iris$Sepal.Length)

# 2.3 Using apply() on a subset of the data frame, find the average of the first
#     four columns and confirm the averages you see when you using summary.
apply(iris[,1:4], 2, mean)

# 2.4 Using character functions:
# i. Get a numeric vector each element of which is a count of the number of
#    letters in each column name("Sepal.Length" should be 12)
vector1 <- nchar(names(iris))
vector1
# ii. Get a character vector each element of which is the associated column
#     name without the first and the last letter.
vector2 <- substr(names(iris), start = 2, stop = vector1)
vector2

# 2.5 Write a function named 'AVG' as follows.
myfunction(iris)
myfunction <- function(arg1) 
  for (i in 1:length(names(arg1))) 
  if (is.numeric(arg1$names(arg1)[i])) print(mean(arg1[,1])) else print("Not Numeric")
myfunction(iris)

# 2.6 Using the 'reshape2' package reshape the iris data set using melt()
#     and dcast() to create a data frame of averages taht looks like.
install.packages("reshape2")
library(reshape2)
aggdata <- aggregate(iris, by = list(iris$Species), FUN = mean, na.rm = TRUE)
View(aggdata)
md <- t(aggdata)
View(md)

# 3.1 For this problem we will continue to explore the 'iris' data set.

data(iris)

# Plot 1. Make a barplot of the column representing Sepal Length.
View(iris)
barplot(iris$Sepal.Length)

# Plot 2. Cut the Sepal Length column into three.
counts <- table(iris$Sepal.Length, iris$Species)
counts
barplot(counts)
print("setosa")

# Plot 3. Creating a Mean Bar Plot.
par(mfrow = c(2,2))
means_SepalLength <- aggregate(iris$Sepal.Length, 
                               by = list(iris$Species),
                               FUN = mean)
means_SepalLength
means_SepalLength <- means_SepalLength[order(means_SepalLength$x),]
barplot(means_SepalLength$x)

# Plot 4. Fan Plot.
dev.off()
means_PetalLength <- aggregate(iris$Petal.Length,
                               by = list(iris$Species),
                               FUN = mean)
means_PetalLength
library(plotrix)
fan.plot(means_PetalLength$x)

# Plot 5. Histogram.
hist(iris$Sepal.Length, breaks = 20, col = "blue")
rug(jitter(iris$Sepal.Length))

# Plot 6. Kernal Density.
plot(density(iris$Petal.Width))
rug(iris$Petal.Width, col = "blue")

# Plot 7. Comparative kernel density plots.
install.packages("sm")
library(sm)
attach(iris)
sm.density.compare(Petal.Width, Species)
legend(locator(1), levels(Species))

# Plot 8. Parallel box plots.
boxplot(iris$Petal.Width ~ iris$Species)

# Plot 9. Box plots for two crossed factors.

# Plot 10. Dot plots.
newdata <- iris[order,(iris$Petal.Width),]
newdata$Speciesl <- factor(newdata$Species)
newdata$color[newdata$Species == "setosa"] <- "red"
newdata$color[newdata$Species == "versicolor"] <- "blue"
newdata$color[newdata$Species == "virginica"] <- "darkgreen"

dotchart(newdata$Petal.Width, labels = row.names(newdata),
        groups = newdata$Speciesl, gcolor = "black", color = newdata$color)
