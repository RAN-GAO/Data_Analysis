# Data Analysis_Permutation Test
# by Ran Gao on 28/11/2018 


rm(list=ls())
setwd('D:/')
options(digits=7, scipen=0)
opar <- par(no.readonly=TRUE)

# 1. For this problem we begin with the dataset 'Loblolly'.

# 1.1 Scatter plots.
# 1.1.1 Load and explore the dataset 'Loblolly'.
data("Loblolly")
View(Loblolly)
help("Loblolly")
summary(Loblolly)
str(Loblolly)

# 1.1.2 Create a subset of the dataframe, Lb305, 
#      with only the seed source "305".
Lb305 <- subset(Loblolly, Seed == '305')
View(Lb305)

# 1.1.3 Plot a basic scatter plot of age vs height.
attach(Lb305)
plot(age, height, main = "Basic Scatter Plot of Age vs Hight",
     xlab = "age",
     ylab = "height")

# 1.1.4 Add a linear line of best fit.
abline(lm(height~age), col='red', lwd=2, lty=1)

# 1.1.5 Add a lowess smoothed line.
lines(lowess(age, height), col='blue', lwd=2, lty=1)

# 1.1.6 What does the graph tell you.
# It shows that there exists a positive relationship between age and height.

# 1.2 Bubble Plots.
# Create a bubble plot for the entire Loblolly data set plotting age vs height
# where the circle radii are based on the seed number of the plotted point.
attach(Loblolly)
r <- sqrt(Seed/pi)
symbols(height, age, circle=r, inches=0.30,
        fg="white", bg="lightblue",
        main="Bubble Plot with point size proportional to Seed",
        xlab="age",
        ylab="height")
detach(Loblolly)

# 1.3 Line Charts.
# Use the dataset 'Loblolly' create a line chart displaying the
# height as age progresses, with a seperate line for each seed number.
plot(Loblolly$age, Loblolly$height,
     xlab="Age", ylab="Height",
     type="b")

# 1.4 Scatter-plot matrices.
# 1.4.1 Using the dataset 'trees' create a scatterplot matrix
#       from the 'car' package.
data("trees")
View(trees)
library(car)
scatterplotMatrix(~Girth+Height+Volume, data=trees, 
                  spread=FALSE, smoother.args=list(lty=2),
                  main="Scatter Plot Matrix via car Package.")

# 1.4.3 Compare the Girth, Height and Volume pairwise and tell us 3 insights you see.
# 1. Grith and Volume is positively related.
# 2. Height and Volume is positively related.
# 3. Grith and Height is positively related but not that significantly.

# 1.5 High-density scatter plots.
# 1.5.1 Use the dataset 'diamonds'£»load and explore the data.
library(ggplot2)
data("diamonds")
View(diamonds)
str(diamonds)
summary(diamonds)

# 1.5.2 Plot a standard plot of carat vs price.
attach(diamonds)
plot(carat, price)

# 1.5.3 Now plot the same comparison with hexbin().
library(hexbin)
with(diamonds, {
  bin <- hexbin(carat, price, xbins=50)
  plot(bin, main="Hexagonal Binning with 10,000 Observations.")
  })

# 1.5.4 Narrow your view by ploting with hexbin() again
# but this time on a subset of the data where carat is less than .7
newdata <- subset(diamonds, diamonds$carat < 0.7)
library(hexbin)
with(newdata, {
  bin <- hexbin(carat, price, xbins=50)
  plot(bin, main="Hexagonal Binning with 10,000 Observations.")
})

# 1.6 Corrgrams.
# 1.6.1 Use the dataset "baseball" from the corrgram package.
library(corrgram)
data("baseball")

# 1.6.2 Read about the dataset in the help and explore the data.
help("baseball")
View(baseball)
summary(baseball)
str(baseball)

# 1.6.3 Use the 5 columns from the dataset.
newbaseball <- baseball[c("Homer","Years","Atbatc","Hitsc","Salary")]

# 1.6.4 Create a corrgram with shaded boxes in the bottom left and actual correlation values in the upper right.
library(corrgram)
corrgram(newbaseball, order=T, lower.panel = panel.shade,
         upper.panel = panel.pie)

# 1.7 Mosaic plots.
# 1.7.1 Use the dataset "HairEyeColor"
data("HairEyeColor")

# 1.7.2 Read about and explore the data.
View(HairEyeColor)
summary(HairEyeColor)

# 1.7.3 Create a mosaic() plot.
library(vcd)
mosaic(HairEyeColor, shade=T, legend=T)

# 2. For most of this problem we will be revisiting problems we worked on dataset "cars".
# Permutation Tests.
# 2.1 we used the chi-squared test to check if two categorical variables were independent.
library(openintro)
data("cars")
mytable2 <- table(cars$driveTrain, cars$type)
chisq.test(mytable2)

# 2.1.1 Use the permutation test to assess the independence.
library(coin)
chisq.test(mytable2)

# 2.1.2 Briefly discuss your comparison of these results to those obtained previously.
# The two different methods show the same results.

# 2.2 we used an independent t-test to determine whether there was a statistically significant difference.
data("cars")
carsFR <- subset(cars, driveTrain=='front' | driveTrain=='rear')
library(coin)
t.test(carsFR$mpgCity, carsFR$driveTrain, var.equal=T)
oneway_test(mpgCity~driveTrain, data=carsFR, distribution="exact")

# 2.3  For the multiple linear regression.
# 2.3.1 Use the permutation test version, repeating the regression using lmp().
library(car)
carsSub <- cars[c("price","mpgCity","weight")]
library(lmPerm)
fit <- lmp(mpgCity~price + weight, data=carsSub, perm="Prob")
summary(fit)

# 2.3.2 Briefly discuss your comparison of these results to those obtained previously.
# The R-squared improves and the significance of the hypothesis test improves.


# 2.4 ANOVA to test for group differences.
# 2.4.1 Perform the ANOVA via permutation tests using aovp().
data("PlantGrowth")
table(PlantGrowth$group)
library(lmPerm)
library(multcomp)
fit <- aovp(PlantGrowth$weight~PlantGrowth$group, perm="Prob")
summary(fit)

# 2.4.2 Briefly discuss your comparison of these results to those obtained previously.
# It shows apparently that the results under permutation test is better.

# 2.5 Now we will step out of permutation tests and into bootstrapping.
# For this problem we will use the data set "Seatbelts".
# 2.5.1 Load and explore the dataset 'Seatbelts.'
data("Seatbelts")
View(Seatbelts)

# 2.5.2 Create bootstrap statistics.
library(boot)
set.seed(1234)
results_mean <- boot(data=Seatbelts, statistic = mean(Seatbelts$front),
                R = 1000)
results_median <- boot(data=Seatbelts, statistic = median(Seatbelts$kms),
                R = 1000)
results_sd <- boot(data=Seatbelts, statistic = stderr(Seatbelts$PetrolPrice),
                R = 1000)

# 2.5.3 Using boot.ci() find the confidence intervals for these statistics.
boot.ci(results_mean,type="bca")
boot.ci(results_median,type="bca")
boot.ci(results_sd,type="bca")