# Data Analysis_General Linear Models
# by Ran Gao on 20/11/2018 


rm(list=ls())
setwd('D:/')

# 1. In this problem we will use the data frame 'debt'.
# 1. Consider the regression model for studying the association of the probability that a mortgage will be renegotiated with its residual duration, the residual amount to pay and the working position of the borrower.
# 1.1 Check if for a self-employed borrower with a residual debt equal to 40 (thousand euros) the probability that the mortgage will be renegotiated is associated with the residual duration of the mortgage.
model1 <- glm(REVIEW ~ ., family = binomial(logit), data = debt)
summary(model1)
# The hypothesis compared by the Wald test are:H0: bDUR = 0 vs. H1: bDUR 1 0.
# The test statistic is given by 3.978 and the p-value by 6.95E-5, so we reject the null hypothesis.

# 1.2 Check if the probability to be renegotiated for a mortgage with a residual amount of 40 (thousand euros) and 60 months of residual duration is associated with the borrower¡¯s working position
model1b <- glm(REVIEW ~ RES_DEBT + DUR, family = binomial(logit), data = debt)
anova(model1b, model1, test = "Chisq")
# The likelihood ratio test is: H0: bWORKSE = bWORKO = 0 vs. H1: at least one of the coefficients is different from zero.
# The test statistic value is 20.034 and the p-value of the test is 4.464E-5. 
# we reject the null hypothesis concluding that the WORK variable is overall significant

# 1.3 Consider now a new regression model for the same problem that distinguishes only between selfemployed borrowers and all the others. 
#     The model must also include the effect of the residual mortgage duration and the possibility that this effect changes according to the working position recoded as described above.
debt$WORK_NEW <- ifelse(debt$WORK == "SE", 1, 0)
model2 <- glm(REVIEW ~ DUR*WORK_NEW, family = binomial(logit), data = debt)
summary(model2)

# 1.4 Assess if this model is globally significant.
model2b <- glm(REVIEW ~ 1, family = binomial(logit), data = debt)
anova(model2b, model2, test = "Chisq")

# 1.5 Using this model (i.e. model2), predict the odds to be renegotiated for a mortgage with 50 months of residual duration signed by a self-employed borrower.
xnew <- data.frame(DUR = 50, WORK_NEW = 1)
p <- predict(model2, newdata = xnew, type = "response")
p/(1 - p)


# 2. In this problem we will use the Trucks dataset from the VCD package.
# 2.1 First, let's look at summary statistics for the dataset.
summary(Trucks)

# 2.2 Let's look at the response variable in more detail.
opar <- par(no.readonly = T)
attach(Trucks)
hist(Freq)
par(opar)

# 2.3 The next step is to fit the Poisson regression.
fit <- glm(Freq ~ period+collision+parked+light, family = poisson())
summary(fit)

# 2.4 Interpret the model parameters.
coef(fit)
exp(coef(fit))

# 2.5 Test the ratio of the residual deviance to the residual degrees of freedom.
deviance(fit)/df.residual(fit)
