# Data Analysis_Focus on Principal Components and factor analysis
# by Ran Gao on 28/11/2018 

rm(list=ls())
setwd('D:/')
options(digits=7, scipen=0)
opar <- par(no.readonly=TRUE)

# Dataset Wine.

# (1) Load, explore and prepare your data.
# Load the dataset according to the methods in the pdf.
wine <- read.table("http://archive.ics.uci.edu/ml/machine-learning-databases/wine/wine.data",
                   sep = ",")
colnames(wine) <- c("Cvs","Alcohol","Malic acid","Ash","Alcalinity of ash",
                    "Magnesium", "Total phenols", "Flavanoids", "Nonflavanoid phenols",
                    "Proanthocyanins", "Color intensity", "Hue", "OD280/OD315 of diluted wines",
                    "Proline")
# Explore the data.
View(wine)
summary(wine)
str(wine)

# (2) With an understanding of your goal, decide on PCA or EFA.
# In the discription of the data, it says that there are too many variables to explore
# and many measurements may be correlated. So it's natural to use PCA to do reduction.

# (3) I use PCA for this question, so i will leave this problem out.

# (4) Using fa.parallel(), decide on the number components to extract.
library(psych)
fa.parallel(wine[,-1], fa="pc", n.iter=100, show.legend=F, main="Screen plot with parallel analysis.")
# According to the parallel analysis and Kaisser-Harris criterion, pc=3 seems resonable.

# (5) Extract the components using principal() with rotate="none".
library(psych)
pc <- principal(wine[,-1], nfactors=3, rotate="none")
pc

# (6) Describe the following information based on the PCA analysis.
# PC1 is highly correlated with each variable except the Ash.
# PC2 is highly correlated with each variable except the Flavanoids.
# PC3 is not that highly correlated with each variable.

# According to h2, almost each variables is explained by these 3 PCs.
# However, the Malic acid, Magnesium, Nonflavanoid phenols is not that explained.

# According to the Proportion Var, 
# PC1 explains the 36% of the variance.
# PC2 explains the 19% of the variance.
# PC3 explains the 11% of the variance.

# (7) Rotate keeping the components orthogonal using principal() with varimax.
rc <- principal(wine[,-1], nfactors = 3, rotate = "varimax")
rc
# The components after purifying improves and the variance it explains more.

# (8) Compute your principal component scores.
#     Create a new dataframe with the original first column and your new component columns(s).
library(psych)
pc <- principal(wine[,-1], nfactors=3, score=T)
head(pc$scores)
cor(wine$Cvs,pc$scores)

# 1.2 Dataset Harman74.cor from the dataset library.
library(datasets)

# (1) Load, explore and prepare your data.
data("Harman74.cor")
View(Harman74.cor)
summary(Harman74.cor)

# (2) With an understanding of your goal, decide on PCA or EFA.
# Focus on the EFA according to the description of the dataset.

# (3) Choose a specific factoring method.
# Choose to use the 'maximum likelihood' approach.

# (4) Using fa.parallel() decide on the number components to extract.
library(psych)
fa.parallel(Harman74.cor$cov, n.obs=145, fa="both", n.iter=100,
            main="Scree plots with parallel analysis.", show.legend=F)
# According to the parallel analysis, try to extract 4 components.

# (5) Extract the components using fa() with rotate="none".
fa <- fa(Harman74.cor$cov, nfactors=4, rotate="none", fm="ml")
fa

# (6) Descrie the outcomes.
# ML1 is highly correlated with each variable.
# ML2-ML4 is not that correlated with each variable.

# Also, the overall variance explained is about 0.5, not that good.

# (7) Rotate keeping the components orthogonal with fa.varimax().
fa.varimax <- fa(Harman74.cor$cov, nfactors=4, rotate="varimax", fm="ml")
fa.varimax

# (8) Compute the factor scores.
fa.varimax$weights