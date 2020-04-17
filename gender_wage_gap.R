# R code Gender Wage Gap


# Clear workspace
rm(list=ls())

# Load xtable library to print table in .text format
library(xtable)

# Set Directory
setwd("")

# Load Dataset and see variables and the number of observations.
load(file="data/pay.discrimination.Rdata")
str(data)
dim(data)

# Attach dataset to current workspace.
attach(data)

# Compute basic stats:
stats.female  <- as.matrix(apply(data[female==1,], 2, mean))
stats.male    <- as.matrix(apply(data[female==0,], 2, mean))
stats         <- cbind(stats.male, stats.female)

# Print basic stats
colnames(stats) = c("male averages", "female averages")
xtable(stats)


############################ Linear Regression ############################

# Wage linear regression
fmla1     <- wage ~  female + sc+ cg+ mw + so + we + exp1 + exp2 + exp3


# Run OlS regression, get coefficients, standard errors and 95% confidence interval
full.fit1 <- lm(fmla1, data=data)
est1      <- summary(full.fit1)$coef[2,1:2]
ci1       <-  confint(full.fit1)[2,]

# Linear regression: Quadratic specification
fmla2     <-  wage ~  female + (sc+ cg+ mw + so + we + exp1 + exp2 + exp3)^2

# Run OlS regression, get coefficients, standard errors and 95% confidence interval
full.fit2 <- lm(fmla2, data=data)
est2      <- summary(full.fit2)$coef[2,1:2]
ci2       <- confint(full.fit2)[2,]


#Create table to store regression results
table1     <- matrix(0, 2, 4)
table1[1,] <- c(est1,ci1)
table1[2,] <- c(est2,ci2)


#Give column and  row names
colnames(table1) <- c("Estimate", "Standard Error", "Lower Conf. Bound", "Upper Conf. Bound")
rownames(table1) <- c("basic reg", "flex reg")


############################ Illustration of Partialling Out: (Linear Specification)    ############################

# Linear regression of y (outcome) on covariates
fmla1.y <- wage ~  sc+ cg+ mw + so + we + exp1 + exp2 + exp3

# Linear regression of d (treatment) on covariates
fmla1.d <- female ~  sc+ cg+ mw + so + we + exp1 + exp2 + exp3

# Residuals of outcome regression
t.Y    <- lm(fmla1.y, data=data)$res

# Residuals of treatment regression
t.D    <-  lm(fmla1.d, data=data)$residuals


# Run OLS coefficient get coefficients and 95% confidence intervals
partial.fit1   <- lm(t.Y~t.D)
partial.est1   <- summary(partial.fit1)$coef[2,1:2]
partial.ci1    <- confint(partial.fit1)[2,]



############################ Illustration of Partialling Out: (Quadratic Specification)    ############################

fmla2.y  <- wage ~  (sc+ cg+ mw + so + we + exp1 + exp2 + exp3)^2
fmla2.d  <- female ~ (sc+ cg+ mw + so + we + exp1 + exp2 + exp3)^2

# get residuals from linear regression
t.Y  <- lm(fmla2.y, data=data)$res
t.D  <- lm(fmla2.d, data=data)$res

# regress residuals one onether to get result from partialled out regression
partial.fit2  <-  lm(t.Y~t.D)
partial.est2  <-  summary(partial.fit2)$coef[2,1:2]
partial.ci2   <-  confint(partial.fit2)[2,]


#Create table to store regression results
table2     <- matrix(0, 4, 2)
table2[1,] <- c(est1)
table2[2,] <- c(est2)
table2[3,] <- c(partial.est1)
table2[4,] <- c(partial.est2)

#Give column and row names
colnames(table2) <- c("Estimate", "Standard Error")
rownames(table2) <- c("basic reg", "flex reg", "basic reg with partialling out", "flex reg with partialling out")

#Print results 
print(table1, digits=3)
print(table2, digits=3)


