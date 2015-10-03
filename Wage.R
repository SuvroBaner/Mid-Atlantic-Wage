# Data Set : Wage (Mid-Atlantic Wage Data)

# Mid-Atlantic region : This region often includes 
# New York, New Jersey, Pennsylvania, Delaware, Maryland, Washington D.C, Virginia and West Virginia

# Description: Wage and other data for a group of 3000 workers in the Mid-Atlantic region

# Source : Data was manually assembled by Steve Miller, of Open BI (www.openbi.com), from the 
# March 2011 Supplement to Current Population Survey data.
# URL : http://thedataweb.rm.census.gov/TheDataWeb

# Data:  3,000 observations (workers )and 12 variables
# a) year : Year that wage information was recorded
# b) age : Age of worker
# c) sex : Gender
# d) maritl : A factor indicating marital status with levels 
#	1. Never Married 2. Married 3. Widowed 4. Divorced 5. Separated
# e) race : A factor indicating race with levels 1. White 2. Black 3. Asian and 4. Other
# f) education : A factor indicating education level with levels
#	1. < HS Grad 2. HS Grad 3. Some College 4. College Grad 5. Advanced Degree
# g) region : Region of the country (mid-atlantic only)
# h) jobclass : A factor indicating type of job with levels 1. Industrial and 2. Information
# i) health : A factor indicating health level of worker with levels 1. <=Good and 2. >=Very Good
# j) health_ins : A factor indicating whether worker has health insurance with levels 1. Yes and 2.
# k) logwage : Log of workers wage
# l) wage : Workers raw wage in thousands (RESPONSE VARIABLE)

install.packages("ISLR")  # to access the "Wage" data set.
install.packages("gam")  # to access General Additive Models
install.packages("akima")  # to do interaction term with GAMs
install.packages("glmnet")  # to do k-fold cross validation.
install.packages("MASS")
install.packages("boot") # to do cv.glm()

library(ISLR)
library(splines)
library(gam)
library(akima)
library(glmnet)
library(MASS)
library(boot)

attach(Wage)
dim(Wage)
names(Wage)
fix(Wage)
summary(Wage)

#################### Task - 1, What is the relationship b/w the wage and the age of an employee ? ##############
####################  (Regression Problem) ###################################

###### a) Fit the polynomial regression   #####

fit = lm(wage ~ poly(age, 4), data = Wage)
summary(fit)

# Here in the results the columns are based on the orthogonal polynomials which essentially means that each column
# is a linear combination of the variables. e.g. poly(age, 4)2 ===>>> age + age^2

fit2 = lm(wage ~ poly(age, 4, raw = TRUE), data = Wage)  # it takes the raw value
summary(fit2)

# Here using raw = TRUE returns the variables (age, age^2, age^3, age^4) directly and separately and not a linear combination.

#Few other ways of doing it-

########################################################################################

fit3 = lm(wage ~ age + I(age^2) + I(age^3) + I(age^4), data = Wage)  # gives you the raw variable estimates
summary(fit3)

fit4 = lm(wage ~ cbind(age, age^2, age^3, age^4), data = Wage)
summary(fit4)

#########################################################################################

### We want to predict the "Wage" for a range of "Age" ###

### Method 1: Using the orthogonal polynomial regression fit, which is here "fit"

agelims = range(age)  # the range is from 18 years to 80 years
age.grid = seq(from = agelims[1], to = agelims[2])

preds = predict(fit, newdata = list(age = age.grid), se = TRUE)
names(preds)

se.bands = cbind(preds$fit + 1.96*preds$se.fit, preds$fit - 1.96*preds$se.fit)  # setting the 95 % confidence interval

wage.predict.data.frame = data.frame(age.grid, se.bands)

# Now let's plot the data and add the orthogonal polynomial model fit.

par(mfrow = c(1, 2), mar = c(4.5, 4.5, 1, 1), oma = c(0, 0, 4, 0))
plot(age, wage, xlim = agelims, cex = .5, col = "darkgrey")

lines(age.grid, preds$fit, lwd = 2, col = "blue")
matlines(age.grid, se.bands, lwd = 1, col = "blue", lty = 3)

### Method 2: Using the raw regression estimates, which is here "fit2"

preds2 = predict(fit2, newdata = list(age = age.grid), se = TRUE)

se.bands = cbind(preds2$fit + 1.96*preds2$se.fit, preds2$fit - 1.96*preds2$se.fit)

plot(age, wage, xlim = agelims, cex = .5, col = "darkgrey")

lines(age.grid, preds2$fit, lwd = 2, col = "blue")
matlines(age.grid, se.bands, lwd = 1, col = "blue", lty = 3)

title("Degree-4 Orthogonal and Raw Polynomial Model Fits", outer = T)

#### Now, let's fit models ranging from linear to a degree-4 polynomial and seek to determine the simplest model which is sufficient
# to explain the relationship between wage and age.

fit.1 = lm(wage ~ age, data = Wage)
fit.2 = lm(wage ~ poly(age, 2), data = Wage)
fit.3 = lm(wage ~ poly(age, 3), data = Wage)
fit.4 = lm(wage ~ poly(age, 4), data = Wage)

# Now in order to choose b/w the models, we'll use ANOVA.
anova(fit.1, fit.2, fit.3, fit.4)

# the null hypothesis that a model M1 is sufficient to explain the data against
# the altrenative hypothesis that a more complex model M2 is required.

# The result gives that M2 or M3 might be required. M1 (which is of degree 1 is not sufficient)

# Let's also check using the K-fold cross validation technique:

cv.errors = rep(NA, 4)

for (i in 1:4)
{
  glm.fit = glm(wage ~ poly(age, i), data = Wage)
  cv.errors[i] = cv.glm(Wage, glm.fit, K = 10)$delta[1]
}

cv.errors

# [1] 1676.528 1600.428 1596.775 1594.939

# Looking at these errors we see that there is a significant drop of the errors at degree = 2 and 3
# this is inline with our anova() testing.

plot(1:4, cv.errors, xlab = "Number of degrees", ylab = "CV error", type = "l")

# From the plot we can see that, from 1 to 2 there is a subsequent drop and the drop continued further from 2 to 3.

# So, let's build this model of degree 2 and degree 3
# Also let's do the model with degree 1 to compare

lm.fit = lm(wage ~ age, data = Wage)
summary(lm.fit)

#            Estimate Std. Error t value Pr(>|t|)    
#(Intercept) 81.70474    2.84624   28.71   <2e-16 ***
#age          0.70728    0.06475   10.92   <2e-16 ***

# Here with age increases by 10 years, the wage would on an average increase by about $ 7,000 (note the wage was in thousands)

lm.fit2 = lm(wage ~ poly(age, 2, raw = TRUE), data = Wage)  # we'll deal with the raw estimates for better interpretation.
summary(lm.fit2)

# By computing the marginal effects (i.e. using partial differentiation ) we see that with every 10 years increase in age,
# the wage is increased by $4,230 on an average. Comparing with the degree-1 model it is less as the Wage increase is not
# constant at all stages of life (age.)

lm.fit3 = lm(wage ~ poly(age, 3, raw = TRUE), data = Wage)
summary(lm.fit3)

# Here for polynomial-3, it could be seen that with increase in age by 10 years the wage increases by $ 7,000 which is 
# very similar to the linear fit. This might not be true. Let's plot these.

agelims = range(age)
age.grid = seq(from = agelims[1], to = agelims[2])

pred.lm.fit = predict(lm.fit, newdata = list(age = age.grid), se = TRUE)
pred.lm.fit2 = predict(lm.fit2, newdata = list(age = age.grid), se = TRUE)
pred.lm.fit3 = predict(lm.fit3, newdata = list(age = age.grid), se = TRUE)

se.bands.pred = cbind(pred.lm.fit$fit + 1.96*pred.lm.fit$se.fit, pred.lm.fit$fit - 1.96*pred.lm.fit$se.fit)
se.bands.pred.2 = cbind(pred.lm.fit2$fit + 1.96*pred.lm.fit2$se.fit, pred.lm.fit2$fit - 1.96*pred.lm.fit2$se.fit)
se.bands.pred.3 = cbind(pred.lm.fit3$fit + 1.96*pred.lm.fit3$se.fit, pred.lm.fit3$fit - 1.96*pred.lm.fit3$se.fit)

par(mfrow = c(1, 3), mar = c(4.5, 4.5, 1, 1), oma = c(0, 0, 4, 0))

plot(age, wage, xlim = agelims, cex = .5, col = "darkgrey")
title("Degree 1")
lines(age.grid, pred.lm.fit$fit, lwd = 2, col = "blue")
matlines(age.grid, se.bands.pred, lwd = 1, col = "blue", lty = 3)

plot(age, wage, xlim = agelims, cex = .5, col = "darkgrey")
title("Degree 2")
lines(age.grid, pred.lm.fit2$fit, lwd = 2, col = "blue")
matlines(age.grid, se.bands.pred.2, lwd = 1, col = "blue", lty = 3)

plot(age, wage, xlim = agelims, cex = .5, col = "darkgrey")
title("Degree 3")
lines(age.grid, pred.lm.fit3$fit, lwd = 2, col = "blue")
matlines(age.grid, se.bands.pred.3, lwd = 1, col = "blue", lty = 3)

# Our knowledge and the pattern says that the Degree 2 is a good fit.


### Perform polynomial regression to predict wage using age and use cross-validation to select the optimal degree d for the polynomial.

set.seed(11)

all.deltas = rep(NA, 10)

for(i in 1:10)  # we are doing a 10-fold cross validation.
{
	glm.fit = glm(wage ~ poly(age, i), data = Wage)
	all.deltas[i] = cv.glm(Wage, glm.fit, K = 10)$delta[2]
}

plot(1:10, all.deltas, xlab = "Degrees", ylab = "CV.Error", type = "l", pch = 20, lwd = 2, ylim = c(1590, 1700))
min.point = min(all.deltas)
sd.points = sd(all.deltas)
abline(h = min.point + 0.2 * sd.points, col = "red", lty = "dashed")
abline(h = min.point - 0.2 * sd.points, col = "red", lty = "dashed")
legend("topright", "0.2-standard deviation lines", lty = "dashed", col = "red")


## The cv-plot with standard deviation lines show that d = 6 is the smallest degree giving reasonably small cross-validation error

fit.1 = lm(wage ~ poly(age, 1), data = Wage)
fit.2 = lm(wage ~ poly(age, 2), data = Wage)
fit.3 = lm(wage ~ poly(age, 3), data = Wage)
fit.4 = lm(wage ~ poly(age, 4), data = Wage)
fit.5 = lm(wage ~ poly(age, 5), data = Wage)
fit.6 = lm(wage ~ poly(age, 6), data = Wage)
fit.7 = lm(wage ~ poly(age, 7), data = Wage)
fit.8 = lm(wage ~ poly(age, 8), data = Wage)
fit.9 = lm(wage ~ poly(age, 9), data = Wage)
fit.10 = lm(wage ~ poly(age, 10), data = Wage)

anova(fit.1, fit.2, fit.3, fit.4, fit.5, fit.6, fit.7, fit.8, fit.9, fit.10)

# We see that all polynomials above degree 2 are statistically insignificant

# We now plot the polynomial prediction on the data.


plot(wage ~ age, data = Wage, col = "darkgrey")
agelims = range(Wage$age)
age.grid = seq(from = agelims[1], to = agelims[2])
lm.fit = lm(wage ~ poly(age, 2), data = Wage)
lm.pred = predict(lm.fit, data.frame(age = age.grid))
lines(age.grid, lm.pred, col = "blue", lwd = 2)


###################### Regression problem WITHOUT the polynomial fits ###############################

##### a) Step function : Where the predictor is divided into various domains

table(cut(age, 4))

# Here cut() automatically picked the cutpoints 33.5, 49 and 64.5 years of age.
# They are the ordered categorical variable, lm() creates a set of dummy variables for use in the regression.

fit = lm(wage ~ cut(age, 4), data = Wage)
coef(summary(fit))

#                         Estimate Std. Error   t value     Pr(>|t|)
#(Intercept)            94.158392   1.476069 63.789970 0.000000e+00
#cut(age, 4)(33.5,49]   24.053491   1.829431 13.148074 1.982315e-38
#cut(age, 4)(49,64.5]   23.664559   2.067958 11.443444 1.040750e-29
#cut(age, 4)(64.5,80.1]  7.640592   4.987424  1.531972 1.256350e-01


# Here, the average salary for those under 33.5 years of age is $ 94,158.
# For people b/w 33.5 and 49 years the average additional salary is $ 24,000.

# Fit a step function to predict wage using age, and perform cross-validation to choose the optimal number of cuts
# Make a plot of the fit obtained.


# We use cut points up to 10

all.cvs = rep(NA, 10)
for(i in 2:10)   # there has to be atleast two intervals for cut() to work.
{
	Wage$age.cut = cut(Wage$age, i)
	lm.fit = glm(wage ~ age.cut, data = Wage)
	all.cvs[i] = cv.glm(Wage, lm.fit, K = 10)$delta[2]
}
plot(2:10, all.cvs[-1], xlab = "Number of cuts", ylab = "CV error", type = "l", pch = 20, lwd = 2)

# The cross validation shows that test error is minimum for k = 8 cuts.

lm.fit = glm(wage ~ cut(age, 8), data = Wage)
agelims = range(Wage$age)
age.grid = seq(from = agelims[1], to = agelims[2])
lm.pred = predict(lm.fit, data.frame(age = age.grid))
plot(wage ~ age, data = Wage, col = "darkgrey")
lines(age.grid, lm.pred , col = "red", lwd = 2)


###### b) Splines : Generates an entire matrix of basis functions.

fit = lm(wage ~ bs(age, knots = c(25, 40, 60)), data = Wage)

# bs() generates the entire matrix of basis functions for splines with the specified set of knots.
# By default a cubic spline is produced.

# Over here, we have prespecified knots at ages 25, 40 and 60. This poduces a spline with 6 basis functions.
# A cubic spline with 3 knots has 7 degrees of freedom; these degrees of freedom are used up by an intercept
# plus six basis functions.

dim(bs(age, knots = c(25, 40, 60)))  # also gives you the number of basis functions, i.e. 6

# Now let's fit natural spline, with 4 degrees of freedom.

fit2= lm(wage ~ ns(age, df = 4), data = Wage)
attr(ns(age, df = 4), "knots")
# Here R choses knowts at ages 33.9 (25th percentile of age), 42 (50th percentile) and 51 (75th percentile) of age.

summary(fit2)

pred2 = predict(fit2, newdata = list(age = age.grid), se = T)

plot(age, wage, xlim = agelims, cex = .5, col = "darkgrey")
lines(age.grid, pred2$fit, col = "red", lwd = 2)
title("Smoothing Spline")

fit = smooth.spline(age, wage, df = 16)
fit2 = smooth.spline(age, wage, cv = TRUE)
fit2$df

lines(fit, col = "red", lwd = 2)
lines("topright", legend = c("16 DF", "6.8 DF"), col = c("red", "blue"), lty = 1, lwd = 2, cex = .8)

# To, perform local regression.

plot(age, wage, xlim = agelims, cex = .5, col = "darkgrey")
title("Local Regression")
fit = loess(wage ~ age, span = .2, data = Wage)
fit2 = loess(wage ~ age, span = .5, data = Wage)
lines(age.grid, predict(fit, data.frame(age = age.grid)), col = "red", lwd = 2)
legend("topright", legend = c("Span = 0.2", "Span = 0.5"), col = c("red", "blue"), lty = 1, lwd = 2, cex = 0.8)

###################### General Additive Models (GAM) ##########################

# Generalized additive models (GAMs) provide a general framework for extending a standard linear model 
# by allowing non-linear functions of each of the variables, while maintaining additivity.

### We now fit a GAM to predict wage using natural spline functions of year and age, treating education as a qualitative predictor.

gam1 = lm(wage ~ ns(year, 4) + ns(age, 5) + education, data = Wage)
summary(gam1)

# Let us do it using a smoothing splines, we'll use the gam library in R.
# The s(), is used to indicate that we would like to use a smoothing spline.

gam.m3 = gam(wage ~ s(year, 4) + s(age, 5) + education, data = Wage)
par(mfrow = c(1, 3))
plot(gam.m3, se = TRUE, col = "blue")

plot.gam(gam1, se = TRUE, col = "red")

gam.m1 = gam(wage ~ s(age, 5) + education, data = Wage)
gam.m2 = gam(wage ~ year + s(age, 5) + education, data = Wage)
anova(gam.m1, gam.m2, gam.m3, test = "F")

# There is a compelling evidence that a GAM with a linear function of year is better than a GAM that does not
# include year at all. The p-value is 0.0001447
# However, there is no evidence that a non-linear function of year is needed (p-value : 0.3485661)
# Based on the result of this ANOVA, M2 is preferred.

attr(ns(year, 4), "knots")  # this helps you to interpret the model
attr(ns(age, 5), "knots")   # this helps you to interpret the model

summary(gam.m3)

# The p-values of year and age correspond to a null hypothesis of a linear relationship v/s the alternative relationship.
# The large p-value for year, reinforces our conclusion from the ANOVA test that a linear function is adequate for this term.
# However, there is a very clear evidence that a non-linear term is required for age.

preds = predict(gam.m2, newdata = Wage)
gam.lo.i = gam(wage ~ lo(year, age, span = 0.5) + education, data = Wage)  # lo() creates an interaction.

plot(gam.lo.i)


# To fit a logistic GAM.

gam.lr = gam(I(wage > 250) ~ year + s(age, df = 5) + education, family = binomial, data = Wage)
par(mfrow = c(1, 3))
plot(gam.lr, se = T, col = "green")

# It is easy to see that there are no high earners in the <HS category.

table(education, I(wage > 250))

# Hence we fit the logistic regression GAM using all but this category. This provides more sensible results.

gam.lr.s = gam(I(wage > 250) ~ year + s(age, df = 5) + education, family = binomial, data = Wage, subset = (education != "1. <HS Grad"))
plot(gam.lr.s, se = T, col = "green")

########## Predict whether an individual earns more than $ 250,000 per year based on the age ################
##################  Classification Problem ###############

# As this is a classification problem, so we'll fit a polynomial logistic regression model.

fit = glm(I(wage > 250) ~ poly(age, 4), data = Wage, family = binomial)

# We use wrapper I() to create a binary response variable on the fly.
# Here, the expression wage > 250 evaluates to a logical variable containing TRUEs and FALSEs, where TRUEs to 1 and FALSEs to 0.

preds = predict(fit, newdata = list(age = age.grid), se = T)

# NOTE: I have removed type = "response" from the predict function, so it only gives the prob of logit and not the response variable.

# Note, this is the predictions for the logit, which is the default prediction type for glm() called type = "link"
# So, let's compute for p(x), i.e. p(wage > 250)

pfit = exp(preds$fit) / (1 + exp(preds$fit))
se.bands.logit = cbind(preds$fit + 1.96*preds$se.fit, preds$fit - 1.96*preds$se.fit)
se.bands = exp(se.bands.logit) / (1 + exp(se.bands.logit))

plot(age, I(wage > 250), xlim = agelims, type = "n", ylim = c(0, .2))  # the y-axis is the probability i.e. p(wage > 250)
points(jitter(age), I((wage > 250) / 5), cex = 0.5, pch = "|", col = "darkgrey")  # rug plot
lines(age.grid, pfit, lwd = 2, col = "blue")
matlines(age.grid, se.bands, lwd = 1, col = "blue", lty = 3)
