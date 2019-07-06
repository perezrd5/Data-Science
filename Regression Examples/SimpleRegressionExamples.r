

########################################################################
########################################################################
# Logistic regression example
########################################################################
########################################################################



# install.packages("aod")
# install.packages("ggplot2")
# install.packages("vctrs")
# install.packages("VGAM")
# install.packages("quantreg")

library(knitr)
library(vctrs)
library(aod)
library(ggplot2)

# Graduate school admissions data from UCLA
mydata <- read.csv("https://stats.idre.ucla.edu/stat/data/binary.csv")
# Saved at: \Consulting\Public Code Samples\DataScience\Data
# Alternate URL:  https://1drv.ms/u/s!AgXALnk1NWLck4JA_01l6gpD6pZxpQ


## view the first few rows of the data
head(mydata)
# The admit variable is a binary response variable
# We will use gre, gpa, and rank as predictor variables.
# gre and gpa are continuous; while rank is a four-level factor

# Here are some examinations of the data:
summary(mydata)
sapply(mydata, sd)
xtabs(~admit + rank, data = mydata)

# Factor the rank variable and create a logistic regretion using glm() - Generalized Linear Model
mydata$rank <- factor(mydata$rank)
mylogit <- glm(admit ~ gre + gpa + rank, data = mydata, family = "binomial")

# See regression results using summary
summary(mylogit)

# We can create confidence levels by log likelihood
confint(mylogit)
# or based on the standard errors:
confint.default(mylogit)

# The wald test will give us the overall effect of the rank variable on admit:
wald.test(b = coef(mylogit), Sigma = vcov(mylogit), Terms = 4:6)

# The chi-squared test statistic of 20.9 indicates that rank *is* statistically significant in predicting admit

# Let's focus on Rank by taking the mean of the other variables, breaking up by each factor level
newdata1 <- with(mydata, data.frame(gre = mean(gre), gpa = mean(gpa), rank = factor(1:4)))

## view data frame
newdata1

# Let's use predict() to see if we can model the rank variable
newdata1$rankP <- predict(mylogit, newdata = newdata1, type = "response")
newdata1

# In the above output we see that the predicted probability of being accepted into a 
# graduate program is 0.52 for students from the highest prestige undergraduate 
# institutions (rank=1), and 0.18 for students from the lowest ranked institutions 
# (rank=4), holding gre and gpa at their means. We can do something very similar to 
# create a table of predicted probabilities varying the value of gre and rank. 

# This is a technique for isolating the predictive effect of a single explanatory variable on the response

# We are going to plot these, so we will create 100 values of gre between 200 and 800,
# at each value of rank (i.e., 1, 2, 3, and 4).

newdata2 <- with(mydata, data.frame(gre = rep(seq(from = 200, to = 800, length.out = 100),
                                              4), gpa = mean(gpa), rank = factor(rep(1:4, each = 100))))



# The code to generate the predicted probabilities (the first line below) is 
# the same as before, except we are also going to ask for standard errors so we 
# can plot a confidence interval. 


# We get the estimates on the link scale and back transform both the predicted values
# and confidence limits into probabilities.

newdata3 <- cbind(newdata2, predict(mylogit, newdata = newdata2, type = "link",
                                    se = TRUE))
newdata3 <- within(newdata3, {
  PredictedProb <- plogis(fit)
  LL <- plogis(fit - (1.96 * se.fit))
  UL <- plogis(fit + (1.96 * se.fit))
})

## view first few rows of final dataset
head(newdata3)

# Below we make a plot with the predicted probabilities, and 95% confidence intervals.
ggplot(newdata3, aes(x = gre, y = PredictedProb)) + 
                  geom_ribbon(aes(ymin = LL, ymax = UL, fill = rank), alpha = 0.2) + 
                  geom_line(aes(colour = rank),size = 1)




# Some methods to test fit below
with(mylogit, null.deviance - deviance)
with(mylogit, df.null - df.residual)
with(mylogit, pchisq(null.deviance - deviance, df.null - df.residual, lower.tail = FALSE))

# The chi-square of 41.46 with 5 degrees of freedom and an associated p-value of 
# less than 0.001 tells us that our model as a whole fits significantly better 
# than an empty model. 

#To see the model's log likelihood
logLik(mylogit)


########################################################################
########################################################################
# Multinomial regression example
########################################################################
########################################################################

require(foreign)
require(nnet)
require(ggplot2)
require(reshape2)

# A data set showing occupational outcomes for vocational or academic educational paths
ml <- read.dta("https://stats.idre.ucla.edu/stat/data/hsbdemo.dta")
# Saved at: \Consulting\Public Code Samples\DataScience\Data\UCLAHighSchoolVocationalSurvey.dta
# Alternate URL:  https://1drv.ms/u/s!AgXALnk1NWLck4JDxYy1aZKct65nOw?e=7JBQzi

# Have a look at the data:
with(ml, table(ses, prog))
with(ml, do.call(rbind, tapply(write, prog, function(x) c(M = mean(x), SD = sd(x)))))

# Let's use relevel to create a new variable weighing the "academic" prog variable response
ml$prog2 <- relevel(ml$prog, ref = "academic")
test <- multinom(prog2 ~ ses + write, data = ml) # multinom() does not require reshaping like mlogit() does
summary(test)
# Test is a multinom() output of our model using the releveled program

# We run a couple of tests on the model:
z <- summary(test)$coefficients/summary(test)$standard.errors
z
# 2-tailed z test
p <- (1 - pnorm(abs(z), 0, 1)) * 2
p

## extract the coefficients from the model and exponentiate
exp(coef(test))

# Use predicted probabilities to help understand the model. Calculate predicted probabilities for each of 
# the outcome levels using the fitted() function. 
# 
# We can start by generating the predicted probabilities for the observations in our dataset and viewing 
# the first few rows:
  
  head(pp <- fitted(test))

# To examine the changes in predicted probability associated with one of our two 
# variables, we can create small datasets varying one variable while holding the 
# other constant. We will first do this holding write at its mean and examining 
# the predicted probabilities for each level of ses.

dses <- data.frame(ses = c("low", "middle", "high"), write = mean(ml$write))
predict(test, newdata = dses, "probs")

dwrite <- data.frame(ses = rep(c("low", "middle", "high"), each = 41), write = rep(c(30:70), 3))

## store the predicted probabilities for each value of ses and write
pp.write <- cbind(dwrite, predict(test, newdata = dwrite, type = "probs", se = TRUE))

## calculate the mean probabilities within each level of ses
by(pp.write[, 3:5], pp.write$ses, colMeans)

## melt data set to long for ggplot2
lpp <- melt(pp.write, id.vars = c("ses", "write"), value.name = "probability")
head(lpp)  # view first few rows

# Plot the predicted probabilities against the writing score using the predictions generated
# for the pp.write object above by the level of ses for different levels of the outcome variable.

ggplot(lpp, aes(x = write, y = probability, colour = ses)) + geom_line() + facet_grid(variable ~ ., scales = "free")

########################################################################
########################################################################
# Poisson regression example (Zero Truncated)
########################################################################
########################################################################

require(foreign)
require(ggplot2)
require(VGAM)
require(boot)


# Import data from hospital tracking patients who have died
dat <- read.dta("https://stats.idre.ucla.edu/stat/data/ztp.dta")
# Saved at: \Consulting\Public Code Samples\DataScience\Data\HospitalDeathTracking.dta
# Alternate URL:  https://1drv.ms/u/s!AgXALnk1NWLck4JBKcMG9lGWFR8TEA?e=jZf1H7

# Convert a couple of the variables we'll be working with to factors
dat <- within(dat, {
  hmo <- factor(hmo)
  died <- factor(died)
})

summary(dat)

# To really get a sense for the data, let's plot some graphs:
# Length of stay with died in facets
ggplot(dat, aes(stay)) +
  geom_histogram() +
  scale_x_log10() +
  facet_grid(hmo ~ died, margins=TRUE, scales="free_y")

# Examine how stay looks across age groups:
ggplot(dat, aes(factor(age), stay)) +
  geom_violin() + # Violin plot is a great choice for this data spread
  geom_jitter(size=1.5) +
  scale_y_log10() +
  stat_smooth(aes(x = age, y = stay, group=1), method="loess")

# Look at live/die *by hmo*
ggplot(dat, aes(age, fill=died)) +
  geom_histogram(binwidth=.5, position="fill") +
  facet_grid(hmo ~ ., margins=TRUE)

# Now let's create a model predicting stay (a count variable) by the other variables in our set
m1 <- vglm(stay ~ age + hmo + died, family = pospoisson(), data = dat)
summary(m1)

# plot of residuals versus fitted values
output <- data.frame(resid = resid(m1), fitted = fitted(m1))
ggplot(output, aes(fitted, resid)) +
  geom_jitter(position=position_jitter(width=.25), alpha=.5) +
  stat_smooth(method="loess")

# Fit lines using quantile regression:
ggplot(output, aes(fitted, resid)) +
  geom_jitter(position=position_jitter(width=.25), alpha=.5) +
  stat_quantile(method="rq")

# Here we see the spread narrowing at higher levels. Let's cut the data into 
# intervals and check box plots for each. We will get the breaks from the algorithm 
# for a histogram.

output <- within(output, {
  broken <- cut(fitted, hist(fitted, plot=FALSE)$breaks)
})

ggplot(output, aes(broken, resid)) +
  geom_boxplot() +
  geom_jitter(alpha=.25)


# First, we get the coefficients from our original model to use as start values for 
# the model to speed up the time it takes to estimate. Then we write a short 
# function that takes data and indices as input and returns the parameters we are 
# interested in. Finally, we pass that to the boot function and do 1200 replicates, 
# using snow to distribute across four cores. 


dput(round(coef(m1),3))
f <- function(data, i) {
  require(VGAM)
  m <- vglm(formula = stay ~ age + hmo + died, family = pospoisson(),
            data = data[i, ], coefstart = c(2.436, -0.014, -0.136, -0.204))
  as.vector(t(coef(summary(m))[, 1:2]))
}

set.seed(10)
res <- boot(dat, f, R = 1200, parallel = "snow", ncpus = 4)

## print results
res

## basic parameter estimates with percentile and bias adjusted CIs
parms <- t(sapply(c(1, 3, 5, 7), function(i) {
  out <- boot.ci(res, index = c(i, i + 1), type = c("perc", "basic"))
  with(out, c(Est = t0, pLL = percent[4], pUL = percent[5],
              basicLL = basic[4], basicLL = basic[5]))
}))

## add row names
row.names(parms) <- names(coef(m1))
## print results
parms

## exponentiated parameter estimates with percentile and bias adjusted CIs
expparms <- t(sapply(c(1, 3, 5, 7), function(i) {
  out <- boot.ci(res, index = c(i, i + 1), type = c("perc", "basic"), h = exp)
  with(out, c(Est = t0, pLL = percent[4], pUL = percent[5],
              basicLL = basic[4], basicLL = basic[5]))
}))

## add row names
row.names(expparms) <- names(coef(m1))
## print results
expparms

# age does not have a significant effect, but hmo and died both do. In order to 
# better understand our results and model, let's plot some predicted values. 
# Because all of our predictors were categorical (hmo and died) or had a small 
# number of unique values (age) we will get predicted values for all possible 
# combinations. First we create a new data set using the expand.grid function, 
# then estimate the predicted values using the predict function, and finally plot them.

newdata <- expand.grid(age = 1:9, hmo = factor(0:1), died = factor(0:1))
newdata$yhat <- predict(m1, newdata, type = "response")

ggplot(newdata, aes(x = age, y = yhat, colour = hmo))  +
  geom_point() +
  geom_line() +
  facet_wrap(~ died)


## function to return predicted values
fpred <- function(data, i, newdata) {
  require(VGAM)
  m <- vglm(formula = stay ~ age + hmo + died, family = pospoisson(),
            data = data[i, ], coefstart = c(2.436, -0.014, -0.136, -0.204))
  predict(m, newdata, type = "response")
}

## set seed and run bootstrap with 1,200 draws
set.seed(10)
respred <- boot(dat, fpred, R = 1200, newdata = newdata,
                parallel = "snow", ncpus = 4)

## get the bootstrapped percentile CIs
yhat <- t(sapply(1:nrow(newdata), function(i) {
  out <- boot.ci(respred, index = i, type = c("perc"))
  with(out, c(Est = t0, pLL = percent[4], pUL = percent[5]))
}))

## merge CIs with predicted values
newdata <- cbind(newdata, yhat)
## graph with CIs
ggplot(newdata, aes(x = age, y = yhat, colour = hmo, fill = hmo))  +
  geom_ribbon(aes(ymin = pLL, ymax = pUL), alpha = .25) +
  geom_point() +
  geom_line() +
  facet_wrap(~ died)




