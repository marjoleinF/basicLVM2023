### Additional excercise LGC

ccawLongG0.c <- c(3.59, 3.11, 3.10, 2.91, 2.80, 2.82, 3.22, 3.05, 2.86, 3.30, 2.88, 2.63, 2.62,
                  2.82, 2.71)
ccawLongG0.mean <- c(11.97 , 11.72 , 12.03 , 11.96 , 12.10)
ccawLongG1.c <- c(3.42, 3.03, 3.18, 2.62, 2.73, 2.69, 2.95, 2.97, 2.59, 3.02, 2.89, 2.91, 2.67,
                  2.83, 3.25)
ccawLongG1.mean <- c(9.80 , 12.00 , 13.94 , 15.96 , 18.10)

# place data in matrices
library(lavaan)
ccawLongG0.cov <- lav_matrix_lower2full(ccawLongG0.c)
ccawLongG1.cov <- lav_matrix_lower2full(ccawLongG1.c)

# name the variables
colnames(ccawLongG0.cov) <- rownames(ccawLongG0.cov) <- paste("T",seq(1,5,1), sep="")
colnames(ccawLongG1.cov) <- rownames(ccawLongG1.cov) <- paste("T",seq(1,5,1), sep="")
names(ccawLongG0.mean) <- paste("T",seq(1,5,1), sep="")
names(ccawLongG1.mean) <- paste("T",seq(1,5,1), sep="")

# combine the data into lists
ccawLong.cov <- list(G0=ccawLongG0.cov, G1=ccawLongG1.cov)
ccawLong.mean <- list(G0=ccawLongG0.mean, G1=ccawLongG1.mean)
ccawLong.n <- list(G0=30,G1=30)


### Now fit the series of models
### What model would you select?
### Interpret the model parameters












# 1. intercept and residual
# 1. intercept and residual
latGrowth0.model <- '
i =~ 1*T1 + 1*T2 + 1*T3 + 1*T4 + 1*T5

# constrain the mean of the latent intercept to be 0.0
i~~0*i
 
# constrain residuals to be the same across time periods
T1~~c(r,r)*T1
T2~~c(r,r)*T2
T3~~c(r,r)*T3
T4~~c(r,r)*T4
T5~~c(r,r)*T5

'
# the growth function automatically fixes the intercept values to zero
# thus simplifying the syntax
# also the group.equal command is used for across groups restrictions
latGrowth0.fit <- growth(latGrowth0.model, 
                         sample.cov=ccawLong.cov, 
                         sample.mean=ccawLong.mean,
                         sample.nobs=ccawLong.n, 
                         group.equal=c("means")) # constrain the means to be equal
summary(latGrowth0.fit)



### just as an example, the full code would look like this:

latGrowth0.model <- '
i =~ 1*T1 + 1*T2 + 1*T3 + 1*T4 + 1*T5
sl =~ 0*T1 + 1*T2 + 2*T3 + 3*T4 + 4*T5

# constrain the variance of the latent intercept to be 0.0
i~~0*i
# constrain the (co)variance of the latent slope to be 0.0
sl ~~ 0*sl + 0*i

# constrain residuals to be the same across time periods
T1~~c(r,r)*T1
T2~~c(r,r)*T2
T3~~c(r,r)*T3
T4~~c(r,r)*T4
T5~~c(r,r)*T5

# means
T1 ~ 0*1
T2 ~ 0*1
T3 ~ 0*1
T4 ~ 0*1
T5 ~ 0*1

# the intercept mean is estimated, but constrained to be equal across groups
i ~ c(baseline,baseline)*1
# the slope mean is restricted to zero
sl ~ 0*1
'
latGrowth0.fit_new <- growth(latGrowth0.model, 
                             sample.cov=ccawLong.cov, 
                             sample.mean=ccawLong.mean,
                             sample.nobs=ccawLong.n)
summary(latGrowth0.fit_new)


# 2. intercept and residual + random intercept
latGrowth1.model <- '
i =~ 1*T1 + 1*T2 + 1*T3 + 1*T4 + 1*T5
T1~~c(r,r)*T1
T2~~c(r,r)*T2
T3~~c(r,r)*T3
T4~~c(r,r)*T4
T5~~c(r,r)*T5
'
latGrowth1.fit <- growth(latGrowth1.model, 
                         sample.cov=ccawLong.cov, 
                         sample.mean=ccawLong.mean,
                         sample.nobs=ccawLong.n, 
                         group.equal=c("means", "lv.variances")) # restrictions on means and variances
summary(latGrowth1.fit)

# 3. intercept and residual + random intercept + random slope and slope-intercept covariance
latGrowth2.model <- '
i =~ 1*T1 + 1*T2 + 1*T3 + 1*T4 + 1*T5
s =~ 0*T1 + 1*T2 + 2*T3 + 3*T4 + 4*T5
# constrain the mean of the latent slope to be 0.0
s ~ 0*1
T1~~c(r,r)*T1
T2~~c(r,r)*T2
T3~~c(r,r)*T3
T4~~c(r,r)*T4
T5~~c(r,r)*T5
'
latGrowth2.fit <- growth(latGrowth2.model, 
                         sample.cov=ccawLong.cov, 
                         sample.mean=ccawLong.mean,
                         sample.nobs=ccawLong.n, 
                         group.equal=c("means", "lv.variances", "lv.covariances")) # add covariance restrictions
summary(latGrowth2.fit)


# 4. intercept and residual + random intercept + random slope and slope-intercept covariance +
# fixed slope
latGrowth3.model <- '
i =~ 1*T1 + 1*T2 + 1*T3 + 1*T4 + 1*T5
s =~ 0*T1 + 1*T2 + 2*T3 + 3*T4 + 4*T5
T1~~c(r,r)*T1
T2~~c(r,r)*T2
T3~~c(r,r)*T3
T4~~c(r,r)*T4
T5~~c(r,r)*T5
'
latGrowth3.fit <- growth(latGrowth3.model, 
                         sample.cov=ccawLong.cov, 
                         sample.mean=ccawLong.mean,
                         sample.nobs=ccawLong.n, 
                         group.equal=c("means", "lv.variances", "lv.covariances"))

summary(latGrowth3.fit)

# 5. intercept and residual + random intercept + random slope and slope-intercept covariance +
# fixed slope + group differences in intercept (intercept x group interaction)
latGrowth4.model <- '
i =~ 1*T1 + 1*T2 + 1*T3 + 1*T4 + 1*T5
s =~ 0*T1 + 1*T2 + 2*T3 + 3*T4 + 4*T5
# constrain the mean of the latent slope to be the same between groups
s~c(sl,sl)*1
T1~~c(r,r)*T1
T2~~c(r,r)*T2
T3~~c(r,r)*T3
T4~~c(r,r)*T4
T5~~c(r,r)*T5
'

latGrowth4.fit <- growth(latGrowth4.model, 
                         sample.cov=ccawLong.cov, 
                         sample.mean=ccawLong.mean,
                         sample.nobs=ccawLong.n, 
                         group.equal=c("lv.variances", "lv.covariances")) # remove restrictions on means
summary(latGrowth4.fit)

# 6. intercept and residual + random intercept + random slope and slope-intercept covariance + 
# fixed slope + group differences in intercept and slope (slope x group interaction)
latGrowth5.model <- '
i =~ 1*T1 + 1*T2 + 1*T3 + 1*T4 + 1*T5
s =~ 0*T1 + 1*T2 + 2*T3 + 3*T4 + 4*T5
T1~~c(r,r)*T1
T2~~c(r,r)*T2
T3~~c(r,r)*T3
T4~~c(r,r)*T4
T5~~c(r,r)*T5
'
latGrowth5.fit <- growth(latGrowth5.model, 
                         sample.cov=ccawLong.cov, 
                         sample.mean=ccawLong.mean,
                         sample.nobs=ccawLong.n, 
                         group.equal=c("lv.variances", "lv.covariances"))
summary(latGrowth5.fit)

anova(latGrowth0.fit,latGrowth1.fit,latGrowth2.fit,latGrowth3.fit) # restrictions with regards to lgc
# -> all random effects improve model fit

anova(latGrowth3.fit,latGrowth4.fit,latGrowth5.fit)
# -> there are mean difference in both intercept and slope across groups