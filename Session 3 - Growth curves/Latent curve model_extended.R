## Lavaan 
library(lavaan)

## observed data
obsnames <- c("year1","year2","year3","year4","gender","famstat")

## This example is taken from Duncan and Duncan (1996), who conducted
## a longitudinal study of alcohol use among adolescents. A sample of 321 adolescents were
## surveyed annually over a 4-year period

values <- c(1.000,
            .640, 1.000,
            .586,	.670, 1.000,
            .454,	.566,	.621, 1.000,
            .001,	.038,	.118,	.091, 1.000,
            -.214, -.149, -.135, -.163, -.025, 1.000)

SD <- c(1.002,.960,.912,.920,.504,.498) 

duncanmeans <- c(2.271,2.560,2.694,2.965,.573,.554) 

duncancor <- lav_matrix_lower2full(values)
duncancov <- diag(SD)%*%duncancor%*%diag(SD)
dimnames(duncancov) <- list(obsnames,obsnames)
names(duncanmeans) <- obsnames


## define latent curve model
duncanmodel1 <- '
			# COVARIANCES
 	    # regression equations
			INTERCEPT =~ 1*year1 + 1*year2 + 1*year3 + 1*year4
			CURVE =~ 0*year1 + 1*year2 + 2*year3 + 3*year4

			# residual variances
			year4 ~~ year4
			year3 ~~ year3
			year2 ~~ year2
			year1 ~~ year1

			# (co)variances common factors
			INTERCEPT ~~ INTERCEPT
			CURVE ~~ CURVE
			INTERCEPT ~~ CURVE

			# MEANS
      # intercepts observed indicators
			year4 ~ 0*1
			year3 ~ 0*1
			year2 ~ 0*1
			year1 ~ 0*1

			# means common factors
			INTERCEPT ~ k1*1
			CURVE ~ k2*1
			'

## run model
duncanmodel1Out <- sem(duncanmodel1, sample.cov=duncancov,
                  sample.mean=duncanmeans, sample.nobs=321)	

## output
summary(duncanmodel1Out, standardized=TRUE)


###
# -> This is the model in which everything is freely estimated
# -> The "unconstrained model" from beaujean (page 82)
# He proposes a series of models, a through e:
# a) only a mean latent intercept (no variance) and residuals restricted to be equal across time
# b) add variance of the intercept to model a
# c) add mean slope to model b
# d) add variance slope to model c, and covariance slope-intercept
# e) add freely estimated residuals to model d (this is the model above)

###
# -> Fit models a through d

## model a
duncanmodel1 <- '
			# COVARIANCES
 	    # regression equations
			INTERCEPT =~ 1*year1 + 1*year2 + 1*year3 + 1*year4
			CURVE =~ 0*year1 + 1*year2 + 2*year3 + 3*year4

			# residual variances
			year4 ~~ r*year4
			year3 ~~ r*year3
			year2 ~~ r*year2
			year1 ~~ r*year1

			# (co)variances common factors
			INTERCEPT ~~ 0*INTERCEPT
			CURVE ~~ 0*CURVE
			INTERCEPT ~~ 0*CURVE

			# MEANS
      # intercepts observed indicators
			year4 ~ 0*1
			year3 ~ 0*1
			year2 ~ 0*1
			year1 ~ 0*1

			# means common factors
			INTERCEPT ~ k1*1
			CURVE ~ 0*1
			'

## run model
duncanmodel1aOut <- sem(duncanmodel1, sample.cov=duncancov,
                        sample.mean=duncanmeans, sample.nobs=321)	

## output
summary(duncanmodel1aOut, standardized=TRUE)

## model b
duncanmodel1 <- '
			# COVARIANCES
 	    # regression equations
			INTERCEPT =~ 1*year1 + 1*year2 + 1*year3 + 1*year4
			CURVE =~ 0*year1 + 1*year2 + 2*year3 + 3*year4

			# residual variances
			year4 ~~ r*year4
			year3 ~~ r*year3
			year2 ~~ r*year2
			year1 ~~ r*year1

			# (co)variances common factors
			INTERCEPT ~~ INTERCEPT
			CURVE ~~ 0*CURVE
			INTERCEPT ~~ 0*CURVE

			# MEANS
      # intercepts observed indicators
			year4 ~ 0*1
			year3 ~ 0*1
			year2 ~ 0*1
			year1 ~ 0*1

			# means common factors
			INTERCEPT ~ k1*1
			CURVE ~ 0*1
			'

## run model
duncanmodel1bOut <- sem(duncanmodel1, sample.cov=duncancov,
                        sample.mean=duncanmeans, sample.nobs=321)	

## output
summary(duncanmodel1bOut, standardized=TRUE)

## model c
duncanmodel1 <- '
			# COVARIANCES
 	    # regression equations
			INTERCEPT =~ 1*year1 + 1*year2 + 1*year3 + 1*year4
			CURVE =~ 0*year1 + 1*year2 + 2*year3 + 3*year4

			# residual variances
			year4 ~~ r*year4
			year3 ~~ r*year3
			year2 ~~ r*year2
			year1 ~~ r*year1

			# (co)variances common factors
			INTERCEPT ~~ INTERCEPT
			CURVE ~~ 0*CURVE
			INTERCEPT ~~ 0*CURVE

			# MEANS
      # intercepts observed indicators
			year4 ~ 0*1
			year3 ~ 0*1
			year2 ~ 0*1
			year1 ~ 0*1

			# means common factors
			INTERCEPT ~ k1*1
			CURVE ~ k2*1
			'

## run model
duncanmodel1cOut <- sem(duncanmodel1, sample.cov=duncancov,
                        sample.mean=duncanmeans, sample.nobs=321)	

## output
summary(duncanmodel1cOut, standardized=TRUE)

## model d
duncanmodel1 <- '
			# COVARIANCES
 	    # regression equations
			INTERCEPT =~ 1*year1 + 1*year2 + 1*year3 + 1*year4
			CURVE =~ 0*year1 + 1*year2 + 2*year3 + 3*year4

			# residual variances
			year4 ~~ r*year4
			year3 ~~ r*year3
			year2 ~~ r*year2
			year1 ~~ r*year1

			# (co)variances common factors
			INTERCEPT ~~ INTERCEPT
			CURVE ~~ CURVE
			INTERCEPT ~~ CURVE

			# MEANS
      # intercepts observed indicators
			year4 ~ 0*1
			year3 ~ 0*1
			year2 ~ 0*1
			year1 ~ 0*1

			# means common factors
			INTERCEPT ~ k1*1
			CURVE ~ k2*1
			'

## run model
duncanmodel1dOut <- sem(duncanmodel1, sample.cov=duncancov,
                       sample.mean=duncanmeans, sample.nobs=321)	

## output
summary(duncanmodel1dOut, standardized=TRUE)

# test the differences between all these models
anova(duncanmodel1aOut,duncanmodel1bOut,duncanmodel1cOut,duncanmodel1dOut,duncanmodel1Out)

###
# -> Which model would you select, and why?
# -> Only the restriction on the equality of residual variances seems tenable; thus model d

###
# -> Interpret the model parameters
# -> There is thus on average a significant change (increase) over time
# -> but also significant individual differences in both baseline values and change values of drinking behavior



## define latent curve models, with predictors
duncanmodel3 <- '
			# MEASUREMENT PART
			# COVARIANCES
			# regression equations
			INTERCEPT =~ 1*year1 + 1*year2 + 1*year3 + 1*year4
			CURVE =~ 0*year1 + 1*year2 + 2*year3 + 3*year4
			
			# residual variances
			year4 ~~ year4
			year3 ~~ year3
			year2 ~~ year2
			year1 ~~ year1

			# MEANS
			# intercepts endogenous variables
			year4 ~ 0*1
			year3 ~ 0*1
			year2 ~ 0*1
			year1 ~ 0*1

			# STRUCTURAL PART
			# COVARIANCES
			# regression equations
			INTERCEPT ~ b31*gender + b41*famstat
			CURVE ~ b32*gender + b42*famstat
			
			# residual (co)variances
			INTERCEPT ~~ INTERCEPT
			CURVE ~~ CURVE
			INTERCEPT ~~ CURVE

			# (co)variances exogenous variables
			gender ~~ gender + famstat
			famstat ~~ famstat

			# MEANS
			# intercepts endogenous variables
			INTERCEPT ~ a1*1
			CURVE ~ a2*1

			# means exogenous variables
			gender ~ a3*1
			famstat ~ a4*1

			# define means observed indicators
			# step 1: common factor means as a function of 
			# path model parameters
			k1 := b31*a3 + b41*a4 + a1
			k2 := b32*a3 + b42*a4 + a2
			k3 := a3
			k4 := a4

			# step 2: observed indicator means as a function of 
			# factor model parameters
			mu1 := k1
			mu2 := k1 + 1*k2
			mu3 := k1 + 2*k2
			mu4 := k1 + 3*k2
			mu5 := k3
			mu6 := k4
			'

## run model
duncanmodel3Out <- sem(duncanmodel3, sample.cov=duncancov,
                  sample.mean=duncanmeans, sample.nobs=321)	

## output
summary(duncanmodel3Out, standardized=TRUE)




### additional models

## define quadratic curve model
duncanmodel4 <- '
			# COVARIANCES
 	            # regression equations
			INTERCEPT =~ 1*year1 + 1*year2 + 1*year3 + 1*year4
			CURVE1 =~ 0*year1 + 1*year2 + 2*year3 + 3*year4
			CURVE2 =~ 0*year1 + 2*year2 + 4*year3 + 9*year4

			# residual variances
			year4 ~~ r*year4
			year3 ~~ r*year3
			year2 ~~ r*year2
			year1 ~~ r*year1

			# (co)variances common factors
			INTERCEPT ~~ INTERCEPT + CURVE1 + CURVE2
			CURVE1 ~~ CURVE1 + CURVE2 
			CURVE2 ~~ CURVE2

			# MEANS
      # intercepts observed indicators
			year4 ~ 0*1
			year3 ~ 0*1
			year2 ~ 0*1
			year1 ~ 0*1

			# means common factors
			INTERCEPT ~ k1*1
			CURVE1 ~ k2*1
			CURVE2 ~ k3*1
			'

## run model
duncanmodel4Out <- sem(duncanmodel4, sample.cov=duncancov,
                       sample.mean=duncanmeans, sample.nobs=321)	

## output
summary(duncanmodel4Out)

anova(duncanmodel1Out,duncanmodel4Out)


