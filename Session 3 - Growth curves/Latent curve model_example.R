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

###
# -> Which model would you select, and why?

###
# -> Interpret the model parameters


